const baseUrl = "https://opendengue.org/assets/";

let metadata = [];
let allData = [];
let filteredPreview = [];
let selectedDataType = null;
let selectedRegion = null;

async function loadMetadata() {
  try {
    const response = await fetch(baseUrl + "metadata.json");
    if (!response.ok) throw new Error(`Failed to load metadata.json: ${response.statusText}`);
    metadata = await response.json();
    console.log("Metadata loaded:", metadata.length, "records");
  } catch (error) {
    console.error("Error loading metadata:", error);
    alert("Unable to load metadata. Please try again later.");
  }
}

function updateCountryOptionsFromMetadata(region) {
  const $countrySelect = $('#countrySelect');
  $countrySelect.empty().trigger('change');

  if (!region) return;

  const countriesInRegion = metadata
    .filter(item => item.region === region)
    .map(item => item.adm_0_name);

  const uniqueCountries = [...new Set(countriesInRegion)].sort();

  if (uniqueCountries.length === 0) {
    $countrySelect.append(new Option("[No countries available]", ""));
    return;
  }

  $countrySelect.append(new Option("[Select All]", "__all__"));
  uniqueCountries.forEach(country => {
    $countrySelect.append(new Option(country, country));
  });

  $countrySelect.select2({
    placeholder: "Select countries...",
    closeOnSelect: false,
    allowClear: true,
    width: '100%'
  });

  $countrySelect.off('select2:select').on('select2:select', function (e) {
    if (e.params.data.id === "__all__") {
      const allValues = uniqueCountries;
      $countrySelect.val(allValues).trigger('change');
    }
  });

  const selectedValues = $countrySelect.val();
  if (selectedValues && selectedValues.includes("__all__")) {
    $countrySelect.val(uniqueCountries).trigger('change');
  }
}

function getZipFileName(dataType, region) {
  return `${dataType}_extract_${region}_V1_3.zip`;
}

function convertToCSV(data) {
  return Papa.unparse(data);
}

function downloadCSV(csvString, filename) {
  const blob = new Blob([csvString], { type: 'text/csv;charset=utf-8;' });
  const link = document.createElement("a");
  const url = URL.createObjectURL(blob);
  link.setAttribute("href", url);
  link.setAttribute("download", filename);
  link.style.visibility = 'hidden';
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  URL.revokeObjectURL(url);
}

async function loadAndParseZip(dataType, region) {
  if (!dataType || !region) return [];

  const zipFileName = getZipFileName(dataType, region);
  const url = baseUrl + zipFileName;
  console.log(`Fetching ZIP: ${url}`);

  try {
    const response = await fetch(url);
    if (!response.ok) {
      alert(`Unable to fetch ZIP file: ${zipFileName}`);
      return [];
    }

    const arrayBuffer = await response.arrayBuffer();
    const zip = await JSZip.loadAsync(arrayBuffer);

    const csvFileName = Object.keys(zip.files).find(name => name.endsWith(".csv"));
    if (!csvFileName) {
      alert("No CSV file found in ZIP.");
      return [];
    }

    const csvText = await zip.file(csvFileName).async("text");
    const parsed = Papa.parse(csvText, { header: true, skipEmptyLines: true });

    if (parsed.errors.length) {
      console.warn("CSV parse errors:", parsed.errors);
    }

    return parsed.data;
  } catch (error) {
    console.error("Error loading or parsing ZIP:", error);
    alert("Error loading or parsing ZIP file.");
    return [];
  }
}

function filterData(data, countries, startDate, endDate) {
  if (!countries.length || !startDate || !endDate) return [];

  const start = new Date(startDate);
  const end = new Date(endDate);

  return data.filter(row => {
    if (!row.adm_0_name || !row.calendar_start_date || !row.calendar_end_date) return false;
    if (!countries.includes(row.adm_0_name)) return false;

    const rowStart = new Date(row.calendar_start_date);
    const rowEnd = new Date(row.calendar_end_date);

    return (rowStart <= end) && (rowEnd >= start);
  });
}

function renderPreviewTable(data) {
  if ($.fn.dataTable.isDataTable('#previewTable')) {
    $('#previewTable').DataTable().clear().destroy();
  }

  $('#previewTable').DataTable({
    data,
    columns: [
      { title: "Country", data: "adm_0_name" },
      { title: "Admin 1", data: "adm_1_name" },
      { title: "Admin 2", data: "adm_2_name" },
      { title: "Date Start", data: "calendar_start_date" },
      { title: "Date End", data: "calendar_end_date" },
      { title: "Temporal Res", data: "T_res" },
      { title: "Spatial Res", data: "S_res" },
      { title: "Dengue cases", data: "dengue_total" }
    ],
    pageLength: 10,
    lengthChange: false,
    searching: false
  });
}

function computeDateRangeFromMetadata(region, countries) {
  const filteredMeta = metadata.filter(item =>
    item.region === region && countries.includes(item.adm_0_name)
  );

  if (!filteredMeta.length) return [null, null];

  const startDates = filteredMeta.map(r => new Date(r.start_date));
  const endDates = filteredMeta.map(r => new Date(r.end_date));

  const minStart = new Date(Math.min(...startDates));
  const maxEnd = new Date(Math.max(...endDates));

  return [minStart.toISOString().slice(0, 10), maxEnd.toISOString().slice(0, 10)];
}

function showLoading() {
  document.getElementById("loadingIndicator").style.display = "block";
}

function hideLoading() {
  document.getElementById("loadingIndicator").style.display = "none";
}

function aggregateData(data, dateKey) {
  const aggregation = {};
  data.forEach(row => {
    const date = row[dateKey];
    const value = parseFloat(row.dengue_total) || 0;
    aggregation[date] = (aggregation[date] || 0) + value;
  });

  const dates = Object.keys(aggregation).sort();
  const totals = dates.map(date => aggregation[date]);
  return { dates, totals };
}

function preparePlotData(data, resolution) {
  const resolutionMap = { weekly: "Week", monthly: "Month", yearly: "Year" };
  const filtered = data.filter(d => d.T_res === resolutionMap[resolution]);
  return aggregateData(filtered, "calendar_start_date");
}

function renderBarPlot(divId, title, plotData) {
  const container = document.getElementById(divId);
  if (!plotData.dates.length) {
    container.innerHTML = '<p style="text-align:center; color:#888;">No data available</p>';
    return;
  }

  container.innerHTML = "";

  const colours = {
    'plot-weekly': '#e76f51',
    'plot-monthly': '#2a9d8f',
    'plot-yearly': '#f4a261'
  };

  const getWeekNumber = d => {
    d = new Date(Date.UTC(d.getFullYear(), d.getMonth(), d.getDate()));
    const dayNum = d.getUTCDay() || 7;
    d.setUTCDate(d.getUTCDate() + 4 - dayNum);
    const yearStart = new Date(Date.UTC(d.getUTCFullYear(), 0, 1));
    return Math.ceil((((d - yearStart) / 86400000) + 1) / 7);
  };

  const hoverText = plotData.dates.map(dateStr => {
    const date = new Date(dateStr);
    switch (divId) {
      case 'plot-weekly': return `W${getWeekNumber(date)} ${date.getFullYear()}`;
      case 'plot-monthly': return date.toLocaleDateString('en-GB', { month: 'short', year: 'numeric' });
      case 'plot-yearly': return date.getFullYear().toString();
      default: return dateStr;
    }
  });

  const tickVals = [];
  const tickText = [];
  const seenYears = new Set();
  plotData.dates.forEach(dateStr => {
    const year = new Date(dateStr).getFullYear();
    if (!seenYears.has(year)) {
      seenYears.add(year);
      tickVals.push(dateStr);
      tickText.push(year.toString());
    }
  });

  const trace = {
    x: plotData.dates,
    y: plotData.totals,
    type: 'bar',
    marker: {
      color: colours[divId] || '#264653',
      line: { width: 1.0, color: '#264653' }
    },
    hovertext: hoverText,
    hoverinfo: 'x+y+text',
    text: hoverText,
  };

  const layout = {
    height: 300,
    title: { text: title, font: { size: 18 } },
    margin: { t: 30, r: 20, b: 50, l: 50 },
    xaxis: {
      title: '',
      tickvals: tickVals,
      ticktext: tickText,
      tickangle: -45,
    },
    yaxis: { title: 'Number of dengue cases' },
    plot_bgcolor: '#f7f9f9',
    paper_bgcolor: '#f7f9f9'
  };

  Plotly.newPlot(divId, [trace], layout, { responsive: true });
}

function updatePlots(filteredData) {
  ['plot-weekly', 'plot-monthly', 'plot-yearly'].forEach(id => {
    const plotData = preparePlotData(filteredData, id.split('-')[1]);
    renderBarPlot(id, `${id.split('-')[1].charAt(0).toUpperCase()}${id.split('-')[1].slice(1)} resolution`, plotData);
  });
}

document.addEventListener("DOMContentLoaded", async () => {
  await loadMetadata();

  const dataTypeSelect = document.getElementById("dataTypeSelect");
  const regionSelect = document.getElementById("regionSelect");
  const countrySelect = document.getElementById("countrySelect");
  const startDateInput = document.getElementById("startDate");
  const endDateInput = document.getElementById("endDate");
  const filterBtn = document.getElementById("filterBtn");
  const downloadBtn = document.getElementById("downloadBtn");
  const loadingIndicator = document.getElementById("loadingIndicator");

  regionSelect.addEventListener("change", () => {
    selectedRegion = regionSelect.value;
    updateCountryOptionsFromMetadata(selectedRegion);
    startDateInput.value = endDateInput.value = "";
    allData = filteredPreview = [];
    renderPreviewTable([]);
    updatePlots([]);
  });

  dataTypeSelect.addEventListener("change", () => {
    selectedDataType = dataTypeSelect.value;
    allData = filteredPreview = [];
    renderPreviewTable([]);
    updatePlots([]);
  });

  $('#countrySelect').on('change', () => {
    const selectedValues = $('#countrySelect').val() || [];
    if (selectedValues.includes("__all__")) {
      const allCountries = Array.from(countrySelect.options)
        .map(opt => opt.value)
        .filter(v => v !== "__all__");
      $('#countrySelect').val(allCountries).trigger('change');
      return;
    }

    const [minStart, maxEnd] = computeDateRangeFromMetadata(selectedRegion, selectedValues);
    if (minStart && maxEnd) {
      startDateInput.min = minStart;
      startDateInput.max = maxEnd;
      endDateInput.min = minStart;
      endDateInput.max = maxEnd;
      startDateInput.value = minStart;
      endDateInput.value = maxEnd;
    } else {
      startDateInput.value = endDateInput.value = "";
    }
  });

  filterBtn.addEventListener("click", () => {
  const selectedCountries = Array.from(countrySelect.selectedOptions).map(opt => opt.value);
  const startDate = startDateInput.value;
  const endDate = endDateInput.value;

  if (!selectedDataType || !selectedRegion) {
    alert("Please select Data Type and Region.");
    return;
  }
  if (!selectedCountries.length) {
    alert("Please select at least one country.");
    return;
  }
  if (!startDate || !endDate) {
    alert("Please select a valid start and end date.");
    return;
  }
  if (startDate > endDate) {
    alert("Start Date cannot be after End Date.");
    return;
  }

  filterBtn.disabled = true;
  if (loadingIndicator) loadingIndicator.style.display = 'block';

  // Allow DOM to update and show spinner before async work
  requestAnimationFrame(async () => {
    allData = await loadAndParseZip(selectedDataType, selectedRegion);
    if (!allData.length) {
      filterBtn.disabled = false;
      if (loadingIndicator) loadingIndicator.style.display = 'none';
      return;
    }

    filteredPreview = filterData(allData, selectedCountries, startDate, endDate);
    if (!filteredPreview.length) {
      alert("No data matches your filter criteria.");
    }

    renderPreviewTable(filteredPreview);
    updatePlots(filteredPreview);

    downloadBtn.onclick = () => {
      if (!filteredPreview.length) {
        alert("No data to download.");
        return false;
      }
      const filename = `filtered_data_${selectedRegion}_${Date.now()}.csv`;
      downloadCSV(convertToCSV(filteredPreview), filename);
      return false;
    };

    filterBtn.disabled = false;
    if (loadingIndicator) loadingIndicator.style.display = 'none';
  });
});
});
