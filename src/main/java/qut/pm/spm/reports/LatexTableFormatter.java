package qut.pm.spm.reports;

public class LatexTableFormatter extends ReportFormatter{
	private static final String CELL_MARKER = " & ";

	public void startReport() {
		reportBuffer.append("\\begin{tabular}");
	}
	
	public void reportHeader(String ...cells ) {
		reportHeader( reportHeaderRowTypes(cells), cells);
	}

	public void reportHeader(String rowTypes, String ...cells ) {
		reportBuffer.append("{");
		reportBuffer.append(rowTypes);
		reportBuffer.append("}");
		reportBuffer.append(SPNDiscoverReporter.LINE_SEP);
		reportBuffer.append("\\hline");
		reportBuffer.append(SPNDiscoverReporter.LINE_SEP);
		reportCells(cells);
		removeLastCellMarker();
		reportBuffer.append("\\\\ ");
		reportBuffer.append("\\hline");
		reportBuffer.append(SPNDiscoverReporter.LINE_SEP);		
	}
	
	private String reportHeaderRowTypes(String... cells) {
		StringBuffer resultBuffer = new StringBuffer();
		resultBuffer.append("| l");
		for (int i = 1; i < cells.length; i++) {
			resultBuffer.append(" c");
		}
		resultBuffer.append("| }");
		return resultBuffer.toString();
	}
	
	
	public void reportCells(String ...cells ) {
		for (String cell: cells) {
			reportBuffer.append(cell.replace("_", "\\_"));
			reportCellBreak();
		}		
	}
	
	public void reportStartLine() {
	}
	
	public void reportNewline() {
		removeLastCellMarker();
		reportBuffer.append("\\\\ ");
		reportBuffer.append(SPNDiscoverReporter.LINE_SEP);
	}

	private void removeLastCellMarker() {
		reportBuffer.setLength( reportBuffer.length() - CELL_MARKER.length() );
	}

	public void reportCellBreak() {
		reportBuffer.append(CELL_MARKER);
	}
	
	public void endReport(){
		reportBuffer.append("\\hline");
		reportBuffer.append(SPNDiscoverReporter.LINE_SEP);
		reportBuffer.append("\\end{tabular}%");
	}

}