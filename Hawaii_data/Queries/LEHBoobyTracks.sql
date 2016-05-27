SELECT Tk.*, BM.Species
FROM Track Tk
	JOIN BirdMetadata BM
		ON Tk.DeployID = BM.DeployID
	JOIN Trip Tp
		ON Tk.DeployID = Tp.DeployID
			AND Tk.TripID = Tp.TripID
WHERE BM.Species IN ("BRBO", "RFBO") 
	AND BM.SubColonyCode = "LEH"
	AND Tp.BeginComplete = 1
	AND Tp.EndComplete = 1
	AND Tp.End - Tp.Begin > 3600
	
	