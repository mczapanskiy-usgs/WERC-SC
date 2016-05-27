SELECT DISTINCT Tk.DeployID, Tk.TripID, Tk.Longitude, Tk.Latitude, BM.Species
FROM WetDry W
	JOIN BirdMetadata BM
		ON W.DeployID = BM.DeployID
	JOIN RediscretizedTrack Tk
		ON Tk.DeployID = W.DeployID
			AND Tk.UTC < W.End
			AND Tk.UTC + 120 >= W.Begin
	JOIN Trip Tp
		ON Tp.DeployID = W.DeployID
			AND Tp.TripID = Tk.TripID
WHERE BM.Species IN ("BRBO", "RFBO") 
	AND BM.SubColonyCode = "LEH"
	AND Tp.BeginComplete = 1
	AND Tp.EndComplete = 1
	AND Tp.End - Tp.Begin > 3600