SELECT T.*, BM.Species, W.DeployID IS NOT NULL AS "Wet", COUNT(D.DiveID) AS "Dives"
FROM RediscretizedTrack T
	JOIN BirdMetadata BM
		ON T.DeployID = BM.DeployID
	LEFT JOIN WetDry W
		ON T.DeployID = W.DeployID
			AND W.End > T.UTC
			AND W.Begin < T.UTC + 120
	LEFT JOIN Dive D
		ON T.DeployID = D.DeployID
			AND D.End > T.UTC
			AND D.Begin < T.UTC + 120
WHERE T.DeployID = 940
	AND T.TripID = 3
GROUP BY T.DeployID, T.TripID, T.UTC;
