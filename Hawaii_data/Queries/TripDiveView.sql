CREATE VIEW TripDive AS
-- SQLite workaround for FULL OUTER JOIN of trips and dives
SELECT T.DeployID "DeployID", T.TripID "TripID", D.DiveID "DiveID"
FROM Trip T LEFT JOIN 
	Dive D ON
		T.DeployID = D.DeployID AND
		T.Begin <= D.Begin AND
		T.End > D.Begin
UNION ALL
SELECT D.DeployID "DeployID", T.TripID "TripID", D.DiveID "DiveID"
FROM Dive D LEFT JOIN
	Trip T ON
		T.DeployID = D.DeployID AND
		T.Begin <= D.Begin AND
		T.End > D.Begin
WHERE T.DeployID IS NULL
ORDER BY DeployID, TripID, DiveID
		