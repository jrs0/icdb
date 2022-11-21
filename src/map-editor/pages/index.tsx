function Column(col) {
    return (
        col
    )
}

function Table(tab) {
    return (
        tab
    )
}

export default function Home() {

    let def = [
        {
            name: "apc",
            source: "ABI.dbo.vw_APC_SEM_001",
            docs: "The SUS database contains hospital episode statistics, which contain records of patient care activities that take place in a hospital visit.",
            columns: {
                nhs_number: {
                    docs: "The NHS number identifies the patient. The field is anonymised.",
                    source_columns: [
                        {
                            "AIMTC_Pseudo_NHS": ""
                        },
                        {
                            "NHSnumber": "This one looks like it is NULL everywhere. Also, this is an INT column"
                        }
                    ],
                    strategy: "coalesce_exclude_null",
                    use: "yes"
                }
            }
        },
        {
            name: "attr",
            docs: "The SWD attributes table contains records of patient attributes at the latest snapshot through the year (monthly updates), PLUS a few other measures such as 'air quality' that is NOT present in the activity history table (these additional measures is the only reason to keep a separate reference to this table, otherwise we could have just filter the activity history table by most recent).",
            source: "MODELLING_SQL_AREA.dbo.swd_attribute",
            columns: {
                nhs_number: {
                    docs: "The NHS number identifies the patient. The field is anonymised.",
                    source_columns: [
                        {
                            "AIMTC_Pseudo_NHS": ""
                        },
                        {
                            "NHSnumber": "This one looks like it is NULL everywhere. Also, this is an INT column"
                        }
                    ],
                    strategy: "coalesce_exclude_null",
                    use: "yes"
                }
            }
        },
    ]


    return (
        <div>
            <ul>
                {def.map((tab) => (
                    <li key={tab.name}>
                        {tab.name}

                    </li>
                ))}
            </ul>
        </div>
    )
}
