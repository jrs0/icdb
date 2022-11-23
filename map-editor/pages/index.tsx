import Link from 'next/link'

export default function Home() {

    return (
        <div>
            <Link href="/mapping">Mappings</Link>
            <Link href="/icd">ICD-10</Link>

        </div>
    )
}
