*          DATA SET PRVAL2     AT LEVEL 200 AS OF 01/28/21                      
*PHASE T00A43A                                                                  
*              'PRVAL2 - PRINT WRITER TABLES'                                   
         TITLE 'PRVAL2 - PRINT WRITER TABLES'                                   
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* KWAN SPEC-52236  01/12/21 TSRECVD KEYWORD                           *         
* MZEI SPEC-44630  09/21/20 COS2BLB KEYWORD                           *         
* AKAT SPEC-5972   09/10/20 BDNC2 KEYWORD                             *         
* KWAN SPEC-20445  07/12/18 AD CODE BILL CONTACT (ADBCON) KEYWORD     *         
* KWAN SPEC-16356  03/06/18 RATE TYPE (RATETYPE) KEYWORD              *         
* KWAN SPEC-13046  09/14/17 SPECIAL REP (SREP/SREPCD/SREPNM) KEYWORD  *         
* AKAT SPEC-13     07/15/16 PUBLOCK KEYWORD                           *         
***********************************************************************         
PRVAL2   CSECT                                                                  
         PRINT NOGEN                                                            
         DC    AL2(WRITTBL-PRVAL2)  01     WRITER KEYWORDS                      
         DC    AL2(CFLTTBL-PRVAL2)  02     WRITER COLUMN FILTER/FORMATS         
         DC    AL2(0)               03                                          
         TITLE 'PRVAL2 - TABLE PHASE - 01 - WRITER KEYWORD FIELDS'              
***********************************************************************         
*                                                                     *         
* (01)   WRITER KEYWORD TABLES                                        *         
*        INTERNAL CODES COME FROM PRWRIEQUS                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
WRIT     PWRTB TYPE=INITIAL,PREFIX=PK                                           
*                                                                               
$BLAC    PWRTB NAME=(,,,$BLBAC),        BUY BILLABLE AGENCY COMMISSION X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BLCD    PWRTB NAME=(,,,$BLBCD),        BUY BILLABLE CASH DISCOUNT     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BLG     PWRTB NAME=(,,,$BLBG),         BUY BILLABLE GROSS             X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BLGC    PWRTB NAME=(,,,$BLBGCD),       BUY BILLABLE GROSS LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BLN     PWRTB NAME=(,,,$BLBN),         BUY BILLABLE NET               X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BLNC    PWRTB NAME=(,,,$BLBNCD),       BUY BILLABLE NET LESS CD       X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BLTX    PWRTB NAME=(,,,$BLBTAX),       BUY BILLABLE TAX               X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BDAC    PWRTB NAME=(,,,$BLDAC),        BUY BILLED AGENCY COMMISSION   X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BDCD    PWRTB NAME=(,,,$BLDCD),        BUY BILLED CASH DISCOUNT       X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BDG     PWRTB NAME=(,,,$BLDG),         BUY BILLED GROSS               X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BDGC    PWRTB NAME=(,,,$BLDGCD),       BUY BILLED GROSS LESS CD       X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BDN     PWRTB NAME=(,,,$BLDN),         BUY BILLED NET                 X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BDNC    PWRTB NAME=(,,,$BLDNCD),       BUY BILLED NET LESS CD         X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BDTX    PWRTB NAME=(,,,$BLDTAX),       BUY BILLED TAX                 X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$CST     PWRTB NAME=(,,,$COST),         CLIENT BILLING COST            X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$CSBD    PWRTB NAME=(,,,$COSTBIL),      CLIENT BILLED  COST            X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$CSBL    PWRTB NAME=(,,,$COSTBLB),      CLIENT BILLABLE COST           X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CS2BL    PWRTB NAME=(,,,COS2BLB),       CLIENT BILLABLE COST2          X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$CSNT    PWRTB NAME=(,,,$COSTNTX),      CLIENT BILLING COST WO TAX     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BPAC    PWRTB NAME=(,,,$DBPAC),        BUY BLD - PD AGENCY COMMISSION X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$,PAY),                                    X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BPCD    PWRTB NAME=(,,,$DBPCD),        BUY BLD - PD CASH DISCOUNT     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$,PAY),                                    X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BPG     PWRTB NAME=(,,,$DBPG),         BUY BLD - PD GROSS             X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$,PAY),                                    X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BPGC    PWRTB NAME=(,,,$DBPGCD),       BUY BLD - PD GROSS LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$,PAY),                                    X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BPN     PWRTB NAME=(,,,$DBPN),         BUY BLD - PD NET               X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$,PAY),                                    X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$BPNC    PWRTB NAME=(,,,$DBPNCD),       BUY BLD - PD NET LESS CD       X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$,PAY),                                    X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$GPC     PWRTB NAME=(,,,$G-PC),         GROSS LESS PLANNED COST        X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OLAC    PWRTB NAME=(,,,$OBLBAC),       BUY OPEN BLB AGENCY COMMISSION X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OLCD    PWRTB NAME=(,,,$OBLBCD),       BUY OPEN BLB CASH DISCOUNT     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OLG     PWRTB NAME=(,,,$OBLBG),        BUY OPEN BLB GROSS             X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OLGC    PWRTB NAME=(,,,$OBLBGCD),      BUY OPEN BLB GROSS LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OLN     PWRTB NAME=(,,,$OBLBN),        BUY OPEN BLB NET               X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OLNC    PWRTB NAME=(,,,$OBLBNCD),      BUY OPEN BLB NET LESS CD       X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OBAC    PWRTB NAME=(,,,$OBLDAC),       BUY OPEN BLD AGENCY COMMISSION X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OBCD    PWRTB NAME=(,,,$OBLDCD),       BUY OPEN BLD CASH DISCOUNT     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OBG     PWRTB NAME=(,,,$OBLDG),        BUY OPEN BLD GROSS             X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OBGC    PWRTB NAME=(,,,$OBLDGCD),      BUY OPEN BLD GROSS LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OBN     PWRTB NAME=(,,,$OBLDN),        BUY OPEN BLD NET               X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OBNC    PWRTB NAME=(,,,$OBLDNCD),      BUY OPEN BLD NET LESS CD       X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OOAC    PWRTB NAME=(,,,$OORDAC),       BUY OPEN ORD AGENCY COMMISSION X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OOCD    PWRTB NAME=(,,,$OORDCD),       BUY OPEN ORD CASH DISCOUNT     X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OOG     PWRTB NAME=(,,,$OORDG),        BUY OPEN ORD GROSS             X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OOGC    PWRTB NAME=(,,,$OORDGCD),      BUY OPEN ORD GROSS LESS CD     X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OON     PWRTB NAME=(,,,$OORDN),        BUY OPEN ORD NET               X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OONC    PWRTB NAME=(,,,$OORDNCD),      BUY OPEN ORD NET LESS CD       X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$OOTX    PWRTB NAME=(,,,$OORDTAX),      BUY OPEN ORD TAX               X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=N,                                         X        
               FLAVORM=BUY                                                      
$ODAC    PWRTB NAME=(,,,$ORDAC),        BUY ORDERED AGENCY COMMISSION  X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$ODCD    PWRTB NAME=(,,,$ORDCD),        BUY ORDERED CASH DISCOUNT      X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$ODG     PWRTB NAME=(,,,$ORDG),         BUY ORDERED GROSS              X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$ODGC    PWRTB NAME=(,,,$ORDGCD),       BUY ORDERED GROSS LESS CD      X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$ODGS    PWRTB NAME=(,,,$ORDGST),       BUY ORDERED GST                X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$ODN     PWRTB NAME=(,,,$ORDN),         BUY ORDERED NET                X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$ODNC    PWRTB NAME=(,,,$ORDNCD),       BUY ORDERED NET LESS CD        X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$ODPS    PWRTB NAME=(,,,$ORDPST),       BUY ORDERED PST                X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$ODTX    PWRTB NAME=(,,,$ORDTAX),       BUY ORDERED TAX                X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PYAC    PWRTB NAME=(,,,$PAYAC),        BUY PAYABLE AGENCY COMMISSION  X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PYCD    PWRTB NAME=(,,,$PAYCD),        BUY PAYABLE CASH DISCOUNT      X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PYG     PWRTB NAME=(,,,$PAYG),         BUY PAYABLE GROSS              X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PYGC    PWRTB NAME=(,,,$PAYGCD),       BUY PAYABLE GROSS LESS CD      X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PYGS    PWRTB NAME=(,,,$PAYGST),       BUY PAYABLE GST                X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PYN     PWRTB NAME=(,,,$PAYN),         BUY PAYABLE NET                X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PYNT    PWRTB NAME=(,,,$PAYNET),       BUY PAYABLE NET                X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=N,                                         X        
               FLAVORM=BUY                                                      
$PYNC    PWRTB NAME=(,,,$PAYNCD),       BUY PAYABLE NET LESS CD        X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PYPS    PWRTB NAME=(,,,$PAYPST),       BUY PAYABLE PST                X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PYTX    PWRTB NAME=(,,,$PAYTAX),       BUY PAYABLE TAX                X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PC      PWRTB NAME=(,,,$PC),           PLANNED COST                   X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PDAC    PWRTB NAME=(,,,$PDAC),         BUY PAID AGENCY COMMISSION     X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PDCD    PWRTB NAME=(,,,$PDCD),         BUY PAID CASH DISCOUNT         X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PDG     PWRTB NAME=(,,,$PDG),          BUY PAID GROSS                 X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PDGC    PWRTB NAME=(,,,$PDGCD),        BUY PAID GROSS LESS CD         X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PDGS    PWRTB NAME=(,,,$PDGST),        BUY PAID GST                   X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PDN     PWRTB NAME=(,,,$PDN),          BUY PAID NET                   X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PDNC    PWRTB NAME=(,,,$PDNCD),        BUY PAID NET LESS CD           X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PDPS    PWRTB NAME=(,,,$PDPST),        BUY PAID PST                   X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$PDTX    PWRTB NAME=(,,,$PDTAX),        BUY PAID TAX                   X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
$RLAC    PWRTB NAME=(,,,$RBLBAC),       BUY REBATE BLB AGENCY COMMISSI X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RLCD    PWRTB NAME=(,,,$RBLBCD),       BUY REBATE BLB CASH DISCOUNT   X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RLG     PWRTB NAME=(,,,$RBLBG),        BUY REBATE BLB GROSS           X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RLGC    PWRTB NAME=(,,,$RBLBGCD),      BUY REBATE BLB GROSS LESS CD   X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RLN     PWRTB NAME=(,,,$RBLBN),        BUY REBATE BLB NET LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RLNC    PWRTB NAME=(,,,$RBLBNCD),      BUY REBATE BLB NET LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RBAC    PWRTB NAME=(,,,$RBLDAC),       BUY REBATE BLD AGENCY COMMISSI X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RBCD    PWRTB NAME=(,,,$RBLDCD),       BUY REBATE BLD CASH DISCOUNT   X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RBG     PWRTB NAME=(,,,$RBLDG),        BUY REBATE BLD GROSS           X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RBGC    PWRTB NAME=(,,,$RBLDGCD),      BUY REBATE BLD GROSS LESS CD   X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RBN     PWRTB NAME=(,,,$RBLDN),        BUY REBATE BLD NET LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RBNC    PWRTB NAME=(,,,$RBLDNCD),      BUY REBATE BLD NET LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$ROCD    PWRTB NAME=(,,,$RORDCD),       BUY REBATE ORD CASH DISCOUNT   X        
               AORM=N,                                                 X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$ROG     PWRTB NAME=(,,,$RORDG),        BUY REBATE ORD GROSS           X        
               AORM=N,                                                 X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$ROGC    PWRTB NAME=(,,,$RORDGCD),      BUY REBATE ORD GROSS LESS CD   X        
               AORM=N,                                                 X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$ROGS    PWRTB NAME=(,,,$RORDGST),      BUY REBATE ORD GST             X        
               AORM=N,                                                 X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RON     PWRTB NAME=(,,,$RORDN),        BUY REBATE ORD NET LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$RONC    PWRTB NAME=(,,,$RORDNCD),      BUY REBATE ORD NET LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$ROTX    PWRTB NAME=(,,,$RORDTAX),      BUY REBATE ORD TAX             X        
               AORM=N,                                                 X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
$TAX     PWRTB NAME=(,,,$TAX),          BUY ORDERED TAX                X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
BDACH    PWRTB NAME=(,,,BDACHG),        BILL ELEMENT ADDL CHG CODE     X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
BDCD     PWRTB NAME=(,,,BDCD),          BILL ELEMENT CASH DISCOUNT     X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
BDG      PWRTB NAME=(,,,BDG),           BILL ELEMENT GROSS             X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
BDGC     PWRTB NAME=(,,,BDGCD),         BILL ELEMENT GROSS LESS CD     X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
BDGS     PWRTB NAME=(,,,BDGST),         BILL ELEMENT GST               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=DD,                                        X        
               FLAVORM=BUY                                                      
BDN      PWRTB NAME=(,,,BDN),           BILL ELEMENT NET               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
BDNC     PWRTB NAME=(,,,BDNCD),         BILL ELEMENT NET LESS CD       X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
*                                                                               
BDNC2    PWRTB NAME=(,,,BDNC2),         BILL ELEMENT COST2 NET         X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDPO#   - PURCHASE ORDER NUMBER                                         
*                                                                               
BDPO#    PWRTB NAME=(,,,BDPO#),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BDPOPER - PURCHASE ORDER PERIOD                                         
*                                                                               
BDPOP    PWRTB NAME=(,,,BDPOPER),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BDPOSTA - PURCHASE ORDER STATUS                                         
*                                                                               
BDPOS    PWRTB NAME=(,,,BDPOSTA),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BDPOSQN - PURCHASE ORDER SEQUENCE NUMBER                                
*                                                                               
BDPOQ    PWRTB NAME=(,,,BDPOSQN),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
BDPS     PWRTB NAME=(,,,BDPST),         BILL ELEMENT PST               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=DD,                                        X        
               FLAVORM=BUY                                                      
BDTX     PWRTB NAME=(,,,BDTAX),         BILL ELEMENT TAX               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=DD,                                        X        
               FLAVORM=BUY                                                      
CLCD     PWRTB NAME=(,,,CLCD),          CLEARED      CASH DISCOUNT     X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CLG      PWRTB NAME=(,,,CLG),           CLEARED      GROSS             X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CLGC     PWRTB NAME=(,,,CLGCD),         CLEARED      GROSS LESS CD     X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CLGS     PWRTB NAME=(,,,CLGST),         CLEARED      GST               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CLN      PWRTB NAME=(,,,CLN),           CLEARED      NET               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CLNC     PWRTB NAME=(,,,CLNCD),         CLEARED      NET LESS CD       X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CLPS     PWRTB NAME=(,,,CLPST),         CLEARED      PST               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CLTX     PWRTB NAME=(,,,CLTAX),         CLEARED      TAX               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=DD,                                        X        
               FLAVORM=BUY                                                      
C2LAC    PWRTB NAME=(,,,C2BLBAC),       COST2 BLB AGENCY COMMISSION    X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2LCD    PWRTB NAME=(,,,C2BLBCD),       COST2 BLB CASH DISCOUNT        X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2LG     PWRTB NAME=(,,,C2BLBG),        COST2 BLB GROSS                X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2LGC    PWRTB NAME=(,,,C2BLBGCD),      COST2 BLB GROSS LESS CD        X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2LN     PWRTB NAME=(,,,C2BLBN),        COST2 BLB NET                  X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2LNC    PWRTB NAME=(,,,C2BLBNCD),      COST2 BLB NET LESS CD          X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2BAC    PWRTB NAME=(,,,C2BLDAC),       COST2 BLD AGENCY COMMISSION    X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2BCD    PWRTB NAME=(,,,C2BLDCD),       COST2 BLD CASH DISCOUNT        X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2BG     PWRTB NAME=(,,,C2BLDG),        COST2 BLD GROSS                X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2BGC    PWRTB NAME=(,,,C2BLDGCD),      COST2 BLD GROSS LESS CD        X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2BN     PWRTB NAME=(,,,C2BLDN),        COST2 BLD NET                  X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2BNC    PWRTB NAME=(,,,C2BLDNCD),      COST2 BLD NET LESS CD          X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2OAC    PWRTB NAME=(,,,C2ORDAC),       COST2 ORD AGENCY COMMISSION    X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2OCD    PWRTB NAME=(,,,C2ORDCD),       COST2 ORD CASH DISCOUNT        X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2OG     PWRTB NAME=(,,,C2ORDG),        COST2 ORD GROSS                X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2OGC    PWRTB NAME=(,,,C2ORDGCD),      COST2 ORD GROSS LESS CD        X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2ON     PWRTB NAME=(,,,C2ORDN),        COST2 ORD NET                  X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2ONC    PWRTB NAME=(,,,C2ORDNCD),      COST2 ORD NET LESS CD          X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
C2OTX    PWRTB NAME=(,,,C2ORDTAX),      COST2 ORD TAX                  X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=N,                                         X        
               FLAVORM=BUY                                                      
CPACT    PWRTB NAME=(,,,CPMACT),        CPM - ACTUAL                   X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CPEST    PWRTB NAME=(,,,CPMEST),        CPM - ESTIMATE                 X        
               MENUM=(BY$),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HBDAC    PWRTB NAME=(,,,HBLDAC),        HIST BILLED  AGENCY COMMISSION X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HBDCD    PWRTB NAME=(,,,HBLDCD),        HIST BILLED  CASH DISCOUNT     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HBDG     PWRTB NAME=(,,,HBLDG),         HIST BILLED  GROSS             X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HBDGC    PWRTB NAME=(,,,HBLDGCD),       HIST BILLED  GROSS LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HBDN     PWRTB NAME=(,,,HBLDN),         HIST BILLED  NET               X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HBDNC    PWRTB NAME=(,,,HBLDNCD),       HIST BILLED  NET LESS CD       X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HOBG     PWRTB NAME=(,,,HOBLDG),        HIST OPEN BLD GROSS            X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
HOBGC    PWRTB NAME=(,,,HOBLDGCD),      HIST OPEN BLD GROSS LESS CD    X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
HPDCD    PWRTB NAME=(,,,HPDCD),         HIST PAID    CASH DISCOUNT     X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HPDG     PWRTB NAME=(,,,HPDG),          HIST PAID    GROSS             X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HPDGC    PWRTB NAME=(,,,HPDGCD),        HIST PAID    GROSS LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HPDN     PWRTB NAME=(,,,HPDN),          HIST PAID    NET               X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HPDNC    PWRTB NAME=(,,,HPDNCD),        HIST PAID    NET LESS CD       X        
               AORM=N,                                                 X        
               MENUM=(BY$,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
HRBG     PWRTB NAME=(,,,HRBLDG),        HIST REBATE BLD GROSS          X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
HRBGC    PWRTB NAME=(,,,HRBLDGCD),      HIST REBATE BLD GROSS LESS CD  X        
               AORM=N,                                                 X        
               MENUM=(BLL,BY$),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=(NWS),DOCM=Y,                                    X        
               FLAVORM=BUY                                                      
*                                                                               
PDAC     PWRTB NAME=(,,,PDAC),          PAY ELEMENT  AGENCY COMMISSION X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
PDACH    PWRTB NAME=(,,,PDACHG),        PAY ELEMENT  ADDL CHG CODE     X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=ROW,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
PDCD     PWRTB NAME=(,,,PDCD),          PAY ELEMENT  CASH DISCOUNT     X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
PDG      PWRTB NAME=(,,,PDG),           PAY ELEMENT  GROSS             X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
PDGC     PWRTB NAME=(,,,PDGCD),         PAY ELEMENT  GROSS LESS CD     X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
PDGS     PWRTB NAME=(,,,PDGST),         PAY ELEMENT  GST               X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
PDN      PWRTB NAME=(,,,PDN),           PAY ELEMENT  NET               X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
PDNC     PWRTB NAME=(,,,PDNCD),         PAY ELEMENT  NET LESS CD       X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
PDPS     PWRTB NAME=(,,,PDPST),         PAY ELEMENT  PST               X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
PDTX     PWRTB NAME=(,,,PDTAX),         PAY ELEMENT  TAX               X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=DD,                                        X        
               FLAVORM=BUY                                                      
*                                                                               
*       #OFBUY - # OF INSERTIONS                                                
*                                                                               
#BUY     PWRTB NAME=(,,,#OFBUY),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ACTVDT  - CLEARANCE ACTIVITY DATE                                       
*                                                                               
ATVDT    PWRTB NAME=(,,,ACTVDT),                                       X        
               AORM=N,                                                 X        
               MENUM=(CFL),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       AD      - JOB CODE AND CAPTION                                          
*                                                                               
AD       PWRTB NAME=(,,,AD),                                           X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADBREP  - AD BILLING REP                                                
*                                                                               
ADBRP    PWRTB NAME=(,,,ADBREP),                                       X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADBRPADR - AD BILLING REP ADDRESS                                       
*                                                                               
ADBRA    PWRTB NAME=(,,,ADBRPADR),                                     X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADBRPATN - AD BILLING REP ATTENTION                                     
*                                                                               
ADBAT    PWRTB NAME=(,,,ADBRPATN),                                     X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADBRPCDE - AD BILLING REP CODE                                          
*                                                                               
ADBRC    PWRTB NAME=(,,,ADBRPCDE),                                     X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADBRPFAX - AD BILLING REP FAX                                           
*                                                                               
ADBRF    PWRTB NAME=(,,,ADBRPFAX),                                     X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADBRPNAM - AD BILLING REP NAME                                          
*                                                                               
ADBRN    PWRTB NAME=(,,,ADBRPNAM),                                     X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADBRPTEL - AD BILLING REP TELEPHONE                                     
*                                                                               
ADBRT    PWRTB NAME=(,,,ADBRPTEL),                                     X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADCODE  - AD CODE                                                       
*                                                                               
ADC      PWRTB NAME=(,,,ADCODE),                                       X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADCAP   - AD CAPTION                                                    
*                                                                               
ADCAP    PWRTB NAME=(,,,ADCAP),                                        X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADCOPY  - AD COPY NUMBER                                                
*                                                                               
ADCPY    PWRTB NAME=(,,,ADCOPY),                                       X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADFILTER - AD FILTER                                                    
*                                                                               
ADFLT    PWRTB NAME=(,,,ADFILTER),                                     X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADID     - AD ID                                                        
*                                                                               
ADID     PWRTB NAME=(,,,ADID),                                         X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADBCON   - AD CODE BILL CONTACT                                         
*                                                                               
ADBCN    PWRTB NAME=(,,,ADBCON),                                       X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADRPTDTE - AD REPEAT DATE                                               
*                                                                               
ADRDT    PWRTB NAME=(,,,ADRPTDTE),                                     X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADSHR   - AD SUB CODE SHARE                                             
*                                                                               
ADSHR    PWRTB NAME=(,,,ADSHR),                                        X        
               MENUM=AD,                                               X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADSIG   - AD AGENCY SIGNATURE                                           
*                                                                               
ADSIG    PWRTB NAME=(,,,ADSIG),                                        X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADDITIONAL CHARGE -CODE AND DESCRIPTION                                 
*                                                                               
ACH      PWRTB NAME=(,,,ACHG),                                         X        
               MENUM=BY$,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADDITIONAL CHARGE -CODE ONLY                                            
*                                                                               
ACHCD    PWRTB NAME=(,,,ACHGCODE),                                     X        
               MENUM=BY$,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADDITIONAL CHARGE - DESCRIPTION - (SORTED BY CODE)                      
*                                                                               
ACHNM    PWRTB NAME=(,,,ACHGNAME),                                     X        
               MENUM=BY$,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADDITIONAL CHARGE - DESCRIPTION - (SORTED BY DESCRIPTION)               
*                                                                               
ACHNA    PWRTB NAME=(,,,ACHGNAMA),                                     X        
               MENUM=BY$,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ADSPACE - AD SPACE DESCRIPTION                                          
*                                                                               
ADSPC    PWRTB NAME=(,,,ADSPACE),                                      X        
               MENUM=(AD,BLL),                                         X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               AORM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       AFORMULA - BILLING FORMULA                                              
*                                                                               
AFORM    PWRTB NAME=(,,,AFORMULA),                                     X        
               AORM=N,                                                 X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AGDAYS  - AGING DAYS                                                    
*                                                                               
AGDYS    PWRTB NAME=(,,,AGDAYS),                                       X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       AGG     - AGING GROSS                                                   
*                                                                               
AGG      PWRTB NAME=(,,,AGG),                                          X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       AGGCD   - AGING GROSS-CD                                                
*                                                                               
AGGCD    PWRTB NAME=(,,,AGGCD),                                        X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       AGN     - AGING NET                                                     
*                                                                               
AGN      PWRTB NAME=(,,,AGN),                                          X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       AGNCD   - AGING NET-CD                                                  
*                                                                               
AGNCD    PWRTB NAME=(,,,AGNCD),                                        X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       AGENCY  - AGENCY CODE & NAME                                            
*                                                                               
AGCNM    PWRTB NAME=(,,,AGENCY),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AGY     - AGENCY CODE                                                   
*                                                                               
AGYC     PWRTB NAME=(,,,AGY),                                          X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AGYADDR - AGENCY ADDRESS                                                
*                                                                               
AGADR    PWRTB NAME=(,,,AGYADDR),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AGYNAME - AGENCY NAME                                                   
*                                                                               
AGNAM    PWRTB NAME=(,,,AGYNAME),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AORAGYN - AOR AGY NAME                                                  
*                                                                               
AORNM    PWRTB NAME=(,,,AORAGYN),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*        ACTUAL IMPRESSIONS                                                     
*                                                                               
AIMP     PWRTB NAME=(,,,AIMPS),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*        ACTUAL IMPRESSIONS CPM                                                 
*                                                                               
AIMPC    PWRTB NAME=(,,,AIMPSCPM),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       AORAMT  - AOR AMOUNT                                                    
*                                                                               
AORAM    PWRTB NAME=(,,,AORAMT),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AORBASIS - AOR BASIS                                                    
*                                                                               
AORBS    PWRTB NAME=(,,,AORBASIS),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AOREFDT - AOR EFF DATE                                                  
*                                                                               
AOREF    PWRTB NAME=(,,,AOREFDT),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AORGSTCD - AOR GST CODE                                                 
*                                                                               
AORGC    PWRTB NAME=(,,,AORGSTCD),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AORINCA - AOR INCOME ACCT                                               
*                                                                               
AORIA    PWRTB NAME=(,,,AORINCA),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AORKDATE - AOR KILL DATE                                                
*                                                                               
AORKD    PWRTB NAME=(,,,AORKDATE),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AORPCT  - AOR PERCENTAGE                                                
*                                                                               
AORPT    PWRTB NAME=(,,,AORPCT),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AORPSTCD - AOR PST CODE                                                 
*                                                                               
AORPC    PWRTB NAME=(,,,AORPSTCD),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       AORRPA  - AOR REC/PAY ACCOUNT                                           
*                                                                               
AORRP    PWRTB NAME=(,,,AORRPA),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ASPO    - ASPO NUMBER                                                   
*                                                                               
ASPO     PWRTB NAME=(,,,ASPO),                                         X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ATT-CONT - ATTENTION NAME FOR CONTRACT                                  
*                                                                               
ATCN     PWRTB NAME=(,,,ATT-CONT),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ATTCODE - ESTIMATE NUMBER                                               
*                                                                               
ATCCD    PWRTB NAME=(,,,ATTCODE),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BACTIVE - BUY ACTIVITY                                                  
*                                                                               
BATV     PWRTB NAME=(,,,BACTIVE),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BACTIVET - BUY ACTIVITY - TOTALS                                        
*                                                                               
BATVT    PWRTB NAME=(,,,BACTIVET),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYCOMS - BUY COMMENTS                                                  
*                                                                               
BCOM     PWRTB NAME=(,,,BUYCOMS),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYCOMSN - BUY COMMENTS - NO IDS                                        
*                                                                               
BCOMN    PWRTB NAME=(,,,BUYCOMSN),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BASENAME - PUB NAME                                                     
*                                                                               
BASNM    PWRTB NAME=(,,,BASENAME),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BASEPUB - PUBLICATION NUMBER                                            
*                                                                               
BASPB    PWRTB NAME=(,,,BASEPUB),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BCDPCT   - BUY CASH DISCOUNT                                            
*                                                                               
BCDPC    PWRTB NAME=(,,,BCDPCT),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BCHANGES - BUY CHANGES                                                  
*                                                                               
BCHGS    PWRTB NAME=(,,,BCHANGES),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BCHGS    - BUY CHANGES WITH PID                                         
*                                                                               
BCHS     PWRTB NAME=(,,,BCHGS),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDACT   - BILL ELEMENT BILLED ACTUAL                                    
*                                                                               
BDACT    PWRTB NAME=(,,,BDACT),                                        X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDDUEDT - BILLED INVOICE DATE                                           
*                                                                               
BDDDT    PWRTB NAME=(,,,BDDUEDT),                                      X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDINV   - BILLED INVOICE NUMBER                                         
*                                                                               
BDINV    PWRTB NAME=(,,,BDINV),                                        X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDINVM  - BILLED INVOICE NUMBER WITH MEDIA                              
*                                                                               
BDIV1    PWRTB NAME=(,,,BDINVM),                                       X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDINVMND- BILLED INVOICE NUMBER WITH MEDIA W/O DASHES                   
*                                                                               
BDIV2    PWRTB NAME=(,,,BDINVMND),                                     X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDINVDT - BILLED INVOICE DATE                                           
*                                                                               
BDIDT    PWRTB NAME=(,,,BDINVDT),                                      X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDINVDT - BILLED INVOICE DATE                                           
*                                                                               
BDIMY    PWRTB NAME=(,,,BDINVDMY),                                     X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDINSDT - BILLED INSERTION DATE                                         
*                                                                               
BDINS    PWRTB NAME=(,,,BDINSDT),                                      X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDPRD   - BILLED PRODUCT                                                
*                                                                               
BDPRD    PWRTB NAME=(,,,BDPRD),                                        X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDRUNDT - BILL RUN DATE                                                 
*                                                                               
BDRDT    PWRTB NAME=(,,,BDRUNDT),                                      X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDTP    - BILL TYPE                                                     
*                                                                               
BDTP     PWRTB NAME=(,,,BDTP),                                         X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BDTYP   - BILL TYPE                                                     
*                                                                               
BDTYP    PWRTB NAME=(,,,BDTYP),                                        X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BFORMULA - BILLING FORMULA                                              
*                                                                               
BFORM    PWRTB NAME=(,,,BFORMULA),                                     X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BHAC    - BILL AGENCY COMMISSION                                        
*                                                                               
BHAC     PWRTB NAME=(,,,BHAC),                                         X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHACT   - BILL ACTUAL                                                   
*                                                                               
BHACT    PWRTB NAME=(,,,BHACT),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHAC1   - BILL ACTUAL - MINUS = YES                                     
*                                                                               
BHAC1    PWRTB NAME=(,,,BHACT1),                                       X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHAGY   - BILL AGENCY COMMISSION                                        
*                                                                               
BHAGY    PWRTB NAME=(,,,BHAGY),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHAORTYP - AOR BILLING TYPE                                             
*                                                                               
BHATP    PWRTB NAME=(,,,BHAORTYP),                                     X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHCD    - CASH DISCOUNT                                                 
*                                                                               
BHCD     PWRTB NAME=(,,,BHCD),                                         X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHCOMTYP - COMMISSION ONLY BILL TYPE                                    
*                                                                               
BHCTP    PWRTB NAME=(,,,BHCOMTYP),                                     X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHC2G   - BILL COST 2 GROSS                                             
*                                                                               
BHC2G    PWRTB NAME=(,,,BHC2G),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHC2N   - BILL COST 2 NET                                               
*                                                                               
BHC2N    PWRTB NAME=(,,,BHC2N),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHDDATE - DUE DATE                                                      
*                                                                               
BHDDT    PWRTB NAME=(,,,BHDDATE),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHEDATE   - BILL TRANSMITTAL DATE                                       
*                                                                               
BHEDT    PWRTB NAME=(,,,BHEDATE),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHDMN   - DUE DATE MONTH AND YEAR                                       
*                                                                               
BHDMN    PWRTB NAME=(,,,BHDMN),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHGRS   - BILL GROSS                                                    
*                                                                               
BHGRS    PWRTB NAME=(,,,BHGRS),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHG     - BILL GROSS                                                    
*                                                                               
BHG      PWRTB NAME=(,,,BHG),                                          X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHGRSLCD - GROSS LESS CASH DISCOUNT                                     
*                                                                               
BHGCD    PWRTB NAME=(,,,BHGRSLCD),                                     X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHGCD    - GROSS LESS CASH DISCOUNT                                     
*                                                                               
BHGC     PWRTB NAME=(,,,BHGCD),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHGST   - GST AMOUNT                                                    
*                                                                               
BHGST    PWRTB NAME=(,,,BHGST),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHGSTBAS - GST BASIS                                                    
*                                                                               
BHGBS    PWRTB NAME=(,,,BHGSTBAS),                                     X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHGSTCD - GST CODE                                                      
*                                                                               
BHGSC    PWRTB NAME=(,,,BHGSTCD),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHHST   - HST AMOUNT                                                    
*                                                                               
BHHST    PWRTB NAME=(,,,BHHST),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHIDATE - INVOICE DATE                                                  
*                                                                               
BHIDT    PWRTB NAME=(,,,BHIDATE),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHIMN   - INVOICE MONTH AND YEAR                                        
*                                                                               
BHIMN    PWRTB NAME=(,,,BHIMN),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHINV   - INVOICE NUMBER                                                
*                                                                               
BHINV    PWRTB NAME=(,,,BHINV),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHINVM  - INVOICE NUMBER - FULL WITH DASHES                             
*                                                                               
BHIV1    PWRTB NAME=(,,,BHINVM),                                       X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHINVMND- INVOICE NUMBER - FULL WITHOUT DASHES                          
*                                                                               
BHIV2    PWRTB NAME=(,,,BHINVMND),                                     X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHNET   - NET AMOUNT                                                    
*                                                                               
BHNET    PWRTB NAME=(,,,BHNET),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHN     - NET AMOUNT                                                    
*                                                                               
BHN      PWRTB NAME=(,,,BHN),                                          X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHNETLCD - NET LESS CASH DISCOUNT                                       
*                                                                               
BHNCD    PWRTB NAME=(,,,BHNETLCD),                                     X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHNCD - NET LESS CASH DISCOUNT                                          
*                                                                               
BHNC     PWRTB NAME=(,,,BHNCD),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHPDATE - POST DATE                                                     
*                                                                               
BHPDT    PWRTB NAME=(,,,BHPDATE),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHPMN   - POST DATE MONTH AND YEAR                                      
*                                                                               
BHPMN    PWRTB NAME=(,,,BHPMN),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHPO#   - PURCHASE ORDER NUMBER                                         
*                                                                               
BHPO#    PWRTB NAME=(,,,BHPO#),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHPOPER - PURCHASE ORDER PERIOD                                         
*                                                                               
BHPOP    PWRTB NAME=(,,,BHPOPER),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHPOSQN - PURCHASE ORDER SEQUENCE NUMBER                                
*                                                                               
BHPOQ    PWRTB NAME=(,,,BHPOSQN),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHPOSTA - PURCHASE ORDER STATUS                                         
*                                                                               
BHPOS    PWRTB NAME=(,,,BHPOSTA),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHPST   - PST AMOUNT                                                    
*                                                                               
BHPST    PWRTB NAME=(,,,BHPST),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHPSTBAS - PST BASIS                                                    
*                                                                               
BHPBS    PWRTB NAME=(,,,BHPSTBAS),                                     X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHPSTCD - PST CODE                                                      
*                                                                               
BHPCD    PWRTB NAME=(,,,BHPSTCD),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHRDATE - RUN DATE                                                      
*                                                                               
BHRDT    PWRTB NAME=(,,,BHRDATE),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHRETL  - RETAIL ACCOUNT CODE                                           
*                                                                               
BHRTL    PWRTB NAME=(,,,BHRETL),                                       X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHRTL$  - RETAIL OUTLET AMOUNT                                          
*                                                                               
BHRT$    PWRTB NAME=(,,,BHRTL$),                                       X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHSDATE - MONTH OF SERVICE                                              
*                                                                               
BHSDT    PWRTB NAME=(,,,BHSDATE),                                      X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHTAX   - BILL TAX                                                      
*                                                                               
BHTAX    PWRTB NAME=(,,,BHTAX),                                        X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BHTYPE  - BILLING TYPE                                                  
*                                                                               
BHTYP    PWRTB NAME=(,,,BHTYPE),                                       X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BILGROUP - CLIENT BILLING GROUP                                         
*                                                                               
BLGRP    PWRTB NAME=(,,,BILGROUP),                                     X        
               AORM=N,                                                 X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BINVOICE - INVOICE NUMBER                                               
*                                                                               
BINV     PWRTB NAME=(,,,BINVOICE),                                     X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BITAPE  - BILL INTERFACE TAPE                                           
*                                                                               
BITAP    PWRTB NAME=(,,,BITAPE),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=DD,                                                X        
               FLAVORM=                                                         
*                                                                               
*       BJOBCD  - BILLING JOB CODE                                              
*                                                                               
BJBCD    PWRTB NAME=(,,,BJOBCD),                                       X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BJOBCODE - JOB CODE                                                     
*                                                                               
BJOBC    PWRTB NAME=(,,,BJOBCODE),                                     X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BJOBNAME - JOB NAME                                                     
*                                                                               
BJOBN    PWRTB NAME=(,,,BJOBNAME),                                     X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       BLBDT   - BILLABLE DATE                                                 
*                                                                               
BLBDT    PWRTB NAME=(,,,BLBDT),                                        X        
               AORM=N,                                                 X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BLBDTE  - BILLABLE DATE                                                 
*                                                                               
BLBD     PWRTB NAME=(,,,BLBDTE),                                       X        
               AORM=N,                                                 X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BLBMON  - BILLABLE MONTH                                                
*                                                                               
BLBMN    PWRTB NAME=(,,,BLBMON),                                       X        
               AORM=N,                                                 X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BTTAPE  - BANKERS TRUST TAPE                                            
*                                                                               
BTTAP    PWRTB NAME=(,,,BTTAPE),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=DD,                                                X        
               FLAVORM=                                                         
*                                                                               
*       BUDTOT  - BUDGET MONTHLY DOLLARS                                        
*                                                                               
BDMON    PWRTB NAME=(,,,BUDMON),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUDTOT  - BUDGET TOTAL                                                  
*                                                                               
BDTOT    PWRTB NAME=(,,,BUDTOT),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUNITRT - BUY UNIT RATE                                                 
*                                                                               
BUNRT    PWRTB NAME=(,,,BUNITRT),                                      X        
               MENUM=BY$,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=NWS,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUNITRTN - BUY NET UNIT RATE                                            
*                                                                               
BURTN    PWRTB NAME=(,,,BUNITRTN),                                     X        
               MENUM=BY$,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=NWS,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BNVDDSTA - BUY NEW INVOICE DETAIL DISCREPANCY STATUS                    
*                                                                               
BNVDD    PWRTB NAME=(,,,BNVDDSTA),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*                                                                               
*       BNVDMSTA - BUY NEW INVOICE DETAIL MATCHING STATUS                       
*                                                                               
BNVDM    PWRTB NAME=(,,,BNVDMSTA),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BNVDSTA  - BUY NEW INVOICE HEADER DISCREPANCY STATUS                    
*                                                                               
BNVDS    PWRTB NAME=(,,,BNVDSTAT),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*                                                                               
*       BNVMSTAT - BUY NEW INVOICE MATCHING STATUS                              
*                                                                               
BNVMT    PWRTB NAME=(,,,BNVMSTAT),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BNVINV#  - BUY NEW INVOICE NUMBER                                       
*                                                                               
BNV#     PWRTB NAME=(,,,BNVINV#),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BNVINVSQ - BUY NEW INVOICE DETAIL SEQUENCE NUMBER                       
*                                                                               
BNVSQ    PWRTB NAME=(,,,BNVINVSQ),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BNVPYSW  - BUY PAYABLE SWITCH                                           
*                                                                               
PYSW     PWRTB NAME=(,,,BNVPYSW),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYACHG - BUY ADDITIANAL CHARGE CODE                                    
*                                                                               
BYACH    PWRTB NAME=(,,,BUYACHG),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BOUNITRT - BUY OPEN UNIT RATE                                           
*                                                                               
BOUNT    PWRTB NAME=(,,,BOUNITRT),                                     X        
               MENUM=BY$,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=NWS,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYAIND - BUY ACTIVITY INDICATOR                                        
*                                                                               
BYAID    PWRTB NAME=(,,,BUYAIND),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYCDATE - BUY CREATION DATE                                            
*                                                                               
BYCDT    PWRTB NAME=(,,,BUYCDATE),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYER   - BUYER'S INITIALS                                              
*                                                                               
BUYER    PWRTB NAME=(,,,BUYER),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYISSNM- BUY ISSUE NAME                                                
*                                                                               
BYISN    PWRTB NAME=(,,,BUYISSNM),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*                                                                               
*       BUYPREM - BUY PREMIUM                                                   
*                                                                               
BYPRM    PWRTB NAME=(,,,BUYPREM),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYPREM - BUY PREMIUM GROSS DOLLARS                                     
*                                                                               
BPRM$    PWRTB NAME=(,,,BUYPREM$),                                     X        
               MENUM=BY$,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYRATE - BUY RATE                                                      
*                                                                               
BRATE    PWRTB NAME=(,,,BUYRATE),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYSER# - BUY SERIAL NUMBER                                             
*                                                                               
BSER#    PWRTB NAME=(,,,BUYSER#),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYSITE - BUY INTERNET SITE                                             
*                                                                               
BSITE    PWRTB NAME=(,,,BUYSITE),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYSRC  - BUY SOURCE                                                    
*                                                                               
BSRC     PWRTB NAME=(,,,BUYSRC),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYSRC1 - BUY SOURCE WITH IDESK3                                        
*                                                                               
BSRC1    PWRTB NAME=(,,,BUYSRC1),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYSTAT - BUY STATUS (LIVE/TEST)                                        
*                                                                               
BSTAT    PWRTB NAME=(,,,BUYSTAT),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYSTATD - BUY STATUS (DELETED)                                         
*                                                                               
BYSTD    PWRTB NAME=(,,,BUYSTATD),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       BUYSTATO  - BUY STATUS (LIVE/TEST/ORDERED)                              
*                                                                               
BYSTO    PWRTB NAME=(,,,BUYSTATO),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       B1      - B1 BILLING                                                    
*                                                                               
B1       PWRTB NAME=(,,,B1),                                           X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       B4      - B4 BILLING                                                    
*                                                                               
B4       PWRTB NAME=(,,,B4),                                           X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       B5      - B5 BILLING                                                    
*                                                                               
B5       PWRTB NAME=(,,,B5),                                           X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CC      - CUSTOM COULMN INDICATOR                                       
*                                                                               
CC       PWRTB NAME=(,,,CC),                                           X        
               AORM=N,                                                 X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CADDRN  - CONTRACT ADDRESS - NAME                                       
*                                                                               
CADDN    PWRTB NAME=(,,,CADDRN),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CADDR   - CONTRACT ADDRESS                                              
*                                                                               
CADDR    PWRTB NAME=(,,,CADDR),                                        X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CADDR1  - CONTRACT ADDRESS - LINE 1                                     
*                                                                               
CADD1    PWRTB NAME=(,,,CADDR1),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CADDR2  - CONTRACT ADDRESS - LINE 2                                     
*                                                                               
CADD2    PWRTB NAME=(,,,CADDR2),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CAPTION - CAPTION                                                       
*                                                                               
CAPTN    PWRTB NAME=(,,,CAPTION),                                      X        
               MENUM=AD,                                               X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CATTN   - CONTRACT ATTENTION                                            
*                                                                               
CATTN    PWRTB NAME=(,,,CATTN),                                        X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CEMAIL  - CONTRACT E-MAIL ADDRESS                                       
*                                                                               
CEML     PWRTB NAME=(,,,CEMAIL),                                       X        
               MENUM=CON,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CFAX    - CONTRACT FAX                                                  
*                                                                               
CFAX     PWRTB NAME=(,,,CFAX),                                         X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CGRP    - CLIENT GROUP CODE AND NAME                                    
*                                                                               
CGRP     PWRTB NAME=(,,,CGRP),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CGRPCODE - CLIENT GROUP CODE                                            
*                                                                               
CGRPC    PWRTB NAME=(,,,CGRPCODE),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CGRPNAME - CLIENT GROUP NAME                                            
*                                                                               
CGRPN    PWRTB NAME=(,,,CGRPNAME),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CGR1    - CLIENT GROUP CODE AND NAME - FIRST LEVEL                      
*                                                                               
CGR1     PWRTB NAME=(,,,CGR1),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CGR1CODE - CLIENT GROUP CODE - FIRST LEVEL                              
*                                                                               
CGR1C    PWRTB NAME=(,,,CGR1CODE),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CGR1NAME - CLIENT GROUP NAME - FIRST LEVEL                              
*                                                                               
CGR1N    PWRTB NAME=(,,,CGR1NAME),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CGR2    - CLIENT GROUP CODE AND NAME - SECOND LEVEL                     
*                                                                               
CGR2     PWRTB NAME=(,,,CGR2),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CGR2CODE - CLIENT GROUP CODE - SECOND LEVEL                             
*                                                                               
CGR2C    PWRTB NAME=(,,,CGR2CODE),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CGR2NAME - CLIENT GROUP NAME - SECOND LEVEL                             
*                                                                               
CGR2N    PWRTB NAME=(,,,CGR2NAME),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CIRC    - CIRCULATION TOTAL                                             
*                                                                               
CIRC     PWRTB NAME=(,,,CIRC),                                         X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CIRC-EFD - CIRC EFFECTIVE DATE                                          
*                                                                               
CRCEF    PWRTB NAME=(,,,CIRC-EFD),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CIRC-SRC - CIRCULATION SOURCE                                           
*                                                                               
CRCSR    PWRTB NAME=(,,,CIRC-SRC),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CITY/ST - CITY/ STATE                                                   
*                                                                               
CTYST    PWRTB NAME=(,,,CITY/ST),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=NWS,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLINV   - CLEARANCE INVOICE NUMBER                                      
*                                                                               
CLINV    PWRTB NAME=(,,,CLINV),                                        X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLCHK   - CHECK NUMBER                                                  
*                                                                               
CLCHK    PWRTB NAME=(,,,CLCHK),                                        X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLBNKDT  - CHECK BANK CLEARED DATE                                      
*                                                                               
CLCLD    PWRTB NAME=(,,,CLBNKDT),                                      X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLBNKD1  - CHECK BANK CLEARED DATE - NO ASTERISK                        
*                                                                               
CLCD1    PWRTB NAME=(,,,CLBNKD1),                                      X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLCHKDT - CHECK DATE                                                    
*                                                                               
CLCDT    PWRTB NAME=(,,,CLCHKDT),                                      X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLCHKST - CHECK STATUS                                                  
*                                                                               
CLCST    PWRTB NAME=(,,,CLCHKST),                                      X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLPRDCD - CLEARANCE PRODUCT CODE                                        
*                                                                               
CLPRD    PWRTB NAME=(,,,CLPRDCD),                                      X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLESTNUM- CLEARANCE ESTIMATE NUMBER                                     
*                                                                               
CLEST    PWRTB NAME=(,,,CLESTNUM),                                     X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLPID   - CLEARANCE PID                                                 
*                                                                               
CLPID    PWRTB NAME=(,,,CLPID),                                        X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLDAYS  - CLEARANCE DAYS                                                
*                                                                               
CLDYS    PWRTB NAME=(,,,CLDAYS),                                       X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLI     - CLIENT CODE                                                   
*                                                                               
CLI      PWRTB NAME=(,,,CLI),                                          X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLIADDR   - CLIENT ADDRESS                                              
*                                                                               
CLIAD    PWRTB NAME=(,,,CLIADDR),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLIACOFC  - CLIENT ACCOUNTING OFFICE CODE                               
*                                                                               
CAOFC    PWRTB NAME=(,,,CLIACOFC),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLIACOFF  - CLIENT ACCOUNTING OFFICE CODE                               
*                                                                               
CAOFF    PWRTB NAME=(,,,CLIACOFF),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLIENT  - CLIENT CODE AND NAME                                          
*                                                                               
CLINT    PWRTB NAME=(,,,CLIENT),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLINAME - CLIENT NAME                                                   
*                                                                               
CLINM    PWRTB NAME=(,,,CLINAME),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLINTER - CLT INTERFACE CODE                                            
*                                                                               
CLIIC    PWRTB NAME=(,,,CLINTER),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLIOFF   - CLIENT OFFICE CODE                                           
*                                                                               
CLIOF    PWRTB NAME=(,,,CLIOFF),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLIVEND# - CLIENT VENDOR NUMBER                                         
*                                                                               
CLIV#    PWRTB NAME=(,,,CLIVEND#),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLOPTS  - CLEARANCE OPTIONS                                             
*                                                                               
CLOPS    PWRTB NAME=(,,,CLOPTS),                                       X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLREP   - CLEARANCE REP - NUMBER AND NAME                               
*                                                                               
CLREP    PWRTB NAME=(,,,CLREP),                                        X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLREPCD - CLEARANCE REP - NUMBER                                        
*                                                                               
CLRPC    PWRTB NAME=(,,,CLREPCD),                                      X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLREPNAM - CLEARANCE REP - NAME                                         
*                                                                               
CLRNM    PWRTB NAME=(,,,CLREPNAM),                                     X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLSEQ   - CLEARANCE SEQUENCE NUMBER                                     
*                                                                               
CLSEQ    PWRTB NAME=(,,,CLSEQ),                                        X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CLT     - CLIENT CODE                                                   
*                                                                               
CLT      PWRTB NAME=(,,,CLT),                                          X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLTMED  - CLIENT MEDIA CODE & NAME                                      
*                                                                               
CLTMD    PWRTB NAME=(,,,CLTMED),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLTMEDNM - CLIENT MEDIA NAME                                            
*                                                                               
CLTMN    PWRTB NAME=(,,,CLTMEDNM),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLTINTER - CLT INTERFACE CODE                                           
*                                                                               
CLTIN    PWRTB NAME=(,,,CLTINTER),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CLTYPE  - CLEARANCE TYPE - CK OR CR OR NORMAL                           
*                                                                               
CLTYP    PWRTB NAME=(,,,CLTYPE),                                       X        
               AORM=N,                                                 X        
               MENUM=(CFL,PAY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CMAXINS - MAXIMUM INSERTS PER ISSUE                                     
*                                                                               
CMAX     PWRTB NAME=(,,,CMAXINS),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CMAXINSZ - MAXIMUM INSERTS PER ISSUE ACROSS ZONES                       
*                                                                               
CMAXZ    PWRTB NAME=(,,,CMAXINSZ),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CMAXINS1 - MAXIMUM INSERTS PER ISSUE                                    
*                                                                               
CMAX1    PWRTB NAME=(,,,CMAXINS1),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       COM     - COMMENT                                                       
*                                                                               
COM      PWRTB NAME=(,,,COM),                                          X        
               MENUM=OTH,                                              X        
               FIELDM=(HEAD,COL),                                      X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
CMREG    PWRTB NAME=(,,,REGCOM),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       COMPUTE - COMPUTE (DUMMY)                                               
*                                                                               
COMP     PWRTB NAME=(,,,COMPUTE),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=DD,                                                X        
               FLAVORM=                                                         
*                                                                               
*       CON-UNAC - CONTRACT UNIT & BUY ACTIVITY INDICATOR                       
*                                                                               
CONUA    PWRTB NAME=(,,,CON-UNAC),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CON-UNIT - CONTRACT UNIT                                                
*                                                                               
CONUN    PWRTB NAME=(,,,CON-UNIT),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONATTN  - CONTRACT ATTENTION                                           
*                                                                               
CONAT    PWRTB NAME=(,,,CONATTN),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONBLG  - CONTRACT BILLING                                              
*                                                                               
CONBL    PWRTB NAME=(,,,CONBLG),                                       X        
               AORM=N,                                                 X        
               MENUM=(BLL),                                            X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CONCOM  - CONTRACT COMMENTS                                             
*                                                                               
CCOM     PWRTB NAME=(,,,CONCOM),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONHL   - HIGHER CONTRACT LEV                                           
*                                                                               
CONHL    PWRTB NAME=(,,,CONHL),                                        X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONHPCT - HIGHER LV CONT DISC                                           
*                                                                               
CHPCT    PWRTB NAME=(,,,CONHPCT),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONHRD  - HIGHER LV RTE EFDATE                                          
*                                                                               
CHRD     PWRTB NAME=(,,,CONHRD),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONHRT  - HIGHER LEV CONT RATE                                          
*                                                                               
CHRT     PWRTB NAME=(,,,CONHRT),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONL    - CURRENT CONTRACT LEV                                          
*                                                                               
CONL     PWRTB NAME=(,,,CONL),                                         X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONLL   - LOWER CONTRACT LEV                                            
*                                                                               
CNLL     PWRTB NAME=(,,,CONLL),                                        X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONLPCT - LOWER LV CONT DISC                                            
*                                                                               
CNLPC    PWRTB NAME=(,,,CONLPCT),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONLRD  - LOWER LV RTE EFFDATE                                          
*                                                                               
CNLRD    PWRTB NAME=(,,,CONLRD),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONLRT  - LOWER LEV CONT RATE                                           
*                                                                               
CNLRT    PWRTB NAME=(,,,CONLRT),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONNXTFR - CONTR NEXT FREQUENCY                                         
*                                                                               
CNNXF    PWRTB NAME=(,,,CONNXTFR),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONPCT  - CURRNT LV CONT DISC                                           
*                                                                               
CNPCT    PWRTB NAME=(,,,CONPCT),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONRC-H - CONT RATE CURR-HIGH                                           
*                                                                               
CNRCH    PWRTB NAME=(,,,CONRC-H),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONRD   - CURRNT LV RTE EFDATE                                          
*                                                                               
CONRD    PWRTB NAME=(,,,CONRD),                                        X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONRGCD - CURRENT CONTRACT RATE LESS CD                                 
*                                                                               
CNRCD    PWRTB NAME=(,,,CONRGCD),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONRL-C - CONT RATE LOW-CURR                                            
*                                                                               
CNRLC    PWRTB NAME=(,,,CONRL-C),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONRL-H - CONT RATE LOW-HIGH                                            
*                                                                               
CNRLH    PWRTB NAME=(,,,CONRL-H),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONOL   - OPEN RTE LEVEL                                                
*                                                                               
CONOL    PWRTB NAME=(,,,CONOL),                                        X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONOPCT - OPEN RATE PER CENT                                            
*                                                                               
COPCT    PWRTB NAME=(,,,CONOPCT),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONORD  - OPEN RTE EFFDATE                                              
*                                                                               
CNORD    PWRTB NAME=(,,,CONORD),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONORT  - OPEN CONT RATE                                                
*                                                                               
CNORT    PWRTB NAME=(,,,CONORT),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONRO-C - CONT RATE OPEN-CURRENT                                        
*                                                                               
CNROC    PWRTB NAME=(,,,CONRO-C),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONRT   - CURRENT LV CONT RATE                                          
*                                                                               
CONRT    PWRTB NAME=(,,,CONRT),                                        X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONRTGCD - CURRENT LV CONT RATE LESS CD                                 
*                                                                               
CRGCD    PWRTB NAME=(,,,CONRTGCD),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONSPACE - CONTRACT SPACE DESCRIPTION                                   
*                                                                               
CNSPC    PWRTB NAME=(,,,CONSPACE),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONSPCOM - CONTRACT COMMENTS IN SPACE DESCRIPTIONS                      
*                                                                               
CSPCM    PWRTB NAME=(,,,CONSPCOM),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONSTCOM - CONTRACT STANDARD COMMENTS                                   
*                                                                               
CSTCM    PWRTB NAME=(,,,CONSTCOM),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONT    - CONT INF SB PUB NAME                                          
*                                                                               
CONT     PWRTB NAME=(,,,CONT),                                         X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONT-ATT - CONT INF WITH "ATT="                                         
*                                                                               
CNATT    PWRTB NAME=(,,,CONT-ATT),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONT-NUM - CONT INF SB PUB NUMB                                         
*                                                                               
CNNUM    PWRTB NAME=(,,,CONT-NUM),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONTCD  - CONTRACT CASH DISCOUNT                                        
*                                                                               
CNCD     PWRTB NAME=(,,,CONTCD),                                       X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONTDATE - CONT START & END DATES                                       
*                                                                               
CNDTS    PWRTB NAME=(,,,CONTDATE),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CONTNUM - CONTRACT NUMBER                                               
*                                                                               
CNNMB    PWRTB NAME=(,,,CONTNUM),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       COPY    - COPY NUMBER                                                   
*                                                                               
COPY     PWRTB NAME=(,,,COPY),                                         X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       COUNT   - COUNT RECS                                                    
*                                                                               
COUNT    PWRTB NAME=(,,,COUNT),                                        X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=DD,                                                X        
               FLAVORM=                                                         
*                                                                               
*       LCOUNT   - COUNT PRINTED LINES                                          
*                                                                               
LNCTR    PWRTB NAME=(,,,LCOUNT),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CRCBASE - CIRCULATION - RATE BASE                                       
*                                                                               
CRCBS    PWRTB NAME=(,,,CRCBASE),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CRCFINAL - CIRCULATION - FINAL AUTHORIZED                               
*                                                                               
CRCFL    PWRTB NAME=(,,,CRCFINAL),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CRCFREQ - CIRCULATION - FREQUENCY                                       
*                                                                               
CRCFR    PWRTB NAME=(,,,CRCFREQ),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CRCISSDT - CIRCULATION - ISSUE DATE                                     
*                                                                               
CRCID    PWRTB NAME=(,,,CRCISSDT),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CRCPRE - CIRCULATION - PRELIMINARY                                      
*                                                                               
CRCPR    PWRTB NAME=(,,,CRCPRE),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CRCSRC - CIRCULATION - SOURCE                                           
*                                                                               
CRCSC    PWRTB NAME=(,,,CRCSRC),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CRCTOT - CIRCULATION - TOTAL                                            
*                                                                               
CRCTL    PWRTB NAME=(,,,CRCTOT),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       CREP    - CON REP CODE & NAME                                           
*                                                                               
CREP     PWRTB NAME=(,,,CREP),                                         X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CREPADDR - CONTRACT REP ADDRESS                                         
*                                                                               
CRADR    PWRTB NAME=(,,,CREPADDR),                                     X        
               MENUM=CON,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CREPATTN - CONTRACT REP ATTENTION                                       
*                                                                               
CRATT    PWRTB NAME=(,,,CREPATTN),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CREPCODE - CONTRACT REP CODE                                            
*                                                                               
CRCDE    PWRTB NAME=(,,,CREPCODE),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CREPFAX - CONTRACT REP FAX                                              
*                                                                               
CRFAX    PWRTB NAME=(,,,CREPFAX),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CREPNAME - CONTRACT REP NAME                                            
*                                                                               
CRNAM    PWRTB NAME=(,,,CREPNAME),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CREPTEL - CONTRACT REP TELEPHONE                                        
*                                                                               
CRTEL    PWRTB NAME=(,,,CREPTEL),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CSAC     PWRTB NAME=(,,,CSHAC),         CASH APPLIED AGY COMM          X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSACT    PWRTB NAME=(,,,CSHACT),        CASH APPLIED ACTUAL            X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSCD     PWRTB NAME=(,,,CSHCD),         CASH APPLIED CASH DISCOUNT     X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSCHK    PWRTB NAME=(,,,CSHCHK),        CASH APPLIED CHECK NUMBER      X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSCDT    PWRTB NAME=(,,,CSHCHKDT),      CASH APPLIED CHECK DATE        X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSDTE    PWRTB NAME=(,,,CSHDATE),       CASH APPLIED DATE              X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSDYS    PWRTB NAME=(,,,CSHDAYS),       CASH APPLIED DAYS              X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSDDT    PWRTB NAME=(,,,CSHDEPDT),      CASH APPLIED CHK DEPOSIT DTE   X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSG      PWRTB NAME=(,,,CSHG),          CASH APPLIED GROSS             X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSGCD    PWRTB NAME=(,,,CSHGCD),        CASH APPLIED GROSS LESS CD     X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSGST    PWRTB NAME=(,,,CSHGST),        CASH APPLIED GST               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=DD,                                        X        
               FLAVORM=BUY                                                      
CSN      PWRTB NAME=(,,,CSHN),          CASH APPLIED NET               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSNCD    PWRTB NAME=(,,,CSHNCD),        CASH APPLIED NET LESS CD       X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSPCT    PWRTB NAME=(,,,CSHPCT),        CASH APPLIED PERCENT           X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
CSPST    PWRTB NAME=(,,,CSHPST),        CASH APPLIED PST               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=DD,                                        X        
               FLAVORM=BUY                                                      
CSTAX    PWRTB NAME=(,,,CSHTAX),        CASH APPLIED TAX               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,DOCM=DD,                                        X        
               FLAVORM=BUY                                                      
*                                                                               
*       CTEL    - CONTRACT TELEPHONE                                            
*                                                                               
CTEL     PWRTB NAME=(,,,CTEL),                                         X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CUENTRY - CU ENTRY FROM BUY                                             
*                                                                               
CUENT    PWRTB NAME=(,,,CUENTRY),                                      X        
               MENUM=(BUY,CON),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CUN-LLV - # UNITS FROM NXT LLV                                          
*                                                                               
CYLLV    PWRTB NAME=(,,,CUN-LLV),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CURRATE - CURRENT RATE 1                                                
*                                                                               
CCRTE    PWRTB NAME=(,,,CURRATE),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CURRATE2 - CURRENT RATE 2                                               
*                                                                               
CCRT2    PWRTB NAME=(,,,CURRATE2),                                     X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       CURRENT - CURRENT DATA                                                  
*                                                                               
CCDAT    PWRTB NAME=(,,,CURRENT),                                      X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DBG     - DAILY BALANCE GROSS                                           
*                                                                               
DBG      PWRTB NAME=(,,,DBG),                                          X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DBGCD   - DAILY BALANCE GROSS-CD                                        
*                                                                               
DBGCD    PWRTB NAME=(,,,DBGCD),                                        X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DBN     - DAILY BALANCE NET                                             
*                                                                               
DBN      PWRTB NAME=(,,,DBN),                                          X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*        DBNCD - DAILY BALANCE NET-CD                                           
*                                                                               
DBNCD    PWRTB NAME=(,,,DBNCD),                                        X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DELDATE - DELETION DATE                                                 
*                                                                               
DELDT    PWRTB NAME=(,,,DELDATE),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DISKADDR - RECORD DISK ADDRESS                                          
*                                                                               
DADDR    PWRTB NAME=(,,,DISKADDR),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       DIS     - DISTRICT CODE & NAME                                          
*                                                                               
DIS      PWRTB NAME=(,,,DIS),                                          X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DISCODE - DISTRICT CODE                                                 
*                                                                               
DSCDE    PWRTB NAME=(,,,DISCODE),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DISNAME - DISTRICT NAME                                                 
*                                                                               
DSNAM    PWRTB NAME=(,,,DISNAME),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DIV     - DIVISION CODE & NAME                                          
*                                                                               
DIV      PWRTB NAME=(,,,DIV),                                          X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DIVCODE - DIVISION CODE                                                 
*                                                                               
DVCDE    PWRTB NAME=(,,,DIVCODE),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DIVNAME - DIVISION NAME                                                 
*                                                                               
DVNAM    PWRTB NAME=(,,,DIVNAME),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       DLC     - DAILY EFFECTIVE CIRCULATION (000)                             
*                                                                               
DLC      PWRTB NAME=(,,,DLC),                                          X        
               MENUM=PUB,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       COUNT   - DRIVER COUNTING KEYWORD                                       
*                                                                               
DRCT     PWRTB NAME=(,,,COUNT),                                        X        
               MENUM=OTH,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OUTCOUNT- DRIVER OUTPUT COUNTING KEYWORD                                
*                                                                               
DRCTO    PWRTB NAME=(,,,OUTCOUNT),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       EST     - EST NUMBER & NAME                                             
*                                                                               
EST      PWRTB NAME=(,,,EST),                                          X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTDATES - ESTIMATE DATES                                               
*                                                                               
ESDTS    PWRTB NAME=(,,,ESTDATES),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTFLTR - ESTIMATE FILTER                                               
*                                                                               
ESFLT    PWRTB NAME=(,,,ESTFLTR),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTFSCHM - ESTIMATE FILTER SCHEME                                       
*                                                                               
ESFSC    PWRTB NAME=(,,,ESTFSCHM),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*        ESTIMATED IMPRESSIONS                                                  
*                                                                               
EIMP     PWRTB NAME=(,,,EIMPS),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*        ESTIMATED IMPRESSIONS CPM                                              
*                                                                               
EIMPC    PWRTB NAME=(,,,EIMPSCPM),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ESTNAME - ESTIMATE NAME                                                 
*                                                                               
ESNAM    PWRTB NAME=(,,,ESTNAME),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTNAM1 - ESTIMATE NAME - LINE 1                                        
*                                                                               
ESNM1    PWRTB NAME=(,,,ESTNAM1),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTNAM2 - ESTIMATE NAME - LINE 2                                        
*                                                                               
ESNM2    PWRTB NAME=(,,,ESTNAM2),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTNUM  - ESTIMATE NUMBER                                               
*                                                                               
ESNUM    PWRTB NAME=(,,,ESTNUM),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTNUM1 - ESTIMATE NUMBER - WITH LEADING ZEROS                          
*                                                                               
ES#1     PWRTB NAME=(,,,ESTNUM1),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTRATE  - ESTIMATE RATE TYPE                                           
*                                                                               
ESRTE    PWRTB NAME=(,,,ESTRATE),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTRSCHM - ESTIMATE RETAIL SCHEME CODE                                  
*                                                                               
ESRSC    PWRTB NAME=(,,,ESTRSCHM),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTSTALK - ESTIMATE STATUS - LOCKED                                     
*                                                                               
ESSTL    PWRTB NAME=(,,,ESTSTALK),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ESTSTATS - ESTIMATE STATUS - TEST                                       
*                                                                               
ESSTT    PWRTB NAME=(,,,ESTSTATS),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       FREECOM - FREE-FORM COMMENT                                             
*                                                                               
FFCOM    PWRTB NAME=(,,,FREECOM),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       FRSTI/O - FIRST I/O NUMBER                                              
*                                                                               
FIONM    PWRTB NAME=(,,,FRSTI/O),                                      X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       FRSTIODT - FIRST I/O DATE                                               
*                                                                               
FIODT    PWRTB NAME=(,,,FRSTIODT),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       FRSTIOTP - FIRST I/O TYPE                                               
*                                                                               
FIOTP    PWRTB NAME=(,,,FRSTIOTP),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       FRSTIOGN - FIRST I/O GENERATED                                          
*                                                                               
FIOGN    PWRTB NAME=(,,,FRSTIOGN),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       FREQNCY - FREQUENCY OF PUB                                              
*                                                                               
FRPUB    PWRTB NAME=(,,,FREQNCY),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       FSI     - FREE STANDING INSERTS                                         
*                                                                               
FSI      PWRTB NAME=(,,,FSI),                                          X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       GAP     - BLANK COLUMN                                                  
*                                                                               
GAP      PWRTB NAME=(,,,GAP),                                          X        
               MENUM=OTH,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       GAPNUM  - BLANK COLUMN-NUMERIC FOR STACKING                             
*                                                                               
GAPNM    PWRTB NAME=(,,,GAPNUM),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       GFTAPE  - GENERAL FOODS TAPE                                            
*                                                                               
GFTPE    PWRTB NAME=(,,,GFTAPE),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=DD,                                                X        
               FLAVORM=BUY                                                      
*                                                                               
*       GPCLCODE - CLIENT GROUP CODE                                            
*                                                                               
GPCLC    PWRTB NAME=(,,,GPCLCODE),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       GPPRCODE - PRODUCT GROUP CODE                                           
*                                                                               
GPPRC    PWRTB NAME=(,,,GPPRCODE),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       GPPBCODE - PUB GROUP CODE                                               
*                                                                               
GPPBC    PWRTB NAME=(,,,GPPBCODE),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       H-SVNGC - POTENTIAL SAVINGS                                             
*                                                                               
HSVC     PWRTB NAME=(,,,H-SVNGC),                                      X        
               MENUM=CON,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       HLV-CUN - #OF UNITS TO NXT HLV                                          
*                                                                               
HCUN     PWRTB NAME=(,,,HLV-CUN),                                      X        
               MENUM=CON,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       I/O HISTORY - NUMBER                                                    
*                                                                               
HSTIO    PWRTB NAME=(,,,HISTI/O),                                      X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       I/O HISTORY - DATE                                                      
*                                                                               
HSTDT    PWRTB NAME=(,,,HISTIODT),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       I/O HISTORY - TYPE                                                      
*                                                                               
HSTTP    PWRTB NAME=(,,,HISTIOTP),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       I/O HISTORY - GENERATED                                                 
*                                                                               
HSTGN    PWRTB NAME=(,,,HISTIOGN),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       I/ODATE - LAST I/O DATE                                                 
*                                                                               
IODTE    PWRTB NAME=(,,,I/ODATE),                                      X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ICCOM   - INSERT ORDER COMMENT                                          
*                                                                               
ICCOM    PWRTB NAME=(,,,ICCOM),                                        X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IGPNUMBR- INTERNET GROUP NUMBER                                         
*                                                                               
IGNUM    PWRTB NAME=(,,,IGPNUMBR),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IGPRTEID- INTERNET GROUP RATE INDICATOR                                 
*                                                                               
IGRID    PWRTB NAME=(,,,IGPRTEID),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IGPTRATE - INTERNET GROUP TOTAL RATE                                    
*                                                                               
IGTRT    PWRTB NAME=(,,,IGPTRATE),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IGPTCPM - INTERNET GROUP TOTAL CPM                                      
*                                                                               
IGCPM    PWRTB NAME=(,,,IGPTCPM),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IGPTIMPS - INTERNET GROUP TOTAL IMPRESSIONS                             
*                                                                               
IGIMP    PWRTB NAME=(,,,IGPTIMPS),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IGP#INS - INTERNET GROUP NUMBER OF INSERTIONS                           
*                                                                               
IGINS    PWRTB NAME=(,,,IGP#INS),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ILLUM   - ILLUMINATION (OUTDOOR)                                        
*                                                                               
ILLUM    PWRTB NAME=(,,,ILLUM),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=OUT,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IMPS    - IMPRESSIONS                                                   
*                                                                               
IMPS     PWRTB NAME=(,,,IMPS),                                         X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSDATE - INSERTION DATE                                                
*                                                                               
INSDT    PWRTB NAME=(,,,INSDATE),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSDATEA - INSERTION DATE - MONTH BUYS ARE ON 1ST OF MONTH              
*                                                                               
INSDA    PWRTB NAME=(,,,INSDATEA),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSDATEB - INSERTION DATE - NO BFD,WEEKOF INDICATORS                    
*                                                                               
INSDB    PWRTB NAME=(,,,INSDATEB),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSDATEX - INSERTION DATE WITHOUT LINE NUMBER                           
*                                                                               
INSDX    PWRTB NAME=(,,,INSDATEX),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSDATEY - INSERTION DATE - MONTH BUYS ARE ON 1ST & NO LINE NO.         
*                                  AND NO TEST/DELETE INDICATORS                
INSDY    PWRTB NAME=(,,,INSDATEY),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSDATEZ - INSERTION DATE - MONTH BUYS ARE ON 1ST & NO LINE NO.         
*                                  AND TEST/DELETE INDICATORS                   
INSDZ    PWRTB NAME=(,,,INSDATEZ),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSMMMDD - INSERTION DATE - MMMDD WITH LINE NUMBER                      
*                                                                               
INSMD    PWRTB NAME=(,,,INSMMMDD),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSYEAR  - INSERTION YEAR                                               
*                                                                               
INYR     PWRTB NAME=(,,,INSYEAR),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSMONTH  - INSERTION MONTH                                             
*                                                                               
INMN     PWRTB NAME=(,,,INSMON),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSDAY  - INSERTION DAY                                                 
*                                                                               
INDY     PWRTB NAME=(,,,INSDAY),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSDDD  - INSERTION DAY OF WEEK                                         
*                                                                               
INDDD    PWRTB NAME=(,,,INSDDD),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INSDYYMM - INSERTION DATE YYMMDD                                        
*                                                                               
INDYY    PWRTB NAME=(,,,INSDYYMM),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       I/OLWCD  - LEGAL WARNING CODE                                           
*                                                                               
IOLW     PWRTB NAME=(,,,I/OLWCD),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       I/OLWQTR - LEGAL & QUARTERLY WARNING CODE                               
*                                                                               
IOLWQ    PWRTB NAME=(,,,I/OLWQTR),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       I/OQTRCD - QUARTERLY WARNING CODE                                       
*                                                                               
IOQTR    PWRTB NAME=(,,,I/OQTRCD),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INCHES   - INCHES IN BUY                                                
*                                                                               
INCHS    PWRTB NAME=(,,,INCHES),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=NWS,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOHST#  - I/O NUMBER - HISTORY                                          
*                                                                               
IOH#     PWRTB NAME=(,,,IOHST#),                                       X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOHSTDT - I/O DATE   - HISTORY                                          
*                                                                               
IOHDT    PWRTB NAME=(,,,IOHSTDTE),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOHSTGEN - I/O GENERATION METHOD   - HISTORY                            
*                                                                               
IOHGN    PWRTB NAME=(,,,IOHSTGEN),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOHSTGEN - I/O STATUS   - HISTORY                                       
*                                                                               
IOHST    PWRTB NAME=(,,,IOHSTSTA),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOHSTTYP - I/O TYPE  - HISTORY                                          
*                                                                               
IOHTP    PWRTB NAME=(,,,IOHSTTYP),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOHSTLWC - I/O LEGAL WARNING CODE  - HISTORY                            
*                                                                               
IOHLW    PWRTB NAME=(,,,IOHSTLWC),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOHSTLWQ - I/O QUARTERLY LEGAL WARNING CODE  - HISTORY                  
*                                                                               
IOHQW    PWRTB NAME=(,,,IOHSTLWQ),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOHSTLWC - I/O LEGAL & QUARTERLY  WARNING CODE  - HISTORY               
*                                                                               
IOH2W    PWRTB NAME=(,,,IOHSTLW2),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOLST#  - I/O NUMBER - LAST                                             
*                                                                               
IOL#     PWRTB NAME=(,,,IOLST#),                                       X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOLSTDT - I/O DATE   - LAST                                             
*                                                                               
IOLDT    PWRTB NAME=(,,,IOLSTDTE),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOLSTGEN - I/O STATUS   - LAST                                          
*                                                                               
IOLST    PWRTB NAME=(,,,IOLSTSTA),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOLSTGEN - I/O GENERATION METHOD   - LAST                               
*                                                                               
IOLGN    PWRTB NAME=(,,,IOLSTGEN),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOLSTTYP - I/O TYPE  - LAST                                             
*                                                                               
IOLTP    PWRTB NAME=(,,,IOLSTTYP),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOLSTLWC - I/O LEGAL WARNING CODE  - LAST                               
*                                                                               
IOLLW    PWRTB NAME=(,,,IOLSTLWC),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOLSTLWQ - I/O QUARTERLY LEGAL WARNING CODE  - LAST                     
*                                                                               
IOLQW    PWRTB NAME=(,,,IOLSTLWQ),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IOLSTLWC - I/O LEGAL & QUARTERLY  WARNING CODE  - LAST                  
*                                                                               
IOL2W    PWRTB NAME=(,,,IOLSTLW2),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IO1ST#  - I/O NUMBER - FIRST                                            
*                                                                               
IO1#     PWRTB NAME=(,,,IO1ST#),                                       X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IO1STDT - I/O DATE   - FIRST                                            
*                                                                               
IO1DT    PWRTB NAME=(,,,IO1STDTE),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IO1STGEN - I/O STATUS   - FIRST                                         
*                                                                               
IO1ST    PWRTB NAME=(,,,IO1STSTA),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IO1STGEN - I/O GENERATION METHOD   - FIRST                              
*                                                                               
IO1GN    PWRTB NAME=(,,,IO1STGEN),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IO1STTYP - I/O TYPE  - FIRST                                            
*                                                                               
IO1TP    PWRTB NAME=(,,,IO1STTYP),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IO1STLWC - I/O LEGAL WARNING CODE  - FIRST                              
*                                                                               
IO1LW    PWRTB NAME=(,,,IO1STLWC),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IO1STLWQ - I/O QUARTERLY LEGAL WARNING CODE  - FIRST                    
*                                                                               
IO1QW    PWRTB NAME=(,,,IO1STLWQ),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       IO1STLWC - I/O LEGAL & QUARTERLY  WARNING CODE  - FIRST                 
*                                                                               
IO12W    PWRTB NAME=(,,,IO1STLW2),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ISSAVAIL - NUMBER OF INSERTS STILL AVAILABLE FOR AN ISSUE               
*                                                                               
ISAVL    PWRTB NAME=(,,,ISSAVAIL),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ISSDATE - ISSUE DATE                                                    
*                                                                               
ISDTE    PWRTB NAME=(,,,ISSDATE),                                      X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       INVNUM  - INVOICE NUMBER                                                
*                                                                               
IVNUM    PWRTB NAME=(,,,INVNUM),                                       X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVPRD  - INVOICE PRODUCT                                               
*                                                                               
IVPRD    PWRTB NAME=(,,,INVPRD),                                       X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVDATE - INVOICE DATE                                                  
*                                                                               
IVDTE    PWRTB NAME=(,,,INVDATE),                                      X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVEST  - INVOICE ESTIMATE (OPTIONAL)                                   
*                                                                               
IVEST    PWRTB NAME=(,,,INVEST),                                       X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVPERST- INVOICE PERIOD START                                          
*                                                                               
IVPST    PWRTB NAME=(,,,INVSTRDT),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVNUM  - INVOICE PERIOD END                                            
*                                                                               
IVPEN    PWRTB NAME=(,,,INVENDDT),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVNUM  - INVOICE DATE ADDED                                            
*                                                                               
IVADD    PWRTB NAME=(,,,INVADDDT),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVACTDT- INVOICE DATE OF LAST ACTIVITY                                 
*                                                                               
IVACT    PWRTB NAME=(,,,INVCHGDT),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVCHGBY- INVOICE LAST CHANGED BY                                       
*                                                                               
IVCBY    PWRTB NAME=(,,,INVWHO),                                       X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVDLRS - INVOICE AMOUNT                                                
*                                                                               
IVDLR    PWRTB NAME=(,,,INVAMT),                                       X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVSREP - INVOICE SPECIAL REP                                           
*                                                                               
IVSRP    PWRTB NAME=(,,,INVSREP),                                      X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVMATCH- INVOICE DETAIL STATUS                                         
*                                                                               
IVSTA    PWRTB NAME=(,,,INVDTSTA),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVMATCH- INVOICE DETAIL RUN DATE                                       
*                                                                               
IVDDT    PWRTB NAME=(,,,INVDTRDT),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVMATCH- INVOICE DETAIL ESTIMATE                                       
*                                                                               
IVDES    PWRTB NAME=(,,,INVDTEST),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVMATCH- INVOICE DETAIL PRODUCT                                        
*                                                                               
IVDPR    PWRTB NAME=(,,,INVDTPRD),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVMATCH- INVOICE DETAIL COMMENT                                        
*                                                                               
IVDCM    PWRTB NAME=(,,,INVDTCOM),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVSPACE- INVOICE DETAIL SPACE                                          
*                                                                               
IVDSP    PWRTB NAME=(,,,INVDTSPC),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVDTG    - INVOICE DETAIL GROSS                                        
*                                                                               
IVG      PWRTB NAME=(,,,INVDTG),                                       X        
               MENUM=INV,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVDTGCD  - INVOICE DETAIL GROSS LESS CD                                
*                                                                               
IVGCD    PWRTB NAME=(,,,INVDTGCD),                                     X        
               MENUM=INV,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVDTN    - INVOICE DETAIL NET                                          
*                                                                               
IVN      PWRTB NAME=(,,,INVDTN),                                       X        
               MENUM=INV,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       INVDTNCD  - INVOICE DETAIL NET   LESS CD                                
*                                                                               
IVNCD    PWRTB NAME=(,,,INVDTNCD),                                     X        
               MENUM=INV,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       JOB     - AD CODE & CAPTION                                             
*                                                                               
JOB      PWRTB NAME=(,,,JOB),                                          X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=DD,                                                X        
               FLAVORM=BUY                                                      
*                                                                               
*       JOBCODE - AD CODE                                                       
*                                                                               
JBCDE    PWRTB NAME=(,,,JOBCODE),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=DD,                                                X        
               FLAVORM=BUY                                                      
*                                                                               
*       KEY      - RECORD KEY                                                   
*                                                                               
KEY      PWRTB NAME=(,,,KEY),                                          X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KRBILAGY - GFEST BILLING AGENCY                                         
*                                                                               
KRBAG    PWRTB NAME=(,,,KRBILAGY),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KRCTVAGY - GFEST CREATIVE AGENCY                                        
*                                                                               
KRCAG    PWRTB NAME=(,,,KRCTVAGY),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KRDEALGV - GFEST DEAL # (GEVALIA)                                       
*                                                                               
KRDGV    PWRTB NAME=(,,,KRDEALGV),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KRDIVBCD - GFEST DIVISION/BRAND CODE                                    
*                                                                               
KRDVB    PWRTB NAME=(,,,KRDIVBCD),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KREXPTYP - GFEST EXPENSE TYPE                                           
*                                                                               
KRXTP    PWRTB NAME=(,,,KREXPTYP),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KRNATRL  - GFEST GF NATURAL                                             
*                                                                               
KRNTL    PWRTB NAME=(,,,KRNATRL),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KRPRDCD  - GFEST PRODUCT CODE                                           
*                                                                               
KRPCD    PWRTB NAME=(,,,KRPRDCD),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KRPRDID  - GFEST PRODUCT ID                                             
*                                                                               
KRPID    PWRTB NAME=(,,,KRPRDID),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KRSRCAGY - GFEST SOURCE AGENCY                                          
*                                                                               
KRSAG    PWRTB NAME=(,,,KRSRCAGY),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KRSUBNTL - GFEST GF SUB-NATURAL                                         
*                                                                               
KRSNL    PWRTB NAME=(,,,KRSUBNTL),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       KRTGTMKT - GFEST TARGET MARKET                                          
*                                                                               
KRTGT    PWRTB NAME=(,,,KRTGTMKT),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       LASTI/O - LAST I/O NUMBER                                               
*                                                                               
LSTIO    PWRTB NAME=(,,,LASTI/O),                                      X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       LBBDINV - LABATT BILLED INVOICE NUMBER                                  
*                                                                               
LBBNV    PWRTB NAME=(,,,LBBDINV),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       LBGL#   - LABATT GENERAL LEDGER #                                       
*                                                                               
LBGL#    PWRTB NAME=(,,,LBGL#),                                        X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       LBVCHR  - LABATT VOUCHER NUMBER                                         
*                                                                               
LBVCH    PWRTB NAME=(,,,LBVCHR),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       LASTIODT - LAST I/O DATE                                                
*                                                                               
LSTID    PWRTB NAME=(,,,LASTIODT),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       LASTIOTP - LAST I/O TYPE                                                
*                                                                               
LSTTP    PWRTB NAME=(,,,LASTIOTP),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       LASTIOGN - LAST I/O GENERATED                                           
*                                                                               
LSTGN    PWRTB NAME=(,,,LASTIOGN),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       LIN/INCH - LINES / INCHES                                               
*                                                                               
LNIN     PWRTB NAME=(,,,LIN/INCH),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=NWS,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       LINES    - LINES  IN BUY                                                
*                                                                               
LINES    PWRTB NAME=(,,,LINES),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=NWS,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       LINE#   - LINE NUMBER                                                   
*                                                                               
LINE#    PWRTB NAME=(,,,LINE#),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       LSTADUSE - LAST DATE OF AD USE                                          
*                                                                               
LSTAD    PWRTB NAME=(,,,LSTADUSE),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       MARKET  - MARKET SORT                                                   
*                                                                               
MKT      PWRTB NAME=(,,,MARKET),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       MATCLOSE - MATERIAL CLOSE DATE                                          
*                                                                               
MCDTE    PWRTB NAME=(,,,MATCLOSE),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MATCLDTS - MATERIAL CLOSE DATE AND EXTENSION DATE                       
*                                                                               
MCDTS    PWRTB NAME=(,,,MATCLDTS),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MATCLOSE - MATERIAL CLOSE DATE EXTENSION - DATE                         
*                                                                               
MCEDT    PWRTB NAME=(,,,MATCLEDT),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MATCLOSE - MATERIAL CLOSE DATE EXTENSION - DAYS                         
*                                                                               
MCDEX    PWRTB NAME=(,,,MATCLEXT),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MCLI    - MASTER CLIENT CODE                                            
*                                                                               
MCLCD    PWRTB NAME=(,,,MCLI),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MCLIENT - MAS CLI CODE & NAME                                           
*                                                                               
MCLT     PWRTB NAME=(,,,MCLIENT),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MCLINAME - MASTER CLIENT NAME                                           
*                                                                               
MCLNM    PWRTB NAME=(,,,MCLINAME),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MCLT    - MASTER CLIENT CODE                                            
*                                                                               
MCLTC    PWRTB NAME=(,,,MCLT),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MED     - MEDIA CODE AND NAME                                           
*                                                                               
MED      PWRTB NAME=(,,,MED),                                          X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MEDCODE - MEDIA CODE                                                    
*                                                                               
MEDCD    PWRTB NAME=(,,,MEDCODE),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MEDNAME - MEDIA NAME                                                    
*                                                                               
MEDNM    PWRTB NAME=(,,,MEDNAME),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       MERGE   - PLACE HOLDER FOR MERGING                                      
*                                                                               
MERGE    PWRTB NAME=(5,,,MERGE=),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=(ROW,COL),                                       X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
MTDT     PWRTB NAME=(,,,MTDT),          MATCHED DATE                   X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
MTINM    PWRTB NAME=(,,,MTINVNM),       MATCHED INVOICE NUMBER         X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,DOCM=Y,                                         X        
               FLAVORM=BUY                                                      
*                                                                               
*       NVD#LINS- INVOICE DETAIL - NUMBER OF LINE ITEMS REPRSENTED              
*                                                                               
ND#LN    PWRTB NAME=(,,,NVD#LINS),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDADCAP - INVOICE DETAIL - AD CAPTION                                  
*                                                                               
NDACP    PWRTB NAME=(,,,NVDADCAP),                                     X        
               MENUM=INV,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDCLTCD - INVOICE DETAIL - CLIENT CODE                                 
*                                                                               
NDCLC    PWRTB NAME=(,,,NVDCLTCD),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDCLTNM - INVOICE DETAIL - CLIENT NAME                                 
*                                                                               
NDCLN    PWRTB NAME=(,,,NVDCLTNM),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDCLT   - INVOICE DETAIL - CLIENT CODE AND NAME                        
*                                                                               
NDCLT    PWRTB NAME=(,,,NVDCLT),                                       X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDCOM   - INVOICE DETAIL - COMMENT                                     
*                                                                               
NDCOM    PWRTB NAME=(,,,NVDCOM),                                       X        
               MENUM=INV,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVHCOM  - INVOICE HEADER - COMMENT                                      
*                                                                               
NHCOM    PWRTB NAME=(,,,NVHCOM),                                       X        
               MENUM=INV,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDBYDTE - INVOICE DETAIL - BUY LINE DATE                               
*                                                                               
NDBDT    PWRTB NAME=(,,,NVDBYDTE),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDBYLIN- INVOICE DETAIL - BUY LINE NUMBER                              
*                                                                               
NDBLN    PWRTB NAME=(,,,NVDBYLIN),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDADCAP- INVOICE DETAIL - BUY SERIAL #                                 
*                                                                               
NDBS#    PWRTB NAME=(,,,NVDBYSR#),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDCD   - INVOICE DETAIL - CASH DISCOUNT Y/N                            
*                                                                               
NDCD     PWRTB NAME=(,,,NVDCDY/N),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDDCM  - INVOICE DETAIL - DISCREPANCY CODE                             
*                                                                               
NDDCM    PWRTB NAME=(,,,NVDDCM),                                       X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDDCMCM- INVOICE DETAIL - DISCREPANCY COMMENT                          
*                                                                               
NDDCC    PWRTB NAME=(,,,NVDDCMCM),                                     X        
               MENUM=INV,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDDCMID- INVOICE DETAIL - DISCREPANCY COMMENT PID                      
*                                                                               
NDDCP    PWRTB NAME=(,,,NVDDCMID),                                     X        
               MENUM=INV,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDADCAP- INVOICE DETAIL - LINE ITEM DATE                               
*                                                                               
NDDTE    PWRTB NAME=(,,,NVDDATE),                                      X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDESTNM- INVOICE DETAIL - ESTIMATE NUMBER                              
*                                                                               
NDES#    PWRTB NAME=(,,,NVDESTCD),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDESTNM- INVOICE DETAIL - ESTIMATE NAME                                
*                                                                               
NDESN    PWRTB NAME=(,,,NVDESTNM),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDEST  - INVOICE DETAIL - ESTIMATE NUMBER AND DESCRIPTION              
*                                                                               
NDEST    PWRTB NAME=(,,,NVDEST),                                       X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDG    - INVOICE DETAIL - GROSS                                        
*                                                                               
NDGRS    PWRTB NAME=(,,,NVDG),                                         X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDG$   - INVOICE DETAIL - GROSS                                        
*                                                                               
NDG$     PWRTB NAME=(,,,NVDG$),                                        X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDNEST - INVOICE DETAIL - NET                                          
*                                                                               
NDNET    PWRTB NAME=(,,,NVDN),                                         X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDPUBCD- INVOICE DETAIL - PUB CODE                                     
*                                                                               
NDPBC    PWRTB NAME=(,,,NVDPUBCD),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDPRDCD- INVOICE DETAIL - PRODUCT CODE                                 
*                                                                               
NDPRC    PWRTB NAME=(,,,NVDPRDCD),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDPREM - INVOICE DETAIL - PREMIUM                                      
*                                                                               
NDPRM    PWRTB NAME=(,,,NVDPREM),                                      X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDRATE - INVOICE DETAIL - RATE                                         
*                                                                               
NDRTE    PWRTB NAME=(,,,NVDRATE),                                      X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDSPACE- INVOICE DETAIL - SPACE DESCRIPTION                            
*                                                                               
NDSPC    PWRTB NAME=(,,,NVDSPACE),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVDSQN  - INVOICE DETAIL - SEQUENCE NUMBER                              
*                                                                               
NDSQN    PWRTB NAME=(,,,NVDSQN),                                       X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NEXT    - NEXT SPACE DESCRIPTION                                        
*                                                                               
NEXT     PWRTB NAME=(,,,NEXT),                                         X        
               MENUM=CON,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       NVHADDDT - INVOICE ADDED DATE                                           
*                                                                               
NVADT    PWRTB NAME=(,,,NVHADDDT),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVH$TP  - INVOICE DOLLAR TYPE                                           
*                                                                               
NV$TP    PWRTB NAME=(,,,NVH$TYP),                                      X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVHINVDT - INVOICE DATE                                                 
*                                                                               
NVDTE    PWRTB NAME=(,,,NVHINVDT),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVHINV# - INVOICE NUMBER                                                
*                                                                               
NVNUM    PWRTB NAME=(,,,NVHINV#),                                      X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVHPER  - INVOICE PERIOD                                                
*                                                                               
NVPER    PWRTB NAME=(,,,NVHPER),                                       X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVHREPCD - INVOICE SPECIAL REP CODE                                     
*                                                                               
NVRPC    PWRTB NAME=(,,,NVHREPCD),                                     X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVHSTAT - INVOICE STATUS                                                
*                                                                               
NVSTA    PWRTB NAME=(,,,NVHSTAT),                                      X        
               MENUM=INV,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       NVHTOT  - INVOICE TOTAL                                                 
*                                                                               
NVTOT    PWRTB NAME=(,,,NVHTOT),                                       X        
               MENUM=INV,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OAN     - OAN NAME & CODE                                               
*                                                                               
OAN      PWRTB NAME=(,,,OAN),                                          X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OAN-NAME - OAN NAME                                                     
*                                                                               
OANNM    PWRTB NAME=(,,,OAN-NAME),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OANCODE - OAN CODE                                                      
*                                                                               
OANCD    PWRTB NAME=(,,,OANCODE),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OFF     - CLIENT OFFICE CODE & NAME                                     
*                                                                               
OFF      PWRTB NAME=(,,,OFF),                                          X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OFFCODE - CLIENT OFFICE CODE                                            
*                                                                               
OFFCD    PWRTB NAME=(,,,OFFCODE),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OFFNAME - CLIENT OFFICE NAME                                            
*                                                                               
OFFNM    PWRTB NAME=(,,,OFFNAME),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OFFNAMA - CLIENT OFFICE NAME - ALPHA ORDER                              
*                                                                               
OFFNA    PWRTB NAME=(,,,OFFNAMA),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       ONSALE  - ON SALE DATE                                                  
*                                                                               
ONDTE    PWRTB NAME=(,,,ONSALE),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       OUTMKT  - OUTLET MARKET - CODE AND NAME                                 
*                                                                               
OMKT     PWRTB NAME=(,,,OUTMKT),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OUTMKTCD - OUTLET MARKET - CODE                                         
*                                                                               
OMKCD    PWRTB NAME=(,,,OUTMKTCD),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OUTMKTNA - OUTLET MARKET - NAME - SORTED BY NAME                        
*                                                                               
OMKNA    PWRTB NAME=(,,,OUTMKTNA),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OUTMKTNM - OUTLET MARKET - NAME - SORTED BY CODE                        
*                                                                               
OMKNM    PWRTB NAME=(,,,OUTMKTNM),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OUTMKTNP - OUTLET MARKET - NO PRINT                                     
*                                                                               
OMKNP    PWRTB NAME=(,,,OUTMKTNP),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OPEN-RTE - OPEN RATES FROM PUB                                          
*                                                                               
OPNRT    PWRTB NAME=(,,,OPEN-RTE),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       OUTSPC2 - OUTDOOR SPACE DESCRIPTION - SECOND LINE                       
*                                                                               
OUTS2    PWRTB NAME=(,,,OUTSPC2),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=OUT,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PADDRN  - PAYING ADDRESS - NAME                                         
*                                                                               
PADDN    PWRTB NAME=(,,,PADDRN),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PADDR   - PAYING ADDRESS                                                
*                                                                               
PADDR    PWRTB NAME=(,,,PADDR),                                        X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PADDR1  - PAYING ADDRESS - LINE 1                                       
*                                                                               
PADD1    PWRTB NAME=(,,,PADDR1),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PADDR2  - PAYING ADDRESS - LINE 2                                       
*                                                                               
PADD2    PWRTB NAME=(,,,PADDR2),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PAGE-EQV - PAGE EQUIVALENCE                                             
*                                                                               
PGEQU    PWRTB NAME=(,,,PAGE-EQV),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PATTN   - PAYING ATTENTION                                              
*                                                                               
PATTN    PWRTB NAME=(,,,PATTN),                                        X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PEMAIL  - PAYING E-MAIL ADDRESS                                         
*                                                                               
PEML     PWRTB NAME=(,,,PEMAIL),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PAYDATE - PAYABLE DATE                                                  
*                                                                               
PAYDT    PWRTB NAME=(,,,PAYDATE),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PBGP    - PUB GROUP CODE AND NAME                                       
*                                                                               
PBGP     PWRTB NAME=(,,,PBGP),                                         X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PBGPCODE - PUB GROUP CODE                                               
*                                                                               
PBGPC    PWRTB NAME=(,,,PBGPCODE),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PBGPNAME - PUB GROUP NAME                                               
*                                                                               
PBGPN    PWRTB NAME=(,,,PBGPNAME),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PBG1    - PUB GROUP CODE AND NAME - FIRST LEVEL                         
*                                                                               
PBG1     PWRTB NAME=(,,,PBG1),                                         X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PBG1CODE - PUB GROUP CODE - FIRST LEVEL                                 
*                                                                               
PBG1C    PWRTB NAME=(,,,PBG1CODE),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PBG1NAME - PUB GROUP NAME - FIRST LEVEL                                 
*                                                                               
PBG1N    PWRTB NAME=(,,,PBG1NAME),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PBG2    - PUB GROUP CODE AND NAME - SECOND LEVEL                        
*                                                                               
PBG2     PWRTB NAME=(,,,PBG2),                                         X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PBG2CODE - PUB GROUP CODE - SECOND LEVEL                                
*                                                                               
PBG2C    PWRTB NAME=(,,,PBG2CODE),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PBG2NAME - PUB GROUP NAME - SECOND LEVEL                                
*                                                                               
PBG2N    PWRTB NAME=(,,,PBG2NAME),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PUBZNE   - PUB ZONE - CODE AND NAME                                     
*                                                                               
PBZN     PWRTB NAME=(,,,PUBZNE),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBZNECD - PUB ZONE - CODE                                              
*                                                                               
PBZNC    PWRTB NAME=(,,,PUBZNECD),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBZNENM - PUB ZONE - NAME - SORTED BY CODE                             
*                                                                               
PBZNN    PWRTB NAME=(,,,PUBZNENM),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBZNENA - PUB ZONE - NAME - SORTED BY NAME                             
*                                                                               
PBZNA    PWRTB NAME=(,,,PUBZNENA),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBNUMCD - PUB NUMBER ONLY                                              
*                                                                               
PBNMC    PWRTB NAME=(,,,PUBNUMCD),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBEDNCD - PUB EDITION CODE                                             
*                                                                               
PBEDC    PWRTB NAME=(,,,PUBEDNCD),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PBKILLDT - PUB KILL DATE                                                
*                                                                               
PBKDT    PWRTB NAME=(,,,PBKILLDT),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PBL     - PUBLSH NAME & NUMBER                                          
*                                                                               
PBL      PWRTB NAME=(,,,PBL),                                          X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PBLCODE - PUBLISHER NUMBER                                              
*                                                                               
PBLCD    PWRTB NAME=(,,,PBLCODE),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PBLNAME - PUBLISHER NAME                                                
*                                                                               
PBLNM    PWRTB NAME=(,,,PBLNAME),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PBLS    - PUBLSH NA & NUM SORT BY NAME                                  
*                                                                               
PBLS     PWRTB NAME=(,,,PBLS),                                         X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCRATE  - PLANNED COST RATE                                             
*                                                                               
PCRTE    PWRTB NAME=(,,,PCRATE),                                       X        
               MENUM=BY$,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCG     - PLANNED COST GROSS                                            
*                                                                               
PCG      PWRTB NAME=(,,,PCG),                                          X        
               MENUM=BY$,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCN     - PLANNED COST NET                                              
*                                                                               
PCN      PWRTB NAME=(,,,PCN),                                          X        
               MENUM=BY$,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCAC    - PLANNED COST AGENCY COMMISSION                                
*                                                                               
PCAC     PWRTB NAME=(,,,PCAC),                                         X        
               MENUM=BY$,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCTAX   - PLANNED COST TAX                                              
*                                                                               
PCTAX    PWRTB NAME=(,,,PCTAX),                                        X        
               MENUM=BY$,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCACTMON- PLANNED COST ACTUALIZATION MONTH                              
*                                                                               
PCAMN    PWRTB NAME=(,,,PCACTMON),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCACTMON- PLANNED COST BILLING START MONTH                              
*                                                                               
PCBMN    PWRTB NAME=(,,,PCBLGMON),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCBLBG  - PLANNED COST BILLABLE GROSS                                   
*                                                                               
PCBLG    PWRTB NAME=(,,,PCBLBG),                                       X        
               MENUM=BY$,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCBLBBLN- PLANNED COST BILLABLE NET                                     
*                                                                               
PCBLN    PWRTB NAME=(,,,PCBLBN),                                       X        
               MENUM=BY$,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCORDG  - PLANNED COST ORDERED GROSS                                    
*                                                                               
PCORG    PWRTB NAME=(,,,PCORDG),                                       X        
               MENUM=BY$,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PCORDN  - PLANNED COST ORDERED NET                                      
*                                                                               
PCORN    PWRTB NAME=(,,,PCORDN),                                       X        
               MENUM=BY$,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PDCTLDT - CHECK CONTROL DATE                                            
*                                                                               
PDCTL    PWRTB NAME=(,,,PDCTLDT),                                      X        
               AORM=N,                                                 X        
               MENUM=PAY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PDDT    - PAID DATE                                                     
*                                                                               
PDDT     PWRTB NAME=(,,,PDDT),                                         X        
               AORM=N,                                                 X        
               MENUM=PAY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PDOPTS  - PAY OPTIONS                                                   
*                                                                               
PDOPS    PWRTB NAME=(,,,PDOPTS),                                       X        
               AORM=N,                                                 X        
               MENUM=PAY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PAYSRC  - PAY SOURCE                                                    
*                                                                               
PDSRC    PWRTB NAME=(,,,PAYSRC),                                       X        
               AORM=N,                                                 X        
               MENUM=PAY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PDREP   - PAY REP - NUMBER AND NAME                                     
*                                                                               
PDREP    PWRTB NAME=(,,,PDREP),                                        X        
               AORM=N,                                                 X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PDREPCD - PAY REP - NUMBER                                              
*                                                                               
PDRPC    PWRTB NAME=(,,,PDREPCD),                                      X        
               AORM=N,                                                 X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PDREPNAM - PAY REP - NAME                                               
*                                                                               
PDRPN    PWRTB NAME=(,,,PDREPNAM),                                     X        
               AORM=N,                                                 X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PDSEQ   - PAY SEQUENCE NUMBER                                           
*                                                                               
PDSEQ    PWRTB NAME=(,,,PDSEQ),                                        X        
               AORM=N,                                                 X        
               MENUM=PAY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PDTYPE  - PAY TYPE - CK OR CR OR NORMAL                                 
*                                                                               
PDTYP    PWRTB NAME=(,,,PDTYPE),                                       X        
               AORM=N,                                                 X        
               MENUM=PAY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PFAX    - PAYING FAX                                                    
*                                                                               
PFAX     PWRTB NAME=(,,,PFAX),                                         X        
               MENUM=PAY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PGACCNT - PGEST ACCOUNT                                                 
*                                                                               
PGACC    PWRTB NAME=(,,,PGACCNT),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PGAVG   - PG AVERGAGE COST                                              
*                                                                               
PGAVG    PWRTB NAME=(,,,PGAVG),                                        X        
               AORM=N,                                                 X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGBRAND - PGEST PG BRAND                                                
*                                                                               
PGBRN    PWRTB NAME=(,,,PGBRAND),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PGBNDSUF - PGEST BRAND SUFFIX                                           
*                                                                               
PGBSF    PWRTB NAME=(,,,PGBNDSUF),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PGCDISC - PG EFFECTIVE DISCOUNT FOR CONTRACT                            
*                                                                               
PGCDS    PWRTB NAME=(,,,PGCDISC),                                      X        
               AORM=N,                                                 X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGCHGPER - PGEST CHARGE PERIOD                                          
*                                                                               
PGCHG    PWRTB NAME=(,,,PGCHGPER),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PGDISC  - PG EFFECTIVE DISCOUNT                                         
*                                                                               
PGDSC    PWRTB NAME=(,,,PGDISC),                                       X        
               AORM=N,                                                 X        
               MENUM=OTH,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGEDIT  - P&G EDIT LIST                                                 
*                                                                               
PGEDL    PWRTB NAME=(,,,PGEDIT),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PGEST - PGEST PG EST                                                    
*                                                                               
PGEST    PWRTB NAME=(,,,PGEST),                                        X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PGEVNTCD - PGEST EVENT CODE                                             
*                                                                               
PGEVN    PWRTB NAME=(,,,PGEVNTCD),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PGMLTBND - PGEST MULTI-BRAND                                            
*                                                                               
PGMLT    PWRTB NAME=(,,,PGMLTBND),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PGNOBRND - PGEST NO BRAND                                               
*                                                                               
PGNOB    PWRTB NAME=(,,,PGNOBRND),                                     X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PGOPEN  - PG OPEN RATE                                                  
*                                                                               
PGOP     PWRTB NAME=(,,,PGOPEN),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGRP    - PRODUCT GROUP CODE AND NAME                                   
*                                                                               
PGRP     PWRTB NAME=(,,,PGRP),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGRPCODE - PRODUCT GROUP CODE                                           
*                                                                               
PGRPC    PWRTB NAME=(,,,PGRPCODE),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGRPNAME - PRODUCT GROUP NAME                                           
*                                                                               
PGRPN    PWRTB NAME=(,,,PGRPNAME),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGR1    - PRODUCT GROUP CODE AND NAME - FIRST LEVEL                     
*                                                                               
PGR1     PWRTB NAME=(,,,PGR1),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGR1CODE - PRODUCT GROUP CODE - FIRST LEVEL                             
*                                                                               
PGR1C    PWRTB NAME=(,,,PGR1CODE),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGR1NAME - PRODUCT GROUP NAME - FIRST LEVEL                             
*                                                                               
PGR1N    PWRTB NAME=(,,,PGR1NAME),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGR2    - PRODUCT GROUP CODE AND NAME - SECOND LEVEL                    
*                                                                               
PGR2     PWRTB NAME=(,,,PGR2),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGR2CODE - PRODUCT GROUP CODE - SECOND LEVEL                            
*                                                                               
PGR2C    PWRTB NAME=(,,,PGR2CODE),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGR2NAME - PRODUCT GROUP NAME - SECOND LEVEL                            
*                                                                               
PGR2N    PWRTB NAME=(,,,PGR2NAME),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PGTAPE  - P&G TAPE                                                      
*                                                                               
PGTPE    PWRTB NAME=(,,,PGTAPE),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=DD,                                                X        
               FLAVORM=BUY                                                      
*                                                                               
*       PICODE  - POSITION INSTRUCTION CODE                                     
*                                                                               
PICD     PWRTB NAME=(,,,PICODE),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PICOM   - POSITION INSTRUCTION                                          
*                                                                               
PICOM    PWRTB NAME=(,,,PICOM),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PNG     - P&G DATA COLLECTION                                           
*                                                                               
PNG      PWRTB NAME=(,,,PNG),                                          X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       POSDATE - POSTING DATE                                                  
*                                                                               
PODTE    PWRTB NAME=(,,,POSDATE),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PO#     - PURCHASE ORDER NUMBER                                         
*                                                                               
PO#      PWRTB NAME=(,,,PO#),                                          X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PO#     - PURCHASE ORDER END DATE                                       
*                                                                               
PO#EN    PWRTB NAME=(,,,PO#END),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PO#     - PURCHASE ORDER GROSS                                          
*                                                                               
PO#G     PWRTB NAME=(,,,PO#G),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PO#     - PURCHASE ORDER NET                                            
*                                                                               
PO#N     PWRTB NAME=(,,,PO#N),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PO#     - PURCHASE ORDER PERIOD                                         
*                                                                               
PO#PR    PWRTB NAME=(,,,PO#PER),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PO#     - PURCHASE ORDER START DATE                                     
*                                                                               
PO#ST    PWRTB NAME=(,,,PO#START),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PO#     - PURCHASE ORDER STATUS                                         
*                                                                               
PO#SX    PWRTB NAME=(,,,PO#STAT),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       POSTER  - POSTER (OUTDOOR)                                              
*                                                                               
POSTR    PWRTB NAME=(,,,POSTER),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=OUT,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PRD     - PRODUCT CODE & NAME                                           
*                                                                               
PRD      PWRTB NAME=(,,,PRD),                                          X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRDALC  - PRODUCT ALLOCATION                                            
*                                                                               
PRDAL    PWRTB NAME=(,,,PRDALC),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PRDCODE - PRODUCT CODE                                                  
*                                                                               
PRDCD    PWRTB NAME=(,,,PRDCODE),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRDINTER - PRD INTERFACE CODE                                           
*                                                                               
PRDIC    PWRTB NAME=(,,,PRDINTER),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRDINFC  - PRD INTERFACE CODE                                           
*                                                                               
PRIFC    PWRTB NAME=(,,,PRDINFC),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRDNAME - PRODUCT NAME                                                  
*                                                                               
PRDNM    PWRTB NAME=(,,,PRDNAME),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PREP    - PAY REP CODE & NAME                                           
*                                                                               
PREP     PWRTB NAME=(,,,PREP),                                         X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       SREP    - SPECIAL REP CODE & NAME                                       
*                                                                               
SREP     PWRTB NAME=(,,,SREP),                                         X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SREPCD   - SPECIAL REP CODE                                             
*                                                                               
SREPC    PWRTB NAME=(,,,SREPCD),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SREPNM   - SPECIAL REP NAME                                             
*                                                                               
SREPN    PWRTB NAME=(,,,SREPNM),                                       X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PREPADDR - PAYING REP ADDRESS                                           
*                                                                               
PRPAD    PWRTB NAME=(,,,PREPADDR),                                     X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PREPATTN - PAYING REP ATTENTION                                         
*                                                                               
PRPAT    PWRTB NAME=(,,,PREPATTN),                                     X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PREPCODE - PAYING REP CODE                                              
*                                                                               
PRPCD    PWRTB NAME=(,,,PREPCODE),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PREPFAX - PAYING REP FAX                                                
*                                                                               
PRPFX    PWRTB NAME=(,,,PREPFAX),                                      X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PREPNAME - PAYING REP NAME                                              
*                                                                               
PRPNM    PWRTB NAME=(,,,PREPNAME),                                     X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PREPTEL - PAYING REP TELEPHONE                                          
*                                                                               
PRPTL    PWRTB NAME=(,,,PREPTEL),                                      X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRODADDR - PRODUCTION HOUSE REP ADDRESS                                 
*                                                                               
PRHAD    PWRTB NAME=(,,,PRODADDR),                                     X        
               MENUM=AD,                                               X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRODATTN - PRODUCTION HOUSE ATTENTION                                   
*                                                                               
PRHAT    PWRTB NAME=(,,,PRODATTN),                                     X        
               MENUM=AD,                                               X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRODCODE - PRODUCTION HOUSE REP CODE                                    
*                                                                               
PRHCD    PWRTB NAME=(,,,PRODCODE),                                     X        
               MENUM=AD,                                               X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRODFAX - PRODUCTION HOUSE REP FAX                                      
*                                                                               
PRHFX    PWRTB NAME=(,,,PRODFAX),                                      X        
               MENUM=AD,                                               X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRODNAME - PRODUCTION HOUSE REP NAME                                    
*                                                                               
PRHNM    PWRTB NAME=(,,,PRODNAME),                                     X        
               MENUM=AD,                                               X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRODREP - PRODUCTION HOUSE REP CODE & NAME                              
*                                                                               
PRHRP    PWRTB NAME=(,,,PRODREP),                                      X        
               MENUM=AD,                                               X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PRODTEL - PRODUCTION HOUSE REP TELEPHONE                                
*                                                                               
PRHTL    PWRTB NAME=(,,,PRODTEL),                                      X        
               MENUM=AD,                                               X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PST-TAX - PST                                                           
*                                                                               
PSTTX    PWRTB NAME=(,,,PST-TAX),                                      X        
               AORM=N,                                                 X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       PTEL    - PAYING TELEPHONE                                              
*                                                                               
PTEL     PWRTB NAME=(,,,PTEL),                                         X        
               MENUM=(PAY,PUB),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PTTAPE  - PHILLIPMORRIS COMMISSION TAPE                                 
*                                                                               
PTTPE    PWRTB NAME=(,,,PTTAPE),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=DD,                                                X        
               FLAVORM=                                                         
*                                                                               
*       PUB     - PUB NAME & NUMBER                                             
*                                                                               
PUB      PWRTB NAME=(,,,PUB),                                          X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBLOCK - PUB LOCK                                                      
*                                                                               
PUBLK    PWRTB NAME=(,,,PUBLOCK),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBADDR - PUB ADDRESS                                                   
*                                                                               
PUBAD    PWRTB NAME=(,,,PUBADDR),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBADDRS - PUB ADDRESS - NO NAME                                        
*                                                                               
PBADS    PWRTB NAME=(,,,PUBADDRS),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBADDR1 - PUB ADDRESS - LINE 1                                         
*                                                                               
PBAD1    PWRTB NAME=(,,,PUBADDR1),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBADDR2 - PUB ADDRESS - LINE 2                                         
*                                                                               
PBAD2    PWRTB NAME=(,,,PUBADDR2),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBATTN - PUB ATTENTION                                                 
*                                                                               
PUBAT    PWRTB NAME=(,,,PUBATTN),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBCLASS - PUB CLASSIFICATION                                           
*                                                                               
PUBCL    PWRTB NAME=(,,,PUBCLASS),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBFAX  - PUB FAX                                                       
*                                                                               
PUBFX    PWRTB NAME=(,,,PUBFAX),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBLANG - PUB LANGUAGE                                                  
*                                                                               
PBLNG    PWRTB NAME=(,,,PUBLANG),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBLBOTH - PUB LIST CODE & NAME                                         
*                                                                               
PUBL     PWRTB NAME=(,,,PUBLBOTH),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBLIST - PUB LIST CODE                                                 
*                                                                               
PUBLC    PWRTB NAME=(,,,PUBLIST),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBLNAME - PUB LIST NAME                                                
*                                                                               
PUBLN    PWRTB NAME=(,,,PUBLNAME),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBNA   - PUB NAME                                                      
*                                                                               
PUBNA    PWRTB NAME=(,,,PUBNA),                                        X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBNAZ  - PUB/ZONE NAME                                                 
*                                                                               
PUBNZ    PWRTB NAME=(,,,PUBNAZ),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBNUM  - PUBLICATION NUMBER                                            
*                                                                               
PUBNB    PWRTB NAME=(,,,PUBNUM),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBNUNA - PUB NUMBER & NAME                                             
*                                                                               
PBNNM    PWRTB NAME=(,,,PUBNUNA),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBNUNZ - PUB# & NAME/ZONE                                              
*                                                                               
PBNNZ    PWRTB NAME=(,,,PUBNUNZ),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBSTCDE - PUB STATE CODE                                               
*                                                                               
PBSTC    PWRTB NAME=(,,,PUBSTCDE),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBTEL  - PUB TEL.                                                      
*                                                                               
PUBTL    PWRTB NAME=(,,,PUBTEL),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBWEB  - PUB WEBSITE                                                   
*                                                                               
PUBWB    PWRTB NAME=(,,,PUBWEB),                                       X        
               MENUM=PUB,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBZ    - PUB/ZONE NAME & #                                             
*                                                                               
PBZ      PWRTB NAME=(,,,PUBZ),                                         X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       PUBZZEEE - PUB PPPPPPPPZZEEE                                            
*                                                                               
PBZED    PWRTB NAME=(,,,PUBZZEEE),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       RANK     - RANK SPECIAL KEYWORD                                         
*                                                                               
RANK     PWRTB NAME=(,,,RANK),                                         X        
               MENUM=OTH,                                              X        
               FIELDM=(ROW,COL),                                       X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       RATECODE - BUY RATE CODE                                                
*                                                                               
RTECD    PWRTB NAME=(,,,RATECODE),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       RATEFDTE - RATE EFF DATE                                                
*                                                                               
RTEFD    PWRTB NAME=(,,,RATEFDTE),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       RATEFDT2 - NXT RATE EFF DATE                                            
*                                                                               
RTEF2    PWRTB NAME=(,,,RATEFDT2),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       RECORD  - DRIVER KEYWORD                                                
*                                                                               
RECRD    PWRTB NAME=(,,,RECORD),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       REFNO   - REFERENCE NUMBER                                              
*                                                                               
REFNO    PWRTB NAME=(,,,REFNO),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       REG     - REGION CODE & NAME                                            
*                                                                               
REG      PWRTB NAME=(,,,REG),                                          X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       REGCODE - REGION CODE                                                   
*                                                                               
REGCD    PWRTB NAME=(,,,REGCODE),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       REGNAME - REGION NAME                                                   
*                                                                               
REGNM    PWRTB NAME=(,,,REGNAME),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       REGULAR - REGULAR (OUTDOOR)                                             
*                                                                               
REGLR    PWRTB NAME=(,,,REGULAR),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=OUT,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       RPT     - NUMBER OF REPAINTS                                            
*                                                                               
RPT      PWRTB NAME=(,,,RPT),                                          X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=OUT,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       RUNDATE - REPORT RUNDATE                                                
*                                                                               
RUNDT    PWRTB NAME=(,,,RUNDATE),                                      X        
               MENUM=(OTH,BUY),                                        X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       RTLSCH  - RETAIL SCHEME CODE                                            
*                                                                               
RTLSC    PWRTB NAME=(,,,RTLSCH),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       R1      - R1 BILLING                                                    
*                                                                               
R1       PWRTB NAME=(,,,R1),                                           X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       R4      - R4 BILLING                                                    
*                                                                               
R4       PWRTB NAME=(,,,R4),                                           X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       R7      - R7 BILLING                                                    
*                                                                               
R7       PWRTB NAME=(,,,R7),                                           X        
               AORM=N,                                                 X        
               MENUM=BLL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       SADDRN  - SHIPPING ADDRESS - NAME                                       
*                                                                               
SADDN    PWRTB NAME=(,,,SADDRN),                                       X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SADDR   - SHIPPING ADDRESS                                              
*                                                                               
SADDR    PWRTB NAME=(,,,SADDR),                                        X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SADDR1  - SHIPPING ADDRESS - LINE 1                                     
*                                                                               
SADD1    PWRTB NAME=(,,,SADDR1),                                       X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SADDR2  - SHIPPING ADDRESS - LINE 2                                     
*                                                                               
SADD2    PWRTB NAME=(,,,SADDR2),                                       X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SATTN   - SHIPPING ATTENTION                                            
*                                                                               
SATTN    PWRTB NAME=(,,,SATTN),                                        X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SEMAIL  - SHIPPING E-MAIL                                               
*                                                                               
SEML     PWRTB NAME=(,,,SEMAIL),                                       X        
               MENUM=TRA,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       SFAX    - SHIPPING FAX                                                  
*                                                                               
SFAX     PWRTB NAME=(,,,SFAX),                                         X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SHOW    - SHOW (OUTDOOR)                                                
*                                                                               
SHOW     PWRTB NAME=(,,,SHOW),                                         X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=OUT,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SHIPDATE - SHIP DATE                                                    
*                                                                               
SHDTE    PWRTB NAME=(,,,SHIPDATE),                                     X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SHPADCDE - SHIPPING AD CODE                                             
*                                                                               
SHPAD    PWRTB NAME=(,,,SHPADCDE),                                     X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SHPDATE  - SHIPPING DATE                                                
*                                                                               
SHPDT    PWRTB NAME=(,,,SHPDATE),                                      X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SHPOVPCT - SHIPPING OVERAGE PER CENT                                    
*                                                                               
SHPOP    PWRTB NAME=(,,,SHPOVPCT),                                     X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SHPPOST  - SHIPPED POSTERS                                              
*                                                                               
SHPPS    PWRTB NAME=(,,,SHPPOST),                                      X        
               MENUM=TRA,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SHPPSTOT - SHIPPED POSTERS AND OVERAGE                                  
*                                                                               
SHPPT    PWRTB NAME=(,,,SHPPSTOT),                                     X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SPACE   - SPACE DESCRIPTION                                             
*                                                                               
SPACE    PWRTB NAME=(,,,SPACE),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       RATETYPE - BUY RATE TYPE                                                
*                                                                               
RATTY    PWRTB NAME=(,,,RATETYPE),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SPACE/NW - SPACE DESC FOR NEWSPAPER                                     
*                                                                               
SPCNW    PWRTB NAME=(,,,SPACE/NW),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=NWS,                                             X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SPCLOSE - SPACE CLOSING DATE                                            
*                                                                               
SPDTE    PWRTB NAME=(,,,SPCLOSE),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRCOM   - SPECIAL REMIT COMMENT                                         
*                                                                               
SRCOM    PWRTB NAME=(,,,SRCOM),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRHST#  - ESR NUMBER - HISTORY                                          
*                                                                               
SRH#     PWRTB NAME=(,,,SRHST#),                                       X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRHSTDT - ESR DATE   - HISTORY                                          
*                                                                               
SRHDT    PWRTB NAME=(,,,SRHSTDTE),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRHSTSTA - ESR STATUS   - HISTORY                                       
*                                                                               
SRHST    PWRTB NAME=(,,,SRHSTSTA),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRHSTGEN - ESR GENERATSRN METHOD   - HISTORY                            
*                                                                               
SRHGN    PWRTB NAME=(,,,SRHSTGEN),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRHSTTYP - ESR TYPE  - HISTORY                                          
*                                                                               
SRHTP    PWRTB NAME=(,,,SRHSTTYP),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRLST#  - ESR NUMBER - LAST                                             
*                                                                               
SRL#     PWRTB NAME=(,,,SRLST#),                                       X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRLSTDT - ESR DATE   - LAST                                             
*                                                                               
SRLDT    PWRTB NAME=(,,,SRLSTDTE),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRLSTSTA - ESR STATUS   - LAST                                          
*                                                                               
SRLST    PWRTB NAME=(,,,SRLSTSTA),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRLSTGEN - ESR GENERATSRN METHOD   - LAST                               
*                                                                               
SRLGN    PWRTB NAME=(,,,SRLSTGEN),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SRLSTTYP - ESR TYPE  - LAST                                             
*                                                                               
SRLTP    PWRTB NAME=(,,,SRLSTTYP),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SR1ST#  - ESR NUMBER - FIRST                                            
*                                                                               
SR1#     PWRTB NAME=(,,,SR1ST#),                                       X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SR1STDT - ESR DATE   - FIRST                                            
*                                                                               
SR1DT    PWRTB NAME=(,,,SR1STDTE),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SR1STSTA - ESR STATUS   - FIRST                                         
*                                                                               
SR1ST    PWRTB NAME=(,,,SR1STSTA),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SR1STGEN - ESR GENERATSRN METHOD   - FIRST                              
*                                                                               
SR1GN    PWRTB NAME=(,,,SR1STGEN),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SR1STTYP - ESR TYPE  - FIRST                                            
*                                                                               
SR1TP    PWRTB NAME=(,,,SR1STTYP),                                     X        
               MENUM=EIO,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       ST/CITY - STATE / CITY                                                  
*                                                                               
STSTY    PWRTB NAME=(,,,ST/CITY),                                      X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       STACK   - PLACE HOLDER FOR STACKING                                     
*                                                                               
STACK    PWRTB NAME=(5,,,STACK=),                                      X        
               MENUM=OTH,                                              X        
               FIELDM=(ROW,COL),                                       X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       STATECDE - STATE CODE                                                   
*                                                                               
STCD     PWRTB NAME=(,,,STATECDE),                                     X        
               MENUM=PUB,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       STEL    - SHIPPING TELEPHONE                                            
*                                                                               
STEL     PWRTB NAME=(,,,STEL),                                         X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       SMED     - SUBMEDIA CODE AND NAME                                       
*                                                                               
SMD      PWRTB NAME=(,,,SMED),                                         X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       SMEDCODE - SUBMEDIA CODE                                                
*                                                                               
SMDCD    PWRTB NAME=(,,,SMEDCODE),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       SMEDNAME - SUBMEDIA NAME                                                
*                                                                               
SMDNM    PWRTB NAME=(,,,SMEDNAME),                                     X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       SVNGL-C - TOTAL SAVINGS                                                 
*                                                                               
SVNLC    PWRTB NAME=(,,,SVNGL-C),                                      X        
               MENUM=CON,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       SVNGL-H - TOTAL SAVINGS                                                 
*                                                                               
SVNLH    PWRTB NAME=(,,,SVNGL-H),                                      X        
               MENUM=CON,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TEXT = ANYTHING                                                         
*                                                                               
TEXT     PWRTB NAME=(4,,,TEXT=),                                       X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TADDRN  - TRAFFIC ADDRESS - NAME                                        
*                                                                               
TADDN    PWRTB NAME=(,,,TADDRN),                                       X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TADDR   - TRAFFIC ADDRESS                                               
*                                                                               
TADDR    PWRTB NAME=(,,,TADDR),                                        X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TADDR1  - TRAFFIC ADDRESS - LINE 1                                      
*                                                                               
TADD1    PWRTB NAME=(,,,TADDR1),                                       X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TADDR2  - TRAFFIC ADDRESS - LINE 2                                      
*                                                                               
TADD2    PWRTB NAME=(,,,TADDR2),                                       X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TATTN   - TRAFFIC ATTENTION                                             
*                                                                               
TATTN    PWRTB NAME=(,,,TATTN),                                        X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TEMAIL  - TRAFFIC E-MAIL                                                
*                                                                               
TEML     PWRTB NAME=(,,,TEMAIL),                                       X        
               MENUM=TRA,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TFAX    - TRAFFIC FAX                                                   
*                                                                               
TFAX     PWRTB NAME=(,,,TFAX),                                         X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TITLE   - COLUMN FOR STACK LINE TITLES                                  
*                                                                               
TITLE    PWRTB NAME=(,,,TITLE),                                        X        
               MENUM=OTH,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=DD,                                                X        
               FLAVORM=                                                         
*                                                                               
*       TRBCHGS  - TRAFFIC CHANGES                                              
*                                                                               
TRBCH    PWRTB NAME=(,,,TRBCHGS),                                      X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       TREP    - TRA REP CODE & NAME                                           
*                                                                               
TREP     PWRTB NAME=(,,,TREP),                                         X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TREPADDR - TRAFFIC REP ADDRESS                                          
*                                                                               
TRPAD    PWRTB NAME=(,,,TREPADDR),                                     X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TREPATTN - TRAFFIC REP ATTENTION                                        
*                                                                               
TRPAT    PWRTB NAME=(,,,TREPATTN),                                     X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TREPCODE - TRAFFIC REP CODE                                             
*                                                                               
TRPCD    PWRTB NAME=(,,,TREPCODE),                                     X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TREPFAX - TRAFFIC REP FAX                                               
*                                                                               
TRPFX    PWRTB NAME=(,,,TREPFAX),                                      X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TREPNAME - TRAFFIC REP NAME                                             
*                                                                               
TRPNM    PWRTB NAME=(,,,TREPNAME),                                     X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TREPTEL - TRAFFIC REP TELEPHONE                                         
*                                                                               
TRPTL    PWRTB NAME=(,,,TREPTEL),                                      X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TRASTAT - TRAFFIC STATUS OF BUY                                         
*                                                                               
TROPT    PWRTB NAME=(,,,TRAOPT),                                       X        
               MENUM=(TRA),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TSCAP   - TEARSHEET CAPTION STATUS                                      
*                                                                               
TSCAP    PWRTB NAME=(,,,TSCAP),                                        X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TSCOM   - TEARSHEET COMMENTS                                            
*                                                                               
TSCOM    PWRTB NAME=(,,,TSCOM),                                        X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TSINSDAT - TEARSHEET INSERTION DATE STATUS                              
*                                                                               
TSIDT    PWRTB NAME=(,,,TSINSDAT),                                     X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TSPAGE  - TEARSHEET PAGE DESCRIPTION                                    
*                                                                               
TSPAG    PWRTB NAME=(,,,TSPAGE),                                       X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TSCAP   - TEARSHEET POSITION                                            
*                                                                               
TSPOS    PWRTB NAME=(,,,TSPOS),                                        X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TSRPR   - TEARSHEET REPRO   STATUS                                      
*                                                                               
TSRPR    PWRTB NAME=(,,,TSREPRO),                                      X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TSSPC   - TEARSHEET SPACE   STATUS                                      
*                                                                               
TSSPC    PWRTB NAME=(,,,TSSPACE),                                      X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TSSTAT  - TEARSHEET STATUS                                              
*                                                                               
TSSTT    PWRTB NAME=(,,,TSSTAT),                                       X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TST1    - TEST KEYWORD 1                                                
*                                                                               
TST1     PWRTB NAME=(,,,TST1),                                         X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TST2    - TEST KEYWORD 2                                                
*                                                                               
TST2     PWRTB NAME=(,,,TST2),                                         X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TSZONES - TEARSHEET ZONES                                               
*                                                                               
TSZNS    PWRTB NAME=(,,,TSZONES),                                      X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
* TEARSHEET RECEIVED (PROOF OF PERFORMANCE)                                     
*                                                                               
TSRVD    PWRTB NAME=(,,,TSRECVD),                                      X        
               MENUM=(BUY),                                            X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       TTEL    - TRAFFIC TELEPHONE                                             
*                                                                               
TTEL     PWRTB NAME=(,,,TTEL),                                         X        
               MENUM=TRA,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       UDEF    - USER FIELDS                                                   
*                                                                               
UDEF     PWRTB NAME=(4,,,UDEF=),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       UCOM    - USER FIELDS                                                   
*                                                                               
UCOM     PWRTB NAME=(4,,,UCOM=),                                       X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
*                                                                               
*       UPID    - UPLOAD UNIQUE ID                                              
*                                                                               
UPID     PWRTB NAME=(,,,UPID),                                         X        
               MENUM=OTH,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       USEAGE  - CONTRACT BUY USEAGE                                           
*                                                                               
USAGE    PWRTB NAME=(,,,USEAGE),                                       X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       WBCLICKS - WEBSITE CLICK THROUGHS                                       
*                                                                               
WBCLK    PWRTB NAME=(,,,WBCLICKS),                                     X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       WBVIEWS - WEBSITE PAGE VIEWS                                            
*                                                                               
WBVWS    PWRTB NAME=(,,,WBVIEWS),                                      X        
               MENUM=BUY,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       WLINV   - INVOICE NUMBER                                                
*                                                                               
WLINV    PWRTB NAME=(,,,WLINV),                                        X        
               MENUM=BUY,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       WLMED   - WARNER LAMBERT SPECIAL MEDIA CODE                             
*                                                                               
WLMED    PWRTB NAME=(,,,WLMED),                                        X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       WSJRATE - WALL STREET JOURNAL UNIT RATE                                 
*                                                                               
WSJRT    PWRTB NAME=(,,,WSJRATE),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       WSJRATE - WALL STREET JOURNAL RATE CODE                                 
*                                                                               
WSJRC    PWRTB NAME=(,,,WSJRC),                                        X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
*       WSJRATE - WALL STREET JOURNAL UNIT                                      
*                                                                               
WSJUN    PWRTB NAME=(,,,WSJUNIT),                                      X        
               MENUM=MCL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
WRITBLX  PWRTB TYPE=END            END OF TABLE                                 
*                                                                               
         TITLE 'PRVAL2 - PWR COLUMN FILTERS AND FORMATS'                        
***********************************************************************         
*                                                                     *         
* (02)   WRITER COLUMN FILTER/FORMAT TABLE                            *         
*        INTERNAL CODES COME FROM PRWRIEQUS                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CFLT     PWRTB TYPE=INITIAL,PREFIX=PR                                           
*                                                                               
CFABS    PWRTB NAME=(,,,ABS),      ABSOLUTE VALUE                      X        
               AORM=N,                                                 X        
               MENUM=CFM,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
CFACH    PWRTB NAME=(4,5,AC=,ACHG=+), ADDITIONAL CHARGES               X        
               CNTRL=CHLD,                                             X        
               AORM=N,                                                 X        
               MENUM=(CFL,OTH),                                        X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFADD    PWRTB NAME=(,,,ADD),      AVERAGE DAYS TO DISBURSEMENT        X        
               AORM=N,                                                 X        
               MENUM=COT,                                              X        
               FIELDM=(HEAD,COL),                                      X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFNAD    PWRTB NAME=(,,,NONADD),   NON ADD CASHFLOW                    X        
               AORM=N,                                                 X        
               MENUM=COT,                                              X        
               FIELDM=(COL),                                           X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFAOR    PWRTB NAME=(,,,AOR),      AOR BILLS ONLY                      X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFNAR    PWRTB NAME=(,,,NAOR),     AOR BILLS EXCLUDED                  X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFCMA    PWRTB NAME=(6,7,,COMMAS=),  COMMAS=Y/N                        X        
               AORM=N,                                                 X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFCOM    PWRTB NAME=(,,,COM),      COMMISSION ONLY BILLS               X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFCT     PWRTB NAME=(,,CT,COUNT),  COUNT OCCURRENCES OF FIELD          X        
               MENUM=COT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFDBD    PWRTB NAME=(2,,,BD=),     DATES - BILLED                      X        
               AORM=N,                                                 X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDBI    PWRTB NAME=(2,,,BI=),     DATES - BILL INVOICE                X        
               AORM=N,                                                 X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDBL    PWRTB NAME=(2,,,BL=),     DATES - BILLABLE                    X        
               AORM=N,                                                 X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDBR    PWRTB NAME=(2,,,BR=),     DATES - BILL RUN                    X        
               AORM=N,                                                 X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDCL    PWRTB NAME=(2,,,CL=),     DATES - SPACE CLOSING               X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDDU    PWRTB NAME=(2,,,DU=),     DATES - INVOICE DUE                 X        
               AORM=N,                                                 X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDED    PWRTB NAME=(2,,,ED=),     DATES - INVOICE EDE                 X        
               AORM=N,                                                 X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDID    PWRTB NAME=(2,,,ID=),     DATES - INSERT                      X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDIO    PWRTB NAME=(2,,,IO=),     DATES - INSERTION ORDER             X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDMC    PWRTB NAME=(2,,,MC=),     DATES - MATERIAL CLOSING            X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDMS    PWRTB NAME=(2,,,MS=),     DATES - MONTH OF SERVICE            X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDOS    PWRTB NAME=(2,,,OS=),     DATES - ON STAND                    X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDPD    PWRTB NAME=(2,,,PD=),     DATES - PAID                        X        
               AORM=N,                                                 X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDPY    PWRTB NAME=(2,,,PY=),     DATES - PAYABLE                     X        
               AORM=N,                                                 X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDUP    PWRTB NAME=(2,,,UP=),     DATES - UNPAID                      X        
               AORM=N,                                                 X        
               CNTRL=CHLD,                                             X        
               MENUM=CDT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFEF     PWRTB NAME=(2,,,EF=),     ESTIMATE FILTER                     X        
               CNTRL=CHLD,                                             X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDEC    PWRTB NAME=(3,,,DEC=),    NUMBER OF DECIMALS                  X        
               MENUM=CFM,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDET    PWRTB NAME=(3,,,DET=),    NUMBER OF DETAIL LEVELS IN RECAP    X        
               MENUM=CFM,                                              X        
               FIELDM=(ROW,COL),                                       X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDIV    PWRTB NAME=(3,,,DIV=),    SCALING FACTOR - DIVIDE BY 10**N    X        
               MENUM=CFM,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFDLZ    PWRTB NAME=(3,,,DELZERO), ZERO IF BUY DELETED                 X        
               MENUM=CFM,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFDUE    PWRTB NAME=(,,,DUE),      USE INVOICE DUE DATE IN ADD CALC    X        
               AORM=N,                                                 X        
               MENUM=COT,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFLIV    PWRTB NAME=(,,,LIVE),     INCLUDE BUYS - LIVE                 X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFTST    PWRTB NAME=(,,,TEST),     INCLUDE BUYS - TEST                 X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFTBS    PWRTB NAME=(,,,TRABUYS),  TRAFFICKED BUYS ONLY                X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFTBX    PWRTB NAME=(,,,XTRABUYS), EXCLUDE TRAFFICK BUYS               X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFGSI    PWRTB NAME=(1,,,IGST),    INCLUDE GST - INPUT                 X        
               AORM=N,                                                 X        
               MENUM=COT,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFGSO    PWRTB NAME=(1,,,OGST),    INCLUDE GST - OUTPUT                X        
               AORM=N,                                                 X        
               MENUM=COT,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFPSI    PWRTB NAME=(1,,,IPST),    INCLUDE PST - INPUT                 X        
               AORM=N,                                                 X        
               MENUM=COT,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFPSO    PWRTB NAME=(1,,,OPST),    INCLUDE PST - OUTPUT                X        
               AORM=N,                                                 X        
               MENUM=COT,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFTRM    PWRTB NAME=(,,,TRIM),     TRIM DECIMALS IF TOO LARGE          X        
               MENUM=CFM,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFMAJ    PWRTB NAME=(3,,,MAJOR),    MAJOR SORT FIELD IN STACKING       X        
               MENUM=COT,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFSP     PWRTB NAME=(2,,,SP=),     SPACE DESRIPTION                    X        
               CNTRL=CHLD,                                             X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFMAT    PWRTB NAME=(,,,MAT),      INVOICE MATCHED FILTER              X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFUNM    PWRTB NAME=(3,,,UNMAT),    INVOICE UNMATCHED FILTER           X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFUFC    PWRTB NAME=(,,,UFC),      UPFRONT COMMISSION BILLS            X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFNET    PWRTB NAME=(,,,NET),      NET ONLY BILLS                      X        
               AORM=N,                                                 X        
               MENUM=CFL,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFCTR    PWRTB NAME=(1,,,CENTER),  PRINT FIELD IN CENTER OF COLUMN     X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFLEF    PWRTB NAME=(1,,,LEFT),     PRINT FIELD ON LEFT OF COLUMN      X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFRIG    PWRTB NAME=(1,,,RIGHT),    PRINT FIELD AT RIGHT OF COLUMN     X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFLEN    PWRTB NAME=(1,,,LENGTH=), COLUMN WIDTH                        X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFRLO    PWRTB NAME=(6,,,NOHELD),  DROP HELD BUYS                      X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFHLO    PWRTB NAME=(8,,,HELDONLY), HELD BUYS ONLY                     X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFH1     PWRTB NAME=(1,,,H1=),     HEADLINE 1                          X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFH2     PWRTB NAME=(2,,,H2=),     HEADLINE 2                          X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFH3     PWRTB NAME=(2,,,H3=),     HEADLINE 3                          X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFH4     PWRTB NAME=(2,,,H4=),     HEADLINE 4                          X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFHT     PWRTB NAME=(2,,,HT=),     HEADER FOR TOTAL LINE               X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=N,                                                 X        
               FLAVORM=                                                         
CFISN    PWRTB NAME=(3,,,ISSUE),   PRINT ISSUE NAME                    X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFND     PWRTB NAME=(,,ND,NODETAIL),    DO NOT PRINT DETAILS           X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFNDT    PWRTB NAME=(,,DROP,SUPPRESS),  PRINT ONLY IF DATA PRESENT     X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFNBX    PWRTB NAME=(,,,NOBOX),    NO BOX ON LEFT OF FIELD             X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFNMD    PWRTB NAME=(,,,NOMEDIA),  DROP MEDIA FROM SORT OF KYWD        X        
               AORM=N,                                                 X        
               MENUM=CFM,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFNP     PWRTB NAME=(,,NP,NOPRINT),    NO PRINTING  FOR THIS COLUMN    X        
               MENUM=CFM,                                              X        
               FIELDM=ROW,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFNT     PWRTB NAME=(,,NT,NOTOTAL),     NO TOTALLING FOR THIS COLUMN   X        
               MENUM=CFM,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFOUT    PWRTB NAME=(6,,,OUTLET=),    OUTLET= FILTER                   X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFPBL    PWRTB NAME=(3,,,PBLANK),     PRINT BLANKS INSTEAD OF ZEROS    X        
               MENUM=CFM,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFPGE    PWRTB NAME=(,,,PAGE),     RESET PAGE NUMBER TO 1              X        
               MENUM=CFM,                                              X        
               FIELDM=(HEAD,COL),                                      X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFSKP    PWRTB NAME=(,,,SKIP),     SKIP TO NEW PAGE BEFORE TOTALS      X        
               MENUM=CFM,                                              X        
               FIELDM=(HEAD,COL),                                      X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFSPC    PWRTB NAME=(,,,SPACE),    SPACING AFTER PRINTING TOTALS       X        
               MENUM=CFM,                                              X        
               FIELDM=(HEAD,COL),                                      X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFSRN    PWRTB NAME=(,,,NO*RATE),  NO *RATE BUYS                       *        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFSRO    PWRTB NAME=(,,,*RATEONLY),  *RATE BUYS ONLY                   X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CFSMD    PWRTB NAME=(3,9,SMD=,SUBMEDIA=),  SUBMEDIA FILTER             X        
               CNTRL=CHLD,                                             X        
               MENUM=CFL,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
CF000    PWRTB NAME=(5,5,,(000)), PRINT RESULTS IN THOUSANDS (000)'S   X        
               MENUM=CFM,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFTHO    PWRTB NAME=(4,4,,THOU),  DIVIDE RESULT BY 1000                X        
               MENUM=CFM,                                              X        
               FIELDM=COL,                                             X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFTOT    PWRTB NAME=(,,*,TOTAL),   SUB-TOTAL ON BREAK                  X        
               MENUM=CFM,                                              X        
               FIELDM=(ROW,COL),                                       X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=                                                         
CFTSS    PWRTB NAME=(3,9,TSS=,TSSTATUS=),  TEARSHEET STATUS            X        
               CNTRL=CHLD,                                             X        
               MENUM=COT,                                              X        
               FIELDM=(ROW,COL),                                       X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
CFTSR    PWRTB NAME=(3,8,TSR=,TSREPRO=),   TEARSHEET REPRO             X        
               CNTRL=CHLD,                                             X        
               MENUM=COT,                                              X        
               FIELDM=(ROW,COL),                                       X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
CFWBF    PWRTB NAME=(5,5,,WKBFD), WEEK OF /BFD INDICATOR               X        
               MENUM=CFM,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
CFXCC    PWRTB NAME=(7,7,,XCRCCLT), EXCLUDE CLIENT CIRCULATION         X        
               MENUM=CFL,                                              X        
               FIELDM=HEAD,                                            X        
               MEDIAM=,                                                X        
               DOCM=Y,                                                 X        
               FLAVORM=BUY                                                      
*                                                                               
CFLTTBLX PWRTB TYPE=END                                                         
*                                                                               
PRVAL2X  DS    0C                  PAD OUT TO CONSTANT SIZE                     
         DS    (30*1024-24-(PRVAL2X-PRVAL2))X'00'                               
         DS    0D                                                               
*                                                                               
         EJECT                                                                  
*PRWRIEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRWRIEQUS                                                      
*PRVALTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRVALTABD                                                      
*DDLANGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'200PRVAL2    01/28/21'                                      
         END                                                                    
