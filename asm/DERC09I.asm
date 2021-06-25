*          DATA SET DERC09I    AT LEVEL 004 AS OF 01/04/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE DERC09IA                                                                 
*---------------------------------------------------------------------*         
* RADIO COUNTY COVERAGE CONVERSION: INPUT PHASE.                      *         
*                                                                     *         
* IPHASE: DERC09I                                                     *         
* OPHASE: DERC09O                                                     *         
*                                                                     *         
* THIS CONVERSION TAKES AS INPUT TWO TEXT FILES PRODUCED BY THE       *         
* PRE-PROCESSOR IN DERCTYICE. THE TWO INPUT FILES ARE CONCATENATED    *         
* IN THE JCL AND LOOK LIKE A SINGLE FILE TO THE CONVERSION.           *         
*                                                                     *         
*                 ******* IMPORTANT *******                           *         
* THE DEFAULT BOOK USED IS APRIL OF THE CURRENT CALENDAR YEAR. THE    *         
* BOOK IS NOT DERIVED FROM THE DATA ITSELF. THAT MEANS THAT IF WE     *         
* BACKLOAD ANY DATA, WE MUST PROVIDE FORCE= AND BOOK= CONTROL CARDS,  *         
* OR WE WILL OVERWRITE DATA FOR THE CURRENT YEAR.                     *         
*                                                                     *         
*---------------------------------------------------------------------*         
         TITLE '- DEMO CONVERSION - RADIO COUNTY COVERAGE'                      
                                                                                
DERC09I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DERC09I,RA,RC,RR=RE                                            
         USING DPRINT,R7           R7 -> PRINT CSECT                            
         USING DEMCOND,R8          R8 -> GLOBAL WORKING STORAGE                 
         L     R5,AIREC            R5 -> INTERIM RECD  (INTERD)                 
         USING INTERD,R5           KEY, INFO, AND DEMO VALUES                   
         LA    RE,MAXIRECL                                                      
         CHI   RE,2000                                                          
         BNH   *+6                 INTERIM RECORD LENGTH OVER MAX               
         DC    H'0'                ALLOWED OF 2000                              
*                                                                               
         B     *+4(R1)             ROUTINE HOOK                                 
         B     READ                PROCESS INPUT TAPE                           
         B     CNVWR               SORT RECORD HOOK                             
         B     ENDJOB              E-O-F ON INPUT                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ - GET INPUT RECORDS ONE AT A TIME AND PROCESS. BUILD INTERIM             
*        RECORDS.                                                               
***********************************************************************         
READ     DS    0H                                                               
*                                                                               
         L     R9,ARREC            INPUT RECORD AREA                            
*                                                                               
         CLI   INTAPESW,1          TEST FOR OPENING INPUT TAPE                  
         BE    READ10                                                           
         OPEN  (IN1,(INPUT))                                                    
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(23),=C'** STARTING INPUT PHASE'                                
         GOTO1 VDEPRNT2                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,FULL)                                      
         MVC   DFLTBOOK(1),FULL    DERIVE DEFAULT BOOK: CURRENT YEAR...         
         MVI   DFLTBOOK+1,MON_APR  ...ALWAYS MONTH APRIL                        
*                                                                               
READ10   DS    0X                                                               
*                                                                               
READ12   GET   IN1,(R9)            GET A RECORD                                 
         USING ICOUNTYD,R9                                                      
*                                                                               
READ20   CLC   IRECTYPE,=AL2(IRECMAIQ)  MAIN RECORD                             
         BE    READMAIN                                                         
         CLC   IRECTYPE,=AL2(IRECPASQ)  PASSIVE RECORD                          
         BE    READPP                                                           
         CLC   IRECTYPE,=AL2(IRECCTYQ)  COUNTY RECORD                           
         BE    READCTY                                                          
         DC    H'0'                     INVALID RECORD TYPE                     
*                                                                               
READMAIN CLC   =C'UUUU',ISTATION   SKIP UNREPORTED STATION DATA                 
         BE    READ12                                                           
         BAS   RE,MAINREC          PROCESS MAIN RECORD                          
         B     RELEASE             AND RELEASE TO DEMNCNV                       
*                                                                               
READPP   CLC   =C'UUUU',IPSTATN-IPASRECD(R9) SKIP UNREPORTED STAT DATA          
         BE    READ12                                                           
         BAS   RE,PPREC            PROCESS PASSIVE POINTERS                     
         B     RELEASE             AND RELEASE TO DEMNCNV                       
*                                                                               
READCTY  BAS   RE,CTYREC           PROCESS COUNTY RECORD                        
         B     RELEASE             AND RELEASE TO DEMNCNV                       
*                                                                               
         B     READ12              GO GET NEXT RECORD                           
*                                                                               
RELEASE  B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CNVWR - SORT RECDS HOOK.  AFTER ALL IRECS BUILT DEMCNV COMES HERE             
* BEFORE IT GOES TO THE OUTPUT PHASE.  LAST CHANCE TO PROCESS BEFORE            
* OUPUT PHASE HANDLING.                                                         
**********************************************************************          
CNVWR    L     RE,ASREC                                                         
*                                                                               
CNVWRX   MVI   BYPSORT,0           RELEASE RECORD FLAG                          
         L     RE,ASREC            MOVE DATA TO WREC                            
         L     RF,AWREC                                                         
         LA    R1,2000                                                          
         MOVE  ((RF),(R1)),(RE)                                                 
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* EOF ON INPUT FILE                                                             
**********************************************************************          
*                                                                               
DONE     DS    0H                                                               
*                                                                               
ENDJOB   DS    0H                                                               
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
EXIT     XMOD1 1                                                                
XIT      XIT1                      END OF PROCEDURE- RETURN TO CALLER           
         EJECT                                                                  
**********************************************************************          
* PROCESS MAIN RECORD                                                           
**********************************************************************          
MAINREC  NTR1                                                                   
*                                                                               
         LA    RE,INTERD           CLEAR INTERIM RECORD AREA                    
         LA    RF,MAXIRECL                                                      
         XCEF                                                                   
*                                                                               
* BUILD INTERIM RECORD FILTERABLE VALUES                                        
*                                                                               
         MVI   INTRTYP,DRCODEQU                                                 
*                                                                               
         CLI   FILTBOOK,0          WAS BOOK SET VIA BOOK= CARD?                 
         BNE   *+10                NO: USE OVERRIDE FROM BOOK= CARD             
         MVC   INTBOOK,DFLTBOOK    NO: DEFAULT TO CURRENT YEAR                  
*                                                                               
         MVC   INTSTA(4),ISTATION                                               
         MVC   INTSTA+4(1),IBAND                                                
*                                                                               
         CLC   =C'ZZZZ',INTSTA     FOR COUNTY TOTALS,                           
         BNE   *+10                 STORE COUNTY NUMBER IN STATION FLD          
         MVC   INTSTA(4),ICOUNTYC+2                                             
*                                                                               
         LA    R4,DPTABLE                                                       
         USING DPTABLED,R4                                                      
MAINR05  CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT DAYPART ID                     
         CLC   DPTCODE,IDPTID                                                   
         BE    MAINR07                                                          
         LA    R4,DPTTABLQ(R4)                                                  
         B     MAINR05                                                          
MAINR07  MVC   INTDAYWK,DPTDAYC    DAY CODE                                     
         MVC   INTDPQH,DPTQHDUR    DAYPART DURATION IN QH'S                     
         GOTO1 VHRTOQH,DMCB,(0,DPTMILS),INTSQH                                  
         GOTO1 VHRTOQH,DMCB,(0,DPTMILE),INTEQH                                  
*                                                                               
* BUILD INTERIM RECORD DATA                                                     
*                                                                               
         MVC   INTDPTNM,DPTNAME    DAYPART NAME                                 
         MVC   INTDMILS,DPTMILS    MILITARY START TIME                          
         MVC   INTDMILE,DPTMILE    MILITARY END TIME                            
         DROP  R4                                                               
*                                                                               
         LA    RE,STACODE          GET THE NUMERIC STATE FIELD                  
MAINR10  CLC   =X'FFFF',0(RE)                                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT STATE ALPHA                    
         CLC   ISTATE,1(RE)                                                     
         BE    MAINR15                                                          
         LA    RE,28(RE)                                                        
         B     MAINR10                                                          
MAINR15  MVC   INTSTATE,0(RE)      NUMERIC STATE CODE                           
*                                                                               
         PACK  DUB,ICOUNTYC+2(4)   USE COUNTY CODE'S LAST 4 DIGITS              
         CVB   R0,DUB               THE FIRST 2 REPRESENT THE STATE             
         STCM  R0,3,INTCNTY        NUMERIC COUNTY CODE                          
         MVC   INTSTAT,INTSTA      STATION                                      
         MVC   INTBAND,IBAND       AM-FM BAND INDICATOR                         
         MVC   INTSTCH,ISTATE      STATE ALPHA 2 CHARACTER ABBREVIATION         
         MVC   INTCTYNA,ICOUNTYA   ALPHA COUNTY NAME                            
*                                                                               
         CLC   IMETROC,BLANKS                                                   
         BE    MAINR30                                                          
         PACK  DUB,IMETROC                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,INTMETRC       METRO MARKET CODE                            
*                                                                               
MAINR30  MVC   INTMETRA,IMETROA    ALPHA METRO MARKET NAME                      
*                                                                               
         PACK  DUB,IDMAC                                                        
         CVB   R0,DUB                                                           
         STCM  R0,3,INTDMAC        DMA CODE                                     
         MVC   INTDMAA,IDMAA       ALPHA DMA NAME                               
*                                                                               
         MVC   INTRACEW,IRACEWGT   RACE WIGHTING INDICATOR                      
         PACK  DUB,IRATIER                                                      
         CVB   R0,DUB                                                           
         STC   R0,INTTIER          TIER                                         
*                                                                               
         MVC   INTSTCIT,ISTACITY   STATION CITY OF LICENSE                      
         MVC   INTSTCTY,ISTACNTY   STATION COUNTY OF LICENSE                    
         MVC   INTSTSTA,ISTASTAT   STATE ABBREVIATION OF LICENSE                
         MVC   INTSTAFS,ISTAAFF1   AFFILIATIONS (MAX 8 * 2 BYTES)               
         MVC   INTSTDYP,ISTADPOW   STATION DAY POWER                            
         MVC   INTSTNTP,ISTANPOW   STATION NIGHT POWER                          
         MVC   INTSTFRQ,ISTAFREQ   STATION FREQUENCY                            
         MVC   INTSTMKN,ISTAMKTA   STATION MARKET NAME                          
*                                                                               
         PACK  DUB,IDPTID                                                       
         CVB   R0,DUB                                                           
         STC   R0,INTDPT           DAYPART ID                                   
*                                                                               
         BAS   RE,SLOTDEM          SLOT DEMOS ON THE INTERIM RECORD             
         BAS   RE,PCUMES           MULTIPLY CUMES BY DAYPART QH                 
*                                                                               
MAINRECX B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SLOT DEMO VALUES INTO THEIR RESPECTIVE SLOTS ON THE INTERIM RECORD.           
**********************************************************************          
SLOTDEM  NTR1                                                                   
*                                                                               
         LA    R4,DEMSLOTS                                                      
         USING DEMSLOTD,R4                                                      
*                                                                               
SLD10    CLC   =X'FFFF',0(R4)                                                   
         BE    SLOTDEMX             FINISHED SLOTTING DEMOS                     
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,DSAIDEM         DISPLACEMENT TO START OF INPUT RECD         
         AR    RE,R9                A(DEMO ON THE INPUT RECORD)                 
*                                                                               
         SR    R0,R0                STORE ZERO DEMO IF BLANK                    
         ZIC   R1,DSIDEMLN          L'INPUT DEMO                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),BLANKS                                                   
         BE    SLD30                                                            
*                                                                               
         ZIC   R1,DSIDEMLN          L'INPUT DEMO                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)          CONVERT DEMO VALUE TO BINARY                
         CVB   R0,DUB                                                           
*                                                                               
SLD30    SR    R1,R1                                                            
         ICM   R1,3,DSSODEM         SLOT NUMBER ON OUTPUT REC (INTACCS)         
         MHI   R1,4                 4-BYTE SLOTS                                
         LA    R2,INTACCS                                                       
         AR    R2,R1                OUTPUT AREA ON THE INTERIM RECORD           
*                                                                               
         STCM  R0,15,0(R2)          SAVE DEMO VALUE ON THE INTERIM RECD         
*                                                                               
         LA    R4,DEMSLOTL(R4)                                                  
         B     SLD10                                                            
         DROP  R4                                                               
*                                                                               
SLOTDEMX B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* PROCESS COUNTY RECORD                                                         
**********************************************************************          
CTYREC   NTR1                                                                   
*                                                                               
         LA    RE,INTERD           CLEAR INTERIM RECORD AREA                    
         LA    RF,MAXIRECL                                                      
         XCEF                                                                   
*                                                                               
* BUILD INTERIM RECORD FILTERABLE VALUES                                        
*                                                                               
         MVI   INTRTYP,CYCODEQU                                                 
*                                                                               
* BUILD INTERIM RECORD DATA                                                     
*                                                                               
         CLI   FILTBOOK,0          WAS BOOK SET VIA BOOK= CARD?                 
         BNE   *+10                NO: USE OVERRIDE FROM BOOK= CARD             
         MVC   INTBOOK,DFLTBOOK    NO: DEFAULT TO CURRENT YEAR                  
*                                                                               
         LA    RE,STACODE          GET THE NUMERIC STATE FIELD                  
CTYR10   CLC   =X'FFFF',0(RE)                                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT STATE ALPHA                    
         CLC   ISTATE,1(RE)                                                     
         BE    CTYR15                                                           
         LA    RE,28(RE)                                                        
         B     CTYR10                                                           
CTYR15   MVC   INTSTATE,0(RE)      NUMERIC STATE CODE                           
*                                                                               
         PACK  DUB,ICOUNTYC+2(4)   USE COUNTY CODE'S LAST 4 DIGITS              
         CVB   R0,DUB               THE FIRST 2 REPRESENT THE STATE             
         STCM  R0,3,INTCNTY        NUMERIC COUNTY CODE                          
         MVC   INTSTCH,ISTATE      STATE ALPHA 2 CHARACTER ABBREVIATION         
         MVC   INTCTYNA,ICOUNTYA   ALPHA COUNTY NAME                            
*                                                                               
         CLC   IMETROC,BLANKS                                                   
         BE    CTYR30                                                           
         PACK  DUB,IMETROC                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,INTMETRC       METRO MARKET CODE                            
*                                                                               
CTYR30   MVC   INTMETRA,IMETROA    ALPHA METRO MARKET NAME                      
*                                                                               
         PACK  DUB,IDMAC                                                        
         CVB   R0,DUB                                                           
         STCM  R0,3,INTDMAC        DMA CODE                                     
         MVC   INTDMAA,IDMAA       ALPHA DMA NAME                               
*                                                                               
         MVC   INTRACEW,IRACEWGT   RACE WIGHTING INDICATOR                      
         PACK  DUB,IRATIER                                                      
         CVB   R0,DUB                                                           
         STC   R0,INTTIER          TIER                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
         DROP  R9                                                               
**********************************************************************          
* PROCESS PASSIVE POINTERS RECORD                                               
**********************************************************************          
PPREC    NTR1                                                                   
*                                                                               
         USING IPASRECD,R9         R9=A(INPUT RECORD)                           
*                                                                               
         LA    RE,INTERD           CLEAR INTERIM RECORD AREA                    
         LA    RF,MAXIRECL                                                      
         XCEF                                                                   
*                                                                               
* BUILD INTERIM RECORD FILTERABLE VALUES                                        
*                                                                               
         MVI   INTRTYP,SBCODEQU                                                 
*                                                                               
         CLI   FILTBOOK,0          WAS BOOK SET VIA BOOK= CARD?                 
         BNE   *+10                NO: USE OVERRIDE FROM BOOK= CARD             
         MVC   INTBOOK,DFLTBOOK    NO: DEFAULT TO CURRENT YEAR                  
*                                                                               
         MVC   INTSTA(4),IPSTATN                                                
         MVC   INTSTA+4(1),IPBAND                                               
*                                                                               
         CLC   =C'ZZZZ',INTSTA     FOR COUNTY TOTALS,                           
         BNE   *+10                 STORE COUNTY NUMBER IN STATION FLD          
         MVC   INTSTA(4),IPCNTYCD+2                                             
*                                                                               
* BUILD INTERIM RECORD DATA                                                     
*                                                                               
         LA    RE,STACODE          GET THE NUMERIC STATE FIELD                  
PPREC10  CLC   =X'FFFF',0(RE)                                                   
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT STATE ALPHA                    
         CLC   IPSTATE,1(RE)                                                    
         BE    PPREC15                                                          
         LA    RE,28(RE)                                                        
         B     PPREC10                                                          
PPREC15  MVC   INTSTATE,0(RE)      NUMERIC STATE CODE                           
*                                                                               
         PACK  DUB,IPCNTYCD+2(4)   USE COUNTY CODE'S LAST 4 DIGITS              
         CVB   R0,DUB               THE FIRST 2 REPRESENT THE STATE             
         STCM  R0,3,INTCNTY        NUMERIC COUNTY CODE                          
         MVC   INTSTAT,INTSTA      STATION                                      
         MVC   INTBAND,IPBAND      AM-FM BAND INDICATOR                         
         MVC   INTSTCH,IPSTATE     STATE ALPHA 2 CHARACTER ABBREVIATION         
*                                                                               
PPREC30  PACK  DUB,IPTIER                                                       
         CVB   R0,DUB                                                           
         STC   R0,INTTIER          TIER                                         
*                                                                               
         B     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
**********************************************************************          
*    STORING CUMES AS "TOTAL QH" CUMES.  FOR TRANSPARENCY IN READING *          
**********************************************************************          
PCUMES   NTR1                                                                   
         LA    R1,CUMETACS                                                      
         SR    R2,R2                                                            
PC10     CLI   0(R1),X'FF'                                                      
         BE    PCX                                                              
         ICM   R2,3,0(R1)           SLOT NUMBER ON OUTPUT REC (INTACCS)         
         MHI   R2,4                 4-BYTE SLOTS                                
         LA    R3,INTACCS                                                       
         AR    R3,R2                OUTPUT AREA ON THE INTERIM RECORD           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,15,0(R3)                                                      
         MH    R0,INTDPQH                                                       
         STCM  R0,15,0(R3)                                                      
         LA    R1,L'CUMETACS(R1)                                                
         B     PC10                                                             
*                                                                               
PCX      B     XIT                                                              
**********************************************************************          
**********************************************************************          
* LTORG, TABLES, AND CONSTANTS                                                  
**********************************************************************          
         LTORG                                                                  
BLANKS   DC    CL256' '                                                         
*                                                                               
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=1033,                                             X        
               MACRF=GM,                                               X        
               EODAD=DONE                                                       
*                                                                               
DFLTBOOK DS    XL2                 DEFAULT IS APRIL OF CURRENT YEAR             
*                                                                               
DPTABLE  DS    0X                DAYPART TABLE                                  
         DC    C'1',CL14'MON-SUN 6A-12M',X'97',AL2(0600,2400),AL2(504)          
         DC    C'2',CL14'MON-FRI 6A-7P ',X'95',AL2(0600,1900),AL2(260)          
         DC    X'FF'                                                            
*                                                                               
       ++INCLUDE DESTACODE       TABLE OF STATE CODES                           
*                                                                               
*--------------------------------------------------------------------*          
* DISPLACEMENTS TO DEMOS ON THE INTERIM RECORD DEMO AREA (INTACCS).             
* THIS CORRESPONDS TO THE DEMDISP TABLE FOR 'RUA'.                              
*--------------------------------------------------------------------*          
*                                                                               
* IMPRESSIONS 'I'                                                               
IIV12P   EQU   0                                                                
IIV1234  EQU   1                                                                
IIV18P   EQU   2                                                                
IIV1834  EQU   3                                                                
IIV2554  EQU   4                                                                
IIV35P   EQU   5                                                                
IIV3564  EQU   6                                                                
IIM18P   EQU   7                                                                
IIW18P   EQU   8                                                                
*                                                                               
* RATINGS 'R'                                                                   
IRV12P   EQU   9                                                                
IRV1234  EQU   10                                                               
IRV18P   EQU   11                                                               
IRV1834  EQU   12                                                               
IRV2554  EQU   13                                                               
IRV35P   EQU   14                                                               
IRV3564  EQU   15                                                               
IRM18P   EQU   16                                                               
IRW18P   EQU   17                                                               
*                                                                               
* STATION SHARES 'S'                                                            
ISV12P   EQU   18                                                               
ISV1234  EQU   19                                                               
ISV18P   EQU   20                                                               
ISV1834  EQU   21                                                               
ISV2554  EQU   22                                                               
ISV35P   EQU   23                                                               
ISV3564  EQU   24                                                               
ISM18P   EQU   25                                                               
ISW18P   EQU   26                                                               
*                                                                               
* CUMES IMPRESSIONS 'C'                                                         
ICV12P   EQU   27                                                               
ICV1234  EQU   28                                                               
ICV18P   EQU   29                                                               
ICV1834  EQU   30                                                               
ICV2554  EQU   31                                                               
ICV35P   EQU   32                                                               
ICV3564  EQU   33                                                               
ICM18P   EQU   34                                                               
ICW18P   EQU   35                                                               
*                                                                               
* CUMES RATINGS 'E'                                                             
IEV12P   EQU   36                                                               
IEV1234  EQU   37                                                               
IEV18P   EQU   38                                                               
IEV1834  EQU   39                                                               
IEV2554  EQU   40                                                               
IEV35P   EQU   41                                                               
IEV3564  EQU   42                                                               
IEM18P   EQU   43                                                               
IEW18P   EQU   44                                                               
*                                                                               
* UNIVERSES 'U'                                                                 
IUV12P   EQU   45                                                               
IUV1234  EQU   46                                                               
IUV18P   EQU   47                                                               
IUV1834  EQU   48                                                               
IUV2554  EQU   49                                                               
IUV35P   EQU   50                                                               
IUV3564  EQU   51                                                               
IUM18P   EQU   52                                                               
IUW18P   EQU   53                                                               
*                                                                               
* INTABS 'K'                                                                    
IKV12P   EQU   54                                                               
IKV1234  EQU   55                                                               
IKV18P   EQU   56                                                               
IKV1834  EQU   57                                                               
IKV2554  EQU   58                                                               
IKV35P   EQU   59                                                               
IKV3564  EQU   60                                                               
IKM18P   EQU   61                                                               
IKW18P   EQU   62                                                               
*                                                                               
* TOTAL COUNTY LISTENING 'Z'                                                    
IZV12P   EQU   63                                                               
IZV1234  EQU   64                                                               
IZV18P   EQU   65                                                               
IZV1834  EQU   66                                                               
IZV2554  EQU   67                                                               
IZV35P   EQU   68                                                               
IZV3564  EQU   69                                                               
IZM18P   EQU   70                                                               
IZW18P   EQU   71                                                               
*            ______                                                             
MAXDIDEM EQU   72   ---------> MAXIMUM NUMBER OF DEMOS                          
*                                                                               
*--------------------------------------------------------------------*          
* TABLE TO TRANSLATE INPUT DEMOS TO INTERIM SLOTS (INTACCS)                     
*--------------------------------------------------------------------*          
DEMSLOTS DS    0X                                                               
*                                                                               
*UNIVERSES                                                                      
         DC    AL2(IPOP12P-ICOUNTYD),AL1(L'IPOP12P),AL2(IUV12P)                 
         DC    AL2(IPOP1234-ICOUNTYD),AL1(L'IPOP1234),AL2(IUV1234)              
         DC    AL2(IPOP18P-ICOUNTYD),AL1(L'IPOP18P),AL2(IUV18P)                 
         DC    AL2(IPOP1834-ICOUNTYD),AL1(L'IPOP1834),AL2(IUV1834)              
         DC    AL2(IPOP2554-ICOUNTYD),AL1(L'IPOP2554),AL2(IUV2554)              
         DC    AL2(IPOP35P-ICOUNTYD),AL1(L'IPOP35P),AL2(IUV35P)                 
         DC    AL2(IPOP3564-ICOUNTYD),AL1(L'IPOP3564),AL2(IUV3564)              
         DC    AL2(IPOPM18P-ICOUNTYD),AL1(L'IPOPM18P),AL2(IUM18P)               
         DC    AL2(IPOPW18P-ICOUNTYD),AL1(L'IPOPW18P),AL2(IUW18P)               
*                                                                               
*IN-TABS                                                                        
         DC    AL2(ITAB12P-ICOUNTYD),AL1(L'ITAB12P),AL2(IKV12P)                 
         DC    AL2(ITAB1234-ICOUNTYD),AL1(L'ITAB1234),AL2(IKV1234)              
         DC    AL2(ITAB18P-ICOUNTYD),AL1(L'ITAB18P),AL2(IKV18P)                 
         DC    AL2(ITAB1834-ICOUNTYD),AL1(L'ITAB1834),AL2(IKV1834)              
         DC    AL2(ITAB2554-ICOUNTYD),AL1(L'ITAB2554),AL2(IKV2554)              
         DC    AL2(ITAB35P-ICOUNTYD),AL1(L'ITAB35P),AL2(IKV35P)                 
         DC    AL2(ITAB3564-ICOUNTYD),AL1(L'ITAB3564),AL2(IKV3564)              
         DC    AL2(ITABM18P-ICOUNTYD),AL1(L'ITABM18P),AL2(IKM18P)               
         DC    AL2(ITABW18P-ICOUNTYD),AL1(L'ITABW18P),AL2(IKW18P)               
*                                                                               
*TOTAL QH PERSONS IMPRESSIONS                                                   
         DC    AL2(IQHR12P-ICOUNTYD),AL1(L'IQHR12P),AL2(IIV12P)                 
         DC    AL2(IQHR1234-ICOUNTYD),AL1(L'IQHR1234),AL2(IIV1234)              
         DC    AL2(IQHR18P-ICOUNTYD),AL1(L'IQHR18P),AL2(IIV18P)                 
         DC    AL2(IQHR1834-ICOUNTYD),AL1(L'IQHR1834),AL2(IIV1834)              
         DC    AL2(IQHR2554-ICOUNTYD),AL1(L'IQHR2554),AL2(IIV2554)              
         DC    AL2(IQHR35P-ICOUNTYD),AL1(L'IQHR35P),AL2(IIV35P)                 
         DC    AL2(IQHR3564-ICOUNTYD),AL1(L'IQHR3564),AL2(IIV3564)              
         DC    AL2(IQHRM18P-ICOUNTYD),AL1(L'IQHRM18P),AL2(IIM18P)               
         DC    AL2(IQHRW18P-ICOUNTYD),AL1(L'IQHRW18P),AL2(IIW18P)               
*                                                                               
*TOTAL QH PERSONS RATINGS                                                       
         DC    AL2(IRTG12P-ICOUNTYD),AL1(L'IRTG12P),AL2(IRV12P)                 
         DC    AL2(IRTG1234-ICOUNTYD),AL1(L'IRTG1234),AL2(IRV1234)              
         DC    AL2(IRTG18P-ICOUNTYD),AL1(L'IRTG18P),AL2(IRV18P)                 
         DC    AL2(IRTG1834-ICOUNTYD),AL1(L'IRTG1834),AL2(IRV1834)              
         DC    AL2(IRTG2554-ICOUNTYD),AL1(L'IRTG2554),AL2(IRV2554)              
         DC    AL2(IRTG35P-ICOUNTYD),AL1(L'IRTG35P),AL2(IRV35P)                 
         DC    AL2(IRTG3564-ICOUNTYD),AL1(L'IRTG3564),AL2(IRV3564)              
         DC    AL2(IRTGM18P-ICOUNTYD),AL1(L'IRTGM18P),AL2(IRM18P)               
         DC    AL2(IRTGW18P-ICOUNTYD),AL1(L'IRTGW18P),AL2(IRW18P)               
*                                                                               
*TOTAL STATION SHARES                                                           
         DC    AL2(ISHR12P-ICOUNTYD),AL1(L'ISHR12P),AL2(ISV12P)                 
         DC    AL2(ISHR1234-ICOUNTYD),AL1(L'ISHR1234),AL2(ISV1234)              
         DC    AL2(ISHR18P-ICOUNTYD),AL1(L'ISHR18P),AL2(ISV18P)                 
         DC    AL2(ISHR1834-ICOUNTYD),AL1(L'ISHR1834),AL2(ISV1834)              
         DC    AL2(ISHR2554-ICOUNTYD),AL1(L'ISHR2554),AL2(ISV2554)              
         DC    AL2(ISHR35P-ICOUNTYD),AL1(L'ISHR35P),AL2(ISV35P)                 
         DC    AL2(ISHR3564-ICOUNTYD),AL1(L'ISHR3564),AL2(ISV3564)              
         DC    AL2(ISHRM18P-ICOUNTYD),AL1(L'ISHRM18P),AL2(ISM18P)               
         DC    AL2(ISHRW18P-ICOUNTYD),AL1(L'ISHRW18P),AL2(ISW18P)               
*                                                                               
*TOTAL COUNTY SHARES ARE NOT BEING USED                                         
*                                                                               
*TOTAL CUME PERSONS IMPRESSIONS                                                 
         DC    AL2(ICME12P-ICOUNTYD),AL1(L'ICME12P),AL2(ICV12P)                 
         DC    AL2(ICME1234-ICOUNTYD),AL1(L'ICME1234),AL2(ICV1234)              
         DC    AL2(ICME18P-ICOUNTYD),AL1(L'ICME18P),AL2(ICV18P)                 
         DC    AL2(ICME1834-ICOUNTYD),AL1(L'ICME1834),AL2(ICV1834)              
         DC    AL2(ICME2554-ICOUNTYD),AL1(L'ICME2554),AL2(ICV2554)              
         DC    AL2(ICME35P-ICOUNTYD),AL1(L'ICME35P),AL2(ICV35P)                 
         DC    AL2(ICME3564-ICOUNTYD),AL1(L'ICME3564),AL2(ICV3564)              
         DC    AL2(ICMEM18P-ICOUNTYD),AL1(L'ICMEM18P),AL2(ICM18P)               
         DC    AL2(ICMEW18P-ICOUNTYD),AL1(L'ICMEW18P),AL2(ICW18P)               
*                                                                               
*TOTAL CUME PERSONS RATINGS                                                     
         DC    AL2(ICMR12P-ICOUNTYD),AL1(L'ICMR12P),AL2(IEV12P)                 
         DC    AL2(ICMR1234-ICOUNTYD),AL1(L'ICMR1234),AL2(IEV1234)              
         DC    AL2(ICMR18P-ICOUNTYD),AL1(L'ICMR18P),AL2(IEV18P)                 
         DC    AL2(ICMR1834-ICOUNTYD),AL1(L'ICMR1834),AL2(IEV1834)              
         DC    AL2(ICMR2554-ICOUNTYD),AL1(L'ICMR2554),AL2(IEV2554)              
         DC    AL2(ICMR35P-ICOUNTYD),AL1(L'ICMR35P),AL2(IEV35P)                 
         DC    AL2(ICMR3564-ICOUNTYD),AL1(L'ICMR3564),AL2(IEV3564)              
         DC    AL2(ICMRM18P-ICOUNTYD),AL1(L'ICMRM18P),AL2(IEM18P)               
         DC    AL2(ICMRW18P-ICOUNTYD),AL1(L'ICMRW18P),AL2(IEW18P)               
*                                                                               
*COUNTY TOTAL LISTENING DEMOS                                                   
         DC    AL2(ICTL12P-ICOUNTYD),AL1(L'ICTL12P),AL2(IZV12P)                 
         DC    AL2(ICTL1234-ICOUNTYD),AL1(L'ICTL1234),AL2(IZV1234)              
         DC    AL2(ICTL18P-ICOUNTYD),AL1(L'ICTL18P),AL2(IZV18P)                 
         DC    AL2(ICTL1834-ICOUNTYD),AL1(L'ICTL1834),AL2(IZV1834)              
         DC    AL2(ICTL2554-ICOUNTYD),AL1(L'ICTL2554),AL2(IZV2554)              
         DC    AL2(ICTL35P-ICOUNTYD),AL1(L'ICTL35P),AL2(IZV35P)                 
         DC    AL2(ICTL3564-ICOUNTYD),AL1(L'ICTL3564),AL2(IZV3564)              
         DC    AL2(ICTLM18P-ICOUNTYD),AL1(L'ICTLM18P),AL2(IZM18P)               
         DC    AL2(ICTLW18P-ICOUNTYD),AL1(L'ICTLW18P),AL2(IZW18P)               
*                                                                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
CUMETACS DS    0XL2                                                             
         DC    AL2(ICV12P)                                                      
         DC    AL2(ICV1234)                                                     
         DC    AL2(ICV18P)                                                      
         DC    AL2(ICV1834)                                                     
         DC    AL2(ICV2554)                                                     
         DC    AL2(ICV35P)                                                      
         DC    AL2(ICV3564)                                                     
         DC    AL2(ICM18P)                                                      
         DC    AL2(ICW18P)                                                      
         DC    X'FF'                                                            
*                                                                               
***********************************************************************         
* DSECTS                                                                        
***********************************************************************         
*                                                                               
DEMSLOTD DSECT                     DEMO SLOTS TABLE                             
DSAIDEM  DS    AL2                 DISPLACEMENT TO DEMO ON INPUT RECD           
DSIDEMLN DS    AL1                 LENGTH OF DEMO ON INPUT RECD                 
DSSODEM  DS    AL2                 SLOT NUMBER IN OUTPUT AREA (INTACCS)         
DEMSLOTL EQU   *-DEMSLOTD                                                       
*                                                                               
DPTABLED DSECT                     DAYPART TABLE                                
DPTCODE  DS      CL1               DAYPART CODE FROM ARBITRON                   
DPTNAME  DS      CL14              DAYPART NAME                                 
DPTDAYC  DS      CL1               DAY CODE                                     
DPTMILS  DS      HL2               MILITARY START TIME                          
DPTMILE  DS      HL2               MILITARY END TIME                            
DPTQHDUR DS      HL2               DAYPART TOTAL QH DURATION                    
DPTTABLQ EQU     *-DPTABLED                                                     
*                                                                               
* INTERIM RECORD DSECT                                                          
       ++INCLUDE DEINTD                                                         
       ++INCLUDE DEINTRCYD                                                      
         DS    (MAXDIDEM)XL4                                                    
MAXIRECL EQU   *-INTERD            MAX LENGTH OF INTERIM RECORD                 
* OTHER DSECTS                                                                  
       ++INCLUDE DERACNTYD                                                      
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDEMCNVD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDMONYREQU                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DERC09I   01/04/13'                                      
         END                                                                    
