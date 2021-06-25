*          DATA SET DECY93I    AT LEVEL 020 AS OF 07/24/08                      
*PHASE DECY93IA                                                                 
*                                                                               
         TITLE 'DEMO CONVERSION: COUNTY COVERAGE'                               
DECY93I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DECY93I                                                        
         USING DECY93I+4096,R3                                                  
         USING DEMCOND,R8                                                       
         L     RC,ARREC                                                         
         LA    RC,4(RC)                                                         
         USING MIREC,RC                                                         
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
         B     *+4(R1)                                                          
         SPACE 2                                                                
         B     READ                GET INPUT (ARREC - INT)                      
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     ENDJOB              CLOSE INPUT                                  
         B     EXIT                                                             
         EJECT                                                                  
READ     CLI   INTAPESW,1                                                       
         BE    OPENOK                                                           
         OPEN  (IN1,(INPUT))                                                    
         EJECT                                                                  
OPENOK   DS    0H                                                               
         CLI   DPTSW,14            ALL DAYPARTS PROCESSED                       
         BE    READ10                                                           
         CLI   DPTSW,0             JUST CAME IN                                 
         BNE   READ10                                                           
*                                                                               
         L     RE,ARREC                                                         
         XC    0(4,RE),0(RE)                                                    
         MVC   0(2,RE),=H'460'                                                  
READ05   GET   IN1,(RC)                                                         
         CLC   MITORIC(3),=C'999'  BYPASSING CANADA                             
         BNE   *+8                                                              
         B     READ05                                                           
*                                                                               
         MVI   DPTSW,1             DAYPART LOOP CONTROL                         
         MVI   RELCPTR,0           COUNTY POINTER FLAG                          
*                                                                               
********************************************************************            
*        BUILD INTKEY (SORTKEY FOR IREC)                           *            
********************************************************************            
READ10   DS    0H                                                               
         LA    RE,INTKEY                                                        
         USING INTKEYD,RE                                                       
         XC    INTKEY,INTKEY                                                    
*                                                                               
         CLC   MITSTCNT(5),SPACES                                               
         BNE   READ11                                                           
         CLC   MITSTABV(4),=C'ZDMA' SKIPPING THESE FOR NOW                      
         BNE   *+8                                                              
         B     READ05                                                           
*                                                                               
         CLC   MITSTABV(4),=C'ZSTN' COUNTY CODE = ORIC                          
         BE    *+6                                                              
         DC    H'0'                                                             
         PACK  DUB,MITORIC                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,SVDMAC                                                      
         MVC   SVDMA,MITDMA                                                     
*        AH    R1,=H'400'                                                       
         STCM  R1,3,INTKCNCD                                                    
         MVC   INTKSTCD,SVSTCD    !!!STATE CODE USING PREVIOUS REC'S            
         B     READ11A                                                          
*                                                                               
READ11   PACK  DUB,MITSTCNT(2)                                                  
         CVB   R1,DUB                                                           
         STC   R1,INTKSTCD         STATE CODE                                   
         MVC   SVSTCD,INTKSTCD                                                  
         PACK  DUB,MITSTCNT+2(3)                                                
         CVB   R1,DUB                                                           
         STCM  R1,3,INTKCNCD       COUNTY CODE                                  
*                                                                               
READ11A  CLC   =C'999M',MITSTAT                                                 
         BNE   READ12                                                           
         MVI   INTKSTAT,C'M'                                                    
         MVC   INTKSTAT+1(3),SVORICH                                            
         B     *+10                                                             
READ12   MVC   INTKSTAT,MITSTAT    STATION                                      
*                                                                               
         CLC   MITDMAC,SPACES                                                   
         BNE   *+14                                                             
         MVC   INTKDMAC,SVDMAC     999M/999C USE PREV DMAC                      
         B     READ13                                                           
         PACK  DUB,MITDMAC(3)                                                   
         CVB   R1,DUB                                                           
         SH    R1,=H'400'                                                       
         STCM  R1,3,INTKDMAC       DMA CODE                                     
         MVC   SVDMAC,INTKDMAC                                                  
         MVC   SVDMA,MITDMA                                                     
*                                                                               
         MVC   INTKDPTC,DPTSW                                                   
*                                                                               
READ13   CLI   DPTSW,14            STATION CHANGED?                             
         BNE   READ15                                                           
         CLI   RELCPTR,1           COUNTY POINTERS ALREADY RELEASED             
         BE    READ14                                                           
         CLC   INTKSTAT,=C'999C'   COUNTY RECORDS                               
         BNE   *+12                                                             
         MVI   INTRTYP,C'C'                                                     
         B     READ15A                                                          
*                                                                               
READ14   MVI   INTRTYP,C'M'        BUILD MARKET/STATION/BOOK PTRS               
         B     READ15A                                                          
*                                                                               
********************************************************************            
*        BUILD INTVALS (FILTER FIELDS FOR IREC                     *            
********************************************************************            
*                                                                               
READ15   MVI   INTRTYP,C'R'                                                     
READ15A  MVC   INTMRKT,INTKCNCD    COUNTY CODE                                  
         CLC   MITSTAT,=C'999M'                                                 
         BNE   READ16                                                           
         MVI   INTSTA,C'M'         'M101' WOULD BE MKT TOTAL FOR NY             
         MVC   INTSTA+1(3),SVORICH                                              
         B     *+10                                                             
READ16   MVC   INTSTA,MITSTAT      STATION                                      
         MVI   INTSTA+4,C'T'                                                    
         MVC   INTBOOK,FILTBOOK                                                 
         MVC   INTBTYP,INTKSTCD    STATE CODE                                   
*                                                                               
         CLI   INTRTYP,C'C'        SKIP THE DAYPART/TIMEZONE CODES              
         BE    READ32              FOR COUNTY/MARKET/STATION PTRS               
         CLI   INTRTYP,C'M'                                                     
         BE    READ32                                                           
         CLC   MITTZONE,SPACES                                                  
         BNE   READ17                                                           
         LH    R1,SVDMAC                                                        
*        SH    R1,=H'400'                                                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         STC   R1,INTZONE                                                       
         B     READ18                                                           
*                                                                               
READ17   PACK  DUB,MITTZONE                                                     
         CVB   R1,DUB                                                           
         STC   R1,INTZONE                                                       
         CLI   INTZONE,0                                                        
         BNE   READ18                                                           
         MVI   INTZONE,1                                                        
*                                                                               
READ18   LA    R4,DPTABLE          USING DAYPART TABLE TO GET S/E QH            
         USING DPTABD,R4                                                        
READ20   CLI   DPCHAR,0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   DPHEX,DPTSW                                                      
         BE    *+12                                                             
READ22   AHI   R4,DPTABLN                                                       
         B     READ20                                                           
*                                                                               
         LA    R1,DPTZONE                                                       
READ25   CLC   INTZONE,0(R1)                                                    
         BE    READ30                                                           
         AHI   R1,1                                                             
         CLI   0(R1),0                                                          
         BE    READ22                                                           
         B     READ25                                                           
*                                                                               
READ30   GOTO1 VHRTOQH,DMCB,(0,DPINTST),INTSQH                                  
         GOTO1 VHRTOQH,DMCB,(0,DPINTET),INTEQH                                  
         ZIC   R1,INTEQH                                                        
         SHI   R1,1                                                             
         STC   R1,INTEQH                                                        
         MVC   INTADUR,DPDURA       DURATION                                    
         MVC   INTDAYWK,DPDAY                                                   
         OI    INTDAYWK,X'80'                                                   
         MVC   INTORIG(14),DPINTPN  STORING DAYPART NAME HERE FOR NOW           
         DROP  RE                                                               
*                                                                               
********************************************************************            
*        BUILD INTDATA AND SLOT THE DEMOS INTO INTACCS             *            
********************************************************************            
READ32   LA    RE,INTKEY                                                        
         USING INTKEYD,RE                                                       
         MVI   INTSPILL,C'Y'                                                    
         MVC   INTKDYWK,DPDAY       FILL IN THE SORTKEY FIELD ALSO              
         OI    INTKDYWK,X'80'                                                   
         MVC   INTSTCD,INTKSTCD     STATE CODE                                  
         MVC   INTCNCD,INTKCNCD     COUNTY CODE                                 
*        MVC   INTSTAT(4),INTSTA                                                
*        MVC   INTSTAT,MITSTAT      STATION                                     
*        MVI   INTSTAT+4,C'T'                                                   
         MVC   INTDMAC,INTKDMAC     DMAC                                        
         CLC   MITDMA,SPACES                                                    
         BNE   *+14                                                             
         MVC   INTDMA,SVDMA                                                     
         B     *+10                                                             
         MVC   INTDMA,MITDMA                                                    
*                                                                               
         CLI   INTRTYP,C'C'                                                     
         BNE   *+18                                                             
         MVI   RELCPTR,1                                                        
         MVC   INTCNTC,SVCNTC                                                   
         B     EXIT                                                             
*                                                                               
         CLI   INTRTYP,C'M'                                                     
         BNE   READ33                                                           
         MVI   DPTSW,0                                                          
         B     EXIT                                                             
         DROP  RE                                                               
READ33   MVC   INTSTBV,MITSTABV     2 CHAR STATE ABBREV                         
         CLC   MITCNTY,SPACES                                                   
         BNE   *+14                                                             
         MVC   INTCNTC,SVCNTC                                                   
         B     *+16                                                             
         MVC   INTCNTC,MITCNTY      16 CHAR COUNTY NAME                         
         MVC   SVCNTC,INTCNTC                                                   
*                                                                               
         CLC   MITORIC,SPACES       M/C RECORDS DOESNT HAVE ORIC                
         BNE   READ35                                                           
         CLC   MITSTAT,=C'999M'     MARKET OR COUNTY RECORD                     
         BNE   *+14                                                             
         MVC   INTORIC,SVORIC       MKT: ORIC = SAVED ORIC                      
*        MVC   INTORIG,SVORIG                                                   
         B     READ37                                                           
         CLC   MITSTAT,=C'999C'                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   INTORIC,SVDMAC      CNT: ORIC = SAVED DMAC                       
*        MVC   INTORIG(L'SVCNTC),SVCNTC                                         
         B     READ37                                                           
READ35   PACK  DUB,MITORIC                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,INTORIC         2 BYTE MARKET OF ORIGIN CODE                
*        MVC   INTORIG,MITORIG      26 CHAR MARKET OF ORIGIN NAME               
         MVC   SVORICH,MITORIC                                                  
         MVC   SVORIC,INTORIC                                                   
*        MVC   SVORIG,INTORIG                                                   
*                                                                               
READ37   DS    0H                                                               
         CLC   MITSTABV(4),=C'ZSTN' NO HOUSEHOLD DATA                           
         BE    READ38                                                           
         PACK  DUB,MITTVHLD         TV HOUSEHOLD                                
         CVB   R1,DUB                                                           
         STCM  R1,7,INTTVHLD                                                    
         PACK  DUB,MITCBHLD         CABLE HOUSEHOLD                             
         CVB   R1,DUB                                                           
         STCM  R1,7,INTCBHLD                                                    
         PACK  DUB,MITNCHLD         NON-CABLE HOUSEHOLD                         
         CVB   R1,DUB                                                           
         STCM  R1,7,INTNCHLD                                                    
         MVC   SVTVHLD(9),INTTVHLD  SAVE THE 3 HHLDS                            
*                                                                               
READ38   MVC   INTAFFC,MITAFFIL     FIRST 3 BYTE ONLY                           
         CLC   MITCHAN,SPACES                                                   
         BE    READ40                                                           
         PACK  DUB,MITCHAN                                                      
         CVB   R1,DUB                                                           
         STC   R1,INTCHAN           1 BYTE CHANNEL NUMBER1                      
*                                                                               
READ40   ZIC   R1,DPTSW             DAYPART                                     
         STC   R1,INTDPT                                                        
         SHI   R1,1                                                             
         MHI   R1,DPTLNQ                                                        
         LA    RE,MITDATA                                                       
         LA    RE,2(RE,R1)                                                      
         ST    RE,DMCB                                                          
         LA    RE,INTACCS                                                       
         ST    RE,DMCB+4                                                        
         BAS   RE,SLOTDEM                                                       
         ZIC   R1,DPTSW                                                         
         AHI   R1,1                                                             
         STC   R1,DPTSW                                                         
         B     EXIT                                                             
*                                                                               
*====================== CONVERT TO WORK RECORD =======================*         
CNVWR    DS    0H                                                               
         B     EXIT                                                             
***********************************************************************         
* SLOTDEM: SLOT THE DEMOS BY DAYPART INTO INTACCS                               
***********************************************************************         
* R1: SLOTTAB                                                                   
* R2: MITDATA                                                                   
* R3: SIZE OF CELL                                                              
* R4: INTACCS                                                                   
* RE: UTILITY                                                                   
* RF: UTILITY                                                                   
***********************************************************************         
SLOTDEM  NTR1                                                                   
         L     R4,DMCB+4                                                        
*                                                                               
         MVC   1(3,R4),INTTVHLD    TV HOUSEHOLD ESTIMATE                        
         MVC   5(3,R4),INTCBHLD    CABLE HOUSEHOLD ESTIMATE                     
         MVC   9(3,R4),INTNCHLD    NON-CABLE HHLD ESTIMATE                      
*                                                                               
         L     R2,DMCB                                                          
         LA    R4,12(R4)                                                        
*                                                                               
         LA    R1,SLOTTAB                                                       
SLOT10   DS    0H                                                               
         ZIC   R3,0(R1)                                                         
         SHI   R3,1                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SPACES                                                   
         BE    SLOT20                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   RE,DUB                                                           
         ST    RE,0(R4)            STORE INTO INTACCS                           
SLOT20   LA    R2,1(R2,R3)         BUMP TO NEXT DEMO                            
         AHI   R1,2                NEXT ENTRY IN TABLE                          
         AHI   R4,4                NEXT SLOT                                    
         CLI   0(R1),X'FF'         DONE?                                        
         BNE   SLOT10                                                           
*                                                                               
SLOTX    XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
MORET    DS    0H'0'                                                            
ENDJOB   CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SLOTTAB  DS    0H                                                               
         DC    AL1(7,0)                                                         
         DC    AL1(4,1)                                                         
         DC    AL1(4,2)                                                         
         DC    AL1(4,3)                                                         
         DC    AL1(7,4)                                                         
         DC    AL1(4,5)                                                         
         DC    AL1(4,6)                                                         
         DC    AL1(7,7)                                                         
         DC    AL1(4,8)                                                         
         DC    AL1(4,9)                                                         
         DC    X'FF'                                                            
SLOTLN   EQU   2                                                                
*                                                                               
CYCTAB   DC    X'0201'             JAN                                          
         DC    X'0302'             FEB                                          
         DC    X'0403'             MAR                                          
         DC    X'0505'             MAY                                          
         DC    X'0707'             JUL                                          
         DC    X'0A0A'             OCT                                          
         DC    X'0B0B'             NOV                                          
         DC    X'0C0C'             DEC                                          
         DC    X'00'                                                            
DAYTABL  DC    X'F210'             MON                                          
         DC    X'F320'             TUE                                          
         DC    X'F430'             WED                                          
         DC    X'F540'             THU                                          
         DC    X'F650'             FRI                                          
         DC    X'F760'             SAT                                          
         DC    X'F870'             SUN                                          
         DC    X'F995'             M-F                                          
         DC    X'00'                                                            
         SPACE 2                                                                
JAN      EQU   1                                                                
FEB      EQU   2                                                                
MAR      EQU   3                                                                
MAY      EQU   5                                                                
JUL      EQU   7                                                                
OCT      EQU   10                                                               
NOV      EQU   11                                                               
DEC      EQU   12                                                               
PFEB     EQU   0                                                                
PMAY     EQU   1                                                                
PJUL     EQU   2                                                                
PNOV     EQU   3                                                                
* DISPLACEMENTS FOR TREND DATA BY BOOK                                          
PROJTAB  DC    AL1(JAN,PNOV,PJUL,PMAY,PFEB)                                     
         DC    AL1(FEB,PNOV,PJUL,PMAY,PFEB)                                     
         DC    AL1(MAR,PNOV,PJUL,PMAY,PFEB)                                     
         DC    AL1(MAY,PFEB,PNOV,PJUL,PMAY)                                     
         DC    AL1(JUL,PMAY,PFEB,PNOV,PJUL)                                     
         DC    AL1(OCT,PJUL,PMAY,PFEB,PNOV)                                     
         DC    AL1(NOV,PJUL,PMAY,PFEB,PNOV)                                     
         DC    AL1(DEC,PJUL,PMAY,PFEB,PNOV)                                     
         DC    X'00'                                                            
PROJLN   EQU   5                                                                
         EJECT                                                                  
***********************************************************************         
*===================== DAYPART CONVERSION TABLE ======================*         
E        EQU   X'1'                EASTERN                                      
C        EQU   X'2'                CENTRAL                                      
M        EQU   X'3'                MOUNTAIN                                     
P        EQU   X'4'                PACIFIC                                      
Y        EQU   X'5'                YUKON                                        
H        EQU   X'6'                HAWAII                                       
*                                                                               
*-------------------------- ORIGINAL TABLE ---------------------------*         
*                                                                               
DPTABLE  DC    C'01',AL1(01),AL1(E,C,M,P,Y,H),AL2(0700,0100)                    
         DC    CL14'SU-SA 7-1A',AL1(72),X'17'                                   
         DC    C'02',AL1(02),AL1(E,P,0,0,0,0),AL2(0900,1600)                    
         DC    CL14'M-F 9A-4P',AL1(28),X'15'                                    
         DC    C'02',AL1(02),AL1(C,M,Y,H,0,0),AL2(0900,1500)                    
         DC    CL14'M-F 9A-3P',AL1(24),X'15'                                    
         DC    C'03',AL1(03),AL1(E,P,0,0,0,0),AL2(1600,1800)                    
         DC    CL14'M-F 4-6P',AL1(8),X'15'                                      
         DC    C'03',AL1(03),AL1(C,M,Y,H,0,0),AL2(1500,1700)                    
         DC    CL14'M-F 3-5P',AL1(8),X'15'                                      
         DC    C'04',AL1(04),AL1(E,P,0,0,0,0),AL2(1700,1730)                    
         DC    CL14'M-F 5-530P',AL1(2),X'15'                                    
         DC    C'04',AL1(04),AL1(C,M,Y,H,0,0),AL2(1600,1630)                    
         DC    CL14'M-F 4-430P',AL1(2),X'15'                                    
         DC    C'05',AL1(05),AL1(E,P,0,0,0,0),AL2(1730,1800)                    
         DC    CL14'M-F 530-6P',AL1(2),X'15'                                    
         DC    C'05',AL1(05),AL1(C,M,Y,H,0,0),AL2(1630,1700)                    
         DC    CL14'M-F 430-5P',AL1(2),X'15'                                    
         DC    C'06',AL1(06),AL1(E,P,0,0,0,0),AL2(1800,1830)                    
         DC    CL14'M-F 6-630P',AL1(2),X'15'                                    
         DC    C'06',AL1(06),AL1(C,M,Y,H,0,0),AL2(1700,1730)                    
         DC    CL14'M-F 5-530P',AL1(2),X'15'                                    
         DC    C'07',AL1(07),AL1(E,P,0,0,0,0),AL2(1800,1930)                    
         DC    CL14'M-F 6-730P',AL1(6),X'15'                                    
         DC    C'07',AL1(07),AL1(C,M,Y,H,0,0),AL2(1700,1830)                    
         DC    CL14'M-F 5-630P',AL1(6),X'15'                                    
         DC    C'08',AL1(08),AL1(E,P,0,0,0,0),AL2(1830,1900)                    
         DC    CL14'M-F 630-7P',AL1(2),X'15'                                    
         DC    C'08',AL1(08),AL1(C,M,Y,H,0,0),AL2(1730,1800)                    
         DC    CL14'M-F 530-6P',AL1(2),X'15'                                    
         DC    C'09',AL1(09),AL1(E,P,0,0,0,0),AL2(1900,1930)                    
         DC    CL14'M-F 7-730P',AL1(2),X'15'                                    
         DC    C'09',AL1(09),AL1(C,M,Y,H,0,0),AL2(1800,1830)                    
         DC    CL14'M-F 6-630P',AL1(2),X'15'                                    
         DC    C'10',AL1(10),AL1(E,P,0,0,0,0),AL2(1930,2000)                    
         DC    CL14'M-F 730-8P',AL1(2),X'15'                                    
         DC    C'10',AL1(10),AL1(C,M,Y,H,0,0),AL2(1830,1900)                    
         DC    CL14'M-F 630-7P',AL1(2),X'15'                                    
         DC    C'11',AL1(11),AL1(E,P,0,0,0,0),AL2(2000,2300)                    
         DC    CL14'PRIME',AL1(12),X'17'                                        
         DC    C'11',AL1(11),AL1(C,M,Y,H,0,0),AL2(1900,2200)                    
         DC    CL14'PRIME',AL1(12),X'17'                                        
         DC    C'12',AL1(12),AL1(E,P,0,0,0,0),AL2(2300,2330)                    
         DC    CL14'M-F 11-1130P',AL1(2),X'15'                                  
         DC    C'12',AL1(12),AL1(C,M,Y,H,0,0),AL2(2200,2230)                    
         DC    CL14'M-F 10-1030P',AL1(2),X'15'                                  
         DC    C'13',AL1(13),AL1(E,P,0,0,0,0),AL2(2330,0100)                    
         DC    CL14'M-F 1130P-1A',AL1(6),X'15'                                  
         DC    C'13',AL1(13),AL1(C,M,Y,H,0,0),AL2(2230,2400)                    
         DC    CL14'M-F 1030P-12A',AL1(6),X'15'                                 
         DC    X'00'                                                            
*                                                                               
***********************************************************************         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
BYPREAD  DC    X'00'                                                            
DPTSW    DC    X'00'                                                            
SMCSW    DC    X'00'               STATION,MARKET OR COUNTY SWITCH              
SPACES   DC    30X'40'                                                          
RELCPTR  DS    XL1                 RELEASE COUNTY POINTER                       
SVDMAC   DS    H                                                                
SVDMA    DS    CL26                                                             
SVORIC   DS    XL2                                                              
SVORICH  DS    CL3                 CHAR 3 BYTE MKT OF ORIGIN                    
SVORIG   DS    CL26                                                             
SVCNCD   DS    XL2                                                              
SVCNTC   DS    CL16                                                             
SVSTCD   DS    XL1                                                              
SVTVHLD  DS    XL3                                                              
SVCBHLD  DS    XL3                                                              
SVNCHLD  DS    XL3                                                              
         EJECT                                                                  
IN1      DCB   DDNAME=IN1,                                             X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00900,                                            X        
               BLKSIZE=9000,                                           X        
               MACRF=GM,                                               X        
               EODAD=MORET                                                      
IN1A     DS    4600C                                                            
         SPACE 1                                                                
         DROP  R2,RB,RC                                                         
         SPACE 1                                                                
NMOD1Q1  EQU   ((((*-DECY93I)/4096)+1)*4096)                                    
         ORG   DECY93I+NMOD1Q1                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
DPTABD   DSECT                     DSECT FOR NET-IN-TAB-COUNTS                  
DPTABST  DS    0C                                                               
DPCHAR   DS    CL2                 DAYPART CHAR CODE                            
DPHEX    DS    AL1                 DAYPART HEX CODE                             
DPTZONE  DS    CL6                 DAYPART TIME ZONE                            
DPINTST  DS    CL2                 START TIME                                   
DPINTET  DS    CL2                 END TIME                                     
DPINTPN  DS    CL14                ALPHA DESCRIPTION                            
DPDURA   DS    AL1                 DURATION                                     
DPDAY    DS    CL1                 DAY CODE                                     
DPTABEN  DS    0C                                                               
DPTABLN  EQU   DPTABEN-DPTABST                                                  
DPTZLEN  EQU   L'DPTZONE                                                        
DPTLNQ   EQU   51                  49 DEMO BYTES + 2 BYTE DAYPART BYTES         
*                                                                               
         EJECT                                                                  
*============================= DEDEMCNVD =============================*         
       ++INCLUDE DEDEMCNVD                                                      
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================== DECOUNTYD ============================*         
       ++INCLUDE DECOUNTYD                                                      
         EJECT                                                                  
*============================ IREC DSECTS ============================*         
       ++INCLUDE DEINTD                                                         
         SPACE 2                                                                
       ++INCLUDE DEINTCYD                                                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================= DEDEMFILE =============================*         
       ++INCLUDE DEDEMFILE                                                      
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020DECY93I   07/24/08'                                      
         END                                                                    
