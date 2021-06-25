*          DATA SET DEARBCLI   AT LEVEL 006 AS OF 12/15/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEARCLIA                                                                 
         TITLE 'ARBITRON RADIO DEMO CONVERSION'                                 
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* Dec15/11 002 SCHO   SUPPORT DIGITAL RADIO                                     
* Aug09/00 016 GLEE - Use alternate means to detect Call Letter Chngs *         
*                      There are times when a station changes its call*         
*                      letters, but did not qualify in a sweep.  As a *         
*                      result, the next sweep that the station does   *         
*                      qualify for will have new call letters, but the*         
*                      call-letter-change indicator will not be on.   *         
*                      Mary McRae of Arbitron assured us that it is   *         
*                      safe to detect call letter changes by referring*         
*                      back to the Station Combo Type & ID            *         
*                                                                     *         
* Jul07/00 066 GLEE - Support auto-PUT upgrade for E- & P- tracks     *         
*                                                                     *         
***********************************************************************         
DEARBCLI CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEARBCLI                                                       
         USING DEMCOND,R8                                                       
         L     RA,ACOMFACS                                                      
         USING COMFACSD,RA                                                      
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
*                                                                               
         ST    RD,BASERD                                                        
*                                                                               
         B     MODES(R1)                                                        
*                                                                               
MODES    DS    0H                                                               
         B     READ                GET INPUT  (ARREC - INT)                     
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     ENDJOB              CLOSE INPUT                                  
         B     EXIT                                                             
         EJECT                                                                  
READ     DS    0H                                                               
         CLI   INTAPESW,1                                                       
         BE    OPENOK                                                           
         OPEN  (IN1,(INPUT))                                                    
         BAS   RE,GETLAT                                                        
         MVI   BYPREAD,0                                                        
*                                                                               
OPENOK   DS    0H                                                               
         CLI   BYPREAD,0                                                        
         BNE   OPEN10                                                           
*                                                                               
         L     R4,ARREC                                                         
         GET   IN1,(R4)                                                         
*                                                                               
OPEN10   L     R4,ARREC                                                         
         AHI   R4,4                                                             
         BAS   RE,PROCREC                                                       
         JL    OPENOK                                                           
         J     EXIT                                                             
*                                                                               
ENDJOB   DS    0H                                                               
         CLOSE (IN1,REWIND)                                                     
         MVI   BYPREAD,0                                                        
         MVI   INTAPESW,X'02'                                                   
         J     EXITE                                                            
         EJECT                                                                  
***********************************************************************         
GETLAT   NTR1                                                                   
         LA    R1,KEY                                                           
         USING BSKEY,R1                                                         
         MVI   BSCODE,BSCODEQU                                                  
         MVI   BSMEDIA,C'R'                                                     
         MVI   BSSRC,C'A'                                                       
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=C'DEMDIRA',KEY,THISKEY             
         LA    R1,THISKEY                                                       
         MVC   LATEST,BSBOOK                                                    
         XC    LATEST,=X'FFFF'     LATEST BOOK ACROSS ALL MARKETS               
         DROP  R1                                                               
*                                                                               
         MVC   P(45),=C'LATEST RADIO BOOK LOADED TO DEMOFILE: XXXX/XX'          
         LLC   RE,LATEST_YR                                                     
         AHI   RE,1900             0 <= DDS YEAR <= 255                         
         EDIT  (RE),(4,P+38)                                                    
         EDIT  LATEST_MO,(2,P+43),FILL=0                                        
         GOTO1 VPRINTER                                                         
*                                                                               
         LH    RE,LATEST                                                        
         SLL   RE,24                                                            
         SRL   RE,24               ISOLATE THE LATEST MONTH                     
*                                                                               
         LA    R3,SEASONQ                                                       
GL05     CLI   0(R3),XFF           TEST EOT                                     
         BNE   GL10                                                             
         LH    RE,LATEST           EOT: LATEST BOOK MUST BE JANUARY             
         SRL   RE,8                ISOLATE THE LATEST YEAR                      
         SHI   RE,1                FORCE BOOK TO NOV. OF *PRIOR* YEAR           
         STC   RE,LATEST_YR                                                     
         MVI   LATEST_MO,MON_NOV                                                
         B     GETLATX                                                          
*                                                                               
GL10     CH    RE,0(R3)            LATEST BOOK IS WITHIN THIS SEASON?           
         BL    GL20                NO: KEEP LOOKING IN SEASON TABLE             
         MVC   LATEST_MO,1(R3)     USE THE LATEST *SEASONAL* BOOK               
         B     GETLATX                                                          
*                                                                               
GL20     LA    R3,L'SEASONQ(,R3)   TRY THE PREVIOUS SEASON                      
         B     GL05                                                             
                                                                                
GETLATX  DS    0H                                                               
         MVC   P(45),=C'EARLIST ELIGIBLE CALL LETTER CHANGES: XXXX/XX'          
         LLC   RE,LATEST_YR                                                     
         AHI   RE,1900             0 <= DDS YEAR <= 255                         
         EDIT  (RE),(4,P+38)                                                    
         EDIT  LATEST_MO,(2,P+43),FILL=0                                        
         GOTO1 VPRINTER                                                         
*                                                                               
         IF (OC,IPHSPARM,IPHSPARM,NZ)         IPHSPARM= CARD PRESENT?           
           GOTO1 VDATVAL,DMCB,(2,IPHSPARM),DUB  YES: VALIDATE BOOK              
           CLC   =C'000000',DUB                 FORMAT MUST BE MMMYY            
           JZ    *+2                            INVALID MMMYY BOOK              
           GOTO1 VDATCON,DMCB,DUB,(3,OVRRIDBK)                                  
           MVC   P(45),=C'OVERRIDE CALL LETTER CHANGE FILTER:   XXXX/XX+        
               '                                                                
           LLC   RE,OVERRIDE_YR                                                 
           AHI   RE,1900             0 <= DDS YEAR <= 255                       
           EDIT  (RE),(4,P+38)                                                  
           EDIT  OVERRIDE_MO,(2,P+43),FILL=0                                    
           GOTO1 VPRINTER                                                       
         ENDIF ,                                                                
*                                                                               
         J     EXIT                                                             
***********************************************************************         
*PROCREC -  PROCESS RECORDS                                                     
***********************************************************************         
PROCREC  NTR1                      PROCESS AND COMBINE THE RECORDS              
*                                                                               
         USING ARBCHNGD,R4                                                      
         CLI   BYPREAD,0                                                        
         BNE   PR10                                                             
*                                                                               
* DEIS DEC/20: DETECT IF WE INADVERTENTLY TRY TO CONVERT A CALL LETTER          
*   FILE IN WHICH THE BAND IS ONLY *ONE* CHARACTER INSTEAD OF *TWO*.            
         IF (CLI,ACTYPE,NE,C'C'),AND,                                           
            (CLI,ACTYPE,NE,C'N'),AND,                                           
            (CLI,ACTYPE,NE,C'O')                                                
           MVC   P(35),=C'***ERROR: INVALID INPUT FILE FORMAT'                  
           GOTO1 VPRINTER                                                       
           GOTO1 VDATAMGR,DMCB,=C'OPMSG',=C'AUTONOTE*USOPSUP:ERROR! INV+        
               ALID INPUT FILE FORMAT'                                          
           MVI RETCODE,8           FATAL ERROR                                  
           J   ENDJOB              DON'T BOTHER PROCESSING MORE RECORDS         
         ENDIF ,                                                                
*                                                                               
         CLI   ACTYPE,C'C'                                                      
         JNE   EXITL                                                            
*                                                                               
         BAS   RE,PROCBK           PROCESS THE BOOK INTO X'FFFF' FORMAT         
         JL    EXITL                                                            
         BAS   RE,PROCDIGI         PROCESS THE BAND INTO DIGITAL EQUIV          
         JNE   EXITL               ONE OF THE UNSUPPORTED BANDS                 
*                                                                               
PR10     L     R2,AIREC                                                         
         USING INTERD,R2                                                        
*                                                                               
         MVI   INTRTYP,DSECDEQU                                                 
*                                                                               
         LA    R6,4(R2)                                                         
         USING DSEKEY,R6                                                        
*                                                                               
         CLI   RLSIFLG,0                                                        
         BE    PROCBKLO                                                         
         CLI   RLSIFLG,RLSIFLG1                                                 
         BE    PROCBKHI                                                         
         CLI   RLSIFLG,RLSIFLG2                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   BYPREAD,0                                                        
         B     PROCX                                                            
*                                                                               
PROCBKLO DS    0H                                                               
         XC    DSEKMAJ,DSEKMAJ                                                  
         MVI   DSECODE,DSECDEQU                                                 
         MVI   DSEMEDIA,C'R'                                                    
         MVI   DSESRC,C'A'                                                      
         MVI   DSEIND,DSEIBKLW                                                  
         MVC   DSEOLDCL,ACPRECA    INCLUDES BAND                                
         MVC   DSENEWCL,ACNEWCA                                                 
         MVC   DSEEFFBK,BOOK                                                    
         MVI   RLSIFLG,RLSIFLG1                                                 
         MVI   BYPREAD,C'Y'                                                     
         BAS   RE,RELIREC          WE DO NOT RETURN FROM THIS CALL!             
*                                                                               
PROCBKHI DS    0H                                                               
         XC    DSEKMAJ,DSEKMAJ                                                  
         MVI   DSECODE,DSECDEQU                                                 
         MVI   DSEMEDIA,C'R'                                                    
         MVI   DSESRC,C'A'                                                      
         MVI   DSEIND,DSEIBKHI                                                  
         MVC   DSEBKEFF,BOOK                                                    
         MVC   DSECLOLD,ACPRECA                                                 
         MVC   DSECLNEW,ACNEWCA                                                 
         MVI   RLSIFLG,RLSIFLG2                                                 
         BAS   RE,RELIREC          WE DO NOT RETURN FROM THIS CALL!             
*                                                                               
PROCX    DS    0H                                                               
         J     EXIT                                                             
******************************************************************              
* R4: POINTS TO INPUT RECORD                                                    
******************************************************************              
PROCBK   NTR1                                                                   
*                                                                               
         DS    0H                  GET BOOK FROM PERIOD SHORT NAME              
         PACK  DUB,ACSRVYR                                                      
         CVB   RE,DUB                                                           
         SHI   RE,1900             0 <= DDS YEAR <= 255                         
         STC   RE,BOOK_YR                                                       
*                                                                               
         PACK  DUB,ACSRVMO                                                      
         CVB   RE,DUB                                                           
         STC   RE,BOOK_MO                                                       
*                                                                               
         OC    OVRRIDBK,OVRRIDBK                                                
         BZ    PROCBK10            NO CALL LETTER BOOK OVERRIDE                 
         CLC   BOOK,OVRRIDBK                                                    
         BE    PROCBK20            MATCH: KEEP THE CALL LETTER CHANGE           
         B     EXITL                                                            
*                                                                               
PROCBK10 DS    0H                                                               
         CLC   BOOK,LATEST                                                      
         JL    EXITL                                                            
*                                                                               
PROCBK20 DS    0H                                                               
         XC    BOOK,=X'FFFF'                                                    
PBX      J     EXITE                                                            
**********************************************************************          
PROCDIGI NTR1                                                                   
*                                                                               
         USING ARDNTRND,RE                                                      
PD05     GOTO1 CDEMTABS,DMCB,ARDNTRN                                            
         ICM   RE,15,0(R1)         RE: ARBITRON DIGITAL TRANSLATIONS            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)            LENGTH OF TABLE ENTRIES                      
*                                                                               
PD10     CLI   0(RE),X'FF'         NEW CALL LETTERS' BAND                       
         BE    PDN                                                              
         CLC   ACNEWBA,ARDNBAND    FOUND A MATCH                                
         BE    PD15                                                             
         AR    RE,RF                                                            
         B     PD10                                                             
*                                                                               
PD15     MVC   ACNEWBA,ARDNAMFM    ORIGINAL BAND                                
         CLI   ACNEWCA,C'W'                                                     
         BNE   *+14                                                             
         MVC   ACNEWCA(1),ARDNEAST EAST COAST EQUATE                            
         B     PD30                                                             
         CLI   ACNEWCA,C'K'                                                     
         BNE   *+14                                                             
         MVC   ACNEWCA(1),ARDNWEST WEST COAST EQUATE                            
         B     PD30                                                             
*                                                                               
PD30     CLI   0(RE),X'FF'         OLD CALL LETTERS' BAND                       
         BE    PDX                                                              
         CLC   ACPREBA,ARDNBAND    FOUND A MATCH                                
         BE    PD35                                                             
         AR    RE,RF                                                            
         B     PD30                                                             
*                                                                               
PD35     MVC   ACPREBA,ARDNAMFM    ORIGINAL BAND                                
         CLI   ACPRECA,C'W'                                                     
         BNE   *+14                                                             
         MVC   ACPRECA(1),ARDNEAST EAST COAST EQUATE                            
         B     PDX                                                              
         CLI   ACPRECA,C'K'                                                     
         BNE   *+14                                                             
         MVC   ACPRECA(1),ARDNWEST WEST COAST EQUATE                            
         B     PDX                                                              
*                                                                               
PDN      CR    RB,RE                                                            
         B     PDX                                                              
PDY      CR    RB,RB                                                            
PDX      J     EXIT                                                             
*                                                                               
**********************************************************************          
*CNVWR - SORT PROCESSING. SLOT UNIVS AND TOT INTO APPROPRIATE RECDS.            
**********************************************************************          
CNVWR    DS    0H                                                               
         L     R2,ASREC                                                         
         USING INTERD,R2                                                        
         L     R6,ASREC                                                         
         LA    R6,4(R6)                                                         
         USING DSEKEY,R6                                                        
         CLI   DSEIND,DSEIBKLW                                                  
         BNE   *+14                                                             
         MVC   BOOK,DSEEFFBK                                                    
         B     CNVWR10                                                          
*                                                                               
         CLI   DSEIND,DSEIBKHI                                                  
         BNE   CNVWR20                                                          
         MVC   BOOK,DSEBKEFF                                                    
*                                                                               
CNVWR10  XC    BOOK,=X'FFFF'                                                    
         OC    OVRRIDBK,OVRRIDBK                                                
         BZ    *+18                NO CALL LETTER BOOK OVERRIDE                 
         CLC   BOOK,OVRRIDBK       EFFECTIVE DATE MATCHES OVERRIDE?             
         BE    CNVWR15             YES: ACCEPT THE CALL LETTER CHANGE           
         B     CNVWR20             IGNORE THE CALL LETTER CHANGE                
*                                                                               
         CLC   BOOK,LATEST         EFFECTIVE DATE IS WITHIN RANGE?              
*********BNH   CNVWR20              (REMOVED BY DEIS: DEC/2020)                 
         BL    CNVWR20             NO: IGNORE THE CALL LETTER CHANGE            
*                                                                               
CNVWR15  DS    0H                  ACCEPT THE CALL LETTER CHANGE                
         IF (CLI,DSEIND,EQ,DSEIBKHI)  NO NEED TO TRACE BOTH HI AND LO           
           MVC   P(39),=C'EFFECTIVE YYYY/MM: OLD=XXXXX, NEW=XXXXX'              
           LLC   RE,BOOK_YR                                                     
           AHI   RE,1900             0 <= DDS YEAR <= 255                       
           EDIT  (RE),(4,P+10)                                                  
           EDIT  BOOK_MO,(2,P+15),FILL=0                                        
           MVC   P+23(5),DSECLOLD                                               
           MVC   P+34(5),DSECLNEW                                               
         GOTO1 VPRINTER                                                         
         ENDIF ,                                                                
         B     CNVWRX              ACCEPT THE CALL LETTER CHANGE                
*                                                                               
CNVWR20  MVI   INTRTYP,0                                                        
*                                                                               
CNVWRX   J     EXIT                                                             
         DROP  R2                                                               
         DROP  R6                                                               
         EJECT                                                                  
         TITLE 'ARBITRON RADIO DEMO CONVERSION'                                 
***********************************************************************         
*======================= RELEASE INTERIM RECORD ======================*         
                                                                                
* Routine is called when an interim record needs to be released to the          
*  sort.  This routine will exit the IPHASE entirely.                           
* At entry,                                                                     
*   CURRTYP = current record type                                               
*   BYPREAD = zero to release record                                            
*           = non-zero if returning from having released record                 
                                                                                
RELIREC  NTR1                                                                   
*                                                                               
RIRRELX  DS    0H                  EXIT IPHASE                                  
         CLI   RLSIFLG,2                                                        
         BNE   *+12                                                             
         MVI   RLSIFLG,0                                                        
         MVI   BYPREAD,0                                                        
*                                                                               
         L     RD,BASERD                                                        
                                                                                
RIRX     DS    0H                                                               
         J     XIT                                                              
***********************************************************************         
         EJECT                                                                  
EXITH    DS    0H                  EXIT W/ CC HIGH                              
         LA    R0,1                                                             
         J     EXITCR                                                           
                                                                                
EXITL    DS    0H                  EXIT W/ CC LOW                               
         LHI   R0,-1                                                            
         J     EXITCR                                                           
                                                                                
EXITE    DS    0H                  EXIT W/ CC EQUAL                             
         SR    R0,R0                                                            
         J     EXITCR                                                           
                                                                                
EXITCR   DS    0H                                                               
         AR    R0,R9                                                            
         CR    R0,R9               SET CC                                       
         J     EXIT                 AND EXIT                                    
*                                                                               
XIT      XIT1                                                                   
EXIT     XMOD1 1                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (LTORG && CONSTANTS)'            
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
         EJECT                                                                  
IN1      DCB   DDNAME=IN1,                                             +        
               DSORG=PS,                                               +        
               RECFM=VB,                                               +        
               LRECL=00100,                                            +        
               MACRF=GM,                                               +        
               EODAD=ENDJOB                                                     
                                                                                
         DS    0H                                                               
LATEST   DS    XL2                                                              
         ORG   LATEST                                                           
LATEST_YR DS   X                                                                
LATEST_MO DS   X                                                                
*                                                                               
BOOK     DC    XL2'00'                                                          
         ORG   BOOK                                                             
BOOK_YR  DS    X                                                                
BOOK_MO  DS    X                                                                
*                                                                               
OVRRIDBK DC    XL2'00'             SET VIA IPHSPARM= CARD                       
         ORG   OVRRIDBK                                                         
OVERRIDE_YR DS X                                                                
OVERRIDE_MO DS X                                                                
*                                                                               
BASERD   DS    A                   RD LINK TO EXIT PHASE                        
KEY      DC    XL18'00'                                                         
THISKEY  DS    XL23                                                             
BYPREAD  DC    X'00'                                                            
RLSIFLG  DC    X'00'                                                            
RLSIFLG1 EQU   1                                                                
RLSIFLG2 EQU   2                                                                
RLSIFLG3 EQU   3                                                                
         DROP  R8,RB                                                            
         TITLE 'ARBITRON RADIO DEMO CONVERSION (EQUATES)'                       
***********************************************************************         
*============================== EQUATES ==============================*         
                                                                                
EOT      EQU   X'00'               END OF TABLE MARKER                          
                                                                                
*                                 ********* BIT MANIPULATIONS *********         
XFF      EQU   X'FF'                                                            
X80      EQU   X'80'                                                            
X40      EQU   X'40'                                                            
X20      EQU   X'20'                                                            
X10      EQU   X'10'                                                            
X08      EQU   X'08'                                                            
X04      EQU   X'04'                                                            
X02      EQU   X'02'                                                            
X01      EQU   X'01'                                                            
X00      EQU   X'00'                                                            
         TITLE 'ARBITRON RADIO DEMO CONVERSION (TABLES)'                        
***********************************************************************         
*=============================== TABLES ==============================*         
         EJECT                                                                  
SEASONQ  DS    0AL(2)                                                           
         DC    AL2(11)                                                          
         DC    AL2(7)                                                           
         DC    AL2(5)                                                           
         DC    AL2(2)                                                           
         DC    AL1(XFF)                                                         
*                                                                               
         EJECT                                                                  
* ++INCLUDE DEDEMCNVD                                                           
* ++INCLUDE DEINTD                                                              
* ++INCLUDE DEDEMFILE                                                           
* ++INCLUDE DDCOMFACS                                                           
* ++INCLUDE DEDEMTABD                                                           
* ++INCLUDE DDMONYREQU                                                          
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
       ++INCLUDE DEINTD                                                         
         EJECT                                                                  
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
       ++INCLUDE DDMONYREQU                                                     
         EJECT                                                                  
         PRINT ON                                                               
***********************************************************************         
ARBCHNGD DSECT                                                                  
ACSRVYR  DS    CL4                 SURVEY YEAR                                  
ACSRVMO  DS    CL2                 SURVEY MONTH                                 
ACNEWCA  DS    CL4                 NEW CALL LETTER                              
ACNEWBA  DS    CL2                 NEW BAND                                     
ACTYPE   DS    CL1                 C=CHANGED N=NEW O=OFF AIR                    
ACPREYR  DS    CL4                 PREVIOUS YEAR                                
ACPREMO  DS    CL2                 PREVIOUS MONTH                               
ACPRECA  DS    CL4                 PREVIOUS CALL LETTERS                        
ACPREBA  DS    CL2                 PREVIOUS BAND                                
ACLNEQU  EQU   *-ARBCHNGD                                                       
         EJECT                                                                  
         TITLE 'ARBITRON RADIO DEMO CONVERSION (OTHER DSECTS)'                  
***********************************************************************         
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006DEARBCLI  12/15/20'                                      
         END                                                                    
