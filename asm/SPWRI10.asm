*          DATA SET SPWRI10    AT LEVEL 019 AS OF 05/01/02                      
*PHASE T20410A,*                                                                
         TITLE 'T20410 - AT&&T BUDGET AND EXPENSE TRACKING TAPES'               
***********************************************************************         
*                                                                     *         
*                 M O D I F I C A T I O N S   L O G                   *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
************** MODULE NOT IN USE **************************************         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*--DATE---LVL-BY-----CHANGE-------------------------------------------*         
* 16JUN98 HISTORY LOST                                                *         
* 16JUN98 17  NRK -- Y2K COMPLIANCE                                   *         
*                                                                     *         
***********************************************************************         
T20410   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20410,RA                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      SPOTIO HOOK                                  
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPFINAL      FINAL HOOK                                   
         BE    FINAL                                                            
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
* INITIALIZATION                                                                
*                                                                               
INIT     OI    SBQSKIP,SBQSKBUY+SBQSKGL+SBQSKBIL                                
         MVI   SBQSEPES,C'Y'       ENSURE EST=ALL                               
         OI    SBQPER,SBQPMN       MONTHS                                       
         MVI   SBQPERLO,1                                                       
         MVI   SBQPERHI,X'FF'                                                   
         OI    DATAIND2,DIEST      ESTIMATE                                     
         XC    LEVELS,LEVELS                                                    
         MVI   MYFIRSTH,11         SET DRIVER'S FIRST HEADLINE                  
         CLI   RPTSCRN,0           TEST USER REPORT SCREEN                      
         BE    INITX                                                            
         LA    R2,ATTTITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
         GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
* FURTHER REQUEST VALIDATION                                                    
*                                                                               
VALID    CLI   SBQBPRD,X'FF'       TEST PRD=POL REQUEST                         
         BNE   *+12                NO                                           
         MVI   SBQBPRD,0           YES-BREAK OUT THE PRODUCTS                   
         OI    SBQPIND,SBQPOLSP                                                 
         LA    R2,ATTPERH                                                       
         GOTO1 ANY                 VALIDATE PERIOD                              
         GOTO1 DATVAL,DMCB,(2,WORK),BILLPER       SINGLE MONTH                  
         OC    0(4,R1),0(R1)                                                    
         BZ    EINV                                                             
         CLC   BILLPER+4(2),ZEROS                                               
         BNE   *+10                                                             
         MVC   BILLPER+4(2),=C'15'                                              
         GOTO1 DATCON,DMCB,(0,BILLPER),(3,FULL)                                 
         ZIC   RF,FULL                                                          
         STC   RF,BILLYR                                                        
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         SLL   RE,4                                                             
         STC   RE,BILLYRMN         BILL YEAR/MONTH                              
         OC    BILLYRMN,FULL+1                                                  
         B     XIT                                                              
         EJECT                                                                  
* ERROR EXITS AND MESSAGES                                                      
*                                                                               
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
         EJECT                                                                  
* SPOTIO INPUT HOOK                                                             
*                                                                               
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCAG     TEST AGENCY RECORD HOOK                      
         BNE   INP1                                                             
         XC    VENDOR,VENDOR       YES-SET VENDOR                               
         MVI   INVOPEN,C'N'        INIT TAPE FILE OPEN SWITCHES                 
         MVI   ESTOPEN,C'N'                                                     
         B     INPX                                                             
*                                                                               
INP1     CLI   SBMODE,SBPROCES     TEST ESTIMATE RECORD HOOK                    
         BNE   INPX                                                             
         XC    KEY,KEY             YES-READ AT&T ESTIMATE RECORD                
         LA    R5,KEY                                                           
         USING PGESTD,R5                                                        
         MVI   PGKRID,PGKNDIRQ                                                  
         MVI   PGKSID,PGKNDISQ                                                  
         MVC   PGKAM,SBBAGYMD                                                   
         MVC   PGKCLT,SBBCLT                                                    
         MVC   PGKPRD,SBPRD                                                     
         MVC   PGKEST,SBBEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(PGKLENQ),KEYSAVE    TEST RECORD FOUND                        
         BNE   INPX                    NO-IGNORE THIS ESTIMATE                  
         LA    R1,ATDATA                                                        
*                                                                               
INP2     CLI   0(R1),0                 CLEAR ALL AT&T DATA FIELDS               
         BE    INP4                                                             
         L     RE,8(R1)                                                         
         ZIC   RF,12(R1)                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         XC    0(0,RE),0(RE)                                                    
         LA    R1,16(R1)                                                        
         B     INP2                                                             
*                                                                               
INP4     L     R5,AIO2                                                          
         ST    R5,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R5,PGKEDQ(R5)       SCAN ELEMENTS FOR AT&T DATA                  
         SR    R0,R0                                                            
*                                                                               
INP6     CLI   0(R5),0                                                          
         BE    INP12                                                            
         CLI   0(R5),PGSTEIDQ                                                   
         BNE   INP10                                                            
         USING PGSTELMD,R5                                                      
         LA    R1,ATDATA                                                        
*                                                                               
INP8     CLI   0(R1),0                                                          
         BE    INP10                                                            
         CLC   PGSTNAME,0(R1)                                                   
         BE    *+12                                                             
         LA    R1,16(R1)                                                        
         B     INP8                                                             
         ZIC   RE,12(R1)                                                        
         BCTR  RE,0                                                             
         L     RF,8(R1)                                                         
         EX    RE,*+4                                                           
         MVC   0(0,RF),PGSTDATA                                                 
*                                                                               
INP10    IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     INP6                                                             
*                                                                               
INP12    MVI   ERRCD,0             SET ERROR CODES                              
         OC    VENDOR,VENDOR                                                    
         BNZ   *+8                                                              
         OI    ERRCD,ERRVEN                                                     
         LA    R1,ATDATA                                                        
*                                                                               
INP14    CLI   0(R1),0                                                          
         BE    INP16                                                            
         L     RE,8(R1)                                                         
         ZIC   RF,12(R1)                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SPACES                                                   
         BH    *+10                                                             
         OC    ERRCD,13(R1)                                                     
         LA    R1,16(R1)                                                        
         B     INP14                                                            
*                                                                               
INP16    XC    KEY,KEY             READ BILL RECORDS                            
         LA    R6,KEY                                                           
         USING BILLRECD,R6                                                      
         MVC   BKEYAM,SBBAGYMD                                                  
         MVC   BKEYCLT,SBBCLT                                                   
         MVC   BKEYPRD,SBPRD                                                    
         MVC   BKEYEST,SBBEST                                                   
         GOTO1 HIGH                                                             
         MVI   INTYPE,0                                                         
         B     INP20                                                            
*                                                                               
INP18    GOTO1 SEQ                                                              
*                                                                               
INP20    CLC   KEY(BKEYYSRV-BKEY),KEYSAVE  TEST SAME AGYMD/CLT/PRD/EST          
         BNE   INP22                                                            
         CLC   BKEYMBIL,BILLYRMN   TEST BILL MONTH MATCHES REQUEST              
         BNE   INP18                                                            
         ZIC   RE,BKEYYSRV         YES-CHECK CORRECT DECADE !                   
         ZIC   RF,BILLYR                                                        
         SR    RE,RF                                                            
         LPR   RE,RE                                                            
         CH    RE,=H'4'                                                         
         BH    INP18                                                            
         L     R6,AIO3             GET THE BILL RECORD                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   INTYPE,C'I'                                                      
*Y2K         MVC   INYEAR+2(2),SBESTND     FORMAT INVOICE RECORD                
*Y2K         MVC   INYEAR(2),=C'19'                                             
*Y2K         CLI   SBESTNDB,80                                                  
*Y2K         BH    *+10                                                         
*Y2K         MVC   INYEAR(2),=C'20'                                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,SBESTND),(20,DUB) GET DATE W/CENTURY              
         MVC   INYEAR(4),DUB       MOVE IN CENTURY                              
*                                                                               
         MVC   INVENDOR,VENDOR                                                  
         MVC   INVNO(6),ZEROS                                                   
         MVC   INVNO+6(6),BINVNO                                                
*Y2K         MVC   INDATE(2),BDATE+2                                            
*Y2K         MVI   INDATE+2,C'/'                                                
*Y2K         MVC   INDATE+3(2),BDATE+4                                          
*Y2K         MVI   INDATE+5,C'/'                                                
*Y2K         MVC   INDATE+6(2),BDATE                                            
*                                                                               
         GOTO1 DATCON,DMCB,(0,BDATE),(10,INDATE) PUT IN MM/DD/YY FMT            
*                                                                               
*Y2K         GOTO1 DATCON,DMCB,(3,BDUEDATE),(0,DUB)                             
*Y2K         MVC   INPAYDUE(2),DUB+2                                            
*Y2K         MVI   INPAYDUE+2,C'/'                                              
*Y2K         MVC   INPAYDUE+3(2),DUB+4                                          
*Y2K         MVI   INPAYDUE+5,C'/'                                              
*Y2K         MVC   INPAYDUE+6(2),DUB                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(10,INPAYDUE) PUT IN MM/DD/YY           
*                                                                               
         MVC   INESTNO(5),ZEROS                                                 
         MVC   INESTNO+5(3),SBEST                                               
         MVI   INAMT,C'0'                                                       
         MVC   INAMT+1(8),BAMT                                                  
         MVI   INAMT+9,C'.'                                                     
         MVC   INAMT+10(2),BAMT+8                                               
         MVC   INBUS,BUSUNIT                                                    
         MVC   INDIV,DIVISION                                                   
         MVC   INRES,RESPCD                                                     
         MVC   INEXT,EXTC                                                       
         MVC   INEST,ESTID                                                      
         MVC   INBUD,BUDGTNO                                                    
         MVC   INDES,DESCRPTN                                                   
         MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
         MVI   GLOPTS,C'I'                                                      
         BAS   RE,GODRIVER                                                      
         B     INP18               NEXT BILL RECORD                             
*                                                                               
INP22    CLI   INTYPE,0            TEST ANY BILL RECORDS FOUND                  
         BNE   INPX                YES-DONE                                     
         CLC   DATE,SPACES         NO-TEST AT&T DATE WITHIN REQUEST             
         BNH   INPX                   BILL MONTH                                
         GOTO1 DATVAL,DMCB,(0,DATE),ATTDATE                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    INPX                                                             
         CLC   BILLPER(4),ATTDATE                                               
         BNE   INPX                                                             
         L     R5,AIO1             YES-                                         
         USING ESTHDRD,R5                                                       
         MVI   ESTYPE,C'I'         FORMAT ESTIMATE RECORD                       
*Y2K         MVC   ESYEAR+2(2),SBESTND                                          
*Y2K         MVC   ESYEAR(2),=C'19'                                             
*Y2K         CLI   SBESTNDB,80                                                  
*Y2K         BH    *+10                                                         
*Y2K         MVC   ESYEAR(2),=C'20'                                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,SBESTND),(20,DUB) GET DATE W/CENTURY              
         MVC   ESYEAR(4),DUB       MOVE IN CENTURY                              
*                                                                               
         MVC   ESMONTH,SBESTST+2                                                
         MVC   ESVENDOR,VENDOR                                                  
         MVC   ESESTNO(5),ZEROS                                                 
         MVC   ESESTNO+5(3),SBEST                                               
*Y2K         MVC   ESDATE(2),ATTDATE+2                                          
*Y2K         MVI   ESDATE+2,C'/'                                                
*Y2K         MVC   ESDATE+3(2),ATTDATE+4                                        
*Y2K         MVI   ESDATE+5,C'/'                                                
*Y2K         MVC   ESDATE+6(2),ATTDATE                                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,ATTDATE),(10,ESDATE) PUT IN MM/DD/YY FMT          
*                                                                               
         SR    RF,RF                                                            
         LA    R1,EORDN            NET IS 85% GROSS                             
         LA    R0,26                                                            
         A     RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,*-8                                                           
         M     RE,=F'170'                                                       
         D     RE,=F'100'                                                       
         LTR   R6,RF                                                            
         BM    *+8                                                              
         A     R6,=F'1'                                                         
         SRA   R6,1                                                             
         EDIT  (R6),(12,ESAMT),2,ZERO=NOBLANK                                   
         MVC   ESBUS,BUSUNIT                                                    
         MVC   ESDIV,DIVISION                                                   
         MVC   ESRES,RESPCD                                                     
         MVC   ESEXT,EXTC                                                       
         MVC   ESEST,ESTID                                                      
         MVC   ESBUD,BUDGTNO                                                    
         MVC   ESDES,DESCRPTN                                                   
         MVI   GLMODE,GLINPUT      CALL DRIVER FOR INPUT                        
         MVI   GLOPTS,C'E'                                                      
         BAS   RE,GODRIVER                                                      
*                                                                               
INPX     B     XIT                                                              
         SPACE 2                                                                
ATDATA   DC    CL8'BUS UNIT',AL4(BUSUNIT),AL1(L'BUSUNIT,ERRBUS,0,0)             
         DC    CL8'DIVISION',AL4(DIVISION),AL1(L'DIVISION,ERRDIV,0,0)           
         DC    CL8'RESPCD  ',AL4(RESPCD),AL1(L'RESPCD,ERRRES,0,0)               
         DC    CL8'EXTC    ',AL4(EXTC),AL1(L'EXTC,ERREXT,0,0)                   
         DC    CL8'ESTID   ',AL4(ESTID),AL1(L'ESTID,ERREST,0,0)                 
         DC    CL8'BUDGTNO ',AL4(BUDGTNO),AL1(L'BUDGTNO,ERRBUD,0,0)             
         DC    CL8'DESCRPTN',AL4(DESCRPTN),AL1(L'DESCRPTN,ERRDES,0,0)           
         DC    CL8'DATE    ',AL4(DATE),AL1(L'DATE,0,0,0)                        
         DC    X'00'                                                            
         EJECT                                                                  
* DRIVER HOOK                                                                   
*                                                                               
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                                      
*                                                                               
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),X'FF'                                                      
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'IREC    ',A(IREC)                                            
         DC    CL8'OREC    ',A(OREC)                                            
         DC    CL8'IERR    ',A(IERR)                                            
         DC    CL8'OERR    ',A(OERR)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
* DRIVER INITIALIZATION                                                         
*                                                                               
DRVINIT  OI    GLINDS,GLPALDET     PRINT ALL DETAILS                            
         B     XIT                                                              
         EJECT                                                                  
* DRIVER HOOK TO EXECUTE ROUTINES                                               
*                                                                               
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   *+8                                                              
         MVI   INDATA,1            YES-ALL DATA IS SIGNIFICANT                  
         L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
* I/O ROUTINES                                                                  
*                                                                               
IREC     CLI   GLARGS,C'I'                                                      
         BNE   *+14                                                             
         MVC   0(L'INREC,R2),INREC    INVOICE RECORD                            
         B     XIT                                                              
         MVC   0(L'ESREC,R2),ESREC    ESTIMATE RECORD                           
         B     XIT                                                              
*                                                                               
OREC     LA    R1,INVOPEN          RECORD OUTPUT ROUTINE                        
         LA    R5,INVFILE                                                       
         CLI   GLARGS,C'I'                                                      
         BE    OREC2                                                            
         LA    R1,ESTOPEN                                                       
         LA    R5,ESTFILE                                                       
         CLI   GLARGS,C'E'                                                      
         BE    OREC2                                                            
         DC    H'0'                                                             
OREC2    CLI   0(R1),C'Y'                                                       
         BE    OREC4                                                            
         MVI   0(R1),C'Y'                                                       
         OPEN  ((R5),OUTPUT)                                                    
OREC4    PUT   (R5),(R2)                                                        
         CLI   GLARGS,C'I'                                                      
         BNE   OREC6                                                            
         MVC   0(99,R3),0(R2)                                                   
         MVC   198(51,R3),99(R2)                                                
         B     XIT                                                              
OREC6    MVC   0(80,R3),0(R2)                                                   
         MVC   198(52,R3),80(R2)                                                
         B     XIT                                                              
*                                                                               
IERR     MVC   0(1,R2),ERRCD                                                    
         B     XIT                                                              
*                                                                               
OERR     MVC   ERRCD,0(R2)                                                      
         LA    R1,INERRMSK                                                      
         CLI   GLARGS,C'I'                                                      
         BE    OERR2                                                            
         LA    R1,ESERRMSK                                                      
         CLI   GLARGS,C'E'                                                      
         BE    OERR2                                                            
         DC    H'0'                                                             
OERR2    NC    ERRCD,0(R1)                                                      
         LR    R5,R3                                                            
         TM    ERRCD,ERRBUS                                                     
         BZ    *+12                                                             
         MVI   0(R5),C'B'                                                       
         LA    R5,1(R5)                                                         
         TM    ERRCD,ERRDIV                                                     
         BZ    *+12                                                             
         MVI   0(R5),C'D'                                                       
         LA    R5,1(R5)                                                         
         TM    ERRCD,ERRRES                                                     
         BZ    *+12                                                             
         MVI   0(R5),C'R'                                                       
         LA    R5,1(R5)                                                         
         TM    ERRCD,ERREXT                                                     
         BZ    *+12                                                             
         MVI   0(R5),C'X'                                                       
         LA    R5,1(R5)                                                         
         TM    ERRCD,ERREST                                                     
         BZ    *+12                                                             
         MVI   0(R5),C'E'                                                       
         LA    R5,1(R5)                                                         
         TM    ERRCD,ERRBUD                                                     
         BZ    *+12                                                             
         MVI   0(R5),C'U'                                                       
         LA    R5,1(R5)                                                         
         TM    ERRCD,ERRDES                                                     
         BZ    *+12                                                             
         MVI   0(R5),C'S'                                                       
         LA    R5,1(R5)                                                         
         TM    ERRCD,ERRVEN                                                     
         BZ    *+12                                                             
         MVI   0(R5),C'V'                                                       
         LA    R5,1(R5)                                                         
         B     XIT                                                              
         EJECT                                                                  
* FINAL HOOK                                                                    
*                                                                               
FINAL    CLI   INVOPEN,C'Y'        CLOSE THE OUTPUT FILES                       
         BNE   FIN2                                                             
         CLOSE (INVFILE)                                                        
*                                                                               
FIN2     CLI   ESTOPEN,C'Y'                                                     
         BNE   FINX                                                             
         CLOSE (ESTFILE)                                                        
*                                                                               
FINX     B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO HOOK TO CALLING PROGRAM TO CALL DRIVER                             
*                                                                               
GODRIVER NTR1                                                                   
         L     RF,ADRIVER                                                       
         L     RE,SAVERD01                                                      
         LM    R0,RC,20(RE)                                                     
         BASR  RE,RF                                                            
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
         DS    0F                                                               
VENDOR   DS    CL4                 AGENCY ID                                    
*                                                                               
BUSUNIT  DS    CL2                 AT&T ESTIMATE RECORD FIELDS                  
DIVISION DS    CL2                                                              
RESPCD   DS    CL9                                                              
EXTC     DS    CL5                                                              
ESTID    DS    CL5                                                              
BUDGTNO  DS    CL3                                                              
DESCRPTN DS    CL50                                                             
DATE     DS    CL8                                                              
*                                                                               
INVOPEN  DS    CL1                                                              
ESTOPEN  DS    CL1                                                              
*                                                                               
ERRCD    DS    XL1                 ERROR BYTE                                   
ERRBUS   EQU   X'80'               BUSINESS UNIT                                
ERRDIV   EQU   X'40'               DIVISION                                     
ERRRES   EQU   X'20'               RESP CODE                                    
ERREXT   EQU   X'10'               EXTC                                         
ERREST   EQU   X'08'               ESTIMATE ID                                  
ERRBUD   EQU   X'04'               BUDGET NO                                    
ERRDES   EQU   X'02'               DESCRIPTION                                  
ERRVEN   EQU   X'01'               VENDOR                                       
*                                                                               
INERRMSK DC    AL1(ERRBUS+ERRDIV+ERRRES+ERREXT+ERREST+ERRBUD+ERRVEN)            
ESERRMSK DC    AL1(ERRBUS+ERRDIV+ERRRES+ERREXT+ERREST+ERRBUD+ERRVEN)            
*                                                                               
BILLYRMN DS    XL1                 REQUEST BILL YEAR/MONTH                      
BILLYR   DS    XL1                 REQUEST BILL YEAR                            
BILLPER  DS    CL6                 REQUEST BILL YEAR/MONTH                      
ATTDATE  DS    CL6                 AT&T ESTIMATE DATE                           
*                                                                               
ZEROS    DC    CL8'00000000'                                                    
SEP      EQU   X'7C'               FIELD SEPERATOR                              
*                                                                               
INVFILE  DCB   DDNAME=INVFILE,DSORG=PS,LRECL=150,BLKSIZE=1500,         X        
               MACRF=(GM,PM),RECFM=FB                                           
*                                                                               
ESTFILE  DCB   DDNAME=ESTFILE,DSORG=PS,LRECL=132,BLKSIZE=1320,         X        
               MACRF=(GM,PM),RECFM=FB                                           
         EJECT                                                                  
INREC    DS    0CL150              INVOICE RECORD                               
INTYPE   DC    CL1'I'                                                           
         DC    AL1(SEP)                                                         
INYEAR   DS    CL4                                                              
         DC    AL1(SEP)                                                         
INVENDOR DS    CL4                                                              
         DC    AL1(SEP)                                                         
INVNO    DS    CL12                                                             
         DC    AL1(SEP)                                                         
INDATE   DS    CL8                                                              
         DC    AL1(SEP)                                                         
         DC    CL1'N'                                                           
         DC    AL1(SEP)                                                         
INPAYDUE DS    CL8                                                              
         DC    AL1(SEP)                                                         
INESTNO  DS    CL8                                                              
         DC    AL1(SEP)                                                         
INAMT    DS    CL12                                                             
         DC    AL1(SEP)                                                         
INBUS    DS    CL2                                                              
         DC    AL1(SEP)                                                         
INDIV    DS    CL2                                                              
         DC    AL1(SEP)                                                         
INRES    DS    CL9                                                              
         DC    AL1(SEP)                                                         
INEXT    DS    CL5                                                              
         DC    AL1(SEP)                                                         
INEST    DS    CL5                                                              
         DC    AL1(SEP)                                                         
INBUD    DS    CL3                                                              
         DC    AL1(SEP)                                                         
INDES    DS    CL50                                                             
         DC    CL1'#'                                                           
         EJECT                                                                  
ESREC    DS    0CL132              ESTIMATE RECORD                              
ESTYPE   DC    CL1'E'                                                           
         DC    AL1(SEP)                                                         
ESYEAR   DS    CL4                                                              
         DC    AL1(SEP)                                                         
ESMONTH  DS    CL2                                                              
         DC    AL1(SEP)                                                         
ESVENDOR DS    CL4                                                              
         DC    AL1(SEP)                                                         
ESESTNO  DS    CL8                                                              
         DC    AL1(SEP)                                                         
         DC    CL1'N'                                                           
         DC    AL1(SEP)                                                         
ESDATE   DS    CL8                                                              
         DC    AL1(SEP)                                                         
ESAMT    DS    CL12                                                             
         DC    AL1(SEP)                                                         
ESBUS    DS    CL2                                                              
         DC    AL1(SEP)                                                         
ESDIV    DS    CL2                                                              
         DC    AL1(SEP)                                                         
ESRES    DS    CL9                                                              
         DC    AL1(SEP)                                                         
ESEXT    DS    CL5                                                              
         DC    AL1(SEP)                                                         
ESEST    DS    CL5                                                              
         DC    AL1(SEP)                                                         
ESBUD    DS    CL3                                                              
         DC    AL1(SEP)                                                         
ESDES    DS    CL50                                                             
         DC    CL1'#'                                                           
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*SPOTTABD                                                                       
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DMPRTQL                                                                        
*DDBUFFALOD                                                                     
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*SPGENPGEST                                                                     
*SPGENEST                                                                       
*SPGENBILL                                                                      
*SPWRIFFD                                                                       
         PRINT   OFF                                                            
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE SPOTTABD                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE SPGENPGEST                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIE8D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPWRI10   05/01/02'                                      
         END                                                                    
