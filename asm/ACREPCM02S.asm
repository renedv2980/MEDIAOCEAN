*          DATA SET ACREPCM02S AT LEVEL 028 AS OF 05/01/02                      
*PHASE ACCM02A,+0                                                               
*INCLUDE ACSLRY                                                                 
*INCLUDE ACSALHST                                                               
*INCLUDE DATVAL                                                                 
*INCLUDE COVAIL                                                                 
*INCLUDE PERCALL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE UNDERLIN                                                               
*INCLUDE RIGHT                                                                  
         TITLE 'ACCM - MISSING TIMESHEETS'                                      
*                                                                               
***************************************************                             
*        REQUEST OPTIONS INCLUDE:                                               
*                                                                               
*        QOPT1 - N EMPLOYEES IN NUMBER ORDER WITHIN                             
*                  DEPT (DEFAULT IS NAME ORDER)                                 
*                                                                               
*        QOPT6 - X DUMP OUT TIME RECORDS (TESTING)                              
*                                                                               
*        QOPT7 - INCLUDE FLAGS? (DEFAULT IS SHOW ALL)                           
*                N - SUPPRESS ALL FLAGGED WEEKS.                                
*                O - SHOW ONLY FLAGGED WEEKS.                                   
*                H - INCLUDE HIRE/TERM FLAGGED WEEKS.                           
*                Z - INCLUDE ZEROED OUT WEEKS.                                  
*                                                                               
***************************************************                             
*                                                                               
ACCM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCM**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACCMD,RC                                                         
         EJECT                                                                  
*--------------------------------------------                                   
*        R U N F I R S T                                                        
*--------------------------------------------                                   
*                                                                               
RUNF00   CLI   MODE,RUNFRST                                                     
         BNE   REQF00                                                           
         LA    RE,RELOTAB                                                       
         LA    R1,ATYPES           RELOCATE A-TYPES                             
RNF01    L     RF,0(RE)                                                         
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RNF01                                                            
*                                                                               
         SR    R0,R0               GET MAIN STORAGE                             
         L     R4,AMAINTAB                                                      
RNF03    CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    RNF05                                                            
         A     R0,0(R4)            ADD THE LENGTH OF EACH TABLE                 
         LA    R4,L'MAINTAB(R4)                                                 
         B     RNF03                                                            
*                                                                               
RNF05    ST    R0,MAINLEN          SAVE LENGTH OF TABLE                         
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,MAINBGN          START OF AREA                                
         L     R4,AMAINTAB                                                      
*                                                                               
RNF07    L     R3,4(R4)                                                         
         ST    R1,0(R3)            A(START OF THIS TABLE)                       
         L     R0,0(R4)            LENGTH OF THIS TABLE                         
         AR    R1,R0               R1 TO NEXT AREA                              
         LA    R4,L'MAINTAB(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   RNF07                                                            
*                                                                               
         L     RF,MAINBGN          START OF AREA                                
         L     RE,ADMASTC                                                       
         USING MASTD,RE                                                         
         STCM  RF,15,MCUSRDMP                                                   
         A     RF,MAINLEN          LENGTH OF AREA                               
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         L     RE,ASALAREA                                                      
         XC    0(SALLNQ,RE),0(RE)  CLEAR SALARY AREA                            
         MVI   NEWCOST,C'N'        DO PERSON RECORDS EXIST?                     
         L     R4,APERAREA         I/O AREA                                     
         USING PERD,R4                                                          
         LA    RF,PERLNQ           CLEAR PERCALL BLOCK TO 00'S                  
         XCEFL PERD                ** NOTE THIS USES RE,RF,R1,R0**              
         MVC   PERDMGR,DATAMGR                                                  
         MVC   PERCOMP,RCCOMPFL    COMPANY                                      
         OI    PERFLAGS,PERRECCK                                                
         GOTO1 PERCALL,PERD                                                     
         TM    PERERR,PERNOREC                                                  
         BO    *+8                                                              
         MVI   NEWCOST,C'Y'                                                     
         XC    PERFLAGS,PERFLAGS                                                
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*-------------------------------------------                                    
*        R E Q F I R S T                                                        
*-------------------------------------------                                    
*                                                                               
*                                                                               
REQF00   CLI   MODE,REQFRST                                                     
         BNE   LDG00                                                            
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDTRNS,C'Y'       READ TRANSACTIONS (DEFAULT)                  
         MVC   PROFS(PROFLEN),PROGPROF                                          
         CLI   PRFBREAK,C' '       PAGE BREAK SET?                              
         BH    *+8                                                              
         MVI   PRFBREAK,C'3'       PAGE BREAK 3 AS DEFAULT                      
         CLI   QOPT1,C'P'          CLEAR PAGE BREAK IF PERSON REPORT            
         BNE   *+8                                                              
         MVI   PRFBREAK,C' '                                                    
         L     RF,APROGTAB         RCSUBPRG TABLE                               
         USING PRGD,RF                                                          
REQF01   CLI   PRGOPT7,EOT                                                      
         BE    REQF01B                                                          
         CLC   PRGOPT7,QOPT7        BASED ON QOPT7                              
         BE    REQF01A                                                          
         LA    RF,PRGLEN(RF)                                                    
         B     REQF01                                                           
REQF01A  MVC   RCSUBPRG,PRGVALU     SET RCSUBPRG VALUE                          
*                                                                               
REQF01B  MVC   FIRSTWK,=C'01'       DEFAULT WEEK NUMBER FILTERS                 
         LA    R1,WEEKMAX                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LASTWK,DUB                                                       
*                                                                               
         CLC   QSELECT,SPACES       WERE WEEK NUMBERS REQUESTED?                
         BNH   REQF02                                                           
*                                                                               
         CLC   QSELECT(L'FIRSTWK),QSELECT+L'FIRSTWK  1ST CAN'T > LAST           
         BNH   *+6                                                              
         DC    H'0'                                                             
         CLC   QSELECT(L'FIRSTWK),FIRSTWK                                       
         BNL   *+6                               1ST CAN'T < C'01'              
         DC    H'0'                                                             
         CLC   QSELECT(L'FIRSTWK),LASTWK         OR HIGHER THAN WEEKMAX         
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   FIRSTWK,QSELECT                                                  
         MVC   LASTWK,QSELECT+L'FIRSTWK                                         
*                                                                               
REQF02   XC    DATES(DATLEN),DATES                                              
         MVC   MOSEND,=X'FFFFFF'                                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAY)  TODAY YMD(PACKED)              
         CLC   QSTART,SPACES                                                    
         BE    REQF04                                                           
         MVC   WKDT,QSTART                                                      
         CLC   WKDT+4(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   WKDT+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WKDT),(1,START)                                   
         XC    MOSSTART,MOSSTART                                                
         CLC   QSTART+4(2),SPACES                                               
         BNE   *+10                                                             
         MVC   MOSSTART,START                                                   
REQF04   CLC   QEND,SPACES                                                      
         BE    REQF06                                                           
         MVC   WKDT,QEND                                                        
         CLC   WKDT+4(2),SPACES                                                 
         BNE   REQF05A                                                          
         MVC   WKDT+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WKDT),(0,WKDT),(1,0) LAST DAY OF MO           
REQF05A  DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WKDT),(1,END)                                     
         CLC   QEND+4(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   MOSEND,END           MOS END                                     
*                                                                               
REQF06   MVC   FISCALS,START        GET YEAR FROM START DATE OR TODAY           
         OC    START,START                                                      
         BNZ   *+10                                                             
         MVC   FISCALS,TODAY                                                    
         MVI   FISCALS+1,1          DEFAULT MONTH IS JAN                        
         MVI   FISCALS+2,1          DAY IS ALWAYS 01                            
*                                                                               
         L     R2,ADCMPEL                                                       
         USING CPYELD,R2                                                        
         CLI   CPYSFST,0           WAS A STARTING MTH SPECIFIED/                
         BE    REQF07              NO, LEAVE AS  DEFAULT                        
         MVC   WORK(1),CPYSFST     YES - ISOLATE IT                             
         NI    WORK,X'0F'                                                       
         SR    R1,R1                                                            
         IC    R1,WORK                                                          
         TM    CPYSFST,X'F0'       WAS IT A NUMBER?                             
         BO    *+8                                                              
         LA    R1,15(0,R1)         NO, ADD CONVERSION FACTOR                    
         STC   R1,FISCALS+1        UPDATE FIELD WITH MTH                        
*                                                                               
REQF07   TM    CPYSTAT7,CPYSTMSY   COMPANY ON TMS?                              
         BZ    *+8                                                              
         OI    CMPSTAT,CMPONTMS    YES                                          
*                                                                               
REQF10   MVI   WEND,SUN            DEFAULT END OF THE WEEK IS SUNDAY            
         CLI   CPYTSD,0            SPECIFIED ON COMPANY RECORD?                 
         BE    *+10                                                             
         MVC   WEND,CPYTSD                                                      
         OC    PRFWKEND,PRFWKEND   PROFILE OVERRIDE FOR WEEK END DAY?           
         BZ    *+10                                                             
         MVC   WEND,PRFWKEND       YES - THEN USE IT                            
         XC    WKDT,WKDT                                                        
         MVC   WKDT(L'TODAY),TODAY                                              
         OC    START,START                                                      
         BZ    *+10                                                             
         MVC   WKDT(L'START),START                                              
         CLC   FISCALS,WKDT        HOW DOES IT COMPARE WITH START?              
         BNH   REQF12              EQUAL OR LOW IS OK                           
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(1,FISCALS),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+10,F'-1'  BACK UP 1 YEAR             
         XC    WORK(5),WORK                                                     
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,WORK)                                 
         MVC   FISCALS(1),WORK                                                  
*                                                                               
REQF12   MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(1,FISCALS),(0,FISCALS2) EBCDIC FOR ADDAY            
         LA    R3,364                                                           
         GOTO1 ADDAY,DMCB,FISCALS2,WKDT,(R3)                                    
         MVC   WKDT+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WKDT),(0,WKDT),(1,0)                          
*                                                                               
REQF20   MVC   FISCALE2,WKDT                      FISCAL END EBCDIC             
         GOTO1 DATCON,DMCB,(0,FISCALE2),(1,FISCALE)      AND PACKED             
*                                                                               
         MVI   CALENDAR,C'N'             SET TO NOT USING CALENDAR REC          
         BAS   RE,BLDCAL                 GO BUILD CALENDAR TABLE                
         OC    START,START               USE RANGE START IF NO QSTART           
         BNZ   REQF24                                                           
         MVC   START,RSTART                                                     
         MVC   END,REND                  THEN REPLACE QSTART AND QEND           
         GOTO1 DATCON,DMCB,(1,START),(0,QSTART)                                 
         GOTO1 (RF),(R1),(1,END),(0,QEND)                                       
*                                                                               
REQF24   DS    0H                                                               
*        MVC   SHSTART,RSTART                                                   
         MVI   SHSTART+2,01                                                     
         MVC   WKDT,QEND                                                        
         MVC   WKDT+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WKDT),(0,WKDT),(1,0)                          
         GOTO1 DATCON,DMCB,(0,WKDT),(1,SHEND)                                   
         MVC   STEND(2),START            STEND IS FOR ACSALARY                  
         MVC   STEND+2(2),END                                                   
         MVC   RANGE+15(L'FIRSTNBR),FIRSTNBR  TIMESHEET NUMBER RANGE            
         MVC   RANGE+26(L'LASTNBR),LASTNBR                                      
*                                                                               
*                                        INIT FOR SORT **                       
         LA    R1,SRTKLEN                SORT KEY LENGTH                        
         CVD   R1,DUB                    CONVERT KEY LEN TO CHARS               
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRTLEN                 SORT RECORD LENGTH                     
         CVD   R1,DUB                    CONVERT REC LEN TO CHARS               
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
*                                                                               
         LA    R0,TOTCNT2                CLEAR REPORT TOTALS                    
         LA    R1,RTOTS                                                         
         ZAP   0(RTOTLN,R1),ZEROS                                               
         LA    R1,RTOTLN(R1)                                                    
         BCT   R0,*-10                                                          
*                                                                               
         MVC   QPERCODE,SPACES                                                  
         CLI   NEWCOST,C'Y'                                                     
         BNE   XIT                                                              
         USING ACQD,RE                                                          
         L     RE,ADQSTACK                                                      
         CLI   ACQTYP1,ACQPRSN                                                  
         BNE   REQF30                                                           
         CLC   ACQFLT1,SPACES      PERSON CODE FILTER?                          
         BNH   REQF30                                                           
         MVC   QPERCODE,ACQFLT1                                                 
         DROP  RE                                                               
*                                                                               
         USING PERD,R4                                                          
REQF30   L     R4,APERAREA                                                      
         LA    RF,PERLNQ           CLEAR PERCALL BLOCK TO 00'S                  
         XCEFL PERD                ** NOTE THIS USES RE,RF,R1,R0**              
         MVC   PERDMGR,DATAMGR                                                  
         MVC   PERCOMP,RCCOMPFL    COMPANY                                      
         MVC   PERSTD,START        START OF PERIOD                              
         MVC   PERENDD,END         END OF PERIOD                                
         OI    PERFLAGS,PEROVER    OVERRIDE LOCATION END WITH TS LOCK           
         L     RF,ASALAREA                                                      
         USING SALARYD,RF                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(8),SALBLOCK+8   ADDR AND LENGTH OF ACQUIRED BUFFER          
         XC    SALBLOCK(SALLNQ),SALBLOCK                                        
         MVC   SALBLOCK+8(8),WORK   RESTORE ADDR/LEN OF BUFFER                  
         MVC   SALCMPY,RCCOMPFL                                                 
         MVI   SALMETHD,C'1'        JUST USE DEFAULT METHOD 1                   
         MVC   SALSTART,SHSTART                                                 
         MVC   SALEND,SHEND                                                     
         MVC   SALACOVL,COVAIL                                                  
         LA    R0,SALACTM#         NUMBER OF MONTHLY HRLY ACCUMS                
         LA    R1,SALACTM1                                                      
         ZAP   0(L'SALACTM1,R1),ZEROS                                           
         LA    R1,L'SALACTM1(R1)                                                
         BCT   R0,*-10                                                          
         B     XIT                                                              
         DROP  R4,RF                                                            
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------                          
*              F I R S T  F O R  L E D G E R                                    
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LDG00    CLI   MODE,LEDGFRST                                                    
         BNE   LVA00                                                            
         L     RF,ADLDGHIR                                                      
         SR    R2,R2                       NUMBER OF ACTUAL LEVELS              
         LA    R2,1(R2)                    AT LEAST ONE LEVEL                   
         USING ACLELD,RF                                                        
         MVC   LLEVA(LLEVELLN),ACLVALS     LEVEL LENGTHS/NAMES                  
         LA    R3,LENLEVLS                 INDIVIDUAL LENGTHS OF LEVELS         
         SR    R4,R4                                                            
         LA    R1,LLEVA                    COMBINED LEVEL LENGTHS               
         LA    R0,LLEVLNUM                 MAXIMUM NUMBER OF LEVELS             
LDG02    DS    0H                                                               
         ZIC   R5,0(R1)                    PREVIOUS COMBINED LENGTH             
         SR    R5,R4                       MINUS NEW COMBINED LENGTH            
         BP    *+6                         EQUALS INDIVIDUAL LEVEL LEN          
         DC    H'0'                                                             
         STC   R5,0(R3)                    SAVE INDIVD LENGTH OF LEVEL          
         CLI   0(R1),MAXLEN                LAST LEV HAS MAXLEN FOR ACCT         
         BE    LDG04                                                            
         LA    R2,1(R2)                    ADD TO LEVEL COUNT                   
         ZIC   R4,0(R1)                    COMBINED LENGTH IN R4                
         LA    R1,LLEVALN(R1)              BUMP TO NEXT COMBINED LENGTH         
         LA    R3,L'LENLEVA(R3)            NEXT INDIVDUAL LEN SAVE AREA         
         BCT   R0,LDG02                                                         
         DC    H'0'                                                             
LDG04    STC   R2,NUMLEVLS                 ACTUAL NUMBER OF LEVELS              
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
*-----------------------------------------------------                          
*              L E V A F I R S T                                                
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LVA00    CLI   MODE,LEVAFRST                                                    
         BNE   LVB00                                                            
*                                  1R HIGHER LEVEL CODE,NAME                    
*                                                                               
         GOTO1 HINAME,DMCB,0,ADLVANAM,ADHEIRA                                   
*                                                                               
         L     RE,ASALAREA                                                      
         USING SALARYD,RE                                                       
         MVC   SALOFFC,SPACES                                                   
         L     R2,ADHEIRA                                                       
         SR    R1,R1                                                            
         IC    R1,LLEVA            LENGTH OF OFFICE                             
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   SALOFFC(0),3(R2)     SAVE OFFICE FOR SALHST                      
         MVC   WKOFFICE,SALOFFC     SAVE OFFICE FOR CALENDAR LOOKUP             
         BAS   RE,BLDCAL            LOOKUP OFFICE CALENDAR                      
         OC    START,START          USE RANGE START IF NO QSTART                
         BNZ   XIT                                                              
         MVC   START,RSTART                                                     
         MVC   END,REND                                                         
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*-----------------------------------------------------                          
*              L E V B F I R S T                                                
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LVB00    CLI   MODE,LEVBFRST                                                    
         BNE   LVC00                                                            
         MVC   LOCATION,SPACES     CLEAR THE LOCATION                           
*                                  1R HIGHER LEVEL CODE,NAME,ADDRESS            
         GOTO1 HINAME,DMCB,ADLVBADD,ADLVBNAM,ADHEIRB                            
         L     RE,ASALAREA                                                      
         USING SALARYD,RE                                                       
         MVC   SALDEPT,SPACES                                                   
         L     R2,ADHEIRB                                                       
         LA    R2,3(R2)                                                         
         SR    R1,R1                                                            
         IC    R1,LLEVA            LENGTH OF OFFICE                             
         AR    R2,R1               R2 NOW POINTS AT DEPT                        
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF DEPT                               
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   SALDEPT(0),0(R2)     SAVE DEPT FOR PERCALL                       
         DROP  RE                                                               
*                                                                               
         OC    PRFFTVAL,PRFFTVAL                                                
         BZ    XIT                                                              
         OC    PROFFILT,PROFFILT                                                
         BZ    XIT                                                              
         MVI   FILTVAL,SPACE                                                    
         XC    FILTDISP,FILTDISP                                                
         L     R2,AFILTAB          TABLE TO FIGURE ATTN FILTER                  
         USING FRCFLTD,R2                                                       
LVB02    CLI   FRCFLT,EOT          END OF TABLE                                 
         BE    XIT                                                              
         CLC   PROFFILT,FRCFLT     FILTER1 OR FILTER2 OR.......                 
         BE    LVB04                                                            
         LA    R2,FRCLEN(R2)                                                    
         B     LVB02                                                            
*                                                                               
LVB04    MVC   FILTVAL,PRFFTVAL    SAVE FILTER VALUE                            
         MVC   FILTDISP,FRCDISP    AND DISPLACEMENT                             
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              L E V C F I R S T                                                
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LVC00    CLI   MODE,LEVCFRST                                                    
         BNE   ACF00                                                            
*                                                                               
         CLI   PROFATIM,C'Y'       ARE WE INCLUDING "A"TIME?                    
         BE    *+10                                                             
         XC    PRFAMISS,PRFAMISS   NO - THEN THIS CAN'T BE SET TO YES           
*                                                                               
         USING ACTRECD,R7                                                       
         L     R7,ADHEIRC          ADDR OF RECORD                               
*                                  1R HIGHER LEVEL CODE,NAME,ADDRESS            
*                                                                               
         GOTO1 HINAME,DMCB,ADLVCADD,ADLVCNAM,ADHEIRC                            
*                                                                               
         L     RE,ASALAREA                                                      
         USING SALARYD,RE                                                       
         MVC   SALSDPT,SPACES                                                   
         L     R2,ADHEIRC                                                       
         LA    R2,3(R2)                                                         
         SR    R1,R1                                                            
         IC    R1,LLEVB            LENGTH OF OFFICE/DEPT                        
         AR    R2,R1               R2 NOW POINTS AT SUBDEPT                     
         SR    R1,R1                                                            
         IC    R1,LENLEVC          LENGTH OF SUBDEPT                            
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   SALSDPT(0),0(R2)     SAVE SUBDEPT                                
         DROP  RE                                                               
*                                                                               
         L     R6,ASRTWRK          SORT WORK AREA                               
         USING SRTD,R6             CLEAR IT                                     
         MVC   SRTKEY(SRTKLEN),SPACES                                           
         ZIC   R1,LLEVC            LENGTH OF LEVEL A+B+C                        
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EXMVC R1,SRTHI,ACTKACT    ALL 1R HIGHER LEVELS TO SORT KEY             
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              F I R S T  F O R  A C C O U N T                                  
*-----------------------------------------------------                          
*                                                                               
*                                                                               
ACF00    CLI   MODE,PROCACC                                                     
         BNE   PTRN00                                                           
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDTIME,C'N'                                                    
         L     RE,ASALAREA                                                      
         USING SALARYD,RE                                                       
         MVC   SALPRSN,SPACES                                                   
         L     R2,ADHEIRD                                                       
         LA    R2,3(R2)                                                         
         SR    R1,R1                                                            
         IC    R1,LLEVC            LENGTH OF OFFICE/DEPT/SUBDPT                 
         AR    R2,R1               R2 NOW POINTS AT PERSON                      
         SR    R1,R1                                                            
         IC    R1,LENLEVD          LENGTH OF PERSON                             
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   SALPRSN(0),0(R2)    SAVE PERSON FOR PERCALL                      
*                                                                               
         GOTO1 =A(EMPCHEK),DMCB,(RC)                                            
         BNE   XIT                 SUPPRESS EMPLOYEE IF NO CC                   
*                                                                               
         L     R7,ADACC                                                         
         USING ACTRECD,R7                                                       
         L     R6,ASRTWRK          SORT WORK AREA                               
         USING SRTD,R6                                                          
*                                  CLEAR SRT AREA TO BINARY ZEROS               
         LA    RE,SRTNUMB          RECEIVING FIELD                              
         LH    RF,=Y(SRTXCLEN)     RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         MVC   SRTEMP(SRTEMLN),SPACES                                           
         ZAP   SRTNUMBM,ZEROS                                                   
*                                                                               
         L     RE,ASALAREA                                                      
         USING SALARYD,RE                                                       
         MVC   SRTEMP,SALPRSN      EMPLOYEE CODE TO SORT KEY                    
         MVC   SRTEMPL,SRTEMP      PRIMARY AND SECONDARY                        
         MVC   SRTHIRE,HIRE        HIRE DATE                                    
         MVC   SRTTERM,FIRE        TERMINATION DATE                             
         CLI   QOPT1,C'P'          SORT BY PERSON CODE HIGH                     
         BNE   *+10                                                             
         MVC   SRTEMPS,SRTEMP                                                   
         CLI   QOPT1,C'C'          EMPLOYEE CODE WITHIN DEPT                    
         BE    *+10                YES -                                        
         MVC   SRTEMP,SPACES       NO  - ALPHA ORDER BY EMPL NAME               
*                                                                               
         L     R2,ADACCNAM                                                      
         CLI   0(R2),NAMELQ        NAME ELEMENT                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 BLDTNAM,DMCB,ADACCNAM                                            
         MVC   SRTNAM,WORK         NAME PASSED BACK IN WORK                     
*                                                                               
         MVI   FCRDTRNS,C'Y'       ALWAYS READ TRANSACTIONS                     
         TM    CMPSTAT,CMPONTMS    COMPANY ON TMS?                              
         BZ    *+8                                                              
         MVI   FCRDTIME,C'Y'       READ TMS TOO                                 
         OC    PROFFILT,PROFFILT   IS THERE A DEPT ATTENTION FILTER             
         BZ    *+8                                                              
         BAS   RE,ATTENT           CHECK ATTENTION FILTER                       
         BAS   RE,CLRTAB           CLEAR STATUS AND ACCUMS IN WEEK TAB          
         B     XIT                                                              
         DROP  R2,R6,R7,RE                                                      
         EJECT                                                                  
*-----------------------------------------------------                          
*              R E A D  T R A N S A C T I O N S                                 
*-----------------------------------------------------                          
*                                                                               
*                                                                               
PTRN00   CLI   MODE,PROCTRNS                                                    
         BNE   PTIM00                                                           
         L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         CLI   TRNEL,TRNELQ        MAKE SURE IT'S A TRANSACTION                 
         BNE   XIT                                                              
         CLC   TRNDATE,START                                                    
         BL    XIT                 IGNORE OLD ONES                              
         CLC   TRNDATE,END                                                      
         BH    XIT                 IGNORE AFTER END DATE OF REQUEST             
         MVC   WKDT(L'TRNDATE),TRNDATE                                          
         MVI   ELCODE,SCIELQ       SUBSIDIARY ELEMENT                           
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING SCIELD,R4                                                        
         CLI   SCITYPE,SCITHOUR    IS THIS AN HOURS TRANSACTION                 
         BNE   XIT                                                              
         ZAP   HOURS,SCIAMNT                                                    
         MVI   ELCODE,TRSELQ       TRANSACTION STATUS ELEMENT                   
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING TRSELD,R4                                                        
         CLI   PROFATIM,C'Y'       INCLUDE "A" TIME (ADJUSTED TIME)             
         BE    *+12                                                             
         CLI   TRSSTAT2,TRSSTADJ   IGNORE ADJUSTMENTS                           
         BE    XIT                                                              
         MVC   STAT,TRSSTAT2       SAVE STATUS                                  
         OC    STAT,STAT                                                        
         BNZ   *+8                                                              
         OI    STAT,TRSSTIME       IF ZERO, USE "T" TIME AS DEFAULT             
         BAS   RE,HITIT            SLOT THE TRANSACTION IN TABLE                
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              R E A D   T I M E   R E C O R D S                                
*-----------------------------------------------------                          
*                                                                               
*                                                                               
PTIM00   CLI   MODE,PROCTIME                                                    
         BNE   ACL00                                                            
*                                                                               
         L     R4,ADTRANS                                                       
         USING TIMELD,R4                                                        
         CLI   TIMEL,TIMELQ        MAKE SURE IT'S A TIME ELEMENT                
         BNE   XIT                                                              
         LR    RF,R4                                                            
         SH    RF,DATADISP                                                      
*                                                                               
         L     R7,AMONACC                                                       
         USING ACMD,R7                                                          
         L     R7,ACMALTN          GET ADDRESS OF TIME RECORD                   
         USING TIMRECD,R7                                                       
         CLC   TIMKPEDT,START                                                   
         BL    XIT                 IGNORE OLD ONES                              
         CLC   TIMKPEDT,END                                                     
         BH    XIT                 IGNORE AFTER END DATE OF REQUEST             
*                                                                               
         CLI   QOPT6,C'X'          DUMP RECORDS, FOR TEST PURPOSES              
         BNE   PTIM10                                                           
         GOTO1 =V(PRNTBL),DMCB,=C'REC',(R7),C'DUMP',320,=C'1D'                  
*                                                                               
PTIM10   MVC   WKDT(L'TIMKPEDT),TIMKPEDT                                        
         ZAP   HOURS,TIMHRS        # OF HOURS                                   
         MVI   STAT,TRSSTIME       USE "T" TIME AS DEFAULT                      
         TM    TIMIND,TIMIADJ      IGNORE ADJUSTMENTS                           
         BZ    *+8                                                              
         MVI   STAT,TRSSTADJ       SET TO ADJUST                                
         CLI   PROFATIM,C'Y'       INCLUDE "A" TIME (ADJUSTED TIME)             
         BE    *+12                                                             
         TM    TIMIND,TIMIADJ      IGNORE ADJUSTMENTS                           
         BO    XIT                                                              
         BAS   RE,HITIT            SLOT THE TRANSACTION IN TABLE                
         B     XIT                                                              
*                                                                               
         DROP  R7                                                               
         EJECT                                                                  
*-----------------------------------------------------                          
*              L A S T  F O R  A C C O U N T                                    
*-----------------------------------------------------                          
*                                                                               
*                                                                               
ACL00    CLI   MODE,ACCLAST                                                     
         BNE   RQL00                                                            
         BAS   RE,STAXUP           STACK THIS EMPL'S TS INFO IN SRT REC         
         L     R6,ASRTWRK          SORT WORK AREA                               
         USING SRTD,R6                                                          
         OC    SRTNUMB,SRTNUMB     ANY WEEKS IN SORT RECORD                     
         BZ    XIT                                                              
         CLC   SRTEMPL,SPACES                                                   
         BE    XIT                                                              
         GOTO1 ADSORTER,DMCB,=C'PUT',(R6)      PUT TO SORT                      
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              L A S T  F O R  R E Q L A S T                                    
*-----------------------------------------------------                          
*                                                                               
*                                                                               
RQL00    CLI   MODE,REQLAST                                                     
         BNE   RUL00                                                            
         NI    FLAG,X'FF'-DATEDONE    HIRE/TERM DATE FLAG                       
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   OFDPSB(LEVSAV),SPACES  CLEAR CODE/NAME SAVE AREA                 
         MVC   PERCODE,SPACES         CLEAR SAVED PESON                         
RQL02    DS    0H                                                               
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R2,4(R1)                                                         
         LTR   R2,R2                                                            
         BZ    RQL20                                                            
         L     R6,ASRTWRK                  COPY THE RECORD TO WORK AREA         
         LA    R7,SRTLEN                   R6 ADDR OF RECEIVE/(R7) LEN          
         LR    R3,R7                       R2 ADDR SENDING/(R3) LEN             
         MVCL  R6,R2                                                            
*                                                                               
         L     R6,ASRTWRK                                                       
         USING SRTD,R6                                                          
         BAS   RE,TOTAL                    TOTALS IF NECESSARY                  
         BAS   RE,HIGHUP                   HEADUP A NEW PAGE                    
         LA    R5,P                                                             
         MVC   P,SPACES                                                         
         USING PRNTD,R5                                                         
         MVC   PNAME,SRTNAM                EMPLOYEE NAME                        
         MVC   PCODE(L'SRTEMPL),SRTEMPL    CODE                                 
         CLI   QOPT1,C'P'                  PERSON ORDER REPORT?                 
         BNE   RQL04                                                            
         MVC   PLEVELS,SPACES                                                   
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'LEVCODES),LEVCODES                                        
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         MVC   PLEVELS,WORK                                                     
         GOTO1 RIGHT,DMCB,PLEVELS,L'PLEVELS                                     
RQL04    DS    0H                                                               
         CURED SRTNUMBM,(L'PMISS,PMISS),0  NUMBER OF MISSING TS                 
         BAS   RE,TOTUP                    ADD TOTALS TO HIGHER LEVELS          
*                                                                               
         LH    R1,SRTNUMB                  NUMBER OF SRT ENTRIES                
         LTR   R1,R1                                                            
         BNP   RQL02                       IF NO SRTENTRIES GET NEXT            
         SR    R2,R2                                                            
         LA    R4,SRTENTRY                 1ST WEEKLY ENTRY IN SORT REC         
         USING SRTENTRY,R4                                                      
*                                                                               
RQL05    LA    R5,P                                                             
         LA    R5,PENTRY                   1ST WEEKLY ENTRY ON P LINE           
         LA    R3,PENTCNT                  # OF WEEKS ON A PRINTLINE            
*                                                                               
RQL06    ST    R4,ALTAB                    ADDR OF SRTENTRY                     
         CH    R2,SRTNUMB                                                       
         BL    *+16                                                             
         OI    FLAG,SRTDONE                                                     
         BAS   RE,PRNTIT                                                        
         B     RQL07A                      FINISHED WITH SORT REC               
         BAS   RE,WEEKIT                   WEEK DATES AND STATUS FLAGS          
         MVC   PSTAT-PENTRY(L'PSTAT,R5),STATUS                                  
         MVC   PWKDAT-PENTRY(L'PWKDAT,R5),PERIOD                                
         MVC   PWKNUM-PENTRY(L'PWKNUM,R5),SRTWKNUM                              
         CLI   PWKNUM-PENTRY(R5),C'0'                                           
         BNE   *+8                                                              
         MVI   PWKNUM-PENTRY(R5),C' '                                           
*                                                                               
         LA    R4,SRTELN(R4)               BUMP NEXT WEEK IN SORT REC           
         LA    R5,PENTRYLN(R5)             BUMP NEXT PRINTLINE POSITION         
         LA    R2,1(R2)                    BUMP WEEK COUNT IN SORT REC          
*                                                                               
*                                                                               
RQL07    BCT   R3,RQL06                                                         
         BAS   RE,PRNTIT                                                        
*                                                                               
RQL07A   MVC   P,SPACES                                                         
         CLI   PRFHRTRD,C'Y'                                                    
         BNE   RQL07B                                                           
         TM    FLAG,DATEDONE       HAVE THE HIRE/TERM DATES BEEN PRNTED         
         BO    RQL07B                                                           
         LA    R5,P                                                             
         MVC   P+2(5),=C'HIRED'                                                 
         GOTO1 DATCON,DMCB,(1,SRTHIRE),(5,PHIRE)                                
         CLC   SRTTERM,=X'FFFFFF'        IS THERE A TERMINATION DATE?           
         BE    RQL07B                    NO                                     
         MVC   P+18(4),=C'TERM'                                                 
         GOTO1 DATCON,DMCB,(1,SRTTERM),(5,PTERM)                                
*                                                                               
RQL07B   OI    FLAG,DATEDONE       HIRE/TERM DATES HAVE BEEN PRINTED            
         TM    FLAG,SRTDONE        IS THE SORTING DONE?                         
         BO    RQL08                                                            
         B     RQL05                       NEXT SUB ENTRY IN SRT REC            
*                                                                               
RQL08    NI    FLAG,X'FF'-DATEDONE                                              
         NI    FLAG,X'FF'-SRTDONE                                               
         CLC   P,SPACES                    ANYTHING LEFT TO PRINT?              
         BE    RQL10                       NO - GET NEXT EMPLOYEE               
         BAS   RE,PRNTIT                                                        
RQL10    BAS   RE,PRNTIT                   SKIP A LINE                          
         B     RQL02                       GET NEXT EMPLOYEE                    
*                                                                               
RQL20    MVC   OFDPSB,SPACES               DO REQUEST TOTAL                     
         BAS   RE,TOTAL                                                         
         B     XIT                                                              
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
*------------------------------------------------                               
*              L A S T  F O R  R U N                                            
*------------------------------------------------                               
*                                                                               
*                                                                               
RUL00    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         LM    R0,R1,MAINLEN                                                    
         FREEMAIN R,LV=(0),A=(1)                                                
         L     RE,ADMASTC                                                       
         USING MASTD,RE                                                         
         XC    MCUSRDMP,MCUSRDMP    CLEAR EXTRA DUMP AREA                       
         DROP  RE                                                               
XIT      XMOD1 1                                                                
         EJECT                                                                  
*************************************************************                   
*              BUILD WEEKLY DATE TABLE FROM CONTROL PROFILE                     
*************************************************************                   
*                                                                               
BLDWEEK  NTR1                                                                   
         MVI   CALENDAR,C'N'       SET SWITCH TO NOT USING CALENDAR REC         
         OC    MOSSTART,MOSSTART   REQUESTED BY MOS                             
         BZ    BLDW00              NO  - CONTINUE                               
         MVC   START,MOSSTART      YES - RESET START AND END                    
         MVC   END,MOSEND                                                       
BLDW00   OC    START,START                                                      
         BNZ   BLDW01                                                           
         MVC   END,=X'FFFFFF'                                                   
*                                  CLEAR TABLE TO BINARY ZEROS                  
BLDW01   L     RE,AWEEKTAB         RECEIVING FIELD                              
         LH    RF,=S(L'WEEKTB1)    RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         MVC   FIRSTNBR,SPACES                CLEAR TS # RANGE                  
         MVC   LASTNBR,SPACES                                                   
         L     R5,AWEEKTAB                                                      
         USING WEEKTABD,R5                                                      
         SR    R4,R4                          WEEK NUMBER COUNTER               
         MVC   WEEKEND,FISCALS2               BEGIN WITH FISCAL START           
         MVC   WEEKSTRT,FISCALS2                                                
         MVC   WKST1,FISCALS2                 1ST TBL ENTRY STARTS WITH         
*                                             FISCAL START DATE                 
         LA    R6,53                                                            
         CLI   PRF52WKS,C'Y'                  LIMIT TO 52 WEEKS?                
         BNE   *+8                                                              
         LA    R6,52                                                            
         STH   R6,NUMBERW                                                       
*                                                                               
BLDW02   MVC   WORK,SPACES                                                      
         MVC   WORK+6(L'WEEKEND),WEEKEND                                        
         LA    R2,7                           LIMIT LOOP TO 7 ATTEMPTS          
         LA    R3,7                           END DATE IS 7 DAYS                
BLDW04   GOTO1 ADDAY,DMCB,WORK+6,WORK,(R3)                                      
         MVC   WKDT,SPACES                                                      
         GOTO1 GETDAY,DMCB,WORK,WKDT          IS IT WEEK ENDING DAY?            
         CLC   WKDT,SPACES                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   WEND,0(R1)                                                       
         BE    BLDW06                         YES - CONTINUE                    
         L     R3,=F'-1'                      NO - BACK 1 DAY TRY AGAIN         
         MVC   WORK+6(L'WEEKEND),WORK                                           
         BCT   R2,BLDW04                                                        
         DC    H'0'                                                             
*                                                                               
BLDW06   MVC   WKEND1,WORK                    END DATE EBCDIC VERSION           
         CLC   WKST1,FISCALS2                 1ST TIME THRU                     
         BE    BLDW08                         YES -START = FISCAL START         
         LA    R3,1                           NO - START IS PREVIOUS            
         GOTO1 ADDAY,DMCB,WEEKEND,WKST1,(R3)  WEEKEND PLUS ONE                  
*                                                                               
BLDW08   MVC   WEEKSTRT,WKST1                 REFRESH WORK DATES                
         MVC   WEEKEND,WKEND1                                                   
         GOTO1 DATCON,DMCB,(0,WKST1),(1,WKST2)         PACKED YMD START         
         GOTO1 (RF),(R1),(0,WKEND1),(1,WKEND2)         PACKED YMD END           
         GOTO1 (RF),(R1),(0,WKST1),(10,WKST3)          MM/DD/YY                 
         GOTO1 (RF),(R1),(0,WKEND1),(10,WKEND3)        MM/DD/YY                 
*                                                                               
         LA    R4,1(R4)                       INCREMENT WEEK #                  
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WKNUM,DUB                                                        
*                                                                               
         CLC   WKEND2,START                   CHECK START AND END DATES         
         BL    BLDW14                                                           
         CLC   WKST2,END                                                        
         BH    BLDW14                                                           
         CLC   WKNUM,FIRSTWK                  CHECK REQUESTED TS#'S             
         BL    BLDW14                                                           
         CLC   WKNUM,LASTWK                                                     
         BH    BLDW14                                                           
         OI    WKSTAT1,WKRQST                 MARK WEEK WITHIN REQUEST          
         CLC   FIRSTNBR,SPACES                                                  
         BNE   *+16                                                             
         MVC   FIRSTNBR,WKNUM                                                   
         MVC   RSTART,WKST2                   SAVE RANGE DATES FOR              
         MVC   LASTNBR,WKNUM                                                    
         MVC   REND,WKEND2                    HEADLINES IF NEEDED               
*                                                                               
         LA    R0,WKBKCNT                     CLEAR BUCKETS TO ZERO             
         LA    R1,WKBUKS                                                        
         ZAP   0(WKBUKLN,R1),ZEROS                                              
         LA    R1,WKBUKLN(R1)                                                   
         BCT   R0,*-10                                                          
BLDW14   ST    R5,ALTAB                       SAVE ADDR OF LAST ENTRY           
         LA    R5,WEEKLEN(R5)                                                   
         BCT   R6,BLDW02                                                        
*                                                                               
         CLI   PRFLASTW,C'Y'                  END WITH FISCAL END               
         BNE   BLDW20                                                           
         L     R5,ALTAB                       SAVE ADDR OF LAST ENTRY           
         CLC   REND,WKEND2                                                      
         BNE   *+10                                                             
         MVC   REND,FISCALE                                                     
         MVC   WKEND2,FISCALE                                                   
         MVC   WKEND1,FISCALE2                                                  
         GOTO1 DATCON,DMCB,(0,WKEND1),(10,WKEND3)        MM/DD/YY               
BLDW20   B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*              BUILD CALENDAR DATE TABLE                                        
*************************************************************                   
*                                                                               
BLDCAL   NTR1                                                                   
         MVC   AWEEKTAB,AWEEKTB1   SET TABLE TO COMPANY CALENDAR                
         CLI   MODE,REQFRST                                                     
         BNE   BLDC00                                                           
         XC    NUMBERW,NUMBERW                                                  
         MVC   WKOFFICE,SPACES     OFFICE TO SPACES                             
         OC    MOSSTART,MOSSTART   REQUESTED BY MOS                             
         BZ    BLDC00A             NO  - CONTINUE                               
         XC    START,START         YES - RESET START AND END                    
         MVC   END,=X'FFFFFF'                                                   
         B     BLDC00A                                                          
BLDC00   CLI   CALENDAR,C'Y'       IF DEFAULT CALENDAR WAS NEVER FOUND          
         BNE   XIT                 USE THE SELF-BUILT CAL FROM PROFILE          
         MVC   AWEEKTAB,AWEEKTB2   START WITH OFFICE CALENDAR                   
         MVC   START,START1        RESTORE START AND END DATES                  
         MVC   END,END1                                                         
BLDC00A  OC    START,START                                                      
         BNZ   *+10                                                             
         MVC   END,=X'FFFFFF'                                                   
*                                                                               
         XC    WKDT,WKDT           REQ START IF FOUND(FISCALS IF NOT)           
         MVC   WKDT(L'FISCALS),FISCALS                                          
         MVI   WKDT+2,7            THERE MAY BE A DIFFERENCE IN FISCAL          
*                                  START AND COST CAL START SO IF THEY          
*                                  REQUEST BY TIME SHEET #'S TRY THE            
*                                  SEVENTH INSTEAD OF THE FIRST AS DAY          
         OC    START,START                                                      
         BZ    *+10                                                             
         MVC   WKDT(L'START),START                                              
         L     R7,ACREC            LOOK FOR CALENDAR PASSIVE POINTER            
         USING CASRECD,R7                                                       
         XC    CASPAS,CASPAS                                                    
         MVI   CASPTYP,CASPTYPQ    X'3E0C'                                      
         MVI   CASPSUB,CASPSUBQ                                                 
         MVC   CASPCPY,RCCOMPFL                                                 
         MVC   CASPEDTE,WKDT                                                    
         MVC   CASPOFC,WKOFFICE                                                 
         MVC   SVKEY,CASPAS                                                     
         MVC   COMMAND,DMRDHI                                                   
         SR    R6,R6               CLEAR COUNT NUMBER OF PERIODS                
BLDC02   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,ACCDIR,ACREC,ACREC                          
         CLC   CASPAS(CASPEDTE-CASRECD),SVKEY                                   
         BNE   BLDC12                                                           
         CLC   CASPSDTE,WKDT       SEE IF START FALLS WITHIN REC DATES          
         BH    BLDC12                                                           
         CLC   CASPEDTE,WKDT                                                    
         BL    BLDC12                                                           
         CLC   END,=X'FFFFFF'                                                   
         BE    BLDC03                                                           
         CLC   CASPEDTE,END                                                     
         BNL   BLDC03                                                           
*        DC    H'0'                CAN'T REQUEST CROSS YEARS                    
         MVI   FCRDACC,C'N'                                                     
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDTIME,C'N'                                                    
         MVC   P+1(35),=CL35'************* E R R O R ***********'               
         MVC   PSECOND+1(35),=CL35'*REQUEST CROSSES COST CALENDAR YRS*'         
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
BLDC03   CLC   CASPOFC,WKOFFICE                                                 
         BE    BLDC04                                                           
         MVC   COMMAND,DMRSEQ                                                   
         B     BLDC02                                                           
*                                                                               
BLDC04   MVC   DA,CASPDA           DISK ADDRESS                                 
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,DA,ACREC,DMWORK                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  CLEAR TABLE TO BINARY ZEROS                  
         L     RE,AWEEKTAB         RECEIVING FIELD                              
         LH    RF,=S(L'WEEKTB1)    RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         SR    R6,R6               COUNT NUMBER OF PERIODS                      
         MVC   FIRSTNBR,SPACES     CLEAR TS # RANGE                             
         MVC   LASTNBR,SPACES                                                   
         L     R5,AWEEKTAB                                                      
         USING WEEKTABD,R5                                                      
         LA    R4,CASRFST          FIRST ELEMENT                                
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         USING TMPELD,R4                                                        
BLDC06   CLI   TMPEL,0                                                          
         BE    BLDC12                                                           
         CLI   TMPEL,TMPELQ        TIME SHEET PERIOD ELEMENT?                   
         BE    BLDC08                                                           
BLDC06A  SR    R1,R1                                                            
         IC    R1,TMPLN                                                         
         AR    R4,R1                                                            
         B     BLDC06                                                           
*                                                                               
BLDC08   SR    R1,R1                PERIOD NUMBER                               
         IC    R1,TMPNUMB                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WKNUM,DUB                                                        
*                                                                               
         MVC   WKST2,TMPSTART       PACKED START/END PERIOD                     
         MVC   WKEND2,TMPEND                                                    
         GOTO1 DATCON,DMCB,(1,WKST2),(0,WKST1)         EBCDIC YYMMDD            
         GOTO1 (RF),(R1),(1,WKEND2),(0,WKEND1)                                  
         GOTO1 (RF),(R1),(0,WKST1),(10,WKST3)          MM/DD/YY                 
         GOTO1 (RF),(R1),(0,WKEND1),(10,WKEND3)        MM/DD/YY                 
*                                                                               
         CLC   TMPMTH,MOSSTART                CHECK REQUESTED MOS               
         BL    BLDC10                                                           
         CLC   TMPMTH,MOSEND                                                    
         BH    BLDC10                                                           
         CLC   WKEND2,START                   CHECK START AND END DATES         
         BL    BLDC10                                                           
         CLC   WKST2,END                                                        
         BH    BLDC10                                                           
         CLC   WKNUM,FIRSTWK                  CHECK REQUESTED TS#'S             
         BL    BLDC10                                                           
         CLC   WKNUM,LASTWK                                                     
         BH    BLDC10                                                           
         OI    WKSTAT1,WKRQST                 MARK WEEK WITHIN REQUEST          
         CLC   FIRSTNBR,SPACES                                                  
         BNE   BLDC09                                                           
         MVC   FIRSTNBR,WKNUM                                                   
         MVC   RSTART,WKST2                   SAVE RANGE DATES FOR              
         MVC   SHSTART(L'TMPMTH),TMPMTH                                         
BLDC09   MVC   LASTNBR,WKNUM                                                    
         MVC   REND,WKEND2                    HEADLINES IF NEEDED               
BLDC10   LA    R0,WKBKCNT                     CLEAR BUCKETS TO ZERO             
         LA    R1,WKBUKS                                                        
         ZAP   0(WKBUKLN,R1),ZEROS                                              
         LA    R1,WKBUKLN(R1)                                                   
         BCT   R0,*-10                                                          
         LA    R6,1(R6)                       COUNT PERIOD NUMBERS              
         CH    R6,=Y(WEEKMAX)                                                   
         BNH   *+6                                                              
         DC    H'0'                           TOO MANY PERIODS                  
         LA    R5,WEEKLEN(R5)                                                   
         B     BLDC06A                                                          
*                                                                               
BLDC12   STH   R6,NUMBERW                                                       
         CLC   AWEEKTAB,AWEEKTB1             COMPANY CALENDAR                   
         BNE   BLDC16                                                           
         MVI   CALENDAR,C'Y'                                                    
         LTR   R6,R6                         IF NO COMP CALENDAR FOUND          
         BP    *+8                                                              
         BAS   RE,BLDWEEK                    BUILD FROM PROFILE                 
         OC    NUMBERW,NUMBERW               MUST HAVE ENTRIES                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   FIRSTNB1,FIRSTNBR             1ST PERIOD OF COMPANY              
         MVC   LASTNBR1,LASTNBR              LAST PERIOD OF COMPANY             
         MVC   NUMBER1,NUMBERW               # OF PERIODS OF COMPANY            
         MVC   START1,START                  START AND END DATES                
         MVC   END1,END                                                         
         MVC   RSTART1,RSTART                RANGE DATES                        
         MVC   REND1,REND                                                       
         B     BLDC18                                                           
*                                            OFFICE CALENDAR                    
BLDC16   CLC   AWEEKTAB,AWEEKTB2                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    NUMBERW,NUMBERW                                                  
         BNZ   BLDC18                        FOUND ONE, USE IT                  
         MVC   AWEEKTAB,AWEEKTB1             OTHERWISE RESET TO COMPANY         
         MVC   FIRSTNBR,FIRSTNB1                                                
         MVC   LASTNBR,LASTNBR1                                                 
         MVC   NUMBERW,NUMBER1                                                  
         MVC   START,START1                                                     
         MVC   END,END1                                                         
         MVC   RSTART,RSTART1                                                   
         MVC   REND,REND1                                                       
*                                                                               
BLDC18   MVC   RANGE+15(L'FIRSTNBR),FIRSTNBR  TIMESHEET NUMBER RANGE            
         MVC   RANGE+26(L'LASTNBR),LASTNBR    FOR HEADLINES                     
         B     XIT                                                              
         DROP  R4,R5,R7                                                         
         EJECT                                                                  
*************************************************************                   
*              CHECK ATTENTION FILTER                                           
*************************************************************                   
*                                                                               
ATTENT   NTR1                                                                   
         OC    FILTDISP,FILTDISP   IS THERE A FILTER DISP ?                     
         BZ    XIT                                                              
         OC    FILTVAL,FILTVAL     IS THERE A FILTER VALUE?                     
         BZ    XIT                                                              
*                                                                               
         L     R2,ADACCSTA                                                      
         CLI   0(R2),RSTELQ        MAKE SURE IT'S THE 30                        
         BNE   XIT                                                              
         AH    R2,FILTDISP         DISP TO FILTER                               
         CLC   0(L'FILTVAL,R2),FILTVAL                                          
         BNE   XIT                                                              
*                                                                               
         USING DHEDCDE,R4                                                       
         LA    R4,WRKAREA                                                       
         MVC   WRKAREA,SPACES                                                   
         L     R6,ASRTWRK                                                       
         USING SRTD,R6                                                          
         ZIC   R1,LLEVB            LENGTH OF LEVEL A+B                          
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   DHEDACT(0),SRTHI    OFFICE DEPT                                  
         MVC   DHEDNAME,SRTNAM                                                  
*                                                                               
         GOTO1 BINADD,DMCB,(R4),ADHEDLST                                        
*                                                                               
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*************************************************************                   
*              CLEAR APPROPRIATE ITEMS IN WEEKTAB                               
*************************************************************                   
*                                                                               
CLRTAB   NTR1                                                                   
         L     R5,AWEEKTAB                                                      
         USING WEEKTABD,R5                                                      
         LH    R2,NUMBERW          NUMBER OF TABLE ENTRIES                      
*                                                                               
CLRT02   NI    WKSTAT1,WKRQST      TURN OFF ALL STATUS EXCEPT REQUEST           
         XC    WKSTAT2,WKSTAT2                                                  
         LA    R0,WKBKCNT          CLEAR BUCKETS TO ZERO                        
         LA    R1,WKBUKS                                                        
         ZAP   0(WKBUKLN,R1),ZEROS                                              
         LA    R1,WKBUKLN(R1)                                                   
         BCT   R0,*-10                                                          
         LA    R5,WEEKLEN(R5)                                                   
         BCT   R2,CLRT02                                                        
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*              SEE IF WE HAVE A HIT IN THE TABLE                                
*              TRANSACTION DATE PASSED IN WKDT                                  
*              TRANS STATUS PASSED IN STATUS                                    
*************************************************************                   
*                                                                               
HITIT    NTR1                                                                   
         L     R5,AWEEKTAB         WEEKLY DATE TABLE                            
         USING WEEKTABD,R5                                                      
         LH    R2,NUMBERW          NUMBER OF TABLE ENTRIES                      
*                                                                               
HIT02    CLC   WKDT(L'WKST2),WKST2    TRANS DATE TO START OF WEEK               
         BL    *+14                                                             
         CLC   WKDT(L'WKEND2),WKEND2  TRANS DATE TO END OF WEEK                 
         BNH   HIT04                                                            
         LA    R5,WEEKLEN(R5)                                                   
         BCT   R2,HIT02                                                         
         DC    H'0'                TRANS DATE DID NOT FIT TABLE                 
*                                                                               
HIT04    OI    WKSTAT1,WKHIT       MARK AS A "HIT"                              
         CLI   QOPT7,C'N'          SUPRESS ALL FLAGGED ITEMS?                   
         BE    HIT06                                                            
         CLC   WKDT(L'HIRE),HIRE   IS THIS TIME BEFORE HE WAS HIRED             
         BNL   *+8                                                              
         OI    WKSTAT1,WKHIRE      MARK TIME BEFORE HIRE                        
         CLC   WKDT(L'FIRE),FIRE   IS THIS TIME AFTER TERM DATE                 
         BNH   *+8                                                              
         OI    WKSTAT1,WKFIRE      MARK TIME AFTER FIRE                         
*                                                                               
         CLI   NEWCOST,C'Y'        ON NEW COST?                                 
         BNE   HIT06               NO - SKIP LOCATION DATE CHECK                
         L     RF,APERAREA                                                      
         USING PERD,RF                                                          
         SR    R1,R1                                                            
         IC    R1,PERLNUM                                                       
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RE,PERLVALS         1ST LOCATION VALUE                           
         USING PERLVALS,RE                                                      
HIT05    OI    WKSTAT1,WKOUTOF     MARK TIME AS OUT OF LOCATION                 
         LA    RF,PERLENDD                                                      
         CLC   PERLENDD,WKST2      IF LOC END IS WITHIN PERIOD                  
         BL    *+18                                                             
         CLC   PERLENDD,WKEND2     USE PERIOD END TO CHECK                      
         BH    *+8                                                              
         LA    RF,WKEND2                                                        
         CLC   WKDT(L'PERLENDD),0(RF)                                           
         BH    HIT05A                                                           
         CLC   WKDT(L'PERLSTD),PERLSTD                                          
         BL    HIT05A                                                           
*                                  TURN OFF OUT OF LOCATION/HIRE/FIRE           
         NI    WKSTAT1,ALL-WKHIRE+WKFIRE+WKOUTOF                                
         B     HIT06                                                            
HIT05A   LA    RE,PERLVLEN(RE)                                                  
         BCT   R1,HIT05                                                         
*                                                                               
         USING HITD,RF                                                          
HIT06    L     RF,AHITTAB          TYPE TIME HIT TABLE                          
HIT06A   CLI   HITTYP,EOT          END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                TYPE OF TIME UNKNOWN                         
         MVC   BYTE,STAT                                                        
         NC    BYTE,HITTYP         WHAT TYPE OF TIME                            
         BM    *+12                                                             
         LA    RF,HITLEN(RF)                                                    
         B     HIT06A                                                           
         LA    R1,WKNUM                                                         
         AH    R1,HITDISP          DISP TO ACCUM                                
         AP    0(WKBUKLN,R1),HOURS                                              
         B     XIT                                                              
         DROP  R5,RE,RF                                                         
         EJECT                                                                  
****************************************************************                
*              STACK EMPLOYEES MISSING TS INFO INTO SORT RECORD                 
****************************************************************                
*                                                                               
STAXUP   NTR1                                                                   
         L     R6,ASRTWRK          SORT WORK AREA                               
         LA    R4,SRTENTRY-SRTD(R6) 1ST TABLE ENTRY                             
         USING SRTENTRY,R4                                                      
         L     R5,AWEEKTAB         WEEKLY DATE TABLE                            
         USING WEEKTABD,R5                                                      
         LH    R2,NUMBERW          NUMBER OF TABLE ENTRIES                      
*                                                                               
STAX02   TM    WKSTAT1,WKRQST      IS THIS A REQUESTED WEEK?                    
         BNO   STAX10                                                           
*                                                                               
         TM    WKSTAT1,WKHIT       ANY TIMESHEETS IN THE WEEK?                  
         BO    STAX04              YES - CHECK IF THEY TOTAL TO ZERO            
         CLI   QOPT7,C'O'          ONLY WANT FLAGGED ITEMS?                     
         BE    STAX10              YES -                                        
         CLC   WKST2,FIRE          NO  - CHECK HIRE AND TERM DATES              
         BNL   STAX10              START IS HIGHER THAN TERM DATE               
         CLC   WKEND2,HIRE         OR END IS LOWER THAN HIRE                    
         BL    STAX10              THEN NO TIMESHEET MISSING FOR THE WK         
         CLI   NEWCOST,C'Y'        IF NOT ON NEW COST, COUNT IT MISSING         
         BNE   STAX08                                                           
         L     RF,APERAREA                                                      
         USING PERD,RF                                                          
         SR    R1,R1                                                            
         IC    R1,PERLNUM                                                       
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RE,PERLVALS                 1ST LOCATION VALUE                   
         USING PERLVALS,RE                                                      
STAX03   CLC   WKST2(L'PERLENDD),PERLENDD  WITHIN ANY LOCATION DATES            
         BH    STAX03A                                                          
         CLC   WKST2(L'PERLSTD),PERLSTD                                         
         BNL   STAX08                      MEANS THAT ITS MISSING               
         CLC   WKEND2(L'PERLSTD),PERLSTD                                        
         BL    STAX03A                                                          
         CLC   WKEND2(L'PERLENDD),PERLENDD                                      
         BNH   STAX08                                                           
STAX03A  LA    RE,PERLVLEN(RE)                                                  
         BCT   R1,STAX03                                                        
         B     STAX10                      OTHERWISE SKIP IT                    
         DROP  RE                                                               
*                                                                               
STAX04   ZAP   DUB,ZEROS           CHECK IF THEY TOTAL TO ZERO                  
         LA    R0,WKBKCNT                                                       
         LA    R1,WKBUKS                                                        
         AP    DUB,0(WKBUKLN,R1)                                                
         LA    R1,WKBUKLN(R1)                                                   
         BCT   R0,*-10                                                          
         CP    DUB,ZEROS           DOES TIME FOR WEEK TOTAL ZERO?               
         BNE   STAX06              NO - CHECK IF IT'S ALL "A" TIME              
         CLI   QOPT7,C'N'          SUPPRESS ALL FLAGGED ITEMS?                  
         BE    STAX10              YES -                                        
         CLI   QOPT7,C'H'          H/T FLAGS ONLY?                              
         BE    STAX10              YES -                                        
         OI    WKSTAT1,WKZERO      NO  - MARK WEEK AS TOTALING TO ZERO          
*                                  YES- PRE HIRE OR POST TERM OR OUTOF?         
         TM    WKSTAT1,WKHIRE+WKFIRE+WKOUTOF                                    
         BNZ   STAX10              YES - NEXT WEEKLY TABLE ENTRY                
         MVC   SRTSTAT1,WKSTAT1    STATUS BYTE TO SORT RECORD                   
         CLI   PROFZERT,C'Y'       ADD ZEROS TO MISSING TOTAL?                  
         BE    STAX08                                                           
         B     STAX08A                                                          
*                                                                               
STAX06   CLI   QOPT7,C'O'                                                       
         BE    STAX06C             FLAGGED ITEMS ONLY?                          
         CLI   QOPT7,C'Z'                                                       
         BE    STAX10              ZERO WEEKS ONLY?                             
         CLI   PRFAMISS,C'Y'       INCLUDE "A" TIME TO ZERO OUT ONLY?           
         BNE   STAX06C             NO - CONTINUE                                
         CP    DUB,WKATIM          ALL TIME "A" HOURS?(DUB SET ABOVE)           
         BNE   STAX06C             NO - CONTINUE                                
         CLC   WKST2,FIRE          YES - ALL 'A' TIME IS MISSING WK             
         BNL   STAX10              START IS HIGHER THAN TERM DATE               
         CLC   WKEND2,HIRE         OR END IS LOWER THAN HIRE                    
         BL    STAX10              THEN NO TIMESHEET MISSING FOR THE WK         
         CLI   NEWCOST,C'Y'        IF NOT ON NEW COST, COUNT IT MISSING         
         BNE   STAX08                                                           
         L     RF,APERAREA                                                      
         USING PERD,RF                                                          
         SR    R1,R1                                                            
         IC    R1,PERLNUM                                                       
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RE,PERLVALS                 1ST LOCATION VALUE                   
         USING PERLVALS,RE                                                      
STAX06A  CLC   WKST2(L'PERLENDD),PERLENDD  WITHIN ANY LOCATION DATES            
         BH    STAX06B                                                          
         CLC   WKST2(L'PERLSTD),PERLSTD                                         
         BNL   STAX08                      MEANS THAT ITS MISSING               
         CLC   WKEND2(L'PERLSTD),PERLSTD                                        
         BL    STAX06B                                                          
         CLC   WKEND2(L'PERLENDD),PERLENDD                                      
         BNH   STAX08                                                           
STAX06B  LA    RE,PERLVLEN(RE)                                                  
         BCT   R1,STAX06A                                                       
         B     STAX10                      OTHERWISE SKIP IT                    
         DROP  RE,RF                                                            
*                                                                               
STAX06C  TM    WKSTAT1,WKHIRE+WKFIRE+WKZERO+WKOUTOF ANY FLAG BITS SET?          
         BZ    STAX10              NO - NEXT WEEKLY TABLE ENTRY                 
         MVC   SRTSTAT1,WKSTAT1    YES- STATUS BYTE TO SORT RECORD              
         B     STAX08A                                                          
*                                                                               
STAX08   AP    SRTNUMBM-SRTD(L'SRTNUMBM,R6),=P'1'  ADD 1 TO MISSING CNT         
STAX08A  MVC   SRTWKNUM,WKNUM         WEEK NUMBER TO SORT RECORD                
         LH    R1,SRTNUMB-SRTD(R6)    ADD 1 TO TOT WEEKS IN SORT RECORD         
         AH    R1,=H'1'                                                         
         STH   R1,SRTNUMB-SRTD(R6)                                              
         LA    R4,SRTELN(R4)       NEXT SORT MINI ENTRY                         
*                                                                               
STAX10   LA    R5,WEEKLEN(R5)                                                   
         BCT   R2,STAX02                                                        
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
****************************************************************                
*              TOTAL UP THE MISSING TIMESHEETS TO HIGER LEVELS                  
****************************************************************                
*                                                                               
TOTUP    NTR1                                                                   
         L     R6,ASRTWRK          SORT WORK AREA                               
         USING SRTD,R6                                                          
         LA    R0,TOTCNT           COUNT OF REPORT TOTALS                       
         LA    R1,RTOTS                                                         
         AP    0(RTOTLN,R1),SRTNUMBM                                            
         LA    R1,RTOTLN(R1)                                                    
         BCT   R0,*-10                                                          
*                                                                               
         CLI   QOPT1,C'P'                                                       
         BNE   XIT                                                              
         ZAP   PERTOT2,SRTNUMBM      SAVE CURRENT PERSON CODE TOTAL             
         CLC   PERCODE,SRTEMPL                                                  
         BNE   *+14                                                             
         AP    PERTOT,SRTNUMBM      SAVE PERSON CODE TOTALS                     
         B     XIT                                                              
         MVC   PERCODE,SRTEMPL                                                  
         ZAP   PERTOT,SRTNUMBM                                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***************************************************************                 
*              ROUTINE TO LOOKUP WEEKLY DATES AND STATUS FLAGS                  
*              RETURNED IN PERIOD AND STATUS                                    
***************************************************************                 
*                                                                               
WEEKIT   NTR1                                                                   
         L     R4,ALTAB                    ADDR WEEKLY ENTRY IN SRTREC          
         L     R5,AWEEKTAB                 WEEKLY DATE TABLE                    
         USING WEEKTABD,R5                                                      
         LH    R2,NUMBERW                  NUMBER OF TABLE ENTRIES              
WKIT02   CLC   WKNUM,SRTWKNUM-SRTENTRY(R4) MATCH ON WEEK NUMBER PASSED          
         BE    WKIT04                                                           
         LA    R5,WEEKLEN(R5)                                                   
         BCT   R2,WKIT02                                                        
         DC    H'0'                        MUST FIND A MATCH                    
*                                                                               
WKIT04   MVC   PERIOD(5),WKST3                                                  
         MVC   PERIOD+6(5),WKEND3          WEEKLY DATES TO PERIOD               
         MVC   STATUS,SPACES                                                    
         OC    SRTSTAT1-SRTENTRY(L'SRTSTAT1,R4),SRTSTAT1-SRTENTRY(R4)           
         BZ    XIT                         STATUS OF 00 FINISHED                
*                                                                               
         L     RF,ASTATTAB                 ADDR OF STATUS TABLE                 
         USING STAD,RF                                                          
WKIT06   CLI   STATYP,EOT                  END OF TABLE?                        
         BNE   *+6                                                              
         DC    H'0'                        STATUS UNKNOWN                       
         MVC   BYTE,SRTSTAT1-SRTENTRY(R4)  STATUS INTO BYTE                     
         NC    BYTE,STATYP                 AND ON LIKE BITS                     
         CLC   BYTE,STATYP                 NEED AN EXACT MATCH                  
         BE    WKIT10                                                           
         LA    RF,STALEN(RF)                                                    
         B     WKIT06                                                           
*                                                                               
WKIT10   MVC   STATUS,STACODE              STATUS CODE FOR PRINT LINE           
         B     XIT                                                              
         DROP  R5,RF                                                            
         EJECT                                                                  
*************************************************************                   
*              ROUTINE TO HEADUP NEW PAGE                                       
*************************************************************                   
*                                                                               
HEADUP   NTR1                                                                   
         USING HEADD,R3                                                         
         LA    R3,HEAD4                                                         
         MVC   HEADRANG,RANGE        TS NUMBER RANGE                            
         CLI   QOPT1,C'P'                                                       
         BE    XIT                                                              
         LA    R3,HEAD5                                                         
         MVC   HEADLEVL,LLEVANAM     LEVEL DESCRIPTION                          
         MVC   HEADOFF,OFFCODE                                                  
         MVC   HEADDESC,OFFNAME      NAME OF OFFICE                             
         CLI   PRFBREAK,C'1'         PAGE BREAK ON LEVEL 1?                     
         BE    HEADUP04                                                         
         LA    R3,HEAD6                                                         
         MVC   HEADLEVL,LLEVBNAM     LEVEL DESCRIPTION                          
         MVC   HEADDPT,DEPCODE                                                  
         MVC   HEADDESC,DEPNAME      NAME OF DEPT                               
         CLI   PRFBREAK,C'2'                                                    
         BE    HEADUP04                                                         
         LA    R3,HEAD7                                                         
         MVC   HEADLEVL,LLEVCNAM     LEVEL DESCRIPTION                          
         MVC   HEADSUB,SUBCODE                                                  
         MVC   HEADDESC,SUBNAME      NAME OF SUB DEPT                           
HEADUP04 CLC   ATTN,SPACES                                                      
         BE    XIT                                                              
         LA    R3,HEAD8                                                         
         MVC   HEADATTN,=C'ATTENTION:     '                                     
         MVC   HEADDESC,ATTN         DEPARTMENTAL ATTENTION NAME                
         B     XIT                                                              
         EJECT                                                                  
* **********************************************************                    
*        PRINT AND CLEAR TOTAL LINES                                            
* **********************************************************                    
*                                                                               
TOTAL    NTR1                                                                   
         L     R6,ASRTWRK                                                       
         USING SRTD,R6                                                          
         CLC   OFDPSB,SRTHI                   ANY LEVEL CHANGES                 
         BE    XIT                            NO                                
         CLI   QOPT1,C'P'                     PERSON LEVEL REPORT?              
         BNE   TOT00                                                            
         LA    R0,TOTCNT1                     YES CLEAR ALL EXCEPT REQ          
         LA    R1,LEV1TOT                                                       
         ZAP   0(RTOTLN,R1),ZEROS                                               
         LA    R1,RTOTLN(R1)                                                    
         BCT   R0,*-10                                                          
TOT00    LA    R0,TOTCNT2                     ANY TOTALS AT ALL?                
         LA    R1,RTOTS                                                         
         CP    0(RTOTLN,R1),ZEROS                                               
         BNZ   TOT02                                                            
         LA    R1,RTOTLN(R1)                                                    
         BCT   R0,*-14                                                          
         B     XIT                                                              
*                                                                               
         USING PRNTD,R5                                                         
TOT02    LA    R5,P                                                             
         CLC   PERCODE,SRTEMPL                                                  
         BE    TOT08                                                            
         CP    PERTOT,ZEROS                   PERSON TOTAL                      
         BE    TOT08                                                            
         CP    PERTOT,PERTOT2                 SKIP IF 1 PERSON CODE             
         BE    TOT08                                                            
         ZAP   DUB,PERTOT                                                       
         BAS   RE,TOTPRNT                     PRINT THE TOTAL                   
         ZAP   PERTOT,ZEROS                   CLEAR ACCUM                       
*                                                                               
TOT08    CP    SUBTOT,ZEROS                   SUB DEPT TOTAL                    
         BE    TOT10                                                            
         CLI   PRFNOSUB,C'Y'       SUPPRESS SUBDEPT PRINTING                    
         BE    TOT09A                                                           
         CP    SUBTOT,DEPTOT       ARE SUB & DPT TOTALS SAME?                   
         BNE   TOT09               NO                                           
         ZIC   R1,LLEVB                                                         
         BCTR  R1,0                                                             
         EXCLC R1,SRTHI,OFDPSB     IS THERE A CHA IN DEPT                       
         BNE   TOT09A              YES-SO SUPPRESS SUB TOTAL                    
         CP    SUBTOT,OFFTOT       ARE SUB & OFFICE TOTALS SAME?                
         BNE   TOT09               NO                                           
         ZIC   R1,LLEVA                                                         
         BCTR  R1,0                                                             
         EXCLC R1,SRTHI,OFDPSB     IS THERE A CHA IN OFFICE?                    
         BNE   TOT09A              YES-SO SUPPRESS SUB TOTAL                    
TOT09    ZAP   DUB,SUBTOT                                                       
         MVC   PTOTN(L'LLEVCNAM),LLEVCNAM     LEVEL DESCRIPTION                 
         BAS   RE,TOTPRNT                     PRINT THE TOTAL                   
TOT09A   ZAP   SUBTOT,ZEROS                   CLEAR ACCUM                       
*                                                                               
*                                             OFFICE DEPT CHANGE?               
TOT10    ZIC   R1,LLEVB                       LEN OF A + B                      
         BCTR  R1,0                                                             
         EXCLC R1,SRTHI,OFDPSB                                                  
         BE    TOT14                                                            
         CP    DEPTOT,ZEROS                   DEPT TOTAL                        
         BE    TOT13                                                            
         CP    DEPTOT,OFFTOT       DEPT & OFFICE TOTALS THE SAME?               
         BNE   TOT12               NO-SO MUST PRINT DEPT                        
         ZIC   R1,LLEVA                                                         
         BCTR  R1,0                SEE IF THERE'S A CHANGE                      
         EXCLC R1,SRTHI,OFDPSB     IN THE OFFICE                                
         BNE   TOT13               YES                                          
TOT12    ZAP   DUB,DEPTOT                                                       
         MVC   PTOTN(L'LLEVBNAM),LLEVBNAM     LEVEL DESCRIPTION                 
         BAS   RE,TOTPRNT                     PRINT THE TOTAL                   
TOT13    ZAP   DEPTOT,ZEROS                                                     
*                                                                               
TOT14    ZIC   R1,LLEVA                       LEN OF A                          
         BCTR  R1,0                                                             
         EXCLC R1,SRTHI,OFDPSB                                                  
         BE    TOT16                                                            
         CP    OFFTOT,ZEROS                   OFFICE TOTAL                      
         BE    TOT16                                                            
         ZAP   DUB,OFFTOT                                                       
         MVC   PTOTN(L'LLEVANAM),LLEVANAM     LEVEL DESCRIPTION                 
         BAS   RE,TOTPRNT                     PRINT THE TOTAL                   
         ZAP   OFFTOT,ZEROS                                                     
*                                                                               
TOT16    CLC   OFDPSB,SPACES                                                    
         BNE   XIT                                                              
         ZAP   DUB,REQTOT                                                       
         MVC   PTOTN(7),=C'REQUEST'                                             
         CURED REQTOT,(L'PMISS,PMISS),0                                         
         BAS   RE,TOTPRNT                     PRINT THE TOTAL                   
         ZAP   REQTOT,ZEROS                                                     
         B     XIT                                                              
*                                                                               
*                                                                               
TOTPRNT  LR    R4,RE                           SAVE RE                          
         MVC   PCODE(5),=C'TOTAL'                                               
         GOTO1 ADSQUASH,DMCB,PTOTN,L'PTOTN                                      
         GOTO1 RIGHT,DMCB,PTOTN,L'PTOTN                                         
         CURED DUB,(L'PMISS,PMISS),0                                            
         BAS   RE,PRNTIT                                                        
         BAS   RE,PRNTIT                       SKIP A LINE                      
         LR    RE,R4                                                            
         BR    RE                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
* **********************************************************                    
*        PRINT A LINE                                                           
* **********************************************************                    
*                                                                               
PRNTIT   NTR1                                                                   
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
* **********************************************************                    
*        CHECK FOR HIGHER LEVEL CHANGES                                         
* **********************************************************                    
*                                                                               
HIGHUP   NTR1                                                                   
         MVI   LEVSTAT,0                                                        
         L     R6,ASRTWRK                                                       
         USING SRTD,R6                                                          
         CLC   OFDPSB,SRTHI                   ANY LEVEL CHANGES                 
         BE    XIT                            NO                                
*                                                                               
         OI    LEVSTAT,LEV3BRK                CHANGE OF LEVEL 3                 
         CLI   PRFBREAK,C'3'                  BREAK ON LEVEL 3?                 
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                  FORCE NEW PAGE                    
         MVC   WRKEY,SPACES                   GET NEW SUB DPT NAME              
         MVC   WRKEY(L'SRTHI),SRTHI                                             
         BAS   RE,HINAM                       GO LOOK IT UP                     
         MVC   SUBNAME,WORK                   NAME OF SUB                       
         LA    R2,SRTHI                                                         
         ZIC   R0,LLEVB                       LEN OF A + B                      
         AR    R2,R0                                                            
         ZIC   R1,LENLEVC                     LENGTH OF LEVEL C                 
         BCTR  R1,0                                                             
         EXMVC R1,SUBCODE,0(R2)                                                 
*                                                                               
*                                             OFFICE DEPT CHANGE?               
         ZIC   R1,LLEVB                       LEN OF A + B                      
         BCTR  R1,0                                                             
         EXCLC R1,SRTHI,OFDPSB                                                  
         BE    HIUP04                                                           
         OI    LEVSTAT,LEV2BRK                CHANGE OF LEVEL 2                 
         CLI   PRFBREAK,C'2'                  BREAK ON LEVEL 2?                 
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                  FORCE NEW PAGE                    
         MVC   WRKEY,SPACES                                                     
         EXMVC R1,WRKEY,SRTHI                                                   
         BAS   RE,HINAM                       GO LOOK IT UP                     
         MVC   DEPNAME,WORK                   NAME OF DPT                       
         MVC   ATTN,SPACES                                                      
         OC    PROFFILT,PROFFILT              IS THERE FILTER FOR ATTN?         
         BZ    *+8                                                              
         BAS   RE,ATTNAM                      LOOK UP DEPT ATTN NAME            
         LA    R2,SRTHI                                                         
         ZIC   R0,LLEVA                       LEN OF A                          
         AR    R2,R0                                                            
         ZIC   R1,LENLEVB                     LENGTH OF LEVEL B                 
         BCTR  R1,0                                                             
         EXMVC R1,DEPCODE,0(R2)                                                 
*                                                                               
         ZIC   R1,LLEVA                       LEN OF A                          
         BCTR  R1,0                                                             
         EXCLC R1,SRTHI,OFDPSB                                                  
         BE    HIUP04                                                           
         OI    LEVSTAT,LEV1BRK                CHANGE OF LEVEL 1                 
         CLI   PRFBREAK,C'1'                  BREAK ON LEVEL 1?                 
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                  FORCE NEW PAGE                    
         MVC   WRKEY,SPACES                                                     
         EXMVC R1,WRKEY,SRTHI                                                   
         BAS   RE,HINAM                       GO LOOK IT UP                     
         MVC   OFFNAME,WORK                   NAME OF OFFICE                    
         ZIC   R1,LLEVA                       LEN OF A                          
         BCTR  R1,0                                                             
         EXMVC R1,OFFCODE,SRTHI                                                 
         MVC   WKOFFICE,OFFCODE     SAVE OFFICE FOR CALENDAR LOOKUP             
         BAS   RE,BLDCAL            LOOKUP OFFICE CALENDAR                      
         OC    START,START          USE RANGE START IF NO QSTART                
         BNZ   HIUP04               (MAY HAVE BEEN REQUESTED BYTS #'S)          
         MVC   START,RSTART                                                     
         MVC   END,REND                                                         
*                                   DATES FOR HEADING(MAY HAVE CHANGED)         
         GOTO1 DATCON,DMCB,(1,START),(0,QSTART)                                 
         GOTO1 (RF),(R1),(1,END),(0,QEND)                                       
*                                                                               
HIUP04   MVC   OFDPSB,SRTHI                   SAVE CODES                        
         CLI   QOPT1,C'P'                     PERSON CODE ORDER?                
         BE    XIT                                                              
         BAS   RE,HEADUP                                                        
*                                                                               
         CLI   PRFBREAK,C'3'                  PAGE BREAK ON LEVEL 3?            
         BE    XIT                                                              
         CLI   PRFBREAK,C'1'                                                    
         BNE   HIUP06                                                           
         TM    LEVSTAT,LEV2BRK                CHANGE OF DEPT?                   
         BNO   HIUP06A                                                          
         LA    R3,P                                                             
         MVC   P,SPACES                                                         
         USING PRNTD,R3                                                         
         MVC   PNAME,DEPNAME                                                    
         MVC   PCODE(L'DEPCODE),DEPCODE                                         
         BAS   RE,MIDLINE                                                       
         B     HIUP06A                                                          
HIUP06   CLI   PRFBREAK,C'2'                  PAGE BREAK ON LEVEL 2?            
         BE    *+6                                                              
         DC    H'0'                                                             
HIUP06A  TM    LEVSTAT,LEV3BRK                                                  
         BNO   XIT                                                              
         LA    R3,P                                                             
         MVC   P,SPACES                                                         
         MVC   PNAME,SUBNAME                                                    
         MVC   PCODE(L'SUBCODE),SUBCODE                                         
         BAS   RE,MIDLINE                                                       
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
* **********************************************************                    
*        MID HEADINE                                                            
* **********************************************************                    
*                                                                               
MIDLINE  NTR1                                                                   
         LA    R3,P                                                             
         USING PRNTD,R3                                                         
         GOTO1 ADSQUASH,DMCB,PNAME,PRNTLNQ                                      
         GOTO1 UNDERLIN,DMCB,('PRNTLNQ',PNAME),(X'BF',PSECOND+1)                
         BAS   RE,PRNTIT                                                        
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* **********************************************************                    
*        LOOKUP ATTN FOR LEVEL                                                  
* **********************************************************                    
*                                                                               
ATTNAM   NTR1                                                                   
         L     R5,ADHEDLST         ADDR OF TABLE                                
         USING BIND,R5                                                          
         MVC   ATTN,SPACES                                                      
         L     R0,BININ            NUMBER OF TABLE ENTRIES                      
         LTR   R0,R0                                                            
         BNP   XIT                                                              
         L     RF,BINTABLE         1ST TABLE ENTRY                              
         USING DHEDCDE,RF                                                       
ATTNM02  CLC   WRKEY(L'DHEDACT),DHEDACT                                         
         BE    ATTNM04                                                          
         LA    RF,DHEDLEN(RF)                                                   
         BCT   R0,ATTNM02                                                       
         B     XIT                                                              
*                                                                               
ATTNM04  MVC   ATTN,DHEDNAME       PASS BACK NAME                               
         B     XIT                                                              
         DROP  R5,RF                                                            
         EJECT                                                                  
* **********************************************************                    
*        LOOKUP HIGHER LEVEL NAMES                                              
* **********************************************************                    
*                                                                               
HINAM    NTR1                                                                   
         L     R5,AEMPCDE          ADDR OF TABLE                                
         USING BIND,R5                                                          
         MVC   WORK,SPACES                                                      
         L     R0,BININ            NUMBER OF TABLE ENTRIES                      
         LTR   R0,R0                                                            
         BNP   XIT                                                              
         L     RF,BINTABLE         1ST TABLE ENTRY                              
         USING EMPD,RF                                                          
HINAM02  CLC   WRKEY(L'EMPACT),EMPACT                                           
         BE    HINAM04                                                          
         LA    RF,EMPLEN(RF)                                                    
         BCT   R0,HINAM02                                                       
         MVC   WORK(20),=C'TEST NAME'                                           
         B     XIT                                                              
*                                                                               
HINAM04  MVC   WORK,EMPNAM         PASS BACK NAME                               
         B     XIT                                                              
         DROP  R5,RF                                                            
         EJECT                                                                  
*******************************************************                         
*        ROUTINE TO ADD 1R HIGHER LEVEL NAMES TO TABLE                          
*        PARM1     R2 - CONTAINS ADDRESS OF ADDR ELEMENT                        
*        PARM2     R3 - CONTAINS ADDRESS OF NAME ELEMENT                        
*        PARM3     R4 - CONTAINS ADDRESS OF THE RECORD                          
*******************************************************                         
*                                                                               
HINAME   NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
         USING ACTRECD,R4                                                       
         USING EMPD,R6                                                          
         LA    R6,WRKAREA                                                       
         MVC   WRKAREA,SPACES               CLEAR THE KEY                       
         MVC   EMPACT,ACTKACT               SAVE OFFICE LEVEL CODE              
         GOTO1 BLDTNAM,DMCB,(R3)            GET THE NAME INTO WORK              
         MVC   EMPNAM,WORK                                                      
         GOTO1 BINADD,DMCB,(R6),AEMPCDE                                         
*                                                                               
         LTR   R2,R2               WAS AN ADDRESS ELEMENT PASSED?               
         BNP   XIT                                                              
         USING ADRELD,R2                                                        
         CLI   ADREL,ADRELQ        MAKE SURE IT'S A ADDR EL                     
         BNE   XIT                                                              
         CLC   ADRADD1,SPACES                                                   
         BE    XIT                                                              
         MVC   LOCATION,ADRADD1    SAVE THE LOCATION                            
         B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*******************************************                                     
*         GETTING THE NAME INTO WORK                                            
*******************************************                                     
*                                                                               
BLDTNAM  NTR1                                                                   
         L     R2,0(R1)            DID WE PASS ADDR OF ELEMENT                  
         LTR   R2,R2                                                            
         BNZ   BLDTNM3                                                          
         L     R2,ACREC                                                         
         AH    R2,DATADISP                                                      
BLDTNM1  CLI   0(R2),0                                                          
         BE    BLDTNM9                                                          
         CLI   0(R2),NAMELQ        NAME ELEMENT                                 
         BE    BLDTNM3                                                          
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    BLDTNM1                                                          
         DC    H'0'                                                             
         USING NAMELD,R2                                                        
BLDTNM3  MVC   WORK,SPACES                                                      
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    BLDTNM9                                                          
         EXMVC R1,WORK,NAMEREC                                                  
BLDTNM9  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
****************************************************************                
*              ADD ITEM TO BINSRCH TABLE                                        
*              P1                  A(ITEM TO BE ADDED)                          
*              P2                  A(TABLE)                                     
****************************************************************                
*                                                                               
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         L     R6,BINTABLE         A(TABLE)                                     
         L     R4,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
******************************                                                  
*        CONSTANTS                                                              
******************************                                                  
*                                                                               
NINES    DC    CL12'999999999999'                                               
RANGE    DC    CL28'RPT RANGE: TS#    TO  TS#   '                               
PERIOD   DC    CL11'     -     '                                                
ZEROS    DC    PL1'0'                                                           
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,)'                              
*                                                                               
RELOTAB  DS    0A                                                               
         DC    V(ACSLRY)                                                        
         DC    V(DATVAL)                                                        
         DC    V(COVAIL)                                                        
         DC    V(ACSALHST)                                                      
         DC    V(PERCALL)                                                       
         DC    V(UNDERLIN)                                                      
         DC    V(RIGHT)                                                         
         DC    4F'0'               SPARE                                        
         DC    A(RECORD)                                                        
         DC    A(SRTWRK)                                                        
         DC    A(WEEKWRK)                                                       
         DC    A(SALAREA)                                                       
         DC    A(PERAREA)                                                       
         DC    A(MAINTAB)                                                       
         DC    A(FILTAB)                                                        
         DC    A(STATTAB)                                                       
         DC    A(HITTAB)                                                        
         DC    A(PROGTAB)                                                       
         DC    A(WEEKTB1)                                                       
         DC    A(WEEKTB2)                                                       
         DC    6F'0'               SPARE                                        
         DC    A(DHEDLST)                                                       
         DC    A(EMPCODE)                                                       
         DC    6F'0'               SPARE                                        
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
******************************                                                  
*        WORK AREAS                                                             
******************************                                                  
*                                                                               
*                                                                               
RECORD   DS    0D                                                               
         DS    CL42                                                             
         DS    CL2000                                                           
*                                                                               
         DS    0D                                                               
SRTWRK   DS    CL(SRTLEN)                                                       
*                                                                               
         DS    0D                                                               
WEEKWRK  DS    CL(WEEKLEN)                                                      
*                                                                               
         DS    0D                                                               
SALAREA  DS    CL(SALLNQ)                                                       
*                                                                               
         DS    0D                                                               
PERAREA  DS    CL(PERLNQ)                                                       
*                                                                               
FLAG     DS    X                                                                
DATEDONE EQU   X'80'               HIRE/TERM DATES HAVE BEEN PRINTED            
SRTDONE  EQU   X'40'               NO MORE SORT ENTRIES                         
         EJECT                                                                  
******************************                                                  
*        TABLES                                                                 
******************************                                                  
*                                                                               
*                                                                               
MAINTAB  DS    0D                                                               
         DC    AL4((((DHEDMAX*DHEDLEN)+7)/8)*8),A(DHEDTAB)                      
         DC    AL4((((EMPNUM*EMPLEN)+7)/8)*8),A(EMPTAB)                         
         DC    X'FF'                                                            
*                                                                               
FILTAB   DS    0D                                                               
         DC    AL1(1),AL1(0),Y(RSTFILT1-RSTELD)                                 
         DC    AL1(2),AL1(0),Y(RSTFILT2-RSTELD)                                 
         DC    AL1(3),AL1(0),Y(RSTFILT3-RSTELD)                                 
         DC    AL1(4),AL1(0),Y(RSTFILT4-RSTELD)                                 
         DC    AL1(5),AL1(0),Y(RSTFILT5-RSTELD)                                 
         DC    AL1(EOT)                                                         
*                                                                               
STATTAB  DS    0D                                                               
         DC    AL1(WKHIRE+WKZERO),CL4'(HZ)'                                     
         DC    AL1(WKFIRE+WKZERO),CL4'(TZ)'                                     
         DC    AL1(WKOUTOF+WKZERO),CL4'(LZ)'                                    
         DC    AL1(WKZERO),CL4' (Z)'                                            
         DC    AL1(WKHIRE),CL4' (H)'                                            
         DC    AL1(WKFIRE),CL4' (T)'                                            
         DC    AL1(WKOUTOF),CL4' (L)'                                           
         DC    AL1(EOT)                                                         
*                                                                               
HITTAB   DS    0D                                                               
         DC    AL1(TRSSTADJ),AL1(0),Y(WKATIM-WEEKTABD)                          
         DC    AL1(TRSSTMSS),AL1(0),Y(WKMTIM-WEEKTABD)                          
         DC    AL1(TRSSTIME),AL1(0),Y(WKTTIM-WEEKTABD)                          
         DC    AL1(ALL-TRSSTADJ-TRSSTMSS-TRSSTIME),AL1(0)                       
         DC    Y(WKTTIM-WEEKTABD)                                               
         DC    AL1(EOT)                                                         
*                                                                               
PROGTAB  DS    0D                                                               
         DC    CL1' ',X'0'                                                      
         DC    CL1'N',X'1'                                                      
         DC    CL1'O',X'2'                                                      
         DC    CL1'H',X'3'                                                      
         DC    CL1'Z',X'4'                                                      
         DC    AL1(EOT)                                                         
*                                                                               
         DS    0D                                                               
WEEKTB1  DS    CL(WEEKMAX*WEEKLEN)                                              
*                                                                               
         DS    0D                                                               
WEEKTB2  DS    CL(WEEKMAX*WEEKLEN)                                              
         DC    X'FF'                                                            
*                                                                               
DHEDLST  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(DHEDLEN)        RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(DHEDKLN)        KEY LENGTH                                   
         DC    AL4(DHEDMAX)        MAX IN TABLE                                 
DHEDTAB  DS    AL4(0)              ADDRESS OF THE TABLE                         
*                                                                               
EMPCODE  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(EMPLEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(EMPKLN)         KEY LENGTH                                   
         DC    AL4(EMPNUM)         MAX IN TABLE                                 
EMPTAB   DS    AL4(0)              ADDRESS OF THE TABLE                         
*                                                                               
WEEKMAX  EQU   80                  MAX NUMBER OF WEEKS                          
DHEDMAX  EQU   350                 MAX DEPT HEADS                               
EMPNUM   EQU   4000                MAX 1R HIGHER LEVELS                         
*                                                                               
         EJECT                                                                  
*************************************************************                   
*              CHECK EMPLOYEE RECORD FOR SALARY HIRE AND TERM                   
*************************************************************                   
*                                                                               
EMPCHEK  NMOD1 0,*EMPC*                                                         
         L     RC,0(R1)                                                         
*                                  CHECK IF IT'S AN OVERHEAD ACCOUNT            
         L     R7,ADACC                                                         
         USING ACTRECD,R7                                                       
         SR    R1,R1                                                            
         IC    R1,LLEVA            LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EXCLC R1,ACTKACT,NINES    CORPORATE LEVEL OVERHEAD                     
         BE    EMPNO                                                            
         LA    R2,ACTKACT                                                       
         SR    R0,R0                                                            
         IC    R0,LLEVA                                                         
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVB          LENGTH OF LEVEL B                            
         BCTR  R1,0                                                             
         EXCLC R1,0(R2),NINES      OFFICE LEVEL OVERHEAD                        
         BE    EMPNO                                                            
         LA    R2,ACTKACT                                                       
         SR    R0,R0                                                            
         IC    R0,LLEVC            LENGTH OF LEVEL A+B+C                        
         AR    R2,R0                                                            
         CLC   0(3,R2),NINES       DEPT LEVEL OVERHEAD                          
         BE    EMPNO                                                            
         CLC   QPERCODE,SPACES                                                  
         BE    EMPC01                                                           
         CLC   QPERCODE,0(R2)      FILTER BY PERSON CODE                        
         BNE   EMPNO                                                            
*                                                                               
         IC    R1,LLEVA                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   QACCOUNT(0),SPACES                                               
         BE    EMPC00A                                                          
         EXCLC R1,ACTKACT,QACCOUNT        CHECK OFFICE EQUAL                    
         BNE   EMPNO                                                            
*                                                                               
EMPC00A  LA    RF,QACCOUNT                CHECK IF DEPT=SPACES                  
         IC    R1,LLEVA                                                         
         AR    RF,R1                                                            
         IC    R1,LENLEVB                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),SPACES                                                   
         BE    EMPC00B                                                          
         LA    R2,ACTKACT                                                       
         IC    R1,LLEVA                                                         
         AR    R2,R1                                                            
         IC    R1,LENLEVB                                                       
         BCTR  R1,0                                                             
         EXCLC R1,0(R2),0(RF)             CHECK DEPT EQUAL                      
         BNE   EMPNO                                                            
*                                                                               
EMPC00B  LA    RF,QACCOUNT                CHECK IF SUBDEPT=SPACES               
         IC    R1,LLEVB                                                         
         AR    RF,R1                                                            
         IC    R1,LENLEVC                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),SPACES                                                   
         BE    EMPC01                                                           
         LA    R2,ACTKACT                                                       
         IC    R1,LLEVB                                                         
         AR    R2,R1                                                            
         IC    R1,LENLEVC                                                       
         BCTR  R1,0                                                             
         EXCLC R1,0(R2),0(RF)             CHECK SUBDEPT EQUAL                   
         BNE   EMPNO                                                            
*                                                                               
EMPC01   XC    HIRE,HIRE           SET LOW                                      
         MVC   FIRE,=X'FFFFFF'     SET HIGH                                     
         XC    SALAEND,SALAEND                                                  
         CLI   PROFLOK,C'N'        SUPPRESS LOCKED ACCOUNTS?                    
         BE    EMPC02                                                           
         L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         TM    RSTSTAT,RSTSACIL    SUPPRESS LOCKED ACCOUNT                      
         BO    EMPNO                                                            
EMPC02   CLI   NEWCOST,C'Y'        ON NEW COST?                                 
         BE    EMPC04                                                           
         L     R4,ADACC                                                         
         MVI   ELCODE,EMPELQ       EMPLOYEE HISTORY ELEMENT                     
         BAS   RE,GETEL            RETURNED IN R4                               
         BNE   EMPC06                                                           
         USING EMPELD,R4                                                        
         OC    EMPHIR,EMPHIR       HIRE DATE PRESENT                            
         BZ    *+10                                                             
         MVC   HIRE,EMPHIR                                                      
         OC    EMPTRM,EMPTRM       TERM DATE PRESENT                            
         BZ    EMPC06                                                           
         MVC   FIRE,EMPTRM                                                      
         B     EMPC06                                                           
*                                                                               
EMPC04   L     R4,APERAREA         I/O AREA                                     
         USING PERD,R4                                                          
         MVC   PERADACC,ADACC                                                   
         MVC   PERALEDG,ADLEDGER                                                
         GOTO1 PERCALL,PERD                                                     
         OC    PERERR,PERERR       ANY ERRORS?                                  
         BZ    EMPC04A                                                          
         TM    PERERR,PERNOTER     NO PERSON RECORD SHOULD LEAVE                
         BO    EMPNO                                                            
         DC    H'0'                DIE FOR ALL OTHER ERRORS                     
EMPC04A  CLI   PERLNUM,0           CHECK NUMBER OF LOCATIONS                    
         BE    EMPNO               NOT IN LOCATION FOR REQUEST PERIOD           
         OC    PERHIR,PERHIR       HIRE DATE PRESENT                            
         BZ    *+10                                                             
         MVC   HIRE,PERHIR                                                      
         OC    PERTRM,PERTRM       TERM DATE PRESENT                            
         BZ    *+10                                                             
         MVC   FIRE,PERTRM                                                      
         SR    R1,R1                                                            
         IC    R1,PERLNUM                                                       
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RE,PERLVALS         1ST LOCATION VALUE                           
         USING PERLVALS,RE                                                      
EMPC05   CLC   FIRE,PERLENDD                                                    
         BNL   *+10                                                             
         MVC   FIRE,PERLENDD       REPLACE FIRE WITH HIGHER LOC END             
         CLC   SALAEND,PERLENDD    SAVE HIGHEST LOC END                         
         BNL   *+10                                                             
         MVC   SALAEND,PERLENDD                                                 
         LA    RE,PERLVLEN(RE)                                                  
         BCT   R1,EMPC05                                                        
         DROP  RE                                                               
*                                                                               
*                                                                               
EMPC06   CLC   FIRE,START          IF TERMINATED BEFORE START SUPPRESS          
         BL    EMPNO                                                            
         CLC   HIRE,END            IF HIRED AFTER END SUPPRESS                  
         BH    EMPNO                                                            
*                                                                               
         CLI   PRFEXEMP,C'Y'       EXCLUDE TERMINATED EMPLOYEES?                
         BNE   EMPC08              NO                                           
         CLC   FIRE,END                                                         
         BL    EMPNO                                                            
*                                                                               
EMPC08   CLI   PROFSAL,C'Y'        SALARY REQUIRED TO INCLUDE EMPLOYEE          
         BNE   EMPYES              NO - WE ARE DONE                             
         L     R3,ASALAREA                                                      
         CLI   NEWCOST,C'Y'        ON NEW COST?                                 
         BE    EMPC10                                                           
         GOTO1 ACSLRY,DMCB,(X'80',ADACC),STEND,ASALAREA,ADCOMFAC                
         USING SLRD,R3                                                          
         CP    SLRTOT,ZEROS                                                     
         BE    EMPNO                                                            
*                                                                               
EMPC10   DS    0H                                                               
         USING SALARYD,R3                                                       
         MVC   SALEND,SALAEND     USE HIGHEST LOCATION END                      
         CLC   SALAEND,=X'FFFFFF' IF YOU DONT HAVE ONE                          
         BNE   *+10                                                             
         MVC   SALEND,SHEND       USE  REQ END                                  
         GOTO1 ACSALHST,DMCB,ACWORKD,(R3),ACREC                                 
         OC    SALSTAT2,SALSTAT2                                                
         BZ    *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
         CP    SALTOTAL,ZEROS                                                   
         BE    EMPNO                                                            
*                                                                               
EMPYES   CR    RB,RB               EQUAL CONDITION ON EXIT                      
         B     EMPX                                                             
*                                                                               
EMPNO    LTR   RB,RB               NOT EQUAL CONDITION ON EXIT                  
EMPX     XMOD1                                                                  
         DROP  R2,R3,R4,R7                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
************************************************                                
*         D S E C T S                                                           
************************************************                                
*                                                                               
*                                                                               
*              DSECT FOR WORKING STORAGE                                        
ACCMD    DSECT                                                                  
ATYPES   DS    0A                                                               
ACSLRY   DS    V                                                                
DATVAL   DS    V                                                                
COVAIL   DS    V                                                                
ACSALHST DS    V                                                                
PERCALL  DS    V                                                                
UNDERLIN DS    V                                                                
RIGHT    DS    V                                                                
         DS    4F                  SPARE                                        
*                                  WORK AREA ADDRESSES                          
ACREC    DS    A                                                                
ASRTWRK  DS    A                                                                
AWEEKWRK DS    A                                                                
ASALAREA DS    A                                                                
APERAREA DS    A                                                                
AMAINTAB DS    A                                                                
AFILTAB  DS    A                                                                
ASTATTAB DS    A                                                                
AHITTAB  DS    A                                                                
APROGTAB DS    A                                                                
AWEEKTB1 DS    A                                                                
AWEEKTB2 DS    A                                                                
         DS    6F                  SPARE                                        
ADHEDLST DS    A                   BINARY TABLES (GETMAIN)                      
AEMPCDE  DS    A                                                                
         DS    6F                  SPARE                                        
         DS    CL1                 END OF TABLE                                 
*                                                                               
ALTAB    DS    A                   GENERAL SAVED ADDR OF TABL ENTRY             
AWEEKTAB DS    A                   ADDR OF WEEKTAB IN USE                       
MAINLEN  DS    F                   LENGTH OF GETMAIN AREA                       
MAINBGN  DS    F                   ADDR OF BEGINNING OG GETMAIN AREA            
*                                                                               
COMMAND  DS    CL8                                                              
DA       DS    XL4                                                              
SVKEY    DS    XL42                                                             
ELCODE   DS    CL1                                                              
WEND     DS    CL1                 WEEK ENDING DAY (EX 7=SUN,6=SAT)             
CALENDAR DS    CL1                 CALENDAR RECORD SWITCH                       
FIRSTNBR DS    CL2                 1ST WEEK NUMBER                              
LASTNBR  DS    CL2                 LAST WEEK NUMBER                             
FIRSTNB1 DS    CL2                 1ST WEEK NUMBER(SAVED FOR COMPANY)           
LASTNBR1 DS    CL2                 LAST WEEK NUMBER(SAVED FOR COMPANY)          
FIRSTWK  DS    CL2                 1ST WEEK NUMBER  (DEFAULT)                   
LASTWK   DS    CL2                 LAST WEEK NUMBER (DEFAULT)                   
HOURS    DS    PL6                 WORK HOURS                                   
STATUS   DS    CL(L'STACODE)       STATUS CODE TO PRINT                         
*                                                                               
RTOTS    DS    PL3                 ***REPORT TOTALS***                          
RTOTLN   EQU   *-RTOTS                                                          
         ORG   RTOTS                                                            
REQTOT   DS    PL3                 REQUEST TOTAL                                
LEV1TOT  DS    0PL3                                                             
OFFTOT   DS    PL3                 OFFICE TOTAL                                 
DEPTOT   DS    PL3                 DEPARTMENT TOTAL                             
SUBTOT   DS    PL3                 SUBDEPARTMENT TOTAL                          
TOTCNT1  EQU   (*-LEV1TOT)/RTOTLN                                               
TOTCNT   EQU   (*-RTOTS)/RTOTLN    *******************                          
PERTOT   DS    PL3                 PERSON TOTAL                                 
PERTOT2  DS    PL3                 LAST PERSON TOTAL                            
TOTCNT2  EQU   (*-RTOTS)/RTOTLN    *******************                          
*                                                                               
STAT     DS    XL1                 STATUS WORK BYTE                             
CMPSTAT  DS    XL1                                                              
CMPONTMS EQU   X'01'               COMPANY ON TMS                               
LEVSTAT  DS    XL1                                                              
LEV3BRK  EQU   X'80'               LEVEL 3 BREAK                                
LEV2BRK  EQU   X'40'               LEVEL 2 BREAK                                
LEV1BRK  EQU   X'20'               LEVEL 1 BREAK                                
*                                                                               
DATES    DS    0X                                                               
TODAY    DS    XL3                 TODAY'S DATE YMD (PACKED)                    
MOSSTART DS    XL3                 MOS START PACKED YYMMDD                      
MOSEND   DS    XL3                 MOS END PACKED YYMMDD                        
START    DS    XL3                 QSTART YMD                                   
END      DS    XL3                 QEND YMD                                     
START1   DS    XL3                 QSTART YMD SAVED FROM COMPANY LEVEL          
END1     DS    XL3                 QEND YMD SAVED FROM COMPANY LEVEL            
RSTART   DS    XL3                 RANGE START YMD                              
REND     DS    XL3                 RANGE END YMD                                
RSTART1  DS    XL3                 RANGE START YMD FROM COMPANY LEVEL           
REND1    DS    XL3                 RANGE END YMD                                
SHSTART  DS    XL3                 RANGE START YMD FOR SALARY HISTORY           
SHEND    DS    XL3                 RANGE END YMD                                
STEND    DS    CL4                 START AND END YYMM FOR ACSALARY              
FISCALS  DS    XL3                 FISCAL YEAR START YMD                        
FISCALE  DS    XL3                 FISCAL YEAR END YMD                          
FISCALS2 DS    XL6                 FISCAL YEAR START EBCDIC                     
FISCALE2 DS    XL6                 FISCAL YEAR END EBCDIC                       
WEEKSTRT DS    XL6                 WEEK STARTING DATE EBCDIC                    
WEEKEND  DS    XL6                 WEEK ENDING DATE EBCDIC                      
WKDT     DS    XL6                 DATE WORK AREA                               
HIRE     DS    XL3                 HIRE DATE                                    
FIRE     DS    XL3                 TERM DATE                                    
SALAEND  DS    XL3                 SALARY END DATE                              
DATLEN   EQU   *-DATES                                                          
*                                                                               
WRKEY    DS    CL15                                                             
SAVEKEY  DS    CL42                                                             
ATTN     DS    CL36                  ATTENTION NAME (BY DEPT)                   
WRKAREA  DS    CL100                                                            
WKOFFICE DS    CL(L'CASPOFC)         WORK OFFICE SAVE AREA                      
NEWCOST  DS    CL1                   PERSON RECORDS EXIST Y OR N                
NUMBERW  DS    H                     NUMBER OF PERIODS                          
NUMBER1  DS    H                     NUMBER OF PERIODS SAVED FOR COMP           
FILTDISP DS    H                     DISP TO FILTER IN STATUS ELEMENT           
FILTVAL  DS    CL(L'RSTFILT1)        FILTER VALUE                               
LOCATION DS    CL(L'ADRADD1)         FLOOR NUMBER WHERE EMPLOYEE WORKS          
OFDPSB   DS    CL(L'SRTHI)                                                      
LEVCODES DS    0CL18                                                            
OFFCODE  DS    CL6                   SAVE OFFICE CODE                           
DEPCODE  DS    CL6                   SAVE DEPT CODE                             
SUBCODE  DS    CL6                   SAVE SUB CODE                              
OFFNAME  DS    CL(L'EMPNAM)                                                     
DEPNAME  DS    CL(L'EMPNAM)                                                     
SUBNAME  DS    CL(L'EMPNAM)                                                     
LEVSAV   EQU   *-OFDPSB              LENGTH OF SAVED LEVELS                     
PERCODE  DS    CL8                   PERSON CODE                                
QPERCODE DS    CL8                   REQUESTED PERSON CODE                      
*                                                                               
LLEVELS  EQU   *                                                                
LLEVA    DS    CL(L'ACLVLEN)         1R LEV A LENGTH                            
LLEVANAM DS    CL(L'ACLVDESC)        1R LEV A NAME                              
LLEVALN  EQU   *-LLEVELS                                                        
LLEVB    DS    CL(L'ACLVLEN)         1R LEV B LENGTH (A+B)                      
LLEVBNAM DS    CL(L'ACLVDESC)        1R LEV B NAME                              
LLEVC    DS    CL(L'ACLVLEN)         1R LEV C LENGTH (A+B+C)                    
LLEVCNAM DS    CL(L'ACLVDESC)        1R LEV C NAME                              
LLEVD    DS    CL(L'ACLVLEN)         1R LEV D LENGTH (A+B+C+D)                  
LLEVDNAM DS    CL(L'ACLVDESC)        1R LEV D NAME                              
LLEVELLN EQU   *-LLEVELS                                                        
LLEVLNUM EQU   LLEVELLN/LLEVALN                                                 
*                                                                               
NUMLEVLS DS    XL1        NUMBER OF LEVELS IN 1R                                
LENLEVLS EQU   *                                                                
LENLEVA  DS    XL1        REAL LENGTH OF LEVEL A                                
LENLEVB  DS    XL1        REAL LENGTH OF LEVEL B                                
LENLEVC  DS    XL1        REAL LENGTH OF LEVEL C                                
LENLEVD  DS    XL1        REAL LENGTH OF LEVEL D                                
LENLEVLN EQU   *-LENLEVLS                                                       
LENLVNUM EQU   LENLEVLN/L'LENLEVA                                               
PROFS    DS    0C                                                               
PROFSAL  DS    CL1             INCLUDE EMPL WITH SAL IN PERIOD ONLY Y,N         
PROFLOK  DS    CL1             SUPPRESS LOCKED 1R ACCOUNTS Y,N                  
PROFATIM DS    CL1             INCLUDE "A" TIME (ADJUSTED) Y,N                  
PRFAMISS DS    CL1             WK WITH "A" ONLY,CONSIDERED MISSING Y,N          
PROFZERT DS    CL1             INCLUDE ZERO BAL WKS IN MISSING TOT Y,N          
PROFFILT DS    CL1             FILTER TO IDENTIFY DEPT ATTENTION NAME           
PRFFTVAL DS    CL1             VALUE OF FILTER                                  
PRFLASTW DS    CL1             LAST WEEK ENDS WITH YEAR END Y,N                 
PRF52WKS DS    CL1             LIMIT YEAR TO 52 WEEKS Y,N                       
PRFWKEND DS    CL1             WEEK ENDING OVERRIDE MON=1,TUES=2,WED=3          
PRFBREAK DS    CL1             PAGE BREAK ON LEVEL 2 OR 3 (DEFAULT 3)           
PRFEXEMP DS    CL1             EXCLUDE TERMINATED EMPLOYEES                     
PRFHRTRD DS    CL1             INCLUDE HIRE/TERM DATE(S) Y,N                    
PRFNOSUB DS    CL1             SUPPRESS PRINTING OF SUBDPT TOTALS Y,N           
PROFLEN  EQU   *-PROFSAL                                                        
*                                                                               
*                                                                               
*              DSECT FOR BINSRCH PARAMETERS                                     
*                                                                               
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTABLE DS    0CL1                                                             
*                                                                               
*                                                                               
*                                                                               
*              DSECT FOR DEPARTMENT HEAD  TABLE                                 
*                                                                               
DHEDCDE  DSECT                                                                  
DHEDACT  DS    CL12                                                             
DHEDKLN  EQU   *-DHEDCDE                                                        
DHEDNAME DS    CL36                                                             
DHEDLEN  EQU   *-DHEDCDE                                                        
*                                                                               
*                                                                               
*                                                                               
*              DSECT FOR WEEK ENDING TABLE DATES                                
*                                                                               
WEEKTABD DSECT                                                                  
WKNUM    DS    CL2                 WEEK NUMBER                                  
WKST1    DS    XL6                 WEEK STARTING YYMMDD EBCDIC                  
WKEND1   DS    XL6                 WEEK ENDING YYMMDD EBCDIC                    
WKST2    DS    PL3                 WEEK STARTING YMD PACKED                     
WKEND2   DS    PL3                 WEEK ENDING YMD PACKED                       
WKST3    DS    CL8                 WEEK STARTING CHARS MM/DD/YY                 
WKEND3   DS    CL8                 WEEK ENDING CHARS MM/DD/YY                   
WKSTAT1  DS    XL1                 WEEKLY STATUS BYTE 1                         
WKRQST   EQU   X'80'               WEEK CHECK REQUESTED                         
WKHIT    EQU   X'40'               WEEK TIMESHEET ENTRY FOUND (A HIT)           
WKHIRE   EQU   X'20'               WEEKLY INPUT BEFORE HIRE                     
WKFIRE   EQU   X'10'               WEEKLY INPUT AFTER TERM                      
WKZERO   EQU   X'08'               WEEKLY TOTAL HOURS ZERO                      
WKOUTOF  EQU   X'04'               WEEKLY INPUT OUT OF LOCATION RANGE           
WKSTAT2  DS    XL1                 WEEKLY STATUS BYTE 2                         
WKBUKS   DS    PL3                                                              
WKBUKLN  EQU   *-WKBUKS                                                         
WKTTIM   DS    PL(WKBUKLN)         WEEKLY T TIME                                
WKMTIM   DS    PL(WKBUKLN)         WEEKLY M TIME                                
WKATIM   DS    PL(WKBUKLN)         WEEKLY A TIME                                
WKBKCNT  EQU   (*-WKBUKS)/WKBUKLN                                               
WEEKLEN  EQU   *-WEEKTABD                                                       
         EJECT                                                                  
*              DSECT FOR 1R LEVEL NAMES                                         
*                                                                               
EMPD     DSECT                                                                  
EMPACT   DS    CL12                EMPLOYEE HIGHER LEVEL ACCOUNT                
EMPKLN   EQU   *-EMPD                                                           
EMPNAM   DS    CL36                HIGHER LEVEL ACCOUNT NAME                    
EMPLEN   EQU   *-EMPD                                                           
*                                                                               
*                                                                               
*              DSECT FOR SORTED FILE                                            
*                                                                               
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTEMPS  DS    CL8                 SORT BY EMPLOYEE CODE                        
SRTHI    DS    CL12                                                             
SRTEMP   DS    CL8                 EMPLOY CODE (SPACES IF ALPHABETICAL)         
SRTNAM   DS    CL36                                                             
SRTEMPL  DS    CL8                                                              
SRTHIRE  DS    PL3                 HIRE DATE (PACKED YMD)                       
SRTTERM  DS    PL3                 TERMINATION DATE (PACKED YMD)                
SRTEMLN  EQU   *-SRTEMP            LENGTH OF SORT EMPLOYEE INFO                 
*                                                                               
SRTKLEN  EQU   *-SRTD              LENGTH OF SORT RECORD KEY                    
SRTNUMB  DS    H                   NUMBER OF WEEKLY SRTENTRIES                  
SRTNUMBM DS    PL2                 NUMBER OF WEEKS ACTUALLY MISSING             
SRTENTRY DS    0C                                                               
SRTWKNUM DS    CL2                 WEEK NUMBER                                  
SRTSTAT1 DS    CL1                 STATUS BYTE 1                                
SRTELN   EQU   *-SRTENTRY          LENGTH OF WEEKLY ENTRY                       
         ORG   SRTENTRY                                                         
SRTWEKS  DS    CL(WEEKMAX*SRTELN)  WEEKLY INDEX                                 
SRTLEN   EQU   *-SRTD              LENGTH OF SORT RECORD                        
SRTXCLEN EQU   *-SRTNUMB           LENGTH TO XC AREA                            
*                                                                               
*                                                                               
*              DSECT FOR HEADLINES                                              
*                                                                               
HEADD    DSECT                                                                  
         DS    CL1                                                              
HEADATTN DS    CL15                                                             
         ORG   HEADATTN                                                         
HEADLEVL DS    CL15                                                             
HEADOFF  DS    CL6                 1R OFFICE                                    
         ORG   HEADOFF                                                          
HEADDPT  DS    CL6                 1R DEPT                                      
         ORG   HEADOFF                                                          
HEADSUB  DS    CL6                 1R SUB DEPT                                  
         DS    CL1                                                              
HEADDESC DS    CL36                DECSRIPTION                                  
         DS    CL45                                                             
HEADRANG DS    CL(L'RANGE)         TS NUMBER RANGE                              
HEADLN   EQU   *-HEADD                                                          
         EJECT                                                                  
*              DSECT FOR PRINTLINE                                              
*                                                                               
PRNTD    DSECT                                                                  
         DS    CL1                 SPACES                                       
PTOTN    DS    0CL38                                                            
PNAME    DS    CL30                EMPLOYEE NAME                                
         ORG   PNAME+7                                                          
PHIRE    DS    CL8                                                              
         DS    CL7                                                              
PTERM    DS    CL8                                                              
         ORG   PNAME+21                                                         
PLEVELS  DS    CL9                 1R LEVELS FOR PERSON CODE ORDER REP          
         DS    CL1                 SPACES                                       
PCODE    DS    CL7                 ACCOUNT CODE                                 
         DS    CL1                 SPACES                                       
PMISS    DS    CL5                 NUMBER OF MISSING TIMESHEETS                 
         DS    CL5                 SPACES                                       
*                                                                               
PENTRY   DS    0CL1                                                             
PSTAT    DS    CL(L'STATUS)        STATUS BLANK OR (H,T,Z)                      
*                                  H - WEEKLY INPUT BEFORE HIRE DATE            
*                                  T - WEEKLY INPUT AFTER TERM DATE             
*                                  Z - WEEKLY INPUT TOTALS ZERO HOURS           
PWKNUM   DS    CL2                 NUMBER OF MISSING TIMESHEETS                 
         DS    CL1                 SPACES                                       
PWKDAT   DS    CL(L'PERIOD)        WEEKLY DATES                                 
         DS    CL2                 SPACES                                       
PENTRYLN EQU   *-PENTRY                                                         
         ORG   PENTRY                                                           
PENTRIES DS    CL(4*PENTRYLN)      4 ENTRIES PER PRINT LINE                     
PENTCNT  EQU   (*-PENTRY)/PENTRYLN NUMBER OF ENTRIES ON A LINE                  
PRNTLNQ  EQU   *-PRNTD                                                          
*                                                                               
*                                                                               
*              DSECT FOR STATUS BYTE TO PRINT LINE                              
*                                                                               
STAD     DSECT                                                                  
STATYP   DS    AL1                 TYPE OF STATUS                               
STACODE  DS    CL4                 CODE TO PRINT                                
STALEN   EQU   *-STAD                                                           
*                                                                               
*                                                                               
*              DSECT FOR HITTING THE WEEKLY DATE TABLE                          
*                                                                               
HITD     DSECT                                                                  
HITTYP   DS    AL1                 TYPE OF TIME (ADJ,MISSING,REGULAR)           
         DS    AL1                 SPARE                                        
HITDISP  DS    Y                   DISP TO TIME ACCUM                           
HITLEN   EQU   *-HITD                                                           
*                                                                               
*                                                                               
*              DSECT FOR RCSUBPRG IN 01 PHASE (KEYS OFF OF QOPT7)               
*                                                                               
PRGD     DSECT                                                                  
PRGOPT7  DS    CL1                 QOPT7 VALUE                                  
PRGVALU  DS    XL1                 CORRESPONDING RCSUBPRG VALUE                 
PRGLEN   EQU   *-PRGD                                                           
*                                                                               
*                                                                               
*              DSECT FOR FORCE FILTERING                                        
*                                                                               
FRCFLTD  DSECT                                                                  
FRCFLT   DS    AL1                 FILTER TO USE                                
         DS    AL1                 SPARE                                        
FRCDISP  DS    Y                   DISP TO FILTER VALUE                         
FRCLEN   EQU   *-FRCFLTD                                                        
         EJECT                                                                  
**************************************                                          
*        EQUATES                                                                
**************************************                                          
*                                                                               
*                                                                               
MAXLEN   EQU   12                  MAX LENGTH OF AN ACCOUNT                     
MON      EQU   1                   WEEKDAY EQUATES FOR GETDAY                   
TUE      EQU   2                                                                
WED      EQU   3                                                                
THUR     EQU   4                                                                
FRI      EQU   5                                                                
SAT      EQU   6                                                                
SUN      EQU   7                                                                
ALL      EQU   X'FF'               FOR USE WITH NI INSTRUX                      
EOT      EQU   X'FF'               END OF TABLE                                 
SPACE    EQU   X'40'               SINGLE SPACE                                 
*                                                                               
*ACGENFILE                                                                      
*ACGENMODES                                                                     
*ACREPWORKD                                                                     
*DDREPMASTD                                                                     
*DDSLRD                                                                         
*ACSALHSTD                                                                      
*ACPERCALLD                                                                     
*ACMASTD                                                                        
*ACQD                                                                           
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDSLRD                                                         
       ++INCLUDE ACSALHSTD                                                      
       ++INCLUDE ACPERCALLD                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACQD                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028ACREPCM02S05/01/02'                                      
         END                                                                    
