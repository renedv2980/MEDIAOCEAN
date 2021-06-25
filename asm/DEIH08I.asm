*          DATA SET DEIH08I    AT LEVEL 067 AS OF 06/28/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEIH08ID                                                                 
         TITLE 'ARBITRON RADIO DEMO CONVERSION'                                 
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   DATE   LVL USER   DESCRIPTION                                     *         
* -------- --- ----   ------------------------------------------------*         
* DEC09/11 058 SCHO - SUPPORT SOFT EMAIL SUBJECT                                
*                                                                               
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
DERA08I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LOCWORKL,DERA08I,RA                                              
         USING DEMCOND,R8                                                       
         USING LOCWORKD,RC                                                      
*                                                                               
         STM   RA,RB,MYBASE2                                                    
         ST    RD,BASERD                                                        
*                                                                               
         BAS   RE,DERAINIT                                                      
*                                                                               
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
*                                                                               
         B     MODES(R1)                                                        
                                                                                
                                                                                
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
         MVI   BYPREAD,0                                                        
         CLI   RELOFRST,1                                                       
         BNE   OPENOK                                                           
                                                                                
*                                                                               
         L     RE,ASTALIST                                                      
         L     RF,=A(STALISTL)     CLEAR STATION LIST                           
         XCEF                                                                   
*                                                                               
         L     R0,ASVSREC                                                       
         LHI   R1,SVSRECL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SAVE SORT RECORD AREA                  
*                                                                               
         L     R0,ASVSREC2                                                      
         LHI   R1,SVSREC2L                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR SAVE SORT RECORD AREA #2               
*                                                                               
         L     R0,ACLCTAB                                                       
         LHI   R1,CLCTABL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,ASCITAB                                                       
         LHI   R1,SCITABL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,AUNIVS                                                        
         LHI   R1,UNIVL                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR UNIVERSE BUFFER                        
*                                                                               
         L     R0,=F'1200000'      GRAB 1.2M FROM HI-CORE                       
                                                                                
         DS    0H                                                               
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF               ANY ERRORS?                                  
         BZ    *+6                 YES, CANNOT ALLOCATE SPACE                   
         DC    H'0'                                                             
         ST    R1,ATOTTBL          SAVE A(TOT TABLE)                            
         ST    R1,ATOTBKT                                                       
         A     R1,=F'1200000'                                                   
         ST    R1,ATOTMAX          MAX BUFFER ADDRESS                           
*                                                                               
         MVI   RELOFRST,0                                                       
         XC    CNTGET,CNTGET                                                    
OPENOK   EQU   *                                                                
         EJECT                                                                  
READ020  DS    0H                                                               
         L     R4,ARREC                                                         
         MVI   PASSFLT,C'Y'        PRESET FOR GOOD RECORD                       
         MVI   DPTSW,C'N'                                                       
*                                                                               
         CLI   BYPREAD,0           BYPASS READING OF RECORD                     
         BNE   READ022X                                                         
                                                                                
         DS    0H                                                               
         LR    RE,R4                                                            
         LHI   RF,PREVRECL                                                      
         L     R0,APREC                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE               SAVE RECORD AS PREVIOUS RECORD               
                                                                                
         LR    R0,R4                                                            
         LHI   R1,2000                                                          
         SR    RE,RE                                                            
         LHI   RF,X40              INSERT A BLANK INTO RF,                      
         SLL   RF,24                AND PROPAGATE TO H.O.B                      
         MVCL  R0,RE               BLANK OUT RREC BUFFER                        
                                                                                
         GET   IN1,(R4)                                                         
                                                                                
         LHI   R0,1                                                             
         A     R0,CNTGET                                                        
         ST    R0,CNTGET           UPDATE # OF RECORDS GOTTEN FROM TAPE         
READ022X EQU   *                                                                
         AHI   R4,4                                                             
                                                                                
*                                                                               
         DS    0H                  CHECK FOR EOF ON INPUT FILE                  
         CLI   BYPREAD,C'Z'                                                     
         BE    ENDJOB                                                           
         CLI   CURRTYP,RTEOFQ       IF STILL PROCESSING FOR EOF,                
         BE    MORET12               THEN GO THERE                              
                                                                                
*                                                                               
         DS    0H                  CHECK FOR RECORD-TYPE CHANGING               
         CLC   CURRTYP,0(R4)                                                    
         BE    READ023X                                                         
         BAS   RE,RELIREC                                                       
READ023X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  LOOK UP RECORD TYPE & GO TO ROUTINE          
         MVC   CURRTYP,0(R4)                                                    
         L     R1,ARTYPGOT                                                      
                                                                                
READ025  DS    0H                                                               
         CLI   0(R1),EOT           IF REACHED END OF TABLE,                     
         BNE   *+6                                                              
         DC    H'0'                 SOMETHING'S WRONG!                          
         CLC   0(1,R1),CURRTYP                                                  
         BE    *+12                                                             
         AHI   R1,L'RTYPGOTB                                                    
         B     READ025                                                          
                                                                                
         ZICM  RF,1(R1),(3)                                                     
         LA    RF,DERA08I(RF)                                                   
         BR    RF                                                               
         EJECT                                                                  
************************************************************                    
*  copyright record descriptor record                                           
************************************************************                    
RA000    DS    0H                  COPYRIGHT RECORD                             
         USING PADSECT,R4                                                       
*                                                                               
         DS    0H                                                               
         BAS   RE,CNVPERID                                                      
         MVC   MKTPERID,CURRPERD   SAVE PERIOD ID THROUGHOUT MARKET             
                                                                                
         BAS   RE,CNVMKT                                                        
*                                                                               
         DS    0H                  GET BOOK FROM PERIOD SHORT NAME              
         PACK  DUB,PAPSNAM+2(4)                                                 
         CVB   RE,DUB                                                           
         SHI   RE,1900              0 <= DDS YEAR <= 255                        
         STC   RE,CURRBOOK+0                                                    
*                                                                               
         L     R1,ABOOKEQ           SURVEY MONTH                                
RA045    DS    0H                                                               
         CLI   0(R1),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PAPSNAM(2),0(R1)                                                 
         BE    *+12                                                             
         AHI   R1,L'BOOKEQ                                                      
         B     RA045                                                            
*                                                                               
         CLC   PAPSNAM(2),=C'23'   HOLIDAY NEEDS ITS OWN BOOKTYPES              
         BNE   *+8                                                              
         MVI   HOLIDAY,1                                                        
*                                                                               
                                                                                
         MVC   CURRBOOK+1(1),2(R1)                                              
*                                                                               
         BAS   RE,CNVRPTP          CONVERT REPORT TYPE                          
*                                                                               
         MVC   STDCNDMK,PASCIND                                                 
         MVI   CURRHO,0                                                         
         MVI   CURRACTV,0                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         L     R0,ADEMQCTB                                                      
         LHI   R1,DEMQCTBL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   F1DREC,C'Y'                                                      
         B     READ020                                                          
         DROP  R4                                                               
         EJECT                                                                  
************************************************************                    
* audience characteristic record                                                
************************************************************                    
RD000    DS    0H                                                               
         USING PDDSECT,R4                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   F1DREC,C'Y'         1ST TIME ENCOUNTERING THIS RECD TYPE         
         BNE   RD009X                                                           
*                                                                               
         DS    0H                                                               
         BAS   RE,CLRDVMTX                                                      
RD009X   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                  GET CNV-DEFINE DEMO NUMBER                   
         L     R1,ADEMODEF                                                      
         LHI   R0,DEMODEFQ                                                      
*                                                                               
RD012    DS    0H                                                               
         CLC   PDDEMCD,0(R1)                                                    
         BE    *+14                                                             
         AHI   R1,L'DEMODEFN                                                    
         BCT   R0,RD012                                                         
         DC    H'0'                                                             
                                                                                
         MVC   CNVDMNUM,L'PDDEMCD(R1)                                           
                                                                                
*                                                                               
         DS    0H                  GET CNV-DEFINE QUAL CATEGORY #               
         L     R1,AQCATDEF                                                      
         LHI   R0,QCATDEFQ                                                      
*                                                                               
RD022    DS    0H                                                               
         CLC   PDQULCD,0(R1)                                                    
         BE    *+14                                                             
         AHI   R1,L'QCATDEFN                                                    
         BCT   R0,RD022                                                         
         DC    H'0'                                                             
                                                                                
         MVC   CNVQCNUM,L'PDQULCD(R1)                                           
                                                                                
*                                                                               
         DS    0H                  GET PRE-PROCESSOR DEM# AND QUAL CODE         
         PACK  DUB,PDPPDMN                                                      
         CVB   R0,DUB                                                           
         STC   R0,PPRDMNUM                                                      
         PACK  DUB,PDPPQCN                                                      
         CVB   R0,DUB                                                           
         STC   R0,PPRQCNUM                                                      
                                                                                
*                                                                               
         DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING DEMQCTBD,R3                                                      
         MVC   DQTPPDMN,PPRDMNUM                                                
         MVC   DQTPPQCN,PPRQCNUM                                                
         MVC   DQTCVDMN,CNVDMNUM                                                
         MVC   DQTCVQCN,CNVQCNUM                                                
*                                                                               
         DS    0H                                                               
         ST    R3,DMCB+00           A(RECORD)                                   
         L     RE,ADEMQCTB                                                      
         LA    RF,8(RE)                                                         
         ST    RF,DMCB+04           A(TABLE)                                    
         MVC   DMCB+08(4),0(RE)     # OF ENTRIES SO FAR                         
         LHI   R0,DEMQCTBQ                                                      
         ST    R0,DMCB+12           LENGTH OF RECORD                            
         LHI   R0,DQTKEYL                                                       
         ST    R0,DMCB+16           DISPL & LENGTH OF KEY                       
         LHI   R0,DEMQCTBM                                                      
         ST    R0,DMCB+20           MAX ENTRIES IN TABLE                        
         GOTO1 VBINSRCH,DMCB                                                    
         CLI   DMCB+0,X01          IF BINSRCH RETURNED THIS ERROR,              
         BE    *+6                                                              
         DC    H'0'                 DUPLICATE RECDS FOUND FROM TAPE             
                                                                                
         DS    0H                                                               
         ST    R3,DMCB+00          SET A(RECORD) AGAIN                          
         MVI   DMCB+00,X01                                                      
         GOTO1 VBINSRCH,DMCB                                                    
         OC    DMCB+1(3),DMCB+1    IF BINSRCH RETURNED THIS ERROR,              
         BNZ   *+6                                                              
         DC    H'0'                 OUR TABLE IS NOT BIG ENOUGH                 
         L     RE,ADEMQCTB                                                      
         MVC   0(4,RE),DMCB+08     UPDATE # OF ENTRIES IN TABLE                 
         DROP  R3                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     READ020                                                          
         DROP  R4                                                               
         EJECT                                                                  
RG000    DS    0H                  INTAB RECORD                                 
         USING PGDSECT,R4                                                       
                                                                                
*                                                                               
** PROCESS RECORD **                                                            
*                                                                               
         DS    0H                                                               
         BAS   RE,CNVPERID                                                      
         CLC   MKTPERID,CURRPERD                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,CNVMKT           GET MARKET FROM INPUT RECORD                 
         BAS   RE,FLTMKT           TEST IF MARKET PASSES FILTER                 
         CLI   PASSFLT,C'Y'                                                     
         BNE   RGRDNXT                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         ZICM  R0,CURRMKT,(3)                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CURRSTA+0(4),DUB                                                 
         OI    CURRSTA+3,X'F0'                                                  
         MVI   CURRSTA+4,C'I'      STATION CALL LETTERS                         
*                                                                               
         MVC   INTMTYP,STDCNDMK                                                 
         MVC   CURRGEO,PGGEOIND    GEOGRAPHIC INDICATOR                         
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   TMPGEO,CURRGEO                                                   
         NI    TMPGEO,X'0F'                                                     
                                                                                
         PACK  DUB,PGPPQCN                                                      
         CVB   R0,DUB                                                           
         STC   R0,PPRQCNUM         QUALIFIER CODE                               
                                                                                
*                                                                               
         DS    0H                                                               
         L     RF,ARREC                                                         
         ZICM  RE,0(RF),(3)                                                     
         LA    R7,0(RE,RF)                                                      
         BCTR  R7,0                R3-->LAST BYTE OF RECORD                     
         LHI   R6,L'PGVAL          R6 = INCREMENT                               
         LA    R5,PGVAL            R5-->INTAB VALUES                            
         SR    R0,R0               R0 TO STORE DEMO NUMBERS                     
*                                                                               
RG042    DS    0H                                                               
         AHI   R0,1                BUMP TO NEXT DEMO NUMBER                     
         STC   R0,PPRDMNUM                                                      
*                                                                               
         CLC   0(L'PGVAL,R5),MYSPACES                                           
         BE    RG048                                                            
*                                                                               
         DS    0H                                                               
         BAS   RE,GDNQC            GET DEMO NUM & QUAL CTGRY #                  
                                                                                
         PACK  DUB,0(L'PGVAL,R5)                                                
         CVB   RE,DUB                                                           
         ST    RE,TMPDMVAL                                                      
         BAS   RE,SEEDMVAL                                                      
*                                                                               
RG048    DS    0H                  BUMP TO NEXT IN-TAB VALUE                    
         BXLE  R5,R6,RG042                                                      
         DROP  R4                                                               
                                                                                
*                                                                               
RGRDNXT  DS    0H                                                               
         B     READ020                                                          
         EJECT                                                                  
RH000    DS    0H                  POPULATION RECORD                            
         USING PHDSECT,R4                                                       
                                                                                
*                                                                               
** PROCESS RECORD **                                                            
*                                                                               
         DS    0H                                                               
         BAS   RE,CNVPERID                                                      
         CLC   MKTPERID,CURRPERD                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,CNVMKT           GET MARKET FROM INPUT RECORD                 
         BAS   RE,FLTMKT           TEST IF MARKET PASSES FILTER                 
         CLI   PASSFLT,C'Y'                                                     
         BNE   RHRDNXT                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         ZICM  R0,CURRMKT,(3)                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CURRSTA+0(4),DUB                                                 
         OI    CURRSTA+3,X'F0'                                                  
         MVI   CURRSTA+4,C'U'      STATION CALL LETTERS                         
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   INTMTYP,STDCNDMK                                                 
         MVC   CURRGEO,PHGEOIND    GEOGRAPHIC INDICATOR                         
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   TMPGEO,CURRGEO                                                   
         NI    TMPGEO,X'0F'                                                     
         PACK  DUB,PHPPQCN                                                      
         CVB   R0,DUB                                                           
         STC   R0,PPRQCNUM         QUALITATIVE CATEGORY CODE                    
                                                                                
*                                                                               
         DS    0H                                                               
         L     RF,ARREC                                                         
         ZICM  RE,0(RF),(3)                                                     
         LA    R7,0(RE,RF)                                                      
         BCTR  R7,0                R3-->LAST BYTE OF RECORD                     
         LHI   R6,L'PHWTPOP        R6 = INCREMENT                               
         LA    R5,PHWTPOP          R5-->POPULATION VALUES                       
         SR    R0,R0               R0 TO STORE DEMO NUMBERS                     
*                                                                               
RH042    DS    0H                                                               
         AHI   R0,1                BUMP TO NEXT DEMO NUMBER                     
         STC   R0,PPRDMNUM                                                      
*                                                                               
         CLC   0(L'PHWTPOP,R5),MYSPACES                                         
         BE    RH048                                                            
*                                                                               
         DS    0H                                                               
         BAS   RE,GDNQC            GET DEMO NUM & QUAL CTGRY #                  
                                                                                
         PACK  DUB,0(L'PHWTPOP,R5)                                              
         CVB   RE,DUB                                                           
         MHI   RE,HUNDREDQ                                                      
         ST    RE,TMPDMVAL                                                      
         BAS   RE,SEEDMVAL                                                      
*                                                                               
RH048    DS    0H                  BUMP TO NEXT POPULATION VALUE                
         BXLE  R5,R6,RH042                                                      
         DROP  R4                                                               
                                                                                
*                                                                               
RHRDNXT  DS    0H                                                               
         B     READ020                                                          
         EJECT                                                                  
RJ000    DS    0H                  STATION COMBO RECORD (J)                     
         USING PJDSECT,R4                                                       
*^^test                                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         CLC   PJSCMBTP,=C' 1'     ONLY WANT INDIVIDUAL STATIONS                
         BE    RJ020                                                            
*        CLC   PJSCMBTP,=C' 8'      AND MARKET TOTALS                           
*        BE    RJ020                                                            
         CLC   PJSCMBTP,=C' 9'      PUMM - TOTAL MARKET LISTING                 
         BE    RJ020                                                            
         CLC   PJSCMBTP,=C' 5'      IHEART COMBO                                
         BE    RJ020                                                            
         B     RJRDNXT                                                          
*^^eotest                                                                       
                                                                                
*                                                                               
** PROCESS RECORD **                                                            
*                                                                               
RJ020    DS    0H                                                               
         MVI   NEWSTTN,C'N'        ASSUME NOT A NEW STTN COMBO ID               
         MVC   TMPCLCIN,PJCLCIND   HOLD ONTO CALL-LETTER-CHNG INDICATOR         
                                                                                
*                                                                               
         DS    0H                                                               
         BAS   RE,CNVPERID                                                      
         CLC   MKTPERID,CURRPERD                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,CNVMKT           GET MARKET FROM INPUT RECORD                 
         BAS   RE,FLTMKT           TEST IF MARKET PASSES FILTER                 
         CLI   PASSFLT,C'Y'                                                     
         BNE   RJRDNXT                                                          
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   CURRPTYP,PJRPTYPE                                                
         MVC   CURRGEO,PJGEOIND                                                 
*                                                                               
         PACK  DUB,PJSCMBTP        STATION COMBO TYPE                           
         CVB   R0,DUB                                                           
         STCM  R0,1,CURRSCTY                                                    
                                                                                
         PACK  DUB,PJSCMBID        STATION COMBO ID                             
         CVB   R0,DUB                                                           
         STCM  R0,7,CURRSCID                                                    
*                                                                               
         MVC   CURRSTA+0(4),PJSTTN                                              
         MVC   CURRSTA+4(1),PJBAND                                              
                                                                                
         CLC   PJBAND,=C'AF'       AM+FM RECORD                                 
         BNE   *+8                                                              
         MVI   CURRSTA+4,C'C'       MAKE IT A COMBO                             
         CLC   PJBAND,=C'AA'       AM+AM RECORD                                 
         BNE   *+8                                                              
         MVI   CURRSTA+4,C'B'       MAKE IT A COMBO                             
         CLC   PJBAND,=C'FF'       FM+FM RECORD                                 
         BNE   *+8                                                              
         MVI   CURRSTA+4,C'D'       MAKE IT A COMBO                             
                                                                                
         CLC   =C'TOTA',CURRSTA                                                 
         BE    *+14                                                             
         CLC   =C'PUMM',CURRSTA     PPM'S VERSION OF MKT TOTAL                  
         BNE   RJ052X                                                           
         ZICM  R0,CURRMKT,(3)                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CURRSTA(4),DUB                                                   
***      MVI   CURRSTA+4,C'A'                                                   
         MVI   CURRSTA+4,C'C'                                                   
RJ052X   EQU   *                                                                
*                                                                               
         CLC   PJSCMBTP,=C' 5'      IHEART COMBO                                
         BNE   *+8                                                              
         MVI   CURRSTA+4,C'C'       MAKE IT A COMBO                             
*                                                                               
*                                                                               
         MVC   CURRACTV,PJACTCD    ACTIVITY CODE                                
         MVC   CURRHO,PJHOMOUT     HOME/OUTSIDE INDICATOR                       
                                                                                
*                                                                               
** PUT STATION IN STATION LIST **                                               
*                                                                               
         DS    0H                                                               
         LHI   R0,MXSTTNS                                                       
         L     R1,ASTALIST                                                      
         USING STALISTD,R1                                                      
                                                                                
RJ102    DS    0H                                                               
         OC    SLMKT,SLMKT         ADD ENTRY AT END OF TABLE                    
         BZ    RJ102X                                                           
         CLC   SLMKT,CURRMKT                                                    
         BNE   *+10                                                             
         CLC   SLSCMBTY,PJSCMBTP                                                
         BNE   *+10                                                             
         CLC   SLSCMBID,PJSCMBID                                                
         BE    RJ105X                                                           
         AHI   R1,L'STALIST                                                     
         BCT   R0,RJ102                                                         
         DC    H'0'                                                             
RJ102X   EQU   *                                                                
*                                                                               
         CLI   RLSIFLG,0           HAVE WE RELEASED MKT RECD?                   
         BNE   RJ104X                                                           
         MVI   RLSIFLG,RIFMKT       NOPE, DO IT NOW                             
         BAS   RE,RELIREC                                                       
RJ104X   EQU   *                                                                
*                                                                               
         DS    0H                                                               
         CLI   RLSIFLG,RIFMKT      MAKE SURE THAT WE'RE WHERE WE'RE S/B         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RLSIFLG,0                                                        
         MVI   BYPREAD,0                                                        
         MVC   SLMKT,CURRMKT                                                    
         MVC   SLSTTN,CURRSTA                                                   
         MVC   SLSCMBTY,PJSCMBTP                                                
         MVC   SLSCMBID,PJSCMBID                                                
         MVC   SLACTCD,PJACTCD                                                  
         MVC   SLHOMOUT,PJHOMOUT                                                
         CLC   PJSCMBTP,=C' 9'     IF PUMM TOTALS,                              
         BNE   *+8                                                              
         MVI   SLHOMOUT,C'0'        FORCE INDICATOR "HOME"                      
RJ105X   EQU   *                                                                
         DROP  R1                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         TM    CURRSTA,X'F0'                                                    
         BO    RJ107X                                                           
*                                                                               
         DS    0H                                                               
         BAS   RE,LKSTNID          LOOK UP STATION COMBO ID ON FILE             
         LA    RF,TPKEY                                                         
         USING DSEKEY,RF                                                        
         CLC   CURRSTA,DSESCALL                                                 
         BE    RJ107G                                                           
         CLI   NEWSTTN,C'Y'        SKIP CHECK IF NEW STTN COMBO ID              
         BE    *+8                                                              
*&&DO                                                                           
         CLI   PJCLCIND,C'+'       OUR CHECK FOR STTN CALL LTTR CHG             
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         CLI   TMPCLCIN,C'+'       OUR CHECK FOR STTN CALL LTTR CHG             
         BE    *+8                                                              
         MVI   TMPCLCIN,C'+'        FUDGE IT IN IF IT'S NOT SET                 
RJ107G   EQU   *                                                                
                                                                                
         MVC   OLDCALL,TPKEY+(DSESCALL-DSEKEY)                                  
         DROP  RF                                                               
RJ107X   EQU   *                                                                
                                                                                
*                                                                               
** HANDLE NEW STATION COMBO IDs **                                              
*                                                                               
         DS    0H                                                               
         CLI   NEWSTTN,C'Y'                                                     
         BNE   RJNEWX                                                           
*                                                                               
         DS    0H                                                               
         OC    CURRSTA,CURRSTA      IF STTN'S CALL LETTERS ARE NULLS,           
         BZ    RJNEWX                                                           
         CLC   CURRSTA,MYSPACES      OR BLANKS,                                 
         BE    RJNEWX                 DON'T CREATE RECORD                       
                                                                                
*                                                                               
         DS    0H                                                               
         LHI   R0,MXSCID                                                        
         L     R3,ASCITAB                                                       
         USING SCILISTD,R3                                                      
                                                                                
RJNEW012 DS    0H                                                               
         OC    0(L'SCITAB,R3),0(R3)  ADD ENTRY AT END OF TABLE                  
         BZ    RJNEW019                                                         
         CLC   SITYPE,CURRSCTY        ENTRY FOR STTN ALREADY EXISTS?            
         BNE   *+10                                                             
         CLC   SIID,CURRSCID          ENTRY FOR STTN ALREADY EXISTS?            
         BE    RJNEWX                                                           
         AHI   R3,L'SCITAB                                                      
         BCT   R0,RJNEW012                                                      
         DC    H'0'                                                             
RJNEW019 EQU   *                                                                
*                                                                               
         CLI   RLSIFLG,0           HAVE WE RELEASE STTN EQIV RECD?              
         BNE   RJNEW023                                                         
         MVI   RLSIFLG,RIFSER3                                                  
         BAS   RE,RELIREC                                                       
RJNEW023 EQU   *                                                                
*                                                                               
         CLI   RLSIFLG,RIFSER3     DID WE JUST RELEASE STTN EQIV RECD?          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RLSIFLG,0                                                        
         MVI   BYPREAD,0                                                        
*                                                                               
         MVC   SITYPE,CURRSCTY                                                  
         MVC   SIID,CURRSCID                                                    
         DROP  R3                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         L     RE,VCPRINT                                                       
         USING DPRINT,RE                                                        
         MVC   P+00(23),=C'NEW STATION COMBO ID: "'                             
         ZICM  R1,CURRSCTY,1                                                    
         EDIT  (R1),(2,P+23),ZERO=NOBLANK                                       
         ZICM  R1,CURRSCID,3                                                    
         EDIT  (R1),(6,P+25),ZERO=NOBLANK                                       
         MVI   P+31,C'"'                                                        
         MVC   P+33(05),CURRSTA                                                 
         DROP  RE                                                               
         GOTO1 VPRINTER                                                         
RJNEWX   EQU   *                                                                
                                                                                
*                                                                               
** HANDLE STATION CALL LETTER CHANGE **                                         
*                                                                               
         DS    0H                                                               
*&&DO                                                                           
         CLI   PJCLCIND,C'+'                                                    
*&&                                                                             
         CLI   TMPCLCIN,C'+'                                                    
         BNE   RJCLCX                                                           
*                                                                               
         DS    0H                                                               
         OC    OLDCALL,OLDCALL      IF ANY CALL LETTERS ARE NULLS,              
         BZ    RJCLCX                                                           
         CLC   OLDCALL,MYSPACES      OR BLANKS,                                 
         BE    RJCLCX                                                           
         CLC   OLDCALL,CURRSTA       OR OLD=NEW (ADDED W/ PREV MKT),            
         BE    RJCLCX                                                           
         OC    CURRSTA,CURRSTA        DON'T CREATE EQUIV RECORDS                
         BZ    RJCLCX                                                           
         CLC   CURRSTA,MYSPACES                                                 
         BE    RJCLCX                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         LHI   R0,MXCLCS                                                        
         L     R3,ACLCTAB                                                       
                                                                                
RJCLC012 DS    0H                                                               
         CLI   0(R3),0             ADD ENTRY AT END OF TABLE                    
         BE    RJCLC019                                                         
         CLC   0(6,R3),PJSTTN      ENTRY FOR STATION ALREADY EXISTS             
         BE    RJCLCX                                                           
         AHI   R3,L'CLCTAB                                                      
         BCT   R0,RJCLC012                                                      
         DC    H'0'                                                             
RJCLC019 EQU   *                                                                
*                                                                               
         CLI   RLSIFLG,0           HAVE WE RELEASE STTN EQIV RECD?              
         BNE   RJCLC023                                                         
         MVI   RLSIFLG,RIFSER1                                                  
         BAS   RE,RELIREC                                                       
RJCLC023 EQU   *                                                                
*                                                                               
         CLI   RLSIFLG,RIFSER1     DID WE JUST RELEASE STTN EQIV RECD?          
         BNE   RJCLC025                                                         
         MVI   RLSIFLG,RIFSER2                                                  
         BAS   RE,RELIREC                                                       
RJCLC025 EQU   *                                                                
*                                                                               
         CLI   RLSIFLG,RIFSER2     DID WE JUST RELEASE STTN EQIV RECD?          
         BNE   RJCLC027                                                         
         MVI   RLSIFLG,RIFSER3                                                  
         BAS   RE,RELIREC                                                       
RJCLC027 EQU   *                                                                
*                                                                               
         CLI   RLSIFLG,RIFSER3     DID WE JUST RELEASE STTN EQIV RECD?          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RLSIFLG,0                                                        
         MVI   BYPREAD,0                                                        
*                                                                               
         MVC   0(4,R3),PJSTTN                                                   
         MVC   4(2,R3),PJBAND                                                   
         MVC   6(5,R3),OLDCALL                                                  
                                                                                
*                                                                               
         DS    0H                                                               
         L     RE,VCPRINT                                                       
         USING DPRINT,RE                                                        
         MVC   P+00(30),=C'STATION CALL LETTER CHANGE TO:'                      
         MVC   P+31(6),0(R3)                                                    
         MVC   P+38(5),=C'FROM:'                                                
         MVC   P+44(5),OLDCALL                                                  
         DROP  RE                                                               
         GOTO1 VPRINTER                                                         
RJCLCX   EQU   *                                                                
*                                                                               
RJ199    EQU   *                                                                
*                                                                               
         DROP  R4                                                               
                                                                                
*                                                                               
RJRDNXT  DS    0H                                                               
         B     READ020                                                          
         EJECT                                                                  
RM000    DS    0H                  STATION WITHIN COMBO RECORD (M)              
         B     READ020                                                          
         EJECT                                                                  
RP000    DS    0H                  NETWORK AFFILIATION RECORD (P)               
         B     READ020                                                          
RS000    DS    0H                  DAYPART RECORD (S)                           
         BAS   RE,CNVDT                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         B     READ020                                                          
         EJECT                                                                  
RV000    DS    0H                  AUDIENCE ESTIMATE RECORD (V)                 
         USING PVDSECT,R4                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   BYPREAD,0           DID WE JUST RELEASE AN IREC?                 
         BE    *+12                                                             
         MVI   BYPREAD,0            YES, SET TO READ NEXT RECORD                
         B     RVRDNXT                                                          
*^^test                                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         CLC   PVSCMBTP,=C' 1'     ONLY WANT INDIVIDUAL STATIONS                
         BE    RV004X                                                           
         CLC   PVSCMBTP,=C' 9'      AND MARKET TOTALS                           
         BE    RV004X                                                           
         CLC   PVSCMBTP,=C' 5'      IHEART COMBO                                
         BE    RV004X                                                           
         B     RVRDNXT                                                          
RV004X   EQU   *                                                                
*^^eotest                                                                       
*^^test                                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         CLC   PVPPQCN,=C'01'      ONLY WANT THIS QUALITATIVE CTGRY             
         BNE   RVRDNXT                                                          
*^^eotest                                                                       
                                                                                
*                                                                               
** PROCESS RECORD **                                                            
*                                                                               
         DS    0H                                                               
         BAS   RE,CNVMKT           GET MARKET FROM INPUT RECORD                 
         BAS   RE,FLTMKT           TEST IF MARKET PASSES FILTER                 
         CLI   PASSFLT,C'Y'                                                     
         BNE   RVRDNXT                                                          
*                                                                               
         DS    0H                                                               
         BAS   RE,CNVSTTN                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
* ONLY WANT IHEART OWNERS                                                       
         TM    CURRSTA,X'F0'       IF MARKET TOTALS,                            
         BO    *+14                                                             
         CLC   =C'IH',CURRSTA                                                   
         BNE   RVRDNXT                                                          
                                                                                
         TM    CURRSTA,X'F0'       IF MARKET TOTALS,                            
         BO    *+8                  SKIP FILTER TEST                            
         BAS   RE,FLTSTTN          TEST IF MARKET PASSES FILTER                 
                                                                                
         CLI   PASSFLT,C'Y'                                                     
         BNE   RVRDNXT                                                          
*                                                                               
         DS    0H                                                               
         BAS   RE,CNVDT                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CURRGEO,PVGEOIND                                                 
         MVC   TMPGEO,CURRGEO                                                   
         NI    TMPGEO,X'0F'                                                     
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   INTDAY,CURRDAY                                                   
         MVC   INTPNAM,CURRPNAM                                                 
         MVC   INTSQH,CURRSQH                                                   
         MVC   INTEQH,CURREQH                                                   
         MVI   INTWEEKS,X'0F'                                                   
                                                                                
         MVC   INTAQH,PVESTYPE                                                  
         MVC   INTDPT,PVDPTID+1                                                 
         MVC   INTLISN,PVLSNLOC                                                 
         MVC   INTPNO+0(1),PVAIRTIM                                             
         MVC   INTPNO+1(1),CURRACTV                                             
*^^FIXARB                                                                       
* THESE INSTRUCTIONS ARE HERE TO FIX ARBITRON'S SPRING 2000 SURVEY.             
*  FOR SOME MARKETS, THEY INADVERTENTLY STUCK AN "N" IN THE STATION             
*  ACTIVITY CODE FIELD.  THESE INSTRUCTIONS REPLACES THE "N" WITH               
*  A BLANK.                                                                     
         CLI   INTPNO+1,C'N'                                                    
         BNE   *+8                                                              
         MVI   INTPNO+1,C' '                                                    
*^^EOFIXARB                                                                     
         CLI   INTPNO+1,C'B'       IF LISTED BELOW LINE IN RMR,                 
         BNE   *+8                                                              
         MVI   INTPNO+1,C'Z'        USE THE "Z" CODE INSTEAD                    
                                                                                
*                                                                               
         DS    0H                  SEED AUDIENCE ESTIMATES                      
         PACK  DUB,PVPPQCN                                                      
         CVB   R0,DUB                                                           
         STC   R0,PPRQCNUM          QUALITATIVE CATEGORY CODE                   
*                                                                               
         DS    0H                                                               
         L     RF,ARREC                                                         
         ZICM  RE,0(RF),(3)                                                     
         LA    R7,0(RE,RF)                                                      
         BCTR  R7,0                R7-->LAST BYTE OF RECORD                     
         LHI   R6,L'PVPRJCTN       R6 = INCREMENT                               
         LA    R5,PVPRJCTN         R5-->PROJECTION VALUES                       
         SR    R0,R0               R0 TO STORE DEMO NUMBERS                     
*                                                                               
RV042    DS    0H                                                               
         AHI   R0,1                BUMP TO NEXT DEMO NUMBER                     
         STC   R0,PPRDMNUM                                                      
*                                                                               
         CLC   0(L'PVPRJCTN,R5),MYSPACES                                        
         BE    RV048                                                            
*                                                                               
         DS    0H                                                               
         BAS   RE,GDNQC            GET DEMO NUM & QUAL CTGRY #                  
                                                                                
         PACK  DUB,0(L'PVPRJCTN,R5)                                             
         CVB   RE,DUB                                                           
         ST    RE,TMPDMVAL                                                      
         BAS   RE,SEEDMVAL                                                      
*                                                                               
RV048    DS    0H                  BUMP TO NEXT POPULATION VALUE                
         BXLE  R5,R6,RV042                                                      
*                                                                               
         DS    0H                                                               
         BAS   RE,BLDIREC          BUILD INTERIM RECORD                         
                                                                                
*                                                                               
         DS    0H                                                               
         TM    INTSTA,X'F0'        MARKET TOTALS: SAVE AWAY TO LATER            
         BNO   RV054X              SLOT INTO RECORD IN SORT  (CNVW..)           
*                                                                               
         LA    RE,*+10             SWITCH INTO 31-BIT                           
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         L     R1,ATOTBKT          NEXT AVAIL BUCKET FOR TOT RECD               
         USING TOTBUFD,R1                                                       
         MVC   TBMKT,INTMRKT        KEY FIELD: MARKET                           
         MVC   TBGEO,INTGEO         KEY FIELD: GEO INDICATOR                    
         MVC   TBAQH,INTAQH         KEY FIELD: ESTIMATE TYPE                    
         MVC   TBDPT,INTDPT         KEY FIELD: DAYPART                          
         MVC   TBLISN,INTLISN       KEY FIELD: LISTENING LOCATION               
                                                                                
         MVC   TBDEMS,IBLK1        SAVE DEMOS                                   
*                                                                               
         AHI   R1,TOTBUFQ          BUMP TO NEXT AVAIL SLOT                      
         C     R1,ATOTMAX          OVERFLOW ADDRESS OF TOTAL'S BUFFER           
         BL    *+6                                                              
         DC    H'0'                TOTAL'S BUFFER OVERFLOW                      
                                                                                
         XC    0(TOTBUFQ,R1),0(R1) CLEAR BUFFER AREA FOR NEXT TOT RECD          
         ST    R1,ATOTBKT                                                       
         DROP  R1                                                               
*                                                                               
         LA    RE,*+6                                                           
         BSM   0,RE                SWITCH OUT OF 31-BIT MODE                    
RV054X   EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   PVLSNLOC,C'3'       AT WORK (==> AWAY FROM HOME)                 
         BE    RVAFH00                                                          
         CLI   PVLSNLOC,C'4'       IN CAR (==> AWAY FROM HOME)                  
         BE    RVAFH00                                                          
         CLI   PVESTYPE,C'3'       EXCLUSIVE CUME                               
         BE    RVEXC00                                                          
         CLI   PVESTYPE,C'2'       CUME                                         
         BE    RVCUM00                                                          
         CLI   PVESTYPE,C'1'       AQH                                          
         BE    RVAQH00                                                          
         B     RVRDNXT                                                          
*                                                                               
RVAFH00  DS    0H                                                               
         MVC   OPAHIMP,IBLK1                                                    
         TM    INTSTA,X'F0'                                                     
         BNO   *+10                                                             
         MVC   OATOT,IBLK1                                                      
         XC    IBLK1(LN),IBLK1                                                  
         B     RV090                                                            
*                                                                               
RVEXC00  DS    0H                  ESTIMATE TYPE = EXCLUSIVE CUME               
         MVC   OEXCL,IBLK1                                                      
         XC    IBLK1(LN),IBLK1                                                  
         B     RV090                                                            
*                                                                               
RVCUM00  DS    0H                  ESTIMATE TYPE = CUME                         
         MVC   OCUME,IBLK1                                                      
         TM    INTSTA,X'F0'                                                     
         BNO   *+10                                                             
         MVC   OCTOT,IBLK1                                                      
         XC    IBLK1(LN),IBLK1                                                  
         B     RV090                                                            
*                                                                               
RVAQH00  DS    0H                  ESTIMATE TYPE = AQH                          
         TM    INTSTA,X'F0'                                                     
         BNO   *+10                                                             
         MVC   OTOT,IBLK1                                                       
         B     RV090                                                            
                                                                                
*                                                                               
RV090    DS    0H                                                               
         BAS   RE,RELIREC                                                       
         DC    H'0'                                                             
         DROP  R4                                                               
                                                                                
*                                                                               
RVRDNXT  DS    0H                                                               
         B     READ020                                                          
         EJECT                                                                  
MORET    DS    0H                  END OF INPUT DATA                            
         MVI   CURRTYP,RTEOFQ                                                   
         MVI   CURRSTA,XFF                                                      
                                                                                
*                                                                               
** RELEASE "EOF" MARKERS **                                                     
*                                                                               
MORET12  DS    0H                                                               
         CLI   RLSIFLG,0                                                        
         BNE   MORET12X                                                         
                                                                                
         MVI   DPTSW,C'Y'                                                       
         MVI   RLSIFLG,RIFEOFD     EOF MARKER FOR DAYPART RECORDS               
         BAS   RE,RELIREC                                                       
         DC    H'0'                SHOULDN'T HAVE RETURNED                      
MORET12X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   RLSIFLG,RIFEOFD                                                  
         BNE   MORET14X                                                         
                                                                                
         MVI   DPTSW,C'N'                                                       
         MVI   RLSIFLG,RIFEOFR     EOF MARKER FOR RATINGS RECORDS               
         BAS   RE,RELIREC                                                       
         DC    H'0'                SHOULDN'T HAVE RETURNED                      
MORET14X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   RLSIFLG,RIFEOFR                                                  
         BNE   MORET16X                                                         
                                                                                
         MVI   DPTSW,C'Y'                                                       
         MVI   RLSIFLG,RIFEOFD2    EOF MARKER #2 FOR DAYPART RECORDS            
         MVI   CURRSTA+1,RTEOFQ                                                 
         BAS   RE,RELIREC                                                       
         DC    H'0'                SHOULDN'T HAVE RETURNED                      
MORET16X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   RLSIFLG,RIFEOFD2                                                 
         BNE   MORET18X                                                         
                                                                                
         MVI   DPTSW,C'N'                                                       
         MVI   RLSIFLG,RIFEOFR2    EOF MARKER #2 FOR RATINGS RECORDS            
         BAS   RE,RELIREC                                                       
         DC    H'0'                SHOULDN'T HAVE RETURNED                      
MORET18X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   RLSIFLG,RIFEOFR2                                                 
         BNE   MORET19G                                                         
                                                                                
         MVI   RLSIFLG,0                                                        
         B     MORET19X                                                         
MORET19G EQU   *                                                                
         DC    H'0'                                                             
                                                                                
*                                                                               
MORET19X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         LA    RE,INTERD            POINT TO AIREC                              
         LHI   RF,80                                                            
         XCEF                                                                   
                                                                                
         MVI   INTKEY,C'Z'         EOF RECD TO RELS LAST SORT RECD              
         MVI   INTRTYP,C'Z'         IN CNVWR ROUTINE                            
         MVI   BYPREAD,C'Z'        EOF                                          
         DROP  R2                                                               
         J     EXITE                                                            
                                                                                
*                                                                               
ENDJOB   DS    0H                                                               
         CLOSE (IN1,REWIND)                                                     
         MVI   BYPREAD,0                                                        
         MVI   INTAPESW,X'02'                                                   
         J     EXITE                                                            
         EJECT                                                                  
**********************************************************************          
*SETKEY -  CREATE SORT KEY                                                      
**********************************************************************          
SETKEY   NTR1                                                                   
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
         LA    R6,4(R2)                                                         
         USING DRKEY,R6                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   DRCODE,DRCODEQU                                                  
         CLI   DPTSW,C'Y'                                                       
         BNE   *+8                                                              
         MVI   DRCODE,C'D'        SET UP FOR DAYPART RECORD                     
         MVI   INTRTYP,C'R'                                                     
                                                                                
         CLI   RLSIFLG,RIFSER1                                                  
         BL    SKCOD04X                                                         
         CLI   RLSIFLG,RIFSER3                                                  
         BH    SKCOD04X                                                         
         MVI   DRCODE,DSECDEQU    SET UP FOR STTN EQUIV RECORD                  
         MVI   INTRTYP,DSECDEQU                                                 
SKCOD04X EQU   *                                                                
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   DRMEDIA,C'R'                                                     
***      MVI   DRSRC,C'A'                                                       
         MVI   DRSRC,C'H'                                                       
*                                                                               
         CLI   RLSIFLG,RIFSER1                                                  
         BE    SKSER1                                                           
         CLI   RLSIFLG,RIFSER2                                                  
         BE    SKSER2                                                           
         CLI   RLSIFLG,RIFSER3                                                  
         BE    SKSER3                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   DRSTAT,CURRSTA                                                   
         MVC   DRKMKT,CURRMKT                                                   
*                                                                               
         MVI   DRHOME,C'H'         SET FOR HOME                                 
         TM    INTSTYP,X'20'                                                    
         BZ    *+8                                                              
         MVI   DRHOME,C'S'         MAKE IT SPILL IF REQUIRED                    
*                                                                               
         MVC   DRSTYP,INTSTYP      SET STATION TYPE IN KEY                      
         MVI   INTSTYP,0           SET STATION TYPE TO ZERO FOR OUTPUT          
*                                                                               
         MVC   DRBOOK,CURRBOOK                                                  
         BAS   RE,GETBTYP          SET BOOK TYPE FOR ETHNICS                    
         BAS   RE,MSGREC                                                        
         MVC   DRBTYP,INTBTYP                                                   
*                                                                               
         CLI   RLSIFLG,RIFMKT      RELEASING MARKET?                            
         BE    SETKEY3                                                          
                                                                                
         MVC   DRHIGHD,INTDAY                                                   
         MVC   DRHIQHR,INTSQH                                                   
         MVC   DRHIQHR+1(1),TMPGEO                                              
         B     SETKEYX                                                          
                                                                                
SETKEY3  DS    0H                                                               
         MVI   DRCODE,BSCODEQU                                                  
         MVI   INTRTYP,C'M'                                                     
         B     SETKEYX                                                          
         DROP  R6                                                               
         EJECT                                                                  
         USING DSEKEY,R6                                                        
                                                                                
*                                                                               
SKSER1   DS    0H                  BUILD STTN EQIV REC W/ BOOK LOW              
         MVI   DSEIND,DSEIBKLW                                                  
         MVC   DSEOLDCL,OLDCALL     PREV STATION CALL LETTERS                   
         MVC   DSENEWCL,CURRSTA     NEW  STATION CALL LETTERS                   
         MVC   DSEEFFBK,CURRBOOK                                                
         XC    DSEEFFBK,=X'FFFF'                                                
         B     SETKEYX                                                          
                                                                                
*                                                                               
SKSER2   DS    0H                  BUILD STTN EQIV REC W/ BOOK HIGH             
         MVI   DSEIND,DSEIBKHI                                                  
         MVC   DSEBKEFF,CURRBOOK                                                
         XC    DSEBKEFF,=X'FFFF'                                                
         MVC   DSECLOLD,OLDCALL     PREV STATION CALL LETTERS                   
         MVC   DSECLNEW,CURRSTA                                                 
         B     SETKEYX                                                          
                                                                                
*                                                                               
SKSER3   DS    0H                  BUILD STTN EQIV REC W/ STTN ID               
         MVI   DSEIND,DSEISTID                                                  
         MVC   DSESTNTY,CURRSCTY                                                
         MVC   DSESTNID,CURRSCID                                                
         MVC   DSESBK,CURRBOOK                                                  
         XC    DSESBK,=X'FFFF'                                                  
         MVC   DSESCALL,CURRSTA                                                 
         B     SETKEYX                                                          
         DROP  R2,R6                                                            
                                                                                
*                                                                               
SETKEYX  DS    0H                                                               
         J     EXITE                                                            
         EJECT                                                                  
**********************************************************************          
*CNVWR - SORT PROCESSING. SLOT UNIVS AND TOT INTO APPROPRIATE RECDS.            
**********************************************************************          
CNVWR    DS    0H                                                               
         L     R2,ASREC            SET TO SORT RECORD                           
         USING INTERD,R2                                                        
*                                                                               
         L     R6,ASREC                                                         
         AHI   R6,4                                                             
         USING DRKEY,R6                                                         
*                                                                               
         DS    0H                                                               
         CLI   0(R6),C'Z'          LAST TIME HOOK                               
         BE    CNVLST               RELEASE LAST RECD IN SAVED BUFFER           
         CLI   0(R6),C'D'          DAYPART SECTION                              
         BE    CNVWR05X                                                         
         CLI   0(R6),C'R'          HOUR BY HOUR SECTION                         
         BE    CNVWR05X                                                         
         B     EXIT                                                             
CNVWR05X EQU   *                                                                
*                                                                               
         DS    0H                                                               
         CLC   DRKMKT,PREVMKT                                                   
         BNE   GOODREC                                                          
         CLC   DRSTAT(5),PREVSTAT  SAME STATION?                                
         BNE   GOODREC              NO - CANNOT BE PARENT OF S1                 
         TM    INTSTA,X'F0'        TEST MARKET TOTAL                            
         BO    GOODREC                                                          
         CLC   DRSTYP,PREVSTYP                                                  
         BE    GOODREC                                                          
         CLI   PREVSTYP,1          IF PREV NOT P+S1 MUST BE OK                  
         BNE   GOODREC                                                          
         CLI   DRSTYP,2            IS IT A PARENT                               
         BNE   GOODREC             NO - CANNOT HAVE S1                          
         MVI   INTRTYP,0           YES PURGE IF PARENT OF P+S1                  
         MVI   INTKEY,C'B'                                                      
         B     EXIT                                                             
                                                                                
*                                                                               
GOODREC  DS    0H                                                               
         BAS   RE,STOTUNV          SLOT DPT TOTS & UNVS INTO SREC               
         MVC   PREVSTYP,DRSTYP                                                  
         MVC   PREVSTAT,DRSTAT                                                  
         MVC   PREVMKT,DRKMKT                                                   
         OI    INTWEEKS,B'00100000' FORCE TYPICAL SWITCH                        
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
** UPDATE SAVED STORAGES WITH CURRENT RECORD **                                 
*                                                                               
         DS    0H                                                               
         L     R0,ASVSREC2                                                      
         LHI   R1,SVSREC2L                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR AREA TO SAVE CURRENT RECORD            
*                                                                               
         DS    0H                                                               
         L     R0,ASVSREC2                                                      
         ZICM  R1,INTRECLN,(3)                                                  
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE CURRENT RECORD                          
                                                                                
*                                                                               
         DS    0H                                                               
         L     RE,ASVSREC                                                       
         OC    0(4,RE),0(RE)       ANYTHING IN PREVIOUS RECORD?                 
         BNZ   CNVWR22              YEP                                         
                                                                                
*                                                                               
*** INITIALIZE SAVE-STORAGE FOR PREVIOUS RECORD ***                             
*                                                                               
         DS    0H                                                               
         ZICM  RF,INTRECLN,(3)                                                  
         LR    R0,R2                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
                                                                                
         MVI   INTRTYP,0                                                        
         MVI   INTKEY,C'B'                                                      
         B     EXIT                                                             
                                                                                
*                                                                               
*** UPDATE SAVE-STORAGE FOR PREVIOUS RECORD ***                                 
*                                                                               
CNVWR22  DS    0H                                                               
         L     R4,ASVSREC                                                       
         CLC   INTKEY,4(R4)        CURRENT KEY MATCH PREVIOUS KEY?              
         BNE   CNVWR40              NOPE                                        
*                                                                               
         CLC   0(2,R4),INTRECLN                                                 
         BH    *+10                                                             
         MVC   0(2,R4),INTRECLN    SET MAX LENGTH                               
*                                                                               
                                                                                
         DS    0H                                                               
         L     RE,ASVSREC2                                                      
         AHI   RE,(INTACCS-INTERD)                                              
         L     R4,ASVSREC                                                       
         AHI   R4,(INTACCS-INTERD)                                              
         LHI   R1,14               NUMBER OF SECTIONS                           
*                                                                               
CNVWR27  DS    0H                                                               
         OC    0(LN,RE),0(RE)                                                   
         BZ    *+10                                                             
         MVC   0(LN,R4),0(RE)                                                   
         AHI   R4,LN                                                            
         AHI   RE,LN                                                            
         BCT   R1,CNVWR27                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   INTRTYP,0                                                        
         MVI   INTKEY,C'B'                                                      
         B     EXIT                                                             
         DROP  R2                                                               
                                                                                
*                                                                               
CNVWR40  DS    0H                  PREVIOUS KEY <> CURRENT KEY                  
         L     RE,ASREC                                                         
         ZICM  RF,0(RE),(3)                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                CLEAR THE SORT RECORD                       
                                                                                
*                                                                               
         DS    0H                                                               
         L     R0,ASREC                                                         
         L     RE,ASVSREC                                                       
         ZICM  R1,0(RE),(3)                                                     
         LR    RF,R1                                                            
         MVCL  R0,RE                COPY PREVIOUS RECORD TO SORT RECD           
*******************************************************                         
* Move b611 & g611 categories to the bottom of intaccs*                         
*******************************************************                         
         BAS   RE,MOVE611                                                       
*******************************************************                         
*                                                                               
         L     RE,ASVSREC                                                       
         ZICM  RF,0(RE),(3)                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                CLEAR PREVIOUS RECORD AREA                  
*                                                                               
         L     RE,ASVSREC2                                                      
         USING INTERD,RE                                                        
         LA    R6,INTKEY                                                        
         USING DRKEY,R6                                                         
         CLI   DRSTAT+1,RTEOFQ                                                  
         BE    CNVWR46X                                                         
         DROP  R6                                                               
                                                                                
         ZICM  RF,INTRECLN,(3)                                                  
         L     R0,ASVSREC                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                SET PREVIOUS RECD W/ CURRENT RECD           
CNVWR46X EQU   *                                                                
         DROP  RE                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     EXIT                                                             
                                                                                
*                                                                               
CNVLST   DS    0H                                                               
         MVI   BYPSORT,X'80'       DON'T RELS SREC TO OPHASE                    
         L     RE,ASVSREC                                                       
         OC    0(4,RE),0(RE)                                                    
         BZ    EXIT                NO PREV RECD IN BUFFER                       
*                                                                               
         DS    0H                                                               
         ZICM  RF,0(RE),(3)        LENGTH OF PREVIOUSLY SAVED RECD              
         L     R0,ASREC                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE               MOVE SAVED RECD INTO SREC                    
         MVI   BYPSORT,X'00'       RELEASE LAST RECD TO OPHASE                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*STOTUNV SLOT TOTALS FROM DAYPART INTO SORT RECD                                
**********************************************************************          
STOTUNV  NTR1                                                                   
         USING INTERD,R2                                                        
                                                                                
*                                                                               
         DS    0H                                                               
         TM    INTSTA,X'F0'        MKT RECD?                                    
         BO    STOT30               JUST NEED UNIVS NOT TOTALS                  
                                                                                
*                                                                               
         DS    0H                                                               
         LA    RE,*+10             SWITCH INTO 31-BIT                           
         O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
         L     R4,ATOTTBL          POINT TO TOTAL'S BUFFER                      
         USING TOTBUFD,R4                                                       
STOT10   DS    0H                                                               
         OC    TBKEY(TBKEYL),TBKEY ANY DATA?                                    
         BZ    STOT30               NO TOTALS FOR THIS DPT FOUND                
         CLC   INTMRKT,TBMKT                                                    
         BNE   STOT15                                                           
         CLC   INTDPT,TBDPT        DAYPARTS MATCH?                              
         BNE   STOT15                                                           
         CLC   INTAQH,TBAQH        CUME/EXC CUME ETC MATCH?                     
         BNE   STOT15                                                           
         CLC   INTGEO,TBGEO                                                     
         BNE   STOT15                                                           
         CLC   INTLISN,TBLISN                                                   
         BE    STOT20                                                           
STOT15   DS    0H                                                               
         AHI   R4,TOTBUFQ                                                       
         C     R4,ATOTMAX          END OF TOTALS BUFFER?                        
         BL    STOT10                                                           
         B     STOT30              NO TOTALS TO SLOT                            
                                                                                
*                                                                               
STOT20   DS    0H                  R4=TOTAL RECD ENTRY IN BUFFER                
         SR    R1,R1                                                            
         CLI   INTAQH,C'3'         EXCLUSIVE CUMES DON'T GET ANYTHING           
         BE    STOT30                                                           
                                                                                
         CLI   INTAQH,C'2'         CUME?                                        
         BNE   *+8                                                              
         LA    R1,OCTOT            'J'=CUME TOTAL                               
         CLI   INTAQH,C'1'         AQH?                                         
         BNE   *+8                                                              
         LA    R1,OTOT             'Q'=TOTAL RECD IMPS                          
                                                                                
         CLI   INTLISN,C'3'        AWAY FROM HOME LISTENING (AT WORK)           
         BE    *+8                                                              
         CLI   INTLISN,C'4'        AWAY FROM HOME LISTENING (IN CAR)            
         BNE   *+8                                                              
         LA    R1,OATOT            'K'=AWAY FROM HOME TOTS                      
*                                                                               
         MVC   0(LN,R1),TBDEMS     MOVE DEMOS FROM TBL INTO SLOT                
         AHI   R1,LN               POINT TO NEW END OF DEMO LIST                
         L     RE,ASREC                                                         
         SR    R1,RE               NEW LENGTH                                   
         STCM  R1,3,0(RE)          SAVE NEW LENGTH OF SREC                      
                                                                                
*                                                                               
STOT30   DS    0H                  SLOT UNIVERSES INTO RECD                     
         LA    RE,*+6               1ST SWITCH OUT OF 31-BIT MODE               
         BSM   0,RE                                                             
*                                                                               
         DS    0H                                                               
         CLI   INTLISN,C'3'        AWAY FROM HOME LISTENING (AT WORK)           
         BE    *+8                  YES, SLOT UNIVS                             
         CLI   INTLISN,C'4'        AWAY FROM HOME LISTENING (IN CAR)            
         BE    *+8                  YES, SLOT UNIVS                             
         CLI   INTAQH,C'1'                                                      
         BNE   STOTX               DON'T SLOT UNIVS                             
*                                                                               
         SR    R1,R1                                                            
         LHI   R0,UNIVMX                                                        
         L     R1,AUNIVS           POINT TO WHERE UNIVS ARE STORED              
         USING UNVBUFFD,R1                                                      
STOT35   DS    0H                                                               
         OC    UBDKEY(UBDKEYL),UBDKEY   NO MORE UNVS                            
         BNZ   *+6                                                              
         DC    H'0'                NO UNIVS FOR THIS MARKET ON TAPE             
         CLC   INTMRKT,UBDMKT                                                   
         BNE   *+10                                                             
         CLC   INTGEO,UBDGEO       SAME AUD TYPE: MSA/TSA/ADI                   
         BE    *+14                                                             
         AHI   R1,UNVBUFFQ                                                      
         BCT   R0,STOT35                                                        
         DC    H'0'                                                             
         MVC   OUNIV,UBDUNIVS      MOVE IN CORRECT DEMO UNIVS                   
         DROP  R1                                                               
*                                                                               
         L     RE,ASREC                                                         
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)          ORIG LENGTH                                  
         LA    R1,OUNIV+L'OUNIV    PT PAST UNIVS                                
         SR    R1,RE               R1=NEW LENGTH                                
         CR    R1,RF               IS 'NEW LENGTH' < OLD LENGTH?                
         BL    *+12                YES, THEN IT'S NOT THE RIGHT LENGTH          
         L     RF,ASREC                                                         
         STCM  R1,3,0(RF)          NO, SAVE NEW LENGTH OF SREC                  
         DROP  R4                                                               
*                                                                               
STOTX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*MOVE611 -MOVE B611 AND G611 TO THE BOTTOM OF INTACCS                           
**********************************************************************          
MOVE611  NTR1                                                                   
         L     R2,ASREC                                                         
         USING INTERD,R2                                                        
         LA    R6,8                                                             
         LA    R3,INTACCS                                                       
         LA    R5,O611                                                          
MOVE10   LA    R4,LN(R3)                                                        
         MVC   0(8,R5),LN-8(R3)                                                 
*                                                                               
         LR    R1,R6                                                            
         SHI   R1,1                                                             
         MHI   R1,LN                                                            
         LA    R0,LN-8(R3)                                                      
         LR    RE,R4                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
*        MVC   LN-8(LN,R3),0(R4)                                                
         LA    R3,LN-8(R3)                                                      
         LA    R5,8(R5)                                                         
         BCT   R6,MOVE10                                                        
         MVC   0(LN-8,R3),O611                                                  
*                                                                               
         B     XIT                                                              
         DROP  R2                                                               
**********************************************************************          
*CNVRPTP - CONVERT REPORT TYPE TO DDS BOOKTYPE                                  
**********************************************************************          
                                                                                
* AT ENTRY,                                                                     
*   R4-->RECORD TO EXTRACT REPORT TYPE                                          
                                                                                
CNVRPTP  NTR1                                                                   
         MVI   SAMPLEBT,0                                                       
                                                                                
         DS    0H                  FIND WHERE PERIOD ID IS W/IN RECD            
         L     R1,ARPTDSPT                                                      
CNVRP012 DS    0H                                                               
         CLI   0(R1),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),0(R4)                                                    
         BE    *+12                                                             
         AHI   R1,L'RPTDSPTB                                                    
         B     CNVRP012                                                         
                                                                                
         ZICM  R5,1(R1),(3)                                                     
         AR    R5,R4               R5-->REPORT TYPE                             
                                                                                
*                                                                               
         DS    0H                  CONVERT REPORT TYPE                          
         L     R3,ASAMBTTB          R1-->TABLE OF KNOWN REPORT TYPES            
CNVRP027 DS    0H                                                               
         CLI   0(R3),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R5),0(R3)                                                    
         BE    *+12                                                             
         AHI   R3,L'SAMBTTAB                                                    
         B     CNVRP027                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,PPMTAB                                                 
         ICM   R2,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            R0 HAS LENGTH OF TABLE ENTRY                 
         USING PPMTABD,R2                                                       
*                                                                               
CNVRP028 CLI   0(R2),X'FF'                                                      
         BE    CNVRP050                                                         
         CLC   CURRMKT,PPMNMKT                                                  
         BNE   CNVRP040                                                         
*                                                                               
         CLI   HOLIDAY,1           HOLIDAY BOOKS                                
         BNE   CNVRP035                                                         
         CLI   3(R3),0                                                          
         BNE   *+12                                                             
         MVI   SAMPLEBT,C'D'                                                    
         B     CNVRPX                                                           
         CLI   3(R3),C'H'                                                       
         BNE   *+12                                                             
         MVI   SAMPLEBT,C'S'                                                    
         B     CNVRPX                                                           
         CLI   3(R3),C'B'                                                       
         BNE   *+12                                                             
         MVI   SAMPLEBT,C'K'                                                    
         B     CNVRPX                                                           
*                                                                               
CNVRP035 CLC   CURRBOOK,PPMSTRTP                                                
         BL    CNVRP050                                                         
         CLC   CURRBOOK,PPMLIVBK                                                
         BNL   CNVRP050                                                         
         MVC   SAMPLEBT,2(R3)                                                   
         B     CNVRPX                                                           
CNVRP040 AR    R2,R0                                                            
         B     CNVRP028                                                         
*                                                                               
CNVRP050 MVC   SAMPLEBT,3(R3)                                                   
*                                                                               
CNVRPX   DS    0H                                                               
         B     EXITE                                                            
         EJECT                                                                  
**********************************************************************          
*CNVPERID - CONVERT YEAR AND MONTH CODE TO DDS BOOK                             
**********************************************************************          
                                                                                
* AT ENTRY,                                                                     
*   R4-->RECORD TO EXTRACT YEAR & MONTH CODE                                    
                                                                                
CNVPERID NTR1                                                                   
                                                                                
         DS    0H                  FIND WHERE PERIOD ID IS W/IN RECD            
         L     R1,APERDSPT                                                      
CNVPI012 DS    0H                                                               
         CLI   0(R1),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),0(R4)                                                    
         BE    *+12                                                             
         AHI   R1,L'PERDSPTB                                                    
         B     CNVPI012                                                         
                                                                                
         ZICM  R5,1(R1),(3)                                                     
         AR    R5,R4               R5-->PERIOD ID CODES                         
                                                                                
*                                                                               
CNVPI100 DS    0H                  GET PERIOD ID                                
         MVC   CURRPERD,0(R5)                                                   
         B     CNVPIX                                                           
                                                                                
*                                                                               
CNVPIX   DS    0H                                                               
         B     EXITE                                                            
         EJECT                                                                  
**********************************************************************          
*CNVMKT  - STORE ARBITRON MARKET NUMBER                                         
**********************************************************************          
                                                                                
* AT ENTRY,                                                                     
*   R4-->RECORD TO EXTRACT ARBITRON MARKET CODE                                 
                                                                                
CNVMKT   NTR1                                                                   
                                                                                
         DS    0H                  FIND WHERE MARKET CODE IS W/IN RECD          
         L     R1,AMKTDSPT                                                      
CNVMK012 DS    0H                                                               
         CLI   0(R1),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),0(R4)                                                    
         BE    *+12                                                             
         AHI   R1,L'MKTDSPTB                                                    
         B     CNVMK012                                                         
                                                                                
         ZICM  R5,1(R1),(3)                                                     
         AR    R5,R4               R5-->ARBITRON MARKET CODE                    
         ZIC   RE,3(R1)            RE = L(ARBITRON MARKET CODE)                 
                                                                                
*                                                                               
         DS    0H                  CONVERT MARKET CODE                          
         BCTR  RE,0                 SET UP FOR  EX  INSTRUCTION                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R5)                                                      
                                                                                
         CVB   RE,DUB                                                           
         STCM  RE,3,CURRMKT                                                     
                                                                                
*                                                                               
         DS    0H                                                               
         B     EXITE                                                            
         EJECT                                                                  
**********************************************************************          
*CNVSTTN - CONVERT STATION FROM STATION COMBO ID                                
**********************************************************************          
                                                                                
* AT ENTRY,                                                                     
*   R4-->RECORD TO EXTRACT ARBITRON STATION CALL LETTERS                        
* AT EXIT,                                                                      
*   CURRSTA set to station call letters                                         
                                                                                
CNVSTTN  NTR1                                                                   
                                                                                
         DS    0H                  FIRST, EXTRACT MARKET OFF RECORD             
         BAS   RE,CNVMKT            MARKET WILL GET STORED IN  CURRMKT          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
                                                                                
         DS    0H                  FIND WHERE STTN CMBO ID IS W/IN RECD         
         L     R1,ASTNDSPT                                                      
CNVST012 DS    0H                                                               
         CLI   0(R1),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),0(R4)                                                    
         BE    *+12                                                             
         AHI   R1,L'STNDSPTB                                                    
         B     CNVST012                                                         
                                                                                
         ZICM  R5,1(R1),(3)                                                     
         AR    R5,R4               R5-->ARBITRON STATION COMBO ID               
         ZIC   RE,3(R1)            RE = L(ARBITRON MARKET CODE)                 
                                                                                
*                                                                               
         DS    0H                  FIND STTN COMBO ID IN OUR STTN LIST          
         LHI   R0,MXSTTNS                                                       
         L     R1,ASTALIST                                                      
         USING STALISTD,R1                                                      
         BCTR  RE,0                                                             
                                                                                
CNVST017 DS    0H                                                               
         OC    SLMKT,SLMKT                                                      
         BZ    CNVST017T                                                        
         EXCLC RE,SLSCMBTY,0(R5)                                                
         BNE   *+10                                                             
         CLC   SLMKT,CURRMKT        MUST MATCH ON MARKET ALSO!                  
         BE    CNVST017X                                                        
         AHI   R1,L'STALIST                                                     
         BCT   R0,CNVST017                                                      
CNVST017T EQU  *                                                                
         DC    H'0'                                                             
CNVST017X EQU  *                                                                
*                                                                               
         DS    0H                  R1-->ENTRY FOR STATION COMBO ID              
*&&DO                                                                           
         CLC   CURRMKT,SLMKT        MAKE SURE MARKET CODE CONSISTENT            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*&&                                                                             
         MVC   CURRSTA,SLSTTN      EXTRACT STATION CALL LETTERS                 
         MVC   CURRACTV,SLACTCD    GET ACTIVITY CODE                            
         MVC   CURRHO,SLHOMOUT     GET HOME/OUTSIDE INDICATOR                   
                                                                                
*                                                                               
         DS    0H                                                               
         B     EXITE                                                            
         EJECT                                                                  
**********************************************************************          
*CNVDT - LOOK UP DAYPART IN TABLE                                               
**********************************************************************          
                                                                                
* AT ENTRY,                                                                     
*   R4-->RECORD TO EXTRACT ARBITRON DAYPART ID                                  
* AT EXIT,                                                                      
*   CC set to NOT equal if daypart is invalid                                   
*   CC set to equal if daypart is valid                                         
*   ADPTNTRY = A(DPTTAB entry) corresponding to valid daypart                   
                                                                                
CNVDT    NTR1                                                                   
                                                                                
         DS    0H                  FIND WHERE MARKET CODE IS W/IN RECD          
         L     R1,ADPTDSPT                                                      
CNVDT012 DS    0H                                                               
         CLI   0(R1),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),0(R4)                                                    
         BE    *+12                                                             
         AHI   R1,L'DPTDSPTB                                                    
         B     CNVDT012                                                         
                                                                                
         ZICM  R5,1(R1),(3)                                                     
         AR    R5,R4               R5-->ARBITRON DAYPART ID                     
         ZIC   RE,3(R1)            RE = L(ARBITRON DAYPART ID)                  
                                                                                
*                                                                               
         DS    0H                  CONVERT MARKET CODE                          
         BCTR  RE,0                 SET UP FOR  EX  INSTRUCTION                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R5)                                                      
                                                                                
         CVB   RE,DUB                                                           
         STCM  RE,3,HALF                                                        
                                                                                
*                                                                               
         DS    0H                                                               
         BCTR  RE,0                                                             
         MHI   RE,DPTTABQ                                                       
         A     RE,ADPTTAB                                                       
         USING DPTTABD,RE                                                       
         CLC   DPTRSVID,HALF                                                    
         BNE   CNVDTXN                                                          
         CLI   DPTIDAY,0                                                        
         BE    CNVDTXN                                                          
                                                                                
         ST    RE,ADPTNTRY         SAVE A(ENTRY FOR VALID DAYPART)              
*                                                                               
         DS    0H                                                               
         MVC   CURRDAY,DPTIDAY                                                  
         MVC   CURRPNAM,DPTNAME                                                 
*                                                                               
         DS    0H                  SET QUARTER HOURS                            
         CLI   DPTIDPT,0                                                        
         BE    CNVDT035G                                                        
         MVC   CURRSQH,DPTIDPT                                                  
         MVC   CURREQH,DPTIDPT                                                  
         MVI   DPTSW,C'Y'                                                       
         B     CNVDT035X                                                        
                                                                                
CNVDT035G DS   0H                                                               
         SR    R0,R0                                                            
         ICM   R0,3,DPTSTIM        SET DDS START QH                             
         CHI   R0,500              IF TIME < 5AM,                               
         BNL   *+8                                                              
         AHI   R0,2400              ADJUST TIME                                 
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         SLL   R1,2                X 4                                          
         AHI   R1,-20              MAKE 5AM AS QUARTER-HOUR BASE                
         STC   R1,CURRSQH                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,DPTETIM        SET DDS END   QH                             
         CHI   R0,500              IF TIME < 5AM                                
         BNL   *+8                                                              
         AHI   R0,2400              ADJUST TIME                                 
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         SLL   R1,2                X 4                                          
         AHI   R1,-20              MAKE 5AM AS QUARTER-HOUR BASE                
         BCTR  R1,0                ADJUST FOR END TIME                          
         STC   R1,CURREQH                                                       
CNVDT035X EQU  *                                                                
         DROP  RE                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         B     CNVDTXY                                                          
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
CNVDTXN  DS    0H                                                               
         J     EXITL                                                            
                                                                                
CNVDTXY  DS    0H                                                               
         J     EXITE                                                            
         EJECT                                                                  
**********************************************************************          
*FLTSTTN -                                                                      
**********************************************************************          
FLTSTTN  SR    RF,RF                                                            
         ICM   RF,1,FILTSTAT                                                    
         BZR   RE                                                               
         LA    R1,FILTSTAT+1                                                    
         CLC   CURRSTA(4),0(R1)    DONT CHECK THE BAND                          
         BER   RE                                                               
         LA    R1,L'INTSTA(R1)                                                  
         BCT   RF,*-12                                                          
         MVI   PASSFLT,C'N'                                                     
         BR    RE                                                               
*                                                                               
**********************************************************************          
*FLTMKT -                                                                       
**********************************************************************          
FLTMKT   SR    RF,RF               FILTER MARKETS                               
         ICM   RF,1,FILTMRKT                                                    
         BZR   RE                                                               
         OC    CURRMKT,CURRMKT                                                  
         BZR   RE                                                               
         LA    R1,FILTMRKT+1                                                    
         CLC   CURRMKT,0(R1)                                                    
         BER   RE                                                               
         LA    R1,L'CURRMKT(R1)                                                 
         BCT   RF,*-12                                                          
         MVI   PASSFLT,C'N'                                                     
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
*GETBTYP-                                                                       
**********************************************************************          
GETBTYP  NTR1                      GET SPECIAL BOOK TYPES                       
         USING INTERD,R2                                                        
*                                                                               
         MVI   INTBTYP,0                                                        
*                                                                               
         GOTO1 VLDCREC,DMCB,0,0    FIND MARKET TABLES                           
         L     RE,DMCB                                                          
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'                DIE IF NONE THERE                            
GETBTYP2 CLC   0(2,RE),=C'AR'      FIND THE ARB RADIO ONE                       
         BE    GETBTYP4                                                         
         ICM   RE,7,2(RE)          NF - TRY NEXT ONE                            
         OC    0(2,RE),0(RE)       EOT - DIE SOMETHING WRONG                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         B     GETBTYP2                                                         
*                                                                               
GETBTYP4 LA    RE,5(RE)            BYPASS HEADER                                
GETBTYP6 OC    0(2,RE),0(RE)       EOT                                          
         BZ    GETBTYP9             JUST GET OUT OF LOOP                        
*                                                                               
         CLC   0(2,RE),CURRMKT     HAVE IT                                      
         BE    *+12                                                             
         A     RE,DMCB+4           L'ENTRY                                      
         B     GETBTYP6            NEXT ONE                                     
*                                                                               
         CLI   2(RE),C' '          DON'T CARE                                   
         BE    *+10                                                             
         MVC   INTBTYP,2(RE)       SET BOOK TYPE                                
         MVC   TEMPMKT,3(RE)       STORE MARKET NAME                            
GETBTYP9 EQU   *                                                                
*                                                                               
         CLI   SAMPLEBT,0                                                       
         BE    *+14                                                             
         MVC   INTBTYP,SAMPLEBT                                                 
         B     GETBTYPX                                                         
*                                                                               
         CLI   BOOKTYPE,0                                                       
         BE    *+14                                                             
         MVC   INTBTYP,BOOKTYPE    USE OVERRIDE IF GIVEN                        
         B     GETBTYPX                                                         
*                                                                               
GETBTYPX DS    0H                                                               
         J     EXITE                                                            
         DROP  R2                                                               
**********************************************************************          
*MSGREC  SEND OUT EMAIL NOTIFICATION OF MARKETS LOADED                          
**********************************************************************          
MSGREC   NTR1                                                                   
         USING INTERD,R2                                                        
         L     RE,=A(UNIQBTY)      STORE UNIQUE BOOKTYPES IN TABLE              
MSGR05   CLI   0(RE),X'FF'                                                      
         BNE   MSGR10                                                           
         CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    MSGRX                                                            
         B     MSGR20                                                           
MSGR10   CLC   INTBTYP,0(RE)       NOT UNIQUE                                   
         BNE   MSGR15                                                           
         CLC   SVMRKT,CURRMKT                                                   
         BE    MSGRX                                                            
         B     MSGR30                                                           
MSGR15   LA    RE,1(RE)                                                         
         B     MSGR05                                                           
MSGR20   MVC   0(1,RE),INTBTYP                                                  
         MVI   1(RE),X'FF'                                                      
************************************************************                    
MSGR30   L     R6,AMREC                                                         
         LA    R6,4(R6)                                                         
         MVC   SVMRKT,CURRMKT                                                   
         ZICM  R0,CURRMKT,(3)                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TEMPMNO(3),DUB                                                   
         OI    CURRSTA+2,X'F0'                                                  
         MVC   0(3,R6),TEMPMNO     MARKET NUMBER                                
         LA    R6,3(R6)                                                         
*                                                                               
         MVI   0(R6),C','          MARKET ALPHA                                 
         LA    R6,1(R6)                                                         
         MVC   0(L'TEMPMKT,R6),TEMPMKT                                          
         LA    R6,L'TEMPMKT(R6)                                                 
*                                                                               
         MVI   0(R6),C','          BOOKTYPE                                     
         LA    R6,1(R6)                                                         
         MVC   0(1,R6),INTBTYP                                                  
         LA    R6,1(R6)                                                         
*                                                                               
MSGR80   MVI   0(R6),C','          BOOK                                         
         MVC   TEMPBK(2),INTBOOK                                                
         MVI   TEMPBK+2,X'1'                                                    
         GOTO1 VDATCON,DMCB,(3,TEMPBK),(6,1(R6))                                
         AHI   R6,7                                                             
*                                                                               
         L     RF,AMREC                                                         
         SR    R6,RF                                                            
         STC   R6,1(RF)                                                         
         MVI   MSGFLAG,1                                                        
*                                                                               
         BC    0,MSGRX                                                          
         MVI   *-3,X'F0'                                                        
         L     RF,AESUB                                                         
         GOTO1 VDATCON,DMCB,(3,TEMPBK),(6,EMMM)                                 
         MVC   ESUBJBK(6),EMMM                                                  
         L     RF,AESUB                                                         
         MVC   0(ESUBJEQU,RF),ESUBJ                                             
*                                                                               
MSGRX    J     EXITE                                                            
*                                                                               
         DROP  R2                                                               
***********************************************************************         
         TITLE 'ARBITRON RADIO DEMO CONVERSION INITIALIZATION'                  
***********************************************************************         
*======================= DERA08I INITIALIZATION ======================*         
DERAINIT NTR1                                                                   
                                                                                
*                                                                               
         DS    0H                  INITIALIZE ADDRESS CONSTANTS                 
         LHI   R0,DISPTABQ                                                      
         LHI   R1,(DISPTAB-DERA08I)                                             
         A     R1,MYBASE1                                                       
*                                                                               
DI012    DS    0H                                                               
         ZICM  RE,1(R1),(7)        ESTABLISH ADDR TO STORE                      
         CLI   0(R1),C'B'           ASSUME OFF BASE NMOD                        
         BNE   *+12                                                             
         A     RE,MYBASE1                                                       
         B     DI013X                                                           
         CLI   0(R1),C'L'           ASSUME WITHIN LOCAL STORAGE                 
         BNE   *+12                                                             
         LA    RE,LOCWORKD(RE)                                                  
         B     DI013X                                                           
         DC    H'0'                                                             
DI013X   EQU   *                                                                
                                                                                
         ZICM  RF,5(R1),(3)        ESTABLISH LOCATION TO STORE ADDR             
         CLI   4(R1),C'L'                                                       
         BNE   *+12                                                             
         LA    RF,LOCWORKD(RF)                                                  
         B     DI015X                                                           
         DC    H'0'                                                             
DI015X   EQU   *                                                                
                                                                                
         STCM  RE,15,0(RF)         STORE ADDRESS                                
         AHI   R1,L'DISPTAB                                                     
         BCT   R0,DI012                                                         
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   HOLIDAY,0                                                        
         J     EXITE                                                            
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (CLEAR DEMO VALUE MATRIX+        
               )'                                                               
***********************************************************************         
*====================== CLEAR DEMO VALUE MATRIX ======================*         
CLRDVMTX NTR1                                                                   
         L     R0,ADVMATRX                                                      
         LHI   R1,DVMATRXL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    ANYDEMO,ANYDEMO                                                  
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXITE                                                            
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (GET DEMO NUMBER && QUAL+        
               ITATIVE CATEGORY NUMBER)'                                        
***********************************************************************         
*=========== GET DEMO NUMBER && QUALITATIVE CATEGORY NUMBER ==========*         
                                                                                
* Routine accepts an Audience Characteristic ID and returns the                 
*  corresponding Conversion-defined demo number and qualitative                 
*  category number                                                              
* At entry,                                                                     
*   PPRQCNUM = pre-processor defined qualitative category number                
*   PPRDMNUM = pre-processor defined demo number                                
* At exit,                                                                      
*   CNVQCNUM = Conversion-defined qualitative category number                   
*   CNVDMNUM = Conversion-defined demo number                                   
                                                                                
GDNQC    NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING DEMQCTBD,R3                                                      
         MVC   DQTPPQCN,PPRQCNUM                                                
         MVC   DQTPPDMN,PPRDMNUM                                                
                                                                                
*                                                                               
         DS    0H                  BUILD BINSRCH PARAMETERS                     
         ST    R3,DMCB+00           A(RECORD)                                   
         L     RE,ADEMQCTB                                                      
         LA    RF,8(RE)                                                         
         ST    RF,DMCB+04           A(TABLE)                                    
         MVC   DMCB+08(4),0(RE)     # OF ENTRIES SO FAR                         
         LHI   R0,DEMQCTBQ                                                      
         ST    R0,DMCB+12           LENGTH OF RECORD                            
         LHI   R0,DQTKEYL                                                       
         ST    R0,DMCB+16           DISPL & LENGTH OF KEY                       
         LHI   R0,DEMQCTBM                                                      
         ST    R0,DMCB+20           MAX ENTRIES IN TABLE                        
                                                                                
         GOTO1 VBINSRCH,DMCB                                                    
         CLI   DMCB+0,0            IF NOT FOUND,                                
         BE    *+6                                                              
         DC    H'0'                 SOMETHING'S WRONG                           
                                                                                
*                                                                               
         DS    0H                                                               
         ZICM  R3,DMCB+1,(7)       R3-->ENTRY IN TABLE                          
         MVC   CNVDMNUM,DQTCVDMN   SET CONVERSION-DEFINED DEMO #                
         MVC   CNVQCNUM,DQTCVQCN   SET CONVERSION-DEFINED QUAL CTGRY #          
         DROP  R3                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXITE                                                            
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (SEED DEMO VALUE)'               
***********************************************************************         
*========================== SEED DEMO VALUE ==========================*         
                                                                                
* Routine seeds demo value into the demo value matrix.                          
* At entry,                                                                     
*   TMPGEO   = Current market segment type                                      
*   CNVDMNUM = Conversion-defined demo number                                   
*   CNVQCNUM = Conversion-defined qualitative category number                   
*   TMPDMVAL = demo value to be seeded                                          
                                                                                
SEEDMVAL NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
         ZIC   RF,TMPGEO                                                        
         BCTR  RF,0                                                             
         MHI   RF,(MAXQC*MAXCDN)                                                
         AR    RE,RF                                                            
                                                                                
         ZIC   RF,CNVQCNUM         RE = "ROW" NUMBER                            
         BCTR  RF,0                                                             
         MHI   RF,MAXCDN                                                        
         AR    RE,RF                                                            
                                                                                
         ZIC   RF,CNVDMNUM         RF = "COLUMN" NUMBER                         
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
*                                                                               
         SLL   RE,2                RE = DISPL INTO DEMO VALUE MATRIX            
         A     RE,ADVMATRX                                                      
         MVC   0(4,RE),TMPDMVAL                                                 
*                                                                               
         ZIC   R1,TMPGEO                                                        
         LA    R1,ANYDEMO-1(R1)                                                 
         MVI   0(R1),C'Y'          FLAG FOR DEMOS IN MATRIX                     
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXITE                                                            
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (EXTRACT DEMO VALUE)'            
***********************************************************************         
*========================= EXTRACT DEMO VALUE ========================*         
                                                                                
* Routine extracts demo value from the demo value matrix.                       
* At entry,                                                                     
*   TMPGEO   = Current market segment type                                      
*   CNVDMNUM = Conversion-defined demo number                                   
*   CNVQCNUM = Conversion-defined qualitative category number                   
* At exit,                                                                      
*   TMPDMVAL = extracted demo value                                             
                                                                                
XTCDMVAL NTR1                                                                   
         XC    TMPDMVAL,TMPDMVAL                                                
                                                                                
*                                                                               
         DS    0H                                                               
         SR    RE,RE                                                            
*                                                                               
         ZIC   RF,TMPGEO                                                        
         BCTR  RF,0                                                             
         MHI   RF,(MAXQC*MAXCDN)                                                
         AR    RE,RF                                                            
                                                                                
         ZIC   RF,CNVQCNUM         RE = "ROW" NUMBER                            
         BCTR  RF,0                                                             
         MHI   RF,MAXCDN                                                        
         AR    RE,RF                                                            
                                                                                
         ZIC   RF,CNVDMNUM         RF = "COLUMN" NUMBER                         
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
*                                                                               
         SLL   RE,2                RE = DISPL INTO DEMO VALUE MATRIX            
         A     RE,ADVMATRX                                                      
         MVC   TMPDMVAL,0(RE)                                                   
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXITE                                                            
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (BUILD INTERIM RECORD)'          
***********************************************************************         
*======================== BUILD INTERIM RECORD =======================*         
                                                                                
* Routine builds an interim record to be released to the sort.                  
* At entry,                                                                     
*   CNVQCNUM = Conversion-defined qualitative category number                   
                                                                                
BLDIREC  NTR1                                                                   
         L     R4,AIREC                                                         
         USING INTERD,R4                                                        
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   INTMTYP,STDCNDMK    STANDARD/CONDENSED MARKET                    
         MVC   INTMRKT,CURRMKT     MARKET                                       
         MVC   INTSTA,CURRSTA      STATION                                      
         MVC   INTBOOK,CURRBOOK    BOOK                                         
         MVC   INTGEO,TMPGEO       GEOGRAPHIC INDICATOR                         
         OI    INTGEO,X'F0'                                                     
                                                                                
         MVI   INTSPILL,C'N'       ASSUME INSIDE MSA                            
         MVI   INTSTYP,0                                                        
         CLI   CURRHO,C'1'         TEST OUTSIDE MSA                             
*        BE    *+8                                                              
*        CLI   CURRHO,C'2'         TEST OUTSIDE MSA                             
         BNE   *+12                                                             
         MVI   INTSPILL,C'Y'       IF OUTSIDE MSA,                              
         MVI   INTSTYP,X'20'        SET THESE FLAGS ON                          
*                                                                               
         BAS   RE,SETKEY                                                        
                                                                                
*                                                                               
         DS    0H                                                               
         L     R2,ACVDSPTB         DRIVING-OFF CNVRSN DISPL TABLE,              
         USING CVDSPTBD,R2                                                      
                                                                                
*                                                                               
BIR022   DS    0H                   SET VALUES INTO INTERIM RECORD              
         CLI   0(R2),EOT                                                        
         BE    BIR049X                                                          
*                                                                               
         DS    0H                                                               
         ZIC   R0,CDTNCDN                                                       
         LA    R3,CDTCDNS                                                       
         SR    R5,R5                                                            
*                                                                               
BIR026   DS    0H                                                               
         MVC   CNVDMNUM,0(R3)                                                   
         BAS   RE,XTCDMVAL                                                      
         A     R5,TMPDMVAL                                                      
         AHI   R3,L'CDTCDNS                                                     
         BCT   R0,BIR026                                                        
*                                                                               
         DS    0H                                                               
         ZICM  RE,CDTIDSP,(3)                                                   
         LA    RE,INTERD(RE)                                                    
         STCM  R5,15,0(RE)                                                      
                                                                                
*                                                                               
         DS    0H                                                               
         ZIC   R0,CDTNLEN                                                       
         AR    R2,R0                                                            
         B     BIR022                                                           
BIR049X  EQU   *                                                                
                                                                                
*                                                                               
** MEN 18-24 **                                                                 
*                                                                               
         DS    0H                                                               
         CLI   CNVQCNUM,QCNONE     ONLY IF WORKING W/ NO QUAL CATGRY            
         BNE   BIR102X                                                          
         SR    R5,R5                                                            
*                                                                               
         MVI   CNVDMNUM,CDNM1820    GET DEMO VALUE FOR MEN 18-20                
         BAS   RE,XTCDMVAL                                                      
         A     R5,TMPDMVAL                                                      
                                                                                
         MVI   CNVDMNUM,CDNM2124    GET DEMO VALUE FOR MEN 21-24                
         BAS   RE,XTCDMVAL                                                      
         A     R5,TMPDMVAL                                                      
*                                                                               
         STCM  R5,15,(CIM1824-CDSCT)(R4)                                        
                                                                                
BIR102X  EQU   *                                                                
                                                                                
*                                                                               
** WOMEN 18-24 **                                                               
*                                                                               
         DS    0H                                                               
         CLI   CNVQCNUM,QCNONE     ONLY IF WORKING W/ NO QUAL CATGRY            
         BNE   BIR104X                                                          
         SR    R5,R5                                                            
*                                                                               
         MVI   CNVDMNUM,CDNW1820    GET DEMO VALUE FOR WOMEN 18-20              
         BAS   RE,XTCDMVAL                                                      
         A     R5,TMPDMVAL                                                      
                                                                                
         MVI   CNVDMNUM,CDNW2124    GET DEMO VALUE FOR WOMEN 21-24              
         BAS   RE,XTCDMVAL                                                      
         A     R5,TMPDMVAL                                                      
*                                                                               
         STCM  R5,15,(CIW1824-CDSCT)(R4)                                        
                                                                                
BIR104X  EQU   *                                                                
         DROP  R2                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         J     EXITE                                                            
         DROP  R4                                                               
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (RELEASE INTERIM RECORD)+        
               '                                                                
***********************************************************************         
*======================= RELEASE INTERIM RECORD ======================*         
                                                                                
* Routine is called when an interim record needs to be released to the          
*  sort.  This routine will exit the IPHASE entirely.                           
* At entry,                                                                     
*   CURRTYP = current record type                                               
*   BYPREAD = zero to release record                                            
*           = non-zero if returning from having released record                 
                                                                                
RELIREC  NTR1                                                                   
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
*                                                                               
         DS    0H                  RELEASE ACCORDING TO RECORD TYPE             
         CLI   CURRTYP,C'A'         COPYRIGHT RECORD                            
         BE    RIR020                                                           
         CLI   CURRTYP,C'D'         AUDIENCE CHARACTERISTIC RECORD              
         BE    RIR020                                                           
         CLI   CURRTYP,C'G'         INTAB RECORD                                
         BE    RIRG000                                                          
         CLI   CURRTYP,C'H'         POPULATION RECORD                           
         BE    RIRH000                                                          
         CLI   CURRTYP,C'J'         STATION COMBO RECORD                        
         BE    RIRJ000                                                          
         CLI   CURRTYP,C'S'         DAYPART RECORD                              
         BE    RIR020                                                           
         CLI   CURRTYP,C'V'         AUDIENCE ESTIMATE RECORD                    
         BE    RIRV000                                                          
         CLI   CURRTYP,RTEOFQ       END OF INPUT TAPE PROCESSING                
         BE    RIR9000                                                          
         B     RIRX                                                             
                                                                                
*                                                                               
RIR020   DS    0H                  NOT RELEASING INTERIM RECORD                 
         BAS   RE,CLRDVMTX          CLEAR DEMO VALUE MATRIX                     
         MVI   BYPREAD,0            RESET FLAG                                  
         B     RIRX                                                             
         EJECT                                                                  
*                                                                               
** RELEASE "G"-TYPE INTERIM RECORD **                                           
*                                                                               
RIRG000  DS    0H                                                               
         CLI   RLSIFLG,0                                                        
         BNE   RIRG009                                                          
         MVI   THISGEO,1                                                        
         MVI   THISQCNM,0                                                       
RIRG009  EQU   *                                                                
                                                                                
*                                                                               
RIRG020  DS    0H                                                               
         CLI   THISGEO,MAXGEO                                                   
         BNL   RIRG100                                                          
*                                                                               
         DS    0H                                                               
         ZIC   R1,THISGEO                                                       
         STC   R1,TMPGEO                                                        
         LA    R1,ANYDEMO-1(R1)                                                 
         CLI   0(R1),C'Y'          ANY DEMO FOR THIS MKT SEGMENT?               
         BNE   RIRG050                                                          
                                                                                
         CLI   THISQCNM,QCNONE                                                  
         BNL   RIRG050                                                          
*                                                                               
         MVI   THISQCNM,QCNONE                                                  
         MVC   CNVQCNUM,THISQCNM                                                
         BAS   RE,BLDIREC                                                       
         MVI   RLSIFLG,RIFIT1                                                   
         B     RIRRELX                                                          
                                                                                
*                                                                               
RIRG050  DS    0H                                                               
         ZIC   R1,THISGEO                                                       
         AHI   R1,1                                                             
         STC   R1,THISGEO                                                       
         MVI   THISQCNM,0                                                       
         B     RIRG020                                                          
                                                                                
*                                                                               
RIRG100  DS    0H                                                               
         MVI   THISGEO,0                                                        
         MVI   THISQCNM,0                                                       
         MVI   RLSIFLG,0                                                        
         B     RIR020                                                           
         EJECT                                                                  
*                                                                               
** RELEASE "H"-TYPE INTERIM RECORD **                                           
*                                                                               
RIRH000  DS    0H                                                               
         CLI   RLSIFLG,0                                                        
         BNE   RIRH009                                                          
         MVI   THISGEO,1                                                        
         MVI   THISQCNM,0                                                       
RIRH009  EQU   *                                                                
                                                                                
*                                                                               
RIRH020  DS    0H                                                               
         CLI   THISGEO,MAXGEO                                                   
         BH    RIRH100                                                          
*                                                                               
         DS    0H                                                               
         ZIC   R1,THISGEO                                                       
         STC   R1,TMPGEO                                                        
         LA    R1,ANYDEMO-1(R1)                                                 
         CLI   0(R1),C'Y'          ANY DEMO FOR THIS MKT SEGMENT?               
         BNE   RIRH050                                                          
                                                                                
         CLI   THISQCNM,QCNONE                                                  
         BNL   RIRH050                                                          
*                                                                               
         MVI   THISQCNM,QCNONE                                                  
         MVC   CNVQCNUM,THISQCNM                                                
         BAS   RE,BLDIREC                                                       
                                                                                
*                                                                               
         DS    0H                  SLOT UNIVERSES FOR MKT/GEO INTO BUFF         
         LA    R0,UNIVMX                                                        
         L     R1,AUNIVS                                                        
         USING UNVBUFFD,R1                                                      
         OC    UBDKEY(UBDKEYL),UBDKEY                                           
         BZ    *+14                                                             
         AHI   R1,UNVBUFFQ                                                      
         B     *-14                                                             
         DC    H'0'                                                             
         MVC   UBDMKT,CURRMKT                                                   
         MVC   UBDGEO,THISGEO                                                   
         OI    UBDGEO,X'F0'                                                     
         MVC   UBDUNIVS,IBLK1                                                   
         DROP  R1                                                               
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   RLSIFLG,RIFUNIV                                                  
         B     RIRRELX                                                          
                                                                                
*                                                                               
RIRH050  DS    0H                                                               
         ZIC   R1,THISGEO                                                       
         AHI   R1,1                                                             
         STC   R1,THISGEO                                                       
         MVI   THISQCNM,0                                                       
         B     RIRH020                                                          
                                                                                
*                                                                               
RIRH100  DS    0H                                                               
         MVI   THISGEO,0                                                        
         MVI   THISQCNM,0                                                       
         MVI   RLSIFLG,0                                                        
         B     RIR020                                                           
         EJECT                                                                  
*                                                                               
** RELEASE "J"-TYPE INTERIM RECORD **                                           
*                                                                               
RIRJ000  DS    0H                                                               
         L     R4,ARREC                                                         
         AHI   R4,4                                                             
         CLI   0(R4),C'J'                                                       
         BNE   RIR020                                                           
         USING RJDSECT,R4                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   INTMRKT,CURRMKT                                                  
         MVC   INTSTA,CURRSTA                                                   
         MVC   INTBOOK,CURRBOOK                                                 
         MVC   INTMTYP,STDCNDMK                                                 
         MVC   TMPGEO,CURRGEO                                                   
         NI    TMPGEO,X'0F'                                                     
                                                                                
         MVI   INTSPILL,C'N'       ASSUME INSIDE MSA                            
         MVI   INTSTYP,0                                                        
         CLI   CURRHO,C'1'         TEST OUTSIDE MSA                             
*        BE    *+8                                                              
*        CLI   CURRHO,C'2'         TEST OUTSIDE MSA                             
         BNE   *+12                                                             
         MVI   INTSPILL,C'Y'       IF OUTSIDE MSA,                              
         MVI   INTSTYP,X'20'        SET THESE FLAGS ON                          
* ONLY WANT IHEART OWNERS                                                       
         TM    CURRSTA,X'F0'       IF MARKET TOTALS,                            
         BO    *+14                                                             
         CLC   =C'IH',CURRSTA                                                   
         BNE   RIRRELX                                                          
*                                                                               
         DS    0H                                                               
         CLI   RLSIFLG,RIFMKT                                                   
         BE    RIRJMKT                                                          
         CLI   RLSIFLG,RIFSER1                                                  
         BE    RIRJSER                                                          
         CLI   RLSIFLG,RIFSER2                                                  
         BE    RIRJSER                                                          
         CLI   RLSIFLG,RIFSER3                                                  
         BE    RIRJSER                                                          
         B     RIRX                                                             
                                                                                
*                                                                               
RIRJMKT  DS    0H                                                               
         BAS   RE,SETKEY                                                        
         B     RIRRELX                                                          
                                                                                
*                                                                               
RIRJSER  DS    0H                  STATION EQUIVALENCE RECORDS                  
         BAS   RE,SETKEY                                                        
         B     RIRRELX                                                          
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
** RELEASE "V"-TYPE INTERIM RECORD **                                           
*                                                                               
RIRV000  DS    0H                                                               
         L     R4,ARREC                                                         
         AHI   R4,4                                                             
         CLI   0(R4),C'V'                                                       
         BNE   RIR020                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         B     RIRRELX                                                          
         EJECT                                                                  
*                                                                               
** RELEASE "EOF" MARKERS **                                                     
*                                                                               
RIR9000  DS    0H                                                               
         MVC   INTMRKT,CURRMKT                                                  
         XC    INTSTA,INTSTA                                                    
         MVC   INTBOOK,CURRBOOK                                                 
         MVC   INTMTYP,STDCNDMK                                                 
         MVC   TMPGEO,CURRGEO                                                   
         NI    TMPGEO,X'0F'                                                     
                                                                                
*                                                                               
         DS    0H                                                               
         CLI   RLSIFLG,RIFEOFD                                                  
         BE    RIR9EOF                                                          
         CLI   RLSIFLG,RIFEOFR                                                  
         BE    RIR9EOF                                                          
         CLI   RLSIFLG,RIFEOFD2                                                 
         BE    RIR9EOF                                                          
         CLI   RLSIFLG,RIFEOFR2                                                 
         BE    RIR9EOF                                                          
         B     RIRX                                                             
                                                                                
*                                                                               
RIR9EOF  DS    0H                                                               
         BAS   RE,SETKEY                                                        
         B     RIRRELX                                                          
         DROP  R2                                                               
                                                                                
*                                                                               
** EXITS **                                                                     
*                                                                               
RIRRELX  DS    0H                  EXIT IPHASE                                  
         MVI   BYPREAD,C'Y'                                                     
         L     RD,BASERD                                                        
                                                                                
RIRX     DS    0H                                                               
         J     EXITE                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*================== LOOK UP STATION COMBO ID ON FILE =================*         
                                                                                
* At exit,                                                                      
*   TPKEY contains the Station Combo ID record                                  
                                                                                
LKSTNID  NTR1                                                                   
         XC    TPKEY,TPKEY                                                      
                                                                                
TK       USING DSEKEY,TPKEY                                                     
TKS      USING DSEKEY,TPKEYSV                                                   
                                                                                
*                                                                               
         DS    0H                                                               
         MVI   TK.DSECODE,DSECDEQU                                              
         MVI   TK.DSEMEDIA,C'R'                                                 
****     MVI   TK.DSESRC,C'A'                                                   
         MVI   TK.DSESRC,C'H'                                                   
         MVI   TK.DSEIND,DSEISTID                                               
         MVC   TK.DSESTNTY,CURRSCTY                                             
         MVC   TK.DSESTNID,CURRSCID                                             
                                                                                
*                                                                               
LSI012   DS    0H                                                               
         MVC   TPKEYSV,TPKEY                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',TPKEYSV,TPKEY                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
         DS    0H                                                               
         CLC   TK.DSEKEY(4+1+3),TKS.DSEKEY   SRC/MED/IND/STN-TY/STTN-ID         
         BE    LSI020                                                           
                                                                                
         MVI   NEWSTTN,C'Y'                   NEW STTN COMBO ID                 
         MVC   TPKEY,TPKEYSV                  RESET BUILT KEY                   
         B     LSI049                                                           
                                                                                
*                                                                               
LSI020   DS    0H                  STATION COMBO ID EXISTS                      
         MVC   TPKEY,TPKEYSV        RESTORE TO INITIAL KEY                      
         MVC   TK.DSESBK,CURRBOOK    AND DO A LOOKUP WITH BOOK                  
         XC    TK.DSESBK,=X'FFFF'                                               
*                                                                               
         DS    0H                                                               
         MVC   TPKEYSV,TPKEY                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',TPKEYSV,TPKEY                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DS    0H                                                               
         CLC   TK.DSEKEY(4+1+3),TKS.DSEKEY   SRC/MED/IND/STN-TY/STTN-ID         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*                                                                               
LSI049   EQU   *                                                                
         DROP  TK,TKS                                                           
                                                                                
*                                                                               
LSI      DS    0H                                                               
         J     EXITE                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*============================ ERROR FOUND ============================*         
                                                                                
ERRFND   NTR1                                                                   
         LTR   R5,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  R5,0                                                             
         MHI   R5,L'ERRMSG00                                                    
         LA    R5,ERRMSG00(R5)            PRINT ERROR MESSAGE IN                
         L     R4,VCPRINT                                                       
         USING DPRINT,R4                                                        
         MVC   P(L'ERRMSG00),0(R5)                                              
         GOTO1 VPRINTER                    AND OUTPUT,                          
         DROP  R4                                                               
         MVI   INTAPESW,X'02'              SET SWITCH                           
         J     EXITE                       AND EXIT                             
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
                                                                                
EXIT     XMOD1 1                                                                
XIT      XIT1                                                                   
         TITLE 'ARBITRON RADIO DEMO CONVERSION (LTORG && CONSTANTS)'            
***********************************************************************         
*========================= LTORG & CONSTANTS =========================*         
         LTORG                                                                  
         EJECT                                                                  
MYSPACES DC    CL80' '                                                          
                                                                                
                                                                                
IN1      DCB   DDNAME=IN1,                                             +        
               DSORG=PS,                                               +        
               RECFM=VB,                                               +        
               LRECL=00255,                                            +        
               MACRF=GM,                                               +        
               EODAD=MORET                                                      
                                                                                
ATOTTBL  DS    A                   A(TOT TBL) FROM GETMAIN                      
ATOTBKT  DS    A                   NEXT AVAIL BKT IN TOTAL'S BUFFER             
ATOTMAX  DS    A                   OVERFLOW ADDRESS OF TOTAL'S BUFF             
EMMM     DS    CL3                 MON                                          
         DS    CL1                 /                                            
EYY      DS    CL2                 YY                                           
*                                                                               
PREVQH   DC    X'00'                                                            
HAV2WH   DC    X'00'                                                            
HLDSTYP  DC    X'00'                                                            
FRST     DC    X'01'                                                            
TOTALSW  DC    C'N'                                                             
PASSFLT  DC    C'Y'                                                             
HOLDPAR  DS    CL5                 HOLD PARENT STATION                          
PREVSTYP DS    CL1                                                              
PREVSTAT DS    CL5                                                              
PREVMKT  DS    CL2                                                              
AFRST    DC    X'01'                                                            
RELOFRST DC    X'01'                                                            
SVBOOK   DS    CL2                                                              
HUTSW    DS    CL1                                                              
*                                                                               
ADAYTAB  DC    C'0',X'95'          ARB DAY CONVERSION TABLE                     
         DC    C'1',X'E7'                                                       
         DC    C'2',X'96'                                                       
         DC    C'3',X'97'                                                       
         DC    C'6',X'60'                                                       
         DC    C'7',X'70'                                                       
         SPACE 2                                                                
         EJECT                                                                  
**********************************************************************          
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION ("SAVED" STORAGE)'               
***********************************************************************         
*========================== "SAVED" STORAGE ==========================*         
                                                                                
*                                 ************** COUNTERS *************         
CNTGET   DC    F'0'                # OF RECORDS GOTTEN FROM TAPE                
                                                                                
*                                 ********** FIRST TIME FLAGS *********         
F1DREC   DC    C'Y'                                                             
F1GREC   DC    C'Y'                                                             
F1HREC   DC    C'Y'                                                             
F1JREC   DC    C'Y'                                                             
F1MREC   DC    C'Y'                                                             
F1PREC   DC    C'Y'                                                             
F1SREC   DC    C'Y'                                                             
                                                                                
*                                 ********* RELEASE IREC FLAGS ********         
RLSIFLG  DC    X'00'                                                            
RIFIT1   EQU    10                 RELEASE INTAB RECORD #1                      
RIFIT2   EQU    11                    "      "     "    #2                      
RIFIT3   EQU    12                    "      "     "    #3                      
RIFUNIV  EQU    20                 RELEASE UNIVERSE RECORD                      
RIFMKT   EQU    30                 RELEASE MARKET RECORD                        
RIFSER1  EQU    40                 RELEASE STTN EQUIV RECD 1                    
RIFSER2  EQU    43                    "     "     "    "   2                    
RIFSER3  EQU    46                    "     "     "    "   3                    
RIFEOFD  EQU    251                RELEASE "EOF" MRKR FOR DAYPART RECDS         
RIFEOFD2 EQU    252                RELEASE "EOF" MRKR FOR DAYPART RECDS         
RIFEOFR  EQU    253                RELEASE "EOF" MRKR FOR RATINGS RECDS         
RIFEOFR2 EQU    254                RELEASE "EOF" MRKR FOR RATINGS RECDS         
                                                                                
*                                 ********* BYPASS READ FLAGS *********         
BYPREAD  DC    X'00'                                                            
                                                                                
*                                 ******** MISCELLANEOUS FLAGS ********         
ANYDEMO  DC    XL(MAXGEO)'00'      ANY DEMOS IN MATRIX?                         
NEWSTTN  DC    CL1' '               IS THIS A NEW STATION?                      
                                                                                
*                                 ********** MKT-LEVEL VALUES *********         
MKTPERID DS    XL(L'PAPERID)       PERIOD ID FROM COPYRIGHT RECORD              
                                                                                
*                                 *********** CURRENT VALUES **********         
CURRTYP  DC    X'00'               CURRENT RECORD TYPE                          
CURRPTYP DC    X'00'               CURRENT REPORT TYPE                          
CURRMKT  DC    XL(L'INTMRKT)'00'   CURRENT MARKET                               
CURRSTA  DC    XL(L'INTSTA)'00'    CURRENT STATION                              
CURRBOOK DC    XL(L'INTBOOK)'00'   CURRENT BOOK                                 
CURRGEO  DC    XL(L'INTGEO)'00'    CURRENT GEOGRAPHICAL INDICATOR               
CURRDAY  DC    XL(L'INTDAY)'00'    CURRENT DDS DAY                              
CURRPNAM DC    XL(L'INTPNAM)'00'   CURRENT DPT DESCRIPTION                      
CURRSQH  DC    XL(L'INTSQH)'00'    CURRENT START QH                             
CURREQH  DC    XL(L'INTEQH)'00'    CURRENT END   QH                             
CURRACTV DC    XL(L'PJACTCD)'00'   CURRENT ACTIVITY CODE                        
CURRHO   DC    XL(L'PJHOMOUT)'00'  CURRENT HOME/OUTSIDE INDICATOR               
CURRPERD DC    XL(L'PAPERID)'00'   CURRENT PERIOD ID                            
CURRSCTY DC    XL(L'DSESTNTY)'00'  CURRENT STATION COMBO TYPE                   
CURRSCID DC    XL(L'DSESTNID)'00'  CURRENT STATION COMBO ID                     
                                                                                
*                                 ********** PREVIOUS VALUES **********         
PREVRTYP DC    X'00'               PREVIOUS RECORD TYPE                         
                                                                                
*                                 ******************  *****************         
THISGEO  DC    XL(L'CURRGEO)'00'   LOOP COUNTER FOR GEO INDICATORS              
THISQCNM DC    XL1'00'             LOOP COUNTER FOR CNV-DEFINED QC NUM          
DPTSW    DC    X'00'                                                            
STDCNDMK DC    X'00'                                                            
**********************************************************************          
         DROP  R8,RA,RB,RC                                                      
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
         EJECT                                                                  
*                                 ** CONVERSION-DEFINED DEMO NUMBERS **         
CDNB1217 EQU   01                  BOYS 12-17                                   
CDNM1820 EQU   02                  MEN 18-20                                    
CDNM2124 EQU   03                  MEN 21-24                                    
CDNM1824 EQU   04                  MEN 18-24                                    
CDNM2534 EQU   05                  MEN 25-34                                    
CDNM3544 EQU   06                  MEN 35-44                                    
CDNM4549 EQU   07                  MEN 45-49                                    
CDNM3549 EQU   08                  MEN 35-49                                    
CDNM5054 EQU   09                  MEN 50-54                                    
CDNM5564 EQU   10                  MEN 55-64                                    
CDNM65   EQU   11                  MEN 65+                                      
CDNG1217 EQU   12                  GIRLS 12-17                                  
CDNW1820 EQU   13                  WOMEN 18-20                                  
CDNW2124 EQU   14                  WOMEN 21-24                                  
CDNW1824 EQU   15                  WOMEN 18-24                                  
CDNW2534 EQU   16                  WOMEN 25-34                                  
CDNW3544 EQU   17                  WOMEN 35-44                                  
CDNW4549 EQU   18                  WOMEN 45-49                                  
CDNW3549 EQU   19                  WOMEN 35-49                                  
CDNW5054 EQU   20                  WOMEN 50-54                                  
CDNW5564 EQU   21                  WOMEN 55-64                                  
CDNW65   EQU   22                  WOMEN 65+                                    
CDNB611  EQU   23                  MEN 6-11                                     
CDNG611  EQU   24                  WOMEN 6-11                                   
MAXCDN   EQU   24   <============  MAX # OF CNV-DEFINED DEMO NUMBERS            
                                                                                
*                                 ** CONVERSION-DEFINED QUAL CATGORY **         
QCNONE   EQU   01                  NO QUALITATIVE CATEGORY                      
QCHSGRAD EQU   02                  HIGH SCHOOL GRAD                             
QCCOLLS  EQU   03                  SOME COLLEGE                                 
QCCOLLH  EQU   04                  COLLEGE +                                    
QCINCL25 EQU   05                  INCOME < $25K                                
QCINCG25 EQU   06                  INCOME $25K+                                 
QCINCG50 EQU   07                  INCOME $50K+                                 
QCINCG75 EQU   08                  INCOME $75K+                                 
QCCHILDY EQU   09                  PRESENCE OF CHILDREN IN HH                   
QCCHILDN EQU   10                  NO PRESENCE OF CHILDREN IN HH                
QCHHZE1  EQU   11                  HH SIZE OF 1                                 
QCHHZE2  EQU   12                  HH SIZE OF 2                                 
QCHHZE3  EQU   13                  HH SIZE OF 3                                 
QCHHZG4  EQU   14                  HH SIZE OF 4+                                
MAXQC    EQU   14   <============  MAX # OF CNV-DEFINED QUAL CTGRY #S           
         EJECT                                                                  
DEMQCTBM EQU   200                 MAX # OF NTRIES IN DEMO/QUL CAT TBL          
         EJECT                                                                  
UNIVMX   EQU   280                 MAX NUMBER MKT TO SAVE IN BUFFER             
MXSTTNS  EQU   5000                MAX NUMBER OF STATIONS                       
MXCLCS   EQU   200                 MAX NUMBER OF CALL LETTER CHANGES            
MXSCID   EQU   1500                MAX NUMBER OF NEW STTN COMBO IDs             
MAXGEO   EQU   3                   MAX NUMBER OF MARKET SEGMENTS                
                                                                                
                                                                                
                                                                                
HUNDREDQ EQU   100                                                              
RTEOFQ   EQU   X'FF'               "RECORD TYPE" FOR EOF PROCESSING             
***********************************************************************         
         TITLE 'ARBITRON RADIO DEMO CONVERSION (TABLES)'                        
***********************************************************************         
*=============================== TABLES ==============================*         
                                                                                
ERRMSG00 DS    0CL50               ERROR MESSAGES                               
         DC     CL50'**ERROR DEMCNV** BOOKTYPE MISMATCH'                        
EMBKMMQ  EQU   ((*-ERRMSG00)/(L'ERRMSG00))                                      
         DS    F                                                                
ESUBJ    DS    0CL70               EMAIL SUBJECT                                
         DC    AL2(ESUBJEQU)                                                    
         DC    H'0'                                                             
         DC    C'SUBJECT=ARBITRON PPM MARKETS LOADED FOR: '                     
ESUBJBK  DS    0CL9                                                             
ESUBJSN  DC    CL6' '                                                           
ESUBJSL  DC    C' '                                                             
ESUBJYR  DC    CL2' '                                                           
ESUBJEQU EQU   *-ESUBJ                                                          
         EJECT                                                                  
DISPTAB  DS    0XL(1+3+1+2)                                                     
         DC     C'B',AL3(RTYPGOTB-DERA08I),C'L',AL2(ARTYPGOT-LOCWORKD)          
         DC     C'B',AL3(SAMBTTAB-DERA08I),C'L',AL2(ASAMBTTB-LOCWORKD)          
         DC     C'B',AL3(RPTDSPTB-DERA08I),C'L',AL2(ARPTDSPT-LOCWORKD)          
         DC     C'B',AL3(PERDSPTB-DERA08I),C'L',AL2(APERDSPT-LOCWORKD)          
         DC     C'B',AL3(MKTDSPTB-DERA08I),C'L',AL2(AMKTDSPT-LOCWORKD)          
         DC     C'B',AL3(STNDSPTB-DERA08I),C'L',AL2(ASTNDSPT-LOCWORKD)          
         DC     C'B',AL3(DPTDSPTB-DERA08I),C'L',AL2(ADPTDSPT-LOCWORKD)          
         DC     C'B',AL3(BOOKEQ-DERA08I),C'L',AL2(ABOOKEQ-LOCWORKD)             
         DC     C'B',AL3(DEMODEFN-DERA08I),C'L',AL2(ADEMODEF-LOCWORKD)          
         DC     C'B',AL3(QCATDEFN-DERA08I),C'L',AL2(AQCATDEF-LOCWORKD)          
         DC     C'B',AL3(DEMQCTAB-DERA08I),C'L',AL2(ADEMQCTB-LOCWORKD)          
         DC     C'B',AL3(CVDSPTAB-DERA08I),C'L',AL2(ACVDSPTB-LOCWORKD)          
         DC     C'B',AL3(DPTTAB-DERA08I),C'L',AL2(ADPTTAB-LOCWORKD)             
         DC     C'B',AL3(UPCASETB-DERA08I),C'L',AL2(AUPCSETB-LOCWORKD)          
         DC     C'B',AL3(UNIVS-DERA08I),C'L',AL2(AUNIVS-LOCWORKD)               
         DC     C'B',AL3(STALIST-DERA08I),C'L',AL2(ASTALIST-LOCWORKD)           
         DC     C'B',AL3(CLCTAB-DERA08I),C'L',AL2(ACLCTAB-LOCWORKD)             
         DC     C'B',AL3(SCITAB-DERA08I),C'L',AL2(ASCITAB-LOCWORKD)             
         DC     C'B',AL3(DVMATRIX-DERA08I),C'L',AL2(ADVMATRX-LOCWORKD)          
         DC     C'B',AL3(PREVREC-DERA08I),C'L',AL2(APREC-LOCWORKD)              
         DC     C'B',AL3(SVSREC-DERA08I),C'L',AL2(ASVSREC-LOCWORKD)             
         DC     C'B',AL3(SVSREC2-DERA08I),C'L',AL2(ASVSREC2-LOCWORKD)           
DISPTABQ EQU   ((*-DISPTAB)/(L'DISPTAB))                                        
         EJECT                                                                  
RTYPGOTB DS    0XL(1+2)            RECORD TYPE PROCESS-ROUTINE DSPLCMNT         
         DC     C'A',AL2(RA000-DERA08I)                                         
         DC     C'D',AL2(RD000-DERA08I)                                         
         DC     C'G',AL2(RG000-DERA08I)                                         
         DC     C'H',AL2(RH000-DERA08I)                                         
         DC     C'J',AL2(RJ000-DERA08I)                                         
         DC     C'M',AL2(RM000-DERA08I)                                         
         DC     C'P',AL2(RP000-DERA08I)                                         
         DC     C'S',AL2(RS000-DERA08I)                                         
         DC     C'V',AL2(RV000-DERA08I)                                         
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
SAMBTTAB DS    0CL(2+1+1)           BOOKTYPE GIVEN IN SAMPLE (TAPE)             
         DC     C' 1',X'00',X'00'   STANDARD SURVEY                             
         DC     C' 2',C'B',C'B'     ETHNIC BLACK SURVEY                         
         DC     C' 3',C'H',C'H'     ETHNIC HISPANIC SURVEY                      
         DC     C' 4',C'N',C'N'     NATIONWIDE SURVEY                           
         DC     C' 5',C'P',X'0'     PPM                                         
         DC     C' 6',X'0',X'0'     SMALL MARKET SURVEY                         
         DC     C' 7',C'E',C'B'     PPM Black                                   
         DC     C' 8',C'I',C'H'     PPM Hispanic                                
         DC     C' D',X'00',X'00'   DMA SUMMARY                                 
         DC     C' C',C'C',C'C'     CSAR                                        
         DC     C' A',X'00',X'00'   IHEART PPM OWNER ROLLUP                     
         DC     C' B',X'00',X'00'   IHEART DIARY OWNER ROLLUP                   
         DC    AL1(EOT)                                                         
                                                                                
*&&DO                                                                           
PPMDATES DC    0XL(2+2+2)            PRELIMINARY END DATES FOR PPM MKT          
         DC     AL2(007),AL2(JAN_07),AL2(MAR_07) PHILADELPHIA                   
         DC     AL2(904),AL2(JAN_07),AL2(MAR_07) PHILADELPHIA BLACK             
         DC     AL2(033),AL2(APR_07),AL2(JUN_07) HOUSTON                        
         DC     AL2(848),AL2(APR_07),AL2(JUN_07) HOUSTON HISPANIC               
         DC     AL2(913),AL2(APR_07),AL2(JUN_07) HOUSTON BLACK                  
         DC     AL2(001),AL2(OCT_07),AL2(SEP_08) NEW YORK                       
         DC     AL2(819),AL2(OCT_07),AL2(SEP_08) NEW YORK HISPANIC              
         DC     AL2(901),AL2(OCT_07),AL2(SEP_08) NEW YORK BLACK                 
         DC     AL2(321),AL2(OCT_07),AL2(SEP_08) NASSAU-SUFFOLK                 
         DC     AL2(413),AL2(OCT_07),AL2(SEP_08) MIDDLESEX-SOMERSET-UNI         
         DC     AL2(003),AL2(JUL_08),AL2(SEP_08) LOS ANGELES                    
         DC     AL2(902),AL2(JUL_08),AL2(SEP_08) LOS ANGELES BLACK              
         DC     AL2(842),AL2(JUL_08),AL2(SEP_08) LOS ANGELES HISPANIC           
         DC     AL2(379),AL2(JUL_08),AL2(SEP_08) RIVERSIDE                      
         DC     AL2(809),AL2(JUL_08),AL2(SEP_08) RIVERSIDE HISPANIC             
         DC     AL2(005),AL2(JUL_08),AL2(SEP_08) CHICAGO                        
         DC     AL2(903),AL2(JUL_08),AL2(SEP_08) CHICAGO BLACK                  
         DC     AL2(852),AL2(JUL_08),AL2(SEP_08) CHICAGO HISPANIC               
         DC     AL2(009),AL2(JUL_08),AL2(SEP_08) SAN FRANCISCO                  
         DC     AL2(860),AL2(JUL_08),AL2(SEP_08) SAN FRANCISCO HISPANIC         
         DC     AL2(215),AL2(JUL_08),AL2(SEP_08) SAN JOSE                       
         DC     AL2(879),AL2(JUL_08),AL2(SEP_08) SAN JOSE HISPANIC              
         DC     AL2(024),AL2(OCT_08),AL2(DEC_08) DALLAS                         
         DC     AL2(846),AL2(OCT_08),AL2(DEC_08) DALLAS HISPANIC                
         DC     AL2(912),AL2(OCT_08),AL2(DEC_08) DALLAS BLACK                   
         DC     AL2(015),AL2(OCT_08),AL2(DEC_08) WASHINGTON                     
         DC     AL2(745),AL2(OCT_08),AL2(DEC_08) WASHINGTON HISPANIC            
         DC     AL2(908),AL2(OCT_08),AL2(DEC_08) WASHINGTON BLACK               
         DC     AL2(011),AL2(OCT_08),AL2(DEC_08) DETROIT                        
         DC     AL2(906),AL2(OCT_08),AL2(DEC_08) DETROIT BLACK                  
         DC     AL2(047),AL2(OCT_08),AL2(DEC_08) ATLANTA                        
         DC     AL2(917),AL2(OCT_08),AL2(DEC_08) ATLANTA BLACK                  
         DC     AL2(013),AL2(JAN_09),AL2(MAR_09) BOSTON                         
         DC     AL2(934),AL2(JAN_09),AL2(MAR_09) BOSTON BLACK                   
         DC     AL2(738),AL2(JAN_09),AL2(MAR_09) BOSTON HISPANIC                
         DC    X'FF'                                                            
*&&                                                                             
                                                                                
RPTDSPTB DS    0XL(1+2)            REPORT TYPE DISPLACEMENT                     
         DC     C'A',AL2(PARPTYPE-PADSECT)                                      
         DC     C'D',AL2(PDRPTYPE-PDDSECT)                                      
         DC     C'G',AL2(PGRPTYPE-PGDSECT)                                      
         DC     C'H',AL2(PHRPTYPE-PHDSECT)                                      
         DC     C'J',AL2(PJRPTYPE-PJDSECT)                                      
         DC     C'M',AL2(PMRPTYPE-PMDSECT)                                      
         DC     C'S',AL2(PSRPTYPE-PSDSECT)                                      
         DC     C'V',AL2(PVRPTYPE-PVDSECT)                                      
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
PERDSPTB DS    0XL(1+2)            PERIOD ID DISPLACEMENT                       
         DC     C'A',AL2(PAPERID-PADSECT)                                       
         DC     C'G',AL2(PGPERID-PGDSECT)                                       
         DC     C'H',AL2(PHPERID-PHDSECT)                                       
         DC     C'J',AL2(PJPERID-PJDSECT)                                       
         DC     C'M',AL2(PMPERID-PMDSECT)                                       
         DC     C'V',AL2(PVPERID-PVDSECT)                                       
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
MKTDSPTB DS    0XL(1+2+1)          MARKET CODE DISPLACEMENT                     
         DC     C'A',AL2(PAMKTCD-PADSECT),AL1(L'PAMKTCD)                        
         DC     C'G',AL2(PGMKTCD-PGDSECT),AL1(L'PGMKTCD)                        
         DC     C'H',AL2(PHMKTCD-PHDSECT),AL1(L'PHMKTCD)                        
         DC     C'J',AL2(RJMKTCD-RJDSECT),AL1(L'RJMKTCD)                        
         DC     C'M',AL2(RMMKTCD-RMDSECT),AL1(L'RMMKTCD)                        
         DC     C'V',AL2(PVMKTCD-PVDSECT),AL1(L'PVMKTCD)                        
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
STNDSPTB DS    0XL(1+2+1)          STATION COMBO ID DISPLACEMENT                
         DC     C'V',AL2(PVSCMBTP-PVDSECT),AL1(L'PVSCMBTP+L'PVSCMBID)           
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
DPTDSPTB DS    0XL(1+2+1)          DAYPART CODE DISPLACEMENT                    
         DC     C'S',AL2(PSDPTID-PSDSECT),AL1(L'PSDPTID)                        
         DC     C'V',AL2(PVDPTID-PVDSECT),AL1(L'PVDPTID)                        
         DC    AL1(EOT)                                                         
         EJECT                                                                  
BOOKEQ   DS    0XL(2+1)            BOOK EQUATES FOR SURVEY MONTH                
         DC     C'01',AL1(1)        JAN                                         
         DC     C'02',AL1(2)        FEB                                         
         DC     C'03',AL1(3)        MAR                                         
         DC     C'04',AL1(4)        APR                                         
         DC     C'05',AL1(5)        MAY                                         
         DC     C'06',AL1(6)        JUN                                         
         DC     C'07',AL1(7)        JULY                                        
         DC     C'08',AL1(8)        AUG                                         
         DC     C'09',AL1(9)        SEP                                         
         DC     C'10',AL1(10)       OCT                                         
         DC     C'11',AL1(11)       NOV                                         
         DC     C'12',AL1(12)       DEC                                         
         DC     C'13',AL1(2)        WINTER                                      
         DC     C'14',AL1(5)        SPRING                                      
         DC     C'15',AL1(7)        SUMMER                                      
         DC     C'16',AL1(11)       FALL                                        
         DC     C'17',AL1(05)       WINTER/SPRING ETHNIC                        
         DC     C'18',AL1(05)       FALL/WINTER/SPRING ETHNIC                   
         DC     C'19',AL1(11)       SUMMER/FALL ETHNIC                          
         DC     C'21',AL1(05)       FALL/SPRING HISPANIC                        
         DC     C'23',AL1(12)       HOLIDAY                                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
DEMODEFN DS    0XL(L'PDDEMCD+1)    DEMO NUMBER DEFINITIONS                      
         DC     CL(L'PDDEMCD)'BOYS 12-17',AL1(CDNB1217)                         
         DC     CL(L'PDDEMCD)'MEN 18-20 ',AL1(CDNM1820)                         
         DC     CL(L'PDDEMCD)'MEN 21-24 ',AL1(CDNM2124)                         
         DC     CL(L'PDDEMCD)'MEN 18-24 ',AL1(CDNM1824)                         
         DC     CL(L'PDDEMCD)'MEN 25-34 ',AL1(CDNM2534)                         
         DC     CL(L'PDDEMCD)'MEN 35-44 ',AL1(CDNM3544)                         
         DC     CL(L'PDDEMCD)'MEN 45-49 ',AL1(CDNM4549)                         
         DC     CL(L'PDDEMCD)'MEN 35-49 ',AL1(CDNM3549)                         
         DC     CL(L'PDDEMCD)'MEN 50-54 ',AL1(CDNM5054)                         
         DC     CL(L'PDDEMCD)'MEN 55-64 ',AL1(CDNM5564)                         
         DC     CL(L'PDDEMCD)'MEN 65+   ',AL1(CDNM65)                           
         DC     CL(L'PDDEMCD)'GRLS 12-17',AL1(CDNG1217)                         
         DC     CL(L'PDDEMCD)'WMN 18-20 ',AL1(CDNW1820)                         
         DC     CL(L'PDDEMCD)'WMN 21-24 ',AL1(CDNW2124)                         
         DC     CL(L'PDDEMCD)'WMN 18-24 ',AL1(CDNW1824)                         
         DC     CL(L'PDDEMCD)'WMN 25-34 ',AL1(CDNW2534)                         
         DC     CL(L'PDDEMCD)'WMN 35-44 ',AL1(CDNW3544)                         
         DC     CL(L'PDDEMCD)'WMN 45-49 ',AL1(CDNW4549)                         
         DC     CL(L'PDDEMCD)'WMN 35-49 ',AL1(CDNW3549)                         
         DC     CL(L'PDDEMCD)'WMN 50-54 ',AL1(CDNW5054)                         
         DC     CL(L'PDDEMCD)'WMN 55-64 ',AL1(CDNW5564)                         
         DC     CL(L'PDDEMCD)'WMN 65+   ',AL1(CDNW65)                           
         DC     CL(L'PDDEMCD)'BOYS 6-11 ',AL1(CDNB611)                          
         DC     CL(L'PDDEMCD)'GRLS 6-11 ',AL1(CDNG611)                          
*        DC     CL(L'PDDEMCD)'MEN 6-11  ',AL1(CDNB611)                          
*        DC     CL(L'PDDEMCD)'WMN 6-11  ',AL1(CDNG611)                          
DEMODEFX EQU   *                                                                
DEMODEFQ EQU   (DEMODEFX-DEMODEFN)/(L'DEMODEFN)                                 
                                                                                
                                                                                
                                                                                
QCATDEFN DS    0XL(L'PDQULCD+1)    QUALITATIVE CATEGORY DEFINITIONS             
         DC     CL(L'PDQULCD)'          ',AL1(QCNONE)                           
         DC     CL(L'PDQULCD)'HS GRAD   ',AL1(QCHSGRAD)                         
         DC     CL(L'PDQULCD)'SOME COLL ',AL1(QCCOLLS)                          
         DC     CL(L'PDQULCD)'COLLEGE + ',AL1(QCCOLLH)                          
         DC     CL(L'PDQULCD)'INC <$25K ',AL1(QCINCL25)                         
         DC     CL(L'PDQULCD)'INC 25-49K',AL1(QCINCG25)                         
         DC     CL(L'PDQULCD)'INC 50-74K',AL1(QCINCG50)                         
         DC     CL(L'PDQULCD)'INC $75K+ ',AL1(QCINCG75)                         
         DC     CL(L'PDQULCD)'NO CHILDRN',AL1(QCCHILDN)                         
         DC     CL(L'PDQULCD)'CHILDREN  ',AL1(QCCHILDY)                         
         DC     CL(L'PDQULCD)'HH SIZE 1 ',AL1(QCHHZE1)                          
         DC     CL(L'PDQULCD)'HH SIZE 2 ',AL1(QCHHZE2)                          
         DC     CL(L'PDQULCD)'HH SIZE 3 ',AL1(QCHHZE3)                          
         DC     CL(L'PDQULCD)'HH SIZE 4+',AL1(QCHHZG4)                          
QCATDEFX EQU   *                                                                
QCATDEFQ EQU   (QCATDEFX-QCATDEFN)/(L'QCATDEFN)                                 
         EJECT                                                                  
CVDSPTAB DS    0X                  CONVERSION'S DISPLACEMENT TABLE              
         DC     AL1(5),AL2(CIM1217-CDSCT),AL1(1)                                
         DC      AL1(CDNB1217)                                                  
         DC     AL1(5),AL2(CIM1824-CDSCT),AL1(1)                                
         DC      AL1(CDNM1824)                                                  
         DC     AL1(5),AL2(CIM2534-CDSCT),AL1(1)                                
         DC      AL1(CDNM2534)                                                  
         DC     AL1(5),AL2(CIM3544-CDSCT),AL1(1)                                
         DC      AL1(CDNM3544)                                                  
         DC     AL1(5),AL2(CIM4549-CDSCT),AL1(1)                                
         DC      AL1(CDNM4549)                                                  
         DC     AL1(5),AL2(CIM5054-CDSCT),AL1(1)                                
         DC      AL1(CDNM5054)                                                  
         DC     AL1(5),AL2(CIM5564-CDSCT),AL1(1)                                
         DC      AL1(CDNM5564)                                                  
         DC     AL1(5),AL2(CIM65-CDSCT),AL1(1)                                  
         DC      AL1(CDNM65)                                                    
         DC     AL1(5),AL2(CIW1217-CDSCT),AL1(1)                                
         DC      AL1(CDNG1217)                                                  
         DC     AL1(5),AL2(CIW1824-CDSCT),AL1(1)                                
         DC      AL1(CDNW1824)                                                  
         DC     AL1(5),AL2(CIW2534-CDSCT),AL1(1)                                
         DC      AL1(CDNW2534)                                                  
         DC     AL1(5),AL2(CIW3544-CDSCT),AL1(1)                                
         DC      AL1(CDNW3544)                                                  
         DC     AL1(5),AL2(CIW4549-CDSCT),AL1(1)                                
         DC      AL1(CDNW4549)                                                  
         DC     AL1(5),AL2(CIW5054-CDSCT),AL1(1)                                
         DC      AL1(CDNW5054)                                                  
         DC     AL1(5),AL2(CIW5564-CDSCT),AL1(1)                                
         DC      AL1(CDNW5564)                                                  
         DC     AL1(5),AL2(CIW65-CDSCT),AL1(1)                                  
         DC      AL1(CDNW65)                                                    
         DC     AL1(5),AL2(CIM1820-CDSCT),AL1(1)                                
         DC      AL1(CDNM1820)                                                  
         DC     AL1(5),AL2(CIM2124-CDSCT),AL1(1)                                
         DC      AL1(CDNM2124)                                                  
         DC     AL1(5),AL2(CIW1820-CDSCT),AL1(1)                                
         DC      AL1(CDNW1820)                                                  
         DC     AL1(5),AL2(CIW2124-CDSCT),AL1(1)                                
         DC      AL1(CDNW2124)                                                  
         DC     AL1(5),AL2(CIB611-CDSCT),AL1(1)                                 
         DC      AL1(CDNB611)                                                   
         DC     AL1(5),AL2(CIG611-CDSCT),AL1(1)                                 
         DC      AL1(CDNG611)                                                   
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*======================== DAYPART CODES TABLE ========================*         
                                                                                
* DAYPART CODE TABLE - CODES USED TO IDENTIFY DIFFERENT TIME PERIODS            
*   AND COMBINATIONS OF TIME PERIODS THAT MAY BE REPORTED ON THE CD             
*   SEE  DPTTABD  FOR DESCRIPTION                                               
                                                                                
                                                                                
         ORG   DERA08I+((((*-DERA08I)+X'000F')/X'0010')*X'0010')                
                                                                                
         DC    CL16'**DAYPART TABLE*'                                           
DPTTAB   DS    0XL(DPTTABQ)                                                     
         DC    AL2(001),X'95',AL2(0500,0600),CL16'M-F 5-6A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(002),X'95',AL2(0600,0700),CL16'M-F 6-7A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(003),X'95',AL2(0700,0800),CL16'M-F 7-8A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(004),X'95',AL2(0800,0900),CL16'M-F 8-9A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(005),X'95',AL2(0900,1000),CL16'M-F 9-10A       '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(006),X'95',AL2(1000,1100),CL16'M-F 10-11A      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(007),X'95',AL2(1100,1200),CL16'M-F 11-12N      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(008),X'95',AL2(1200,1300),CL16'M-F 12N-1P      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(009),X'95',AL2(1300,1400),CL16'M-F 1-2P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(010),X'95',AL2(1400,1500),CL16'M-F 2-3P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(011),X'95',AL2(1500,1600),CL16'M-F 3-4P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(012),X'95',AL2(1600,1700),CL16'M-F 4-5P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(013),X'95',AL2(1700,1800),CL16'M-F 5-6P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(014),X'95',AL2(1800,1900),CL16'M-F 6-7P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(015),X'95',AL2(1900,2000),CL16'M-F 7-8P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(016),X'95',AL2(2000,2100),CL16'M-F 8-9P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(017),X'95',AL2(2100,2200),CL16'M-F 9-10P       '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(018),X'95',AL2(2200,2300),CL16'M-F 10-11P      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(019),X'95',AL2(2300,2400),CL16'M-F 11P-12M     '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(020),X'95',AL2(2400,0100),CL16'M-F 12M-1A      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(021),X'95',AL2(0100,0500),CL16'M-F 1A-5A       '             
         DC     C'D',C' ',AL1(21)                                               
*                                                                               
         DC    AL2(022),X'60',AL2(0500,0600),CL16'SAT 5-6A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(023),X'60',AL2(0600,0700),CL16'SAT 6-7A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(024),X'60',AL2(0700,0800),CL16'SAT 7-8A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(025),X'60',AL2(0800,0900),CL16'SAT 8-9A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(026),X'60',AL2(0900,1000),CL16'SAT 9-10A       '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(027),X'60',AL2(1000,1100),CL16'SAT 10-11A      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(028),X'60',AL2(1100,1200),CL16'SAT 11A-12N     '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(029),X'60',AL2(1200,1300),CL16'SAT 12N-1P      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(030),X'60',AL2(1300,1400),CL16'SAT 1-2P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(031),X'60',AL2(1400,1500),CL16'SAT 2-3P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(032),X'60',AL2(1500,1600),CL16'SAT 3-4P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(033),X'60',AL2(1600,1700),CL16'SAT 4-5P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(034),X'60',AL2(1700,1800),CL16'SAT 5-6P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(035),X'60',AL2(1800,1900),CL16'SAT 6-7P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(036),X'60',AL2(1900,2000),CL16'SAT 7-8P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(037),X'60',AL2(2000,2100),CL16'SAT 8-9P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(038),X'60',AL2(2100,2200),CL16'SAT 9-10P       '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(039),X'60',AL2(2200,2300),CL16'SAT 10-11P      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(040),X'60',AL2(2300,2400),CL16'SAT 11P-12M     '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(041),X'60',AL2(2400,0100),CL16'SAT 12M-1A      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(042),X'60',AL2(0100,0500),CL16'SAT 1-5A        '             
         DC     C'H',C' ',AL1(0)                                                
*                                                                               
         DC    AL2(043),X'70',AL2(0500,0600),CL16'SUN 5-6A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(044),X'70',AL2(0600,0700),CL16'SUN 6-7A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(045),X'70',AL2(0700,0800),CL16'SUN 7-8A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(046),X'70',AL2(0800,0900),CL16'SUN 8-9A        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(047),X'70',AL2(0900,1000),CL16'SUN 9-10A       '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(048),X'70',AL2(1000,1100),CL16'SUN 10-11A      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(049),X'70',AL2(1100,1200),CL16'SUN 11A-12N     '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(050),X'70',AL2(1200,1300),CL16'SUN 12N-1P      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(051),X'70',AL2(1300,1400),CL16'SUN 1-2P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(052),X'70',AL2(1400,1500),CL16'SUN 2-3P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(053),X'70',AL2(1500,1600),CL16'SUN 3-4P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(054),X'70',AL2(1600,1700),CL16'SUN 4-5P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(055),X'70',AL2(1700,1800),CL16'SUN 5-6P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(056),X'70',AL2(1800,1900),CL16'SUN 6-7P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(057),X'70',AL2(1900,2000),CL16'SUN 7-8P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(058),X'70',AL2(2000,2100),CL16'SUN 8-9P        '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(059),X'70',AL2(2100,2200),CL16'SUN 9-10P       '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(060),X'70',AL2(2200,2300),CL16'SUN 10-11P      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(061),X'70',AL2(2300,2400),CL16'SUN 11P-12M     '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(062),X'70',AL2(2400,0100),CL16'SUN 12M-1A      '             
         DC     C'H',C' ',AL1(0)                                                
         DC    AL2(063),X'70',AL2(0100,0500),CL16'SUN 1A-5A       '             
         DC     C'D',C' ',AL1(21)                                               
*                                                                               
         DC    AL2(064),X'95',AL2(0600,1000),CL16'M-F 6-10A       '             
         DC     C'D',C'X',AL1(1)                                                
         DC    AL2(065),X'95',AL2(1000,1500),CL16'M-F 10A-3P      '             
         DC     C'A',C'X',AL1(2)                                                
         DC    AL2(066),X'95',AL2(1500,1900),CL16'M-F 3-7P        '             
         DC     C'D',C'X',AL1(3)                                                
         DC    AL2(067),X'95',AL2(1900,2400),CL16'M-F 7P-12M      '             
         DC     C'A',C'X',AL1(4)                                                
         DC    AL2(068),X'95',AL2(0600,1900),CL16'M-F 6A-7P       '             
         DC     C'D',C'D',AL1(5)                                                
         DC    AL2(069),X'95',AL2(0600,2400),CL16'M-F 6A-12M      '             
         DC     C'D',C'D',AL1(6)                                                
         DC    AL2(070),X'95',AL2(0600,1000),CL16'M-F 6-10/3-7    '             
         DC     C'A',C'C',AL1(7)                                                
*                                                                               
         DC    AL2(071),X'60',AL2(0600,1000),CL16'SAT 6-10A       '             
         DC     C'D',C'D',AL1(1)                                                
         DC    AL2(072),X'60',AL2(1000,1500),CL16'SAT 10A-3P      '             
         DC     C'D',C'D',AL1(2)                                                
         DC    AL2(073),X'60',AL2(1500,1900),CL16'SAT 3-7P        '             
         DC     C'D',C'D',AL1(3)                                                
         DC    AL2(074),X'60',AL2(1900,2400),CL16'SAT 7P-12M      '             
         DC     C'D',C'D',AL1(4)                                                
*                                                                               
         DC    AL2(075),X'70',AL2(0600,1000),CL16'SUN 6-10A       '             
         DC     C'D',C'D',AL1(1)                                                
         DC    AL2(076),X'70',AL2(1000,1500),CL16'SUN 10A-3P      '             
         DC     C'D',C'D',AL1(2)                                                
         DC    AL2(077),X'70',AL2(1500,1900),CL16'SUN 3-7P        '             
         DC     C'D',C'D',AL1(3)                                                
         DC    AL2(078),X'70',AL2(1900,2400),CL16'SUN 7P-12M      '             
         DC     C'D',C'D',AL1(4)                                                
         DC    AL2(079),X'E7',AL2(0600,2400),CL16'S-S 6A-12M      '             
         DC     C'D',C'C',AL1(6)                                                
         DC    AL2(080),X'96',AL2(0600,1000),CL16'M-SA 6-10A      '             
         DC     C'D',C'D',AL1(1)                                                
         DC    AL2(081),X'96',AL2(1000,1500),CL16'M-SA 10A-3P     '             
         DC     C'D',C'D',AL1(2)                                                
         DC    AL2(082),X'96',AL2(1500,1900),CL16'M-SA 3P-7P      '             
         DC     C'D',C'D',AL1(3)                                                
         DC    AL2(083),X'96',AL2(1900,2400),CL16'M-SA 7P-12M     '             
         DC     C'D',C'D',AL1(4)                                                
         DC    AL2(084),X'97',AL2(0600,2400),CL16'M-SU 6A-12M     '             
         DC     C'A',C'X',AL1(6)                                                
         DC    AL2(085),X'E7',AL2(1000,1900),CL16'S-S 10A-7P      '             
         DC     C'D',C'C',AL1(9)                                                
         DC    AL2(086),X'97',AL2(2400,0600),CL16'M-SU 12M-6A     '             
         DC     C'D',C'C',AL1(19)                                               
         DC    AL2(087),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(088),X'97',AL2(0100,0500),CL16'M-SU 1A-5A      '             
         DC     C'D',C'C',AL1(21)                                               
*                                                                               
         DC    AL2(089),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(090),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(091),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(092),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(093),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(094),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(095),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
*                                                                               
         DC    AL2(096),X'95',AL2(0600,1500),CL16'M-F 6A-3P       '             
         DC     C' ',C'D',AL1(18)                                               
         DC    AL2(097),X'95',AL2(1000,1900),CL16'M-F 10A-7P      '             
         DC     C' ',C'D',AL1(9)                                                
         DC    AL2(098),X'95',AL2(1000,2400),CL16'M-F 10A-12M     '             
         DC     C' ',C'D',AL1(10)                                               
         DC    AL2(099),X'95',AL2(1500,2400),CL16'M-F 3P-12M      '             
         DC     C' ',C'D',AL1(11)                                               
         DC    AL2(100),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(101),X'95',AL2(0600,1500),CL16'M-F 6-3/7-12M   '             
         DC     C' ',C'C',AL1(15)                                               
         DC    AL2(102),X'95',AL2(0600,1000),CL16'M-F 6-10/3-12M  '             
         DC     C' ',C'C',AL1(13)                                               
         DC    AL2(103),X'95',AL2(0600,1000),CL16'M-F 6-10/7-12M  '             
         DC     C' ',C'C',AL1(14)                                               
         DC    AL2(104),X'95',AL2(1000,1500),CL16'M-F 10-3/7-12M  '             
         DC     C' ',C'C',AL1(17)                                               
         DC    AL2(105),X'95',AL2(0600,1000),CL16'MF 6-10/3-7/WK  '             
         DC     C' ',C'C',AL1(16)                                               
*                                                                               
         DC    AL2(106),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(107),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(108),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(109),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
*                                                                               
         DC    AL2(110),X'96',AL2(0600,1000),CL16'M-SA 6-10/3-7   '             
         DC     C' ',C'C',AL1(7)                                                
         DC    AL2(111),X'95',AL2(0500,1000),CL16'M-F 5A-10A      '             
         DC     C' ',C'D',AL1(12)                                               
         DC    AL2(112),X'97',AL2(1500,2400),CL16'M-SU 3P-12M     '             
         DC     C' ',C'D',AL1(11)                                               
         DC    AL2(113),X'97',AL2(1900,2400),CL16'M-SU 7P-12M     '             
         DC     C' ',C'D',AL1(4)                                                
         DC    AL2(114),X'97',AL2(1000,1500),CL16'M-SU 10A-3P     '             
         DC     C' ',C'D',AL1(2)                                                
         DC    AL2(115),X'97',AL2(1000,1500),CL16'M-SU 10-3/7-12M '             
         DC     C' ',C'C',AL1(17)                                               
*                                                                               
         DC    AL2(116),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(117),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(118),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(119),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(120),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(121),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(122),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(123),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(124),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(125),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(126),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(127),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
         DC    AL2(128),X'00',AL2(0000,0000),CL16'**NOT  DEFINED**'             
         DC     C' ',C' ',AL1(00)                                               
*                                                                               
         DC    AL2(129),X'97',AL2(0600,3000),CL16'M-SU 6A-6A      '             
         DC     C'D',C'C',AL1(20)                                               
*                                                                               
         DC    X'FFFF'                                                          
                                                                                
DPTTABX  EQU   *                                                                
DPTTABL  EQU   DPTTABX-DPTTAB                                                   
DPTTABN  EQU   ((DPTTABL-2)/(L'DPTTAB))                                         
                                                                                
         DS    0XL((DPTTABL-((DPTTABN*L'DPTTAB)+2))+1)                          
         DS    0XL((((DPTTABN*L'DPTTAB)+2)-DPTTABL)+1)                          
         EJECT                                                                  
*========================== UPPER CASE TABLE =========================*         
                                                                                
UPCASETB DS    0X                  TABLE TO TRANSLATE TO UPPER CASE             
         DC    XL16'000102030405060708090A0B0C0D0E0F'   00 - 0F                 
         DC    XL16'101112131415161718191A1B1C1D1E1F'   10 - 1F                 
         DC    XL16'202122232425262728292A2B2C2D2E2F'   20 - 2F                 
         DC    XL16'303132333435363738393A3B3C3D3E3F'   30 - 3F                 
         DC    XL16'404142434445464748494A4B4C4D4E4F'   40 - 4F                 
         DC    XL16'505152535455565758595A5B5C5D5E5F'   50 - 5F                 
         DC    XL16'606162636465666768696A6B6C6D6E6F'   60 - 6F                 
         DC    XL16'707172737475767778797A7B7C7D7E7F'   70 - 7F                 
         DC    XL16'80C1C2C3C4C5C6C7C8C98A8B8C8D8E8F'   80 - 8F                 
         DC    XL16'90D1D2D3D4D5D6D7D8D99A9B9C9D9E9F'   90 - 9F                 
         DC    XL16'A0A1E2E3E4E5E6E7E8E9AAABACADAEAF'   A0 - AF                 
         DC    XL16'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'   B0 - BF                 
         DC    XL16'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'   C0 - CF                 
         DC    XL16'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'   D0 - DF                 
         DC    XL16'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'   E0 - EF                 
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'   F0 - FF                 
UPCSETBX DS    0X                                                               
         DS    0CL(X'0100'-(UPCSETBX-UPCASETB)+1)                               
         DS    0CL((UPCSETBX-UPCASETB)-X'0100'+1)                               
         EJECT                                                                  
*                                 ********** UNIQUE BOOKTYPE **********         
UNIQBTY  DS    0XL10                                                            
         DC    X'FF'                                                            
         DS    XL9                                                              
         DC    X'FF'                                                            
*                                 ********** PREVIOUS RECORD **********         
         DS    0D                                                               
         DC    C'**PREC**'                                                      
PREVREC  DS    0D                                                               
         DS     XL2000                                                          
PREVRECX EQU   *                                                                
PREVRECL EQU   PREVRECX-PREVREC                                                 
         DS    0XL((PREVRECL-2000)+1)                                           
         DS    0XL((2000-PREVRECL)+1)                                           
                                                                                
                                                                                
                                                                                
*                                 ******** SAVE SORT RECORD #1 ********         
         DS    0D                                                               
         DC    C'*SVSREC*'                                                      
SVSREC   DS    0D                                                               
         DS    2000C                                                            
SVSRECX  EQU   *                                                                
SVSRECL  EQU   SVSRECX-SVSREC                                                   
                                                                                
                                                                                
                                                                                
*                                 ******** SAVE SORT RECORD #2 ********         
         DS    0D                                                               
         DC    C'*SVSREC2'                                                      
SVSREC2  DS    0D                                                               
         DS    2000C                                                            
SVSREC2X EQU   *                                                                
SVSREC2L EQU   SVSREC2X-SVSREC2                                                 
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*DMQCTB*'                                                      
DEMQCTAB DS    0D                  DEMO/QUAL CTGRY TABLE (DEMQCTBD)             
         DS     F                   NUMBER OF ENTRIES                           
         DS     F                   (SPARE)                                     
         DS     (DEMQCTBM)XL(DEMQCTBQ)                                          
DEMQCTBX EQU   *                                                                
DEMQCTBL EQU   (DEMQCTBX-DEMQCTAB)                                              
                                                                                
                                                                                
                                                                                
         DS    0D                                                               
         DC    C'**UNIV**'                                                      
UNIVS    DS    0D                  UNIVERSE TABLE                               
         DS    (UNIVMX)XL(UNVBUFFQ)                                             
UNIVX    EQU   *                                                                
UNIVL    EQU   UNIVX-UNIVS         LENGTH OF UNIVERSE BUFFER                    
                                                                                
                                                                                
                                                                                
         DS    0D                                                               
         DC    C'*STALST*'                                                      
STALIST  DS    0XL(STALISTQ)                                                    
         DS     (MXSTTNS)XL(L'STALIST)                                          
STALISTX EQU   *                                                                
STALISTL EQU   STALISTX-STALIST                                                 
                                                                                
                                                                                
                                                                                
         DS    0D                                                               
         DC    C'*CLCTAB*'                                                      
CLCTAB   DS    0XL(6+6)                                                         
         DS    (MXCLCS)XL(L'CLCTAB)                                             
CLCTABX  EQU   *                                                                
CLCTABL  EQU   CLCTABX-CLCTAB                                                   
                                                                                
                                                                                
                                                                                
         DS    0D                                                               
         DC    C'*NEWSCI*'                                                      
SCITAB   DS    0XL(SCILISTQ)                                                    
         DS    (MXSCID)XL(L'SCITAB)                                             
SCITABX  EQU   *                                                                
SCITABL  EQU   SCITABX-SCITAB                                                   
         EJECT                                                                  
                                                                                
                                                                                
                                                                                
*                                 ********* DEMO VALUE MATRIX *********         
         DS    0D                                                               
         DC    C'*DVMATX*'                                                      
DVMATRIX DS    0D                                                               
         DS     (MAXGEO*MAXQC*MAXCDN)F  N'MKT_SEGMTS  X  N'DEM  X  N'QC         
DVMATRXX EQU   *                                                                
DVMATRXL EQU   DVMATRXX-DVMATRIX                                                
                                                                                
                                                                                
                                                                                
***********************************************************************         
       ++INCLUDE DERAREC                                                        
         EJECT                                                                  
*                                                                               
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (DEDEMCNVD)'                     
***********************************************************************         
*============================= DEDEMCNVD =============================*         
       ++INCLUDE DEDEMCNVD                                                      
***********************************************************************         
*============================= DDCOMFACS =============================*         
       ++INCLUDE DDCOMFACS                                                      
***********************************************************************         
*============================= DEDEMTABD =============================*         
       ++INCLUDE DEDEMTABD                                                      
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (DEINTD)'                        
***********************************************************************         
*=============================== DEINTD ==============================*         
                                                                                
       ++INCLUDE DEINTD                                                         
                                                                                
                                                                                
       ++INCLUDE DEINTTP95D                                                     
                                                                                
                                                                                
         ORG   INTACCS                                                          
OIMP     DS     XL(LN)              IMPRESSIONS                                 
OPAHIMP  DS     XL(LN)              AWAY FROM HOME IMPS                         
OUNIV    DS     XL(LN)              UNIVERSES                                   
OCUME    DS     XL(LN)              CUMES                                       
OEXCL    DS     XL(LN)              EXCLUSIVE CUMES                             
OTOT     DS     XL(LN)              MSA/TSA/ADI TOTALS                          
OCTOT    DS     XL(LN)              CUME TOTALS                                 
OATOT    DS     XL(LN)              PAH IMPS TOT                                
O611     DS     XL(LN)              B/G611                                      
*                                                                               
IBLK1    EQU   OIMP                INTACCS                                      
IBLK2    EQU   OPAHIMP             INTACCS+LN                                   
IBLK3    EQU   OUNIV               INTACCS+LN+LN                                
IBLK4    EQU   OCUME               INTACCS+LN+LN+LN                             
*                                                                               
NDEMS    EQU   22                  DEMO CATAGORIES ON RECORDS                   
LN       EQU   NDEMS*4             LENGTH OF A DEMO BLOCK                       
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (INTERIM RECORD)'                
***********************************************************************         
*=========================== INTERIM RECORD ==========================*         
CDSCT    DSECT                                                                  
                                                                                
         ORG   CDSCT+(INTACCS-INTERD)                                           
CACCS    DS   0XL4                ACCUMULATORS                                  
CIM1217  DS    XL4                 IM1217                                       
CIM1824  DS    XL4                 IM1824                                       
CIM2534  DS    XL4                 IM2534                                       
CIM3544  DS    XL4                 IM3544                                       
CIM4549  DS    XL4                 IM4549                                       
CIM5054  DS    XL4                 IM5054                                       
CIM5564  DS    XL4                 IM5564                                       
CIM65    DS    XL4                 IM65+                                        
CIW1217  DS    XL4                 IW1217                                       
CIW1824  DS    XL4                 IW1824                                       
CIW2534  DS    XL4                 IW2534                                       
CIW3544  DS    XL4                 IW3544                                       
CIW4549  DS    XL4                 IW4549                                       
CIW5054  DS    XL4                 IW5054                                       
CIW5564  DS    XL4                 IW5564                                       
CIW65    DS    XL4                 IW65+                                        
CIM1820  DS    XL4                 IM1820                                       
CIM2124  DS    XL4                 IM2124                                       
CIW1820  DS    XL4                 IW1820                                       
CIW2124  DS    XL4                 IW2124                                       
CIB611   DS    XL4                 MEN 6-11                                     
CIG611   DS    XL4                 WOMEN 6-11                                   
                                                                                
         DS    0XL(((*-CACCS)-LN)+1)                                            
         DS    0XL((LN-(*-CACCS))+1)                                            
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (DEDEMFILE)'                     
***********************************************************************         
*============================= DEDEMFILE =============================*         
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (LOCAL WORK AREA)'               
***********************************************************************         
*========================== LOCAL WORK AREA ==========================*         
                                                                                
LOCWORKD DSECT                                                                  
*                                 *********** BASE REGISTERS **********         
MYBASE2  DS    A                   BASE TO 2ND 4096 BYTES OF MAIN NMOD          
MYBASE1  DS    A                   BASE TO 1ST 4096 BYTES OF MAIN NMOD          
BASERD   DS    A                   RD LINK TO EXIT PHASE                        
                                                                                
*                                 ********* ADDRESS CONSTANTS *********         
ARTYPGOT DS    A                   A(RTYPGOTB)                                  
ASAMBTTB DS    A                   A(SAMBTTAB)                                  
ARPTDSPT DS    A                   A(RPTDSPTB)                                  
APERDSPT DS    A                   A(PERDSPTB)                                  
AMKTDSPT DS    A                   A(MKTDSPTB)                                  
ASTNDSPT DS    A                   A(STNDSPTB)                                  
ADPTDSPT DS    A                   A(DPTDSPTB)                                  
ABOOKEQ  DS    A                   A(BOOKEQ)                                    
ADEMODEF DS    A                   A(DEMODEFN)                                  
AQCATDEF DS    A                   A(QCATDEFN)                                  
ACVDSPTB DS    A                   A(CVDSPTAB)                                  
ADPTTAB  DS    A                   A(DPTTAB)                                    
AUPCSETB DS    A                   A(UPCASETB)                                  
ADEMQCTB DS    A                   A(DEMQCTAB)                                  
AUNIVS   DS    A                   A(UNIVS)                                     
ASTALIST DS    A                   A(STALIST)                                   
ACLCTAB  DS    A                   A(CLCTAB)                                    
ASCITAB  DS    A                   A(SCITAB)                                    
ADVMATRX DS    A                   A(DVMATRIX)                                  
APREC    DS    A                   A(PREVREC)                                   
ASVSREC  DS    A                   A(SVSREC)                                    
ASVSREC2 DS    A                   A(SVSREC2)                                   
                                                                                
ADPTNTRY DS    A                   A(DPTTAB ENTRY)                              
                                                                                
*                                 ************ TEMP STORAGE ***********         
TMPDMVAL DS    F                   DEMO VALUE                                   
TMPGEO   DS    XL(L'CURRGEO)                                                    
TMPCLCIN DS    XL(L'PJCLCIND)      CALL-LETTER-CHANGE INDICATOR                 
                                                                                
*                                 *************************************         
AUDCHID  DS    CL(L'RDACHID)       AUDIENCE CHARACTERISTIC ID                   
PPRDMNUM DS    XL1                 PRE-PROCESSOR DEMO NUMBER                    
PPRQCNUM DS    XL1                 PRE-PROCESSOR QUAL CTGRY #                   
CNVDMNUM DS    XL1                 CONVERSION-DEFINED DEMO NUMBER               
CNVQCNUM DS    XL1                 CONVERSION-DEFINED QUAL CTGRY #              
                                                                                
SAMPLEBT DS    CL1                 BOOKTYPE FROM SAMPLE, I.E. TAPE              
HOLIDAY  DS    XL1                 HOLIDAY BOOK?                                
                                                                                
OLDCALL  DS    CL(L'DSEOLDCL)      OLD CALL LETTERS                             
                                                                                
TPKEY    DS    XL24                KEY FROM TIME-PERIOD FILE                    
TPKEYSV  DS    XL(L'TPKEY)         KEY SAVE AREA                                
                                                                                
TEMPMNO  DS    CL3                 TEMP FIELDS USED FOR MSGREC ROUTINE          
TEMPMKT  DS    CL30                                                             
TEMPBK   DS    XL3                                                              
SVMRKT   DS    XL2                                                              
*                                                                               
LOCWORKX EQU   *                                                                
LOCWORKL EQU   (LOCWORKX-LOCWORKD)                                              
***********************************************************************         
                                                                                
         TITLE 'ARBITRON RADIO DEMO CONVERSION (OTHER DSECTS)'                  
***********************************************************************         
*============================ OTHER DSECTS ===========================*         
                                                                                
DPTTABD  DSECT                                                                  
DPTRSVID DS    XL2                 RATING SERVICES DAYPART NUMBER               
DPTIDAY  DS    XL1                 INTERNAL DAY                                 
DPTSTIM  DS    XL2                 START TIME                                   
DPTETIM  DS    XL2                 END   TIME                                   
DPTNAME  DS    CL16                DAYPART NAME                                 
DPTATYPE DS    CL1                 AQH TYPE: A=AWAY,D=DPT,H=HOUR,' '=NA         
DPTCTYPE DS    CL1                 CUME TYP: C=CUME,D=DPT,X=EXCL,' '=NA         
DPTIDPT  DS    XL1                 DDS DAYPART NUMBER                           
DPTTABQ  EQU   *-DPTTABD                                                        
                                                                                
                                                                                
                                                                                
DEMQCTBD DSECT                                                                  
DQTPPQCN DS    XL1                 PRE-PROCESSOR'S QUAL CATGRY #                
DQTPPDMN DS    XL1                        "        DEMO NUMBER                  
DQTKEYL  EQU   *-DEMQCTBD                                                       
DQTCVQCN DS    XL1                 CONVERSION-DEFINED QUAL CATGRY #             
DQTCVDMN DS    XL1                           "        DEMO NUMBER               
DEMQCTBQ EQU   *-DEMQCTBD                                                       
                                                                                
                                                                                
                                                                                
CVDSPTBD DSECT                                                                  
CDTNLEN  DS    XL1                 LENGTH OF ENTRY                              
CDTIDSP  DS    XL2                 DISPLACEMENT INTO INTERIM RECORD             
CDTNCDN  DS    XL1                 NUMBER OF CONV-DEFINED DEMO #S               
CDTCDNS  DS    0XL1                VARIABLE # OF CONV-DEFINED DEMO #S           
                                                                                
                                                                                
                                                                                
UNVBUFFD DSECT                                                                  
UBDKEY   DS    0X                                                               
UBDMKT   DS     XL(L'INTMRKT)       MARKET                                      
UBDGEO   DS     XL(L'INTGEO)        GEOGRAPHIC INDICATOR                        
         DS     XL1                 (SPARE)                                     
UBDKEYL  EQU   *-UBDKEY                                                         
UBDUNIVS DS    XL(LN)              BLOCK OF UNIVERSE VALUES                     
UNVBUFFX EQU   *                                                                
UNVBUFFQ EQU   UNVBUFFX-UNVBUFFD                                                
         EJECT                                                                  
STALISTD DSECT                                                                  
SLMKT    DS    XL(L'CURRMKT)       MARKET                                       
SLSTTN   DS    XL(L'CURRSTA)       STATION CALL LETTERS                         
SLSCMBTY DS    XL(L'PJSCMBTP)      STATION COMBO TYPE                           
SLSCMBID DS    XL(L'PJSCMBID)      STATION COMBO ID                             
SLACTCD  DS    XL(L'PJACTCD)       STATION ACTIVITY CODE                        
SLHOMOUT DS    XL(L'PJHOMOUT)      HOME/OUTSIDE INDICATOR                       
STALISTQ EQU   *-STALISTD                                                       
                                                                                
                                                                                
                                                                                
SCILISTD DSECT                                                                  
SITYPE   DS    XL(L'CURRSCTY)      STATION COMBO TYPE                           
SIID     DS    XL(L'CURRSCID)      STATION COMBO ID                             
SCILISTQ EQU   *-SCILISTD                                                       
         EJECT                                                                  
TOTBUFD  DSECT                                                                  
TBKEY    DS    0X                  KEY                                          
TBMKT    DS     XL(L'INTMRKT)       MARKET                                      
TBGEO    DS     XL(L'INTGEO)        GEOGRAPHICAL INDICATOR                      
TBAQH    DS     XL(L'INTAQH)        ESTIMATE TYPE                               
TBDPT    DS     XL(L'INTDPT)        DAYPART                                     
TBLISN   DS     XL(L'INTLISN)       LISTENING LOCATION                          
TBKEYL   EQU   *-TBKEY                                                          
TBDEMS   DS    XL(LN)               16-4BYTE DEMO BCKTS FROM IBLK1              
TOTBUFQ  EQU   *-TOTBUFD           DISPL TO NEXT TOT ENTRY                      
                                                                                
                                                                                
                                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDMONYREQU                                                     
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067DEIH08I   06/28/16'                                      
         END                                                                    
