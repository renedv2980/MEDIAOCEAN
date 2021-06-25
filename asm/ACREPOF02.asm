*          DATA SET ACREPOF02  AT LEVEL 002 AS OF 03/23/15                      
*PHASE ACOF02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
         TITLE 'OFFICE LOCATOR'                                                 
**********************************************************************          
* QOPT1 = Y SUPRESS PRINTING DETAILS OF EACH OFFCE AND ACCOUNT       *          
**********************************************************************          
ACOF02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACOF**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACOFD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
         CLI   MODE,REQLAST              LAST FOR REQUEST                       
         BE    REQL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
*                                                                               
         LA    RE,IO               RE=A(IO AREA)                                
         LA    RF,IOLNQ            RF=(LENGTH OF IO AREA)                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'         SET WIDTH FOR REPORT                    
*                                                                               
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVC   SVOFF,SPACES                                                     
         MVI   FLAG,0                                                           
         MVI   RCSUBPRG,1                                                       
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD                                    
*                                                                               
         USING BIND,R1             CLEAR BIN TABLES                             
         L     R1,AOFFTAB          BIN TABLE BUILT OFF RECOVERY TAPE            
         XC    BININ,BININ                                                      
         DROP  R1                                                               
*                                                                               
         BAS   RE,GETLEVS                                                       
*                                                                               
         USING OFFRECD,R2                                                       
         LA    R2,SVKEY                                                         
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ              TYPE 01                            
         MVC   OFFKCPY,RCCOMPFL              MOVE IN COMPANY CODE               
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)        READ OFFICE RECORD                 
         B     REQF20                                                           
REQF10   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
REQF20   CLC   SVKEY(OFFKOFF-OFFKEY),IOKEY                                      
         BNE   REQF50                                                           
         LA    R2,IOKEY                                                         
         CLC   QSELECT,SPACES                                                   
         BE    *+14                                                             
         CLC   OFFKOFF,QSELECT                                                  
         BNE   REQF10                                                           
*                                                                               
         USING OFFD,R4                                                          
         LA    R4,OFFWRK           POINT TO RECOVERY TABLE TO BE BUILT          
         XC    OFFWRK,OFFWRK                                                    
         MVC   OFFCODE,OFFKOFF      SAVE OFF OFFICE CODE                        
         MVI   OFFLIST,C'N'                                                     
         TM    OFFKSTAT,OFFSLIST    IS THIS OFFICE AN OFFICE LIST               
         BNO   *+8                                                              
         MVI   OFFLIST,C'L'                                                     
*                                                                               
         LA    R0,OFFBKCT          NO. OF BUCKETS                               
         LA    R1,OFFBKT                                                        
         ZAP   0(OFFBKLN,R1),=P'0'     CLEAR FIELDS                             
         LA    R1,OFFBKLN(R1)                                                   
         BCT   R0,*-10             NUMBER OF BUCKETS TO LOOP                    
         GOTO1 ABINADD,DMCB,(RC),OFFWRK,AOFFTAB   ADD TABLE ENTRY               
         B     REQF10                                                           
         EJECT                                                                  
*                                                                               
         USING BIND,R1             CLEAR BIN TABLES                             
REQF50   L     R1,AOFFTAB          BIN TABLE BUILT OFF RECOVERY TAPE            
         ICM   R0,15,BININ                                                      
         BZ    REQFX               MAKE IT REQFX                                
*                                                                               
         USING OFARECD,R2                                                       
REQF60   DS    0H                                                               
         LA    R2,SVKEY                                                         
         MVC   OFAKEY,SPACES                                                    
         MVC   OFAKCPY,RCCOMPFL              MOVE IN COMPANY                    
         MVC   OFAKULA,QUNIT                 MOVE IN U/L/ACC FROM REQST         
*                                                                               
REQF65   GOTO1 =A(DMHIGHDR),DMCB,(RC)        READ OFFICE RECORD                 
         B     REQF80                                                           
REQF70   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
REQF80   CLC   SVKEY(OFAKULA-OFAKEY),IOKEY                                      
         BNE   REQFX                                                            
*                                                                               
         LA    R2,IOKEY                                                         
         CLC   QUNIT,SPACES                                                     
         BE    *+14                                                             
         CLC   QUNIT(2),IOKEY+1                                                 
         BNE   REQF70                                                           
*                                                                               
         CLC   =C'SJ',OFAKUNT      ARE WE DOING SJ ACCOUNT                      
         BNE   REQF82                                                           
         BAS   RE,GETSJ                                                         
         B     REQF70                                                           
*                                                                               
REQF82   CLC   OFAKOFF,SPACES                                                   
         BNH   REQF70                                                           
*                                                                               
         CLC   OFAKOFF+2(L'ACTKCULA),SPACES    IS ANYTHNG IN CNTRA ACC          
         BNH   REQF85             NO                                            
*                                                                               
         MVC   SVKEY,IOKEY                                                      
         MVI   SVKEY+(CHDKCCPY-CHDKEY),X'FF'   READ FOR NEXT OFFICE             
         B     REQF65                       READ HIGH FOR NEXT ACCOUNT          
*                                                                               
REQF85   CLC   QSELECT,SPACES                                                   
         BE    *+14                                                             
         CLC   OFAKOFF,QSELECT                                                  
         BNE   REQF70                                                           
*                                                                               
         USING BIND,R1             SEARCH BIN TABLES                            
         L     R1,AOFFTAB          BIN TABLE BUILT OFF OFFICE RECORD            
         ICM   R0,15,BININ                                                      
         LA    R4,BINTAB                                                        
         DROP  R1                                                               
         CLC   OFFCODE,OFAKOFF                                                  
         BE    REQF90                                                           
         LA    R4,OFFLNQ(R4)                                                    
         BCT   R0,*-14                                                          
         B     REQF70              OFFICE FOUND W/O AN OFFICE RECD              
*                                                                               
REQF90   DS    0H                                                               
NEW      USING OFFD,R6                                                          
         LA    R6,OFFWRK                                                        
         MVC   OFFWRK,SPACES                                                    
         MVC   NEW.OFFCODE,OFFCODE                                              
         MVC   NEW.OFFLIST,OFFLIST                                              
*                                                                               
         LHI   R0,OFFBKCT          NO. OF BUCKETS                               
         LA    R1,NEW.OFFBKT                                                    
         ZAP   0(OFFBKLN,R1),=P'0'     CLEAR FIELDS                             
         LA    R1,OFFBKLN(R1)                                                   
         BCT   R0,*-10             NUMBER OF BUCKETS TO LOOP                    
*                                                                               
         MVC   MSG,=CL10'BINTABBFR'                                             
         GOTO1 ADUMP,DMCB,(RC),(R4),OFFLNQ                                      
*                                                                               
         ZAP   NEW.OFFACCNT,=P'1'      UPDATE ACCOUNT COUNTER                   
         ZAP   NEW.OFFTOTAL,=P'1'      UPDATE TOTAL COUNTER                     
         DROP  NEW                                                              
*                                                                               
         MVC   MSG,=CL10'BINTAB #1'                                             
         GOTO1 ADUMP,DMCB,(RC),OFFWRK,L'OFFWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),OFFWRK,AOFFTAB   UPDATE BUCKETS                
*                                                                               
         CLI   QOPT1,C'Y'          DO WE WANT TO PRINT DETAILS                  
         BE    REQF70                                                           
*                                                                               
         USING SRTD,R5                                                          
         LA    R5,SRTWRK                                                        
         MVC   SRTREC(SRTLNQ),SPACES      CLEAR WORK AREA                       
         MVC   SRTOFF,OFFCODE                                                   
         MVC   SRTLIST,OFFLIST                                                  
         MVC   SRTACC,OFAKULA                                                   
*                                                                               
         MVC   MSG,=CL10'SORTER-PUT'                                            
         GOTO1 ADUMP,DMCB,(RC),SRTWRK,SRTLNQ                                    
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SRTREC                                      
         B     REQF70                                                           
*                                                                               
REQFX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING SRTD,R5                                                          
REQL     DS    0H                                                               
         LA    R5,SRTWRK                                                        
*                                                                               
REQL10   GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   RE,15,DMCB+4        LAST RECORD FROM SORT                        
         BZ    REQL20                                                           
         MVC   SRTREC(SRTLNQ),0(RE)                                             
*                                                                               
         MVC   MSG,=CL10'SORTER-GET'                                            
         GOTO1 ADUMP,DMCB,(RC),SRTREC,SRTLNQ                                    
*                                                                               
*        CLI   QOPT1,C'Y'                                                       
*        BE    REQL10              SUPRESS PRINTING DETAILS                     
         BAS   RE,CLEARXP                                                       
         BAS   RE,PRINTIT                                                       
         B     REQL10                                                           
*                                                                               
REQL20   DS    0H                 START PRINTING SUMMARY PAGE                   
         NI    FLAG,X'FF'-FLGPCPY       TURN OF COMPANY PRINTED FLAG            
         MVI   RCSUBPRG,2         FROM BINTABLE COUNTERS                        
         MVI   FORCEHED,C'Y'      START NEW PAGE                                
         BAS   RE,CLEARXP                                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
REQL30   DS    0H                 START PRINTING SUMMARY PAGE                   
         NI    FLAG,X'FF'-FLGPCPY       TURN OF COMPANY PRINTED FLAG            
         MVI   RCSUBPRG,3         FROM BINTABLE COUNTERS                        
         MVI   FORCEHED,C'Y'      START NEW PAGE                                
         BAS   RE,CLEARXP                                                       
         BAS   RE,PRINTIT                                                       
REQLX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* CLEAR XP TO SPACES                                                            
**********************************************************************          
         SPACE 1                                                                
CLEARXP  NTR1                                                                   
         LA    R5,XP                                                            
         MVI   XP,C' '             FILL XP WITH SPACES                          
         MVC   XP+1(L'XP-1),XP                                                  
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT A LINE                                                                  
**********************************************************************          
         USING PLINED,R5                                                        
         SPACE 1                                                                
PRNTLIN  NTR1                                                                   
         LA    R5,XP                                                            
         MVI   POFF-1,X'BF'            FILL XP WITH DASHES                      
         MVC   POFF(PLINELNQ-2),POFF-1                                          
         BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
* R2=A(ACCOUNT RECORD)                                                          
* R4=A(BINTABLE ENTRY)                                                          
**********************************************************************          
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         USING SRTD,R5                                                          
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
*                                                                               
         TM    FLAG,FLGPCPY        HAVE WE PRINTED THE COMPANY                  
         BO    PRINT10                                                          
         GOTO1 HEXOUT,DMCB,RCCOMPFL,PCPY,L'RCCOMPFL                             
         OI    FLAG,FLGPCPY                                                     
PRINT10  DS    0H                                                               
         CLI   RCSUBPRG,2                                                       
         BE    PRINT20             PRINT SUMMARY PAGE                           
*                                                                               
         CLI   RCSUBPRG,3                                                       
         BE    PRINT40             PRINT LIST SUMMARY PAGE                      
*                                                                               
         CLC   SVOFF,SRTOFF                                                     
         BE    *+14                                                             
         MVC   SVOFF,SRTOFF        UPDATE SAVED OFFICE                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   POFF,SRTOFF         OFF THE BINTABLE                             
         MVC   POFF,SRTOFF         OFF THE BINTABLE                             
         MVC   PACCODE,SRTACC                                                   
         GOTO1 ACREPORT                                                         
         B     PRINTX                                                           
         DROP  R5                                                               
*                                                                               
PRINT20  DS    0H                  START PRINTING SUMMARY                       
*                                                                               
         USING BIND,R1                                                          
         USING OFFD,R4                                                          
         L     R1,AOFFTAB                                                       
         ICM   R3,15,BININ                                                      
         BZ    PRINTX                                                           
         EDIT  (R3),SVTOTAL       TOTAL NUMBER OF OFFICES                       
         LA    R4,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
PRINT30  MVC   POFF,OFFCODE        PRINT OFFICE CODE                            
         MVC   PLIST(1),OFFLIST    PRINT L OR N                                 
         EDIT  OFFACCNT,PACTOTAL   PRINT ACCOUNT TOTAL                          
         EDIT  OFFSJCNT,PSJTOTAL   PRINT SJ ACCOUNT TOTAL                       
         EDIT  OFFTOTAL,PTOTAL                                                  
         GOTO1 ACREPORT                                                         
         LA    R4,OFFLNQ(R4)                                                    
         BCT   R3,PRINT30                                                       
         B     PRINTX                                                           
*                                                                               
PRINT40  DS    0H                                                               
         USING BIND,R1                                                          
         USING OFFD,R4                                                          
         L     R1,AOFFTAB                                                       
         ICM   R3,15,BININ                                                      
         BZ    PRINTX                                                           
         LA    R4,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
PRINT45  MVC   POFF,OFFCODE        PRINT OFFICE CODE                            
         USING OFFRECD,R2                                                       
         LA    R2,SVKEY                                                         
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ              TYPE 01                            
         MVC   OFFKCPY,RCCOMPFL              MOVE IN COMPANY CODE               
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)        READ OFFICE RECORD                 
         B     PRINT60                                                          
PRINT50  GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
PRINT60  CLC   SVKEY(OFFKOFF-OFFKEY),IOKEY                                      
         BNE   PRINT110                                                         
         LA    R2,IOKEY                                                         
         TM    OFFKSTAT,OFFSLIST                                                
         BNO   PRINT50                                                          
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,IO                                                            
         USING OFLELD,R5                                                        
         LA    R5,OFFRFST                                                       
PRINT70  CLI   0(R5),0                                                          
         BE    PRINT50                                                          
         CLI   0(R5),OFLELQ        IS IT X'D2' ELEMENT                          
         BE    PRINT80                                                          
PRINT75  ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     PRINT70                                                          
*                                                                               
         USING OFFD,R4                                                          
PRINT80  DS    0H                                                               
         ZIC   R1,OFLLN                                                         
         SHI   R1,OFLLN1Q          SUBTRACT OVERHEAD                            
         SRA   R1,1                DIVIDE BY 2                                  
         LA    R6,OFLNTRY                                                       
PRINT90  CLC   0(L'OFLNTRY,R6),OFFCODE     IS OFFICE IN THIS LIST               
         BE    PRINT100                                                         
         LA    R6,L'OFLNTRY(R6)                                                 
         BCT   R1,PRINT90                                                       
         B     PRINT75                                                          
PRINT100 MVC   PLIST,OFFKOFF        OFFICE LIST CODE                            
         GOTO1 ACREPORT                                                         
         B     PRINT50             READ NEXT OFFICE RECORD                      
*                                                                               
PRINT110 LA    R4,OFFLNQ(R4)                                                    
         BCT   R3,PRINT45                                                       
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         MVC   XP(26),=CL26'TOTAL NUMBER OF OFFICES = '                         
         MVC   XP+27(L'SVTOTAL),SVTOTAL                                         
         GOTO1 ACREPORT                                                         
PRINTX   B     EXIT                                                             
         DROP  R4,R5,R7                                                         
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R5                                                        
         USING ACTRECD,R2                                                       
GETLEVS  NTR1                                                                   
         LA    R2,SVKEY                                                         
         MVC   SVKEY,SPACES                                                     
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKUNT(2),=C'SJ'                                                
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
         BNE   GLEVX                    SJ NOT SETUP FOR THIS AGENCY            
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,IO                                                            
         LA    R5,ACTRFST          GET HEIRARCHY LEVELS                         
GLEV05   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),X'16'                                                      
         BE    GLEV07                                                           
*                                                                               
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     GLEV05                                                           
*                                                                               
GLEV07   MVC   LEVELS(LEVLNQ),SPACES                                            
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         EX    R1,*+4                                                           
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
GLEV10   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GLEV20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GLEV10                                                        
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* GET SJ RECORD AT CLIENT AND PRODUCT LEVEL ONLY                     *          
* R2=KEY OF ACCOUNT                                                             
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
GETSJ    NTR1                                                                   
         CLI   LEVNUM,4                                                         
         BNE   *+6                                                              
         DC    H'0'                ASSUMING SJ IS ALWAY 3 LEVEL LEDGER          
*                                                                               
         ZIC   R1,LEVB             LEVA+LEVB LENGTH                             
         LA    RE,IOKEY                                                         
         LA    RE,3(R1,RE)         POINT TO JOB LEVEL                           
         CLI   0(RE),C' '          ARE WE AT THE JOB LEVEL ALREADY              
         BNE   GETSJX                                                           
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,IO                                                            
         LA    R5,ACTRFST                                                       
         USING PPRELD,R5                                                        
GETSJ10  CLI   0(R5),0                                                          
         BE    GETSJX                                                           
         CLI   0(R5),X'24'         IS IT A PPRELQ ELEMENT                       
         BE    GETSJ20                                                          
         ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     GETSJ10                                                          
*                                                                               
GETSJ20  DS    0H                                                               
         USING BIND,R1             CLEAR BIN TABLES                             
         L     R1,AOFFTAB          BIN TABLE BUILT OFF OFFICE RECORD            
         ICM   R0,15,BININ                                                      
         BZ    GETSJX                                                           
*                                                                               
GETSJ30  DS    0H                                                               
         USING OFFD,R4                                                          
         LA    R4,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
GETSJ40  CLC   PPRGAOFF,OFFCODE    COMPARE WITH WHATS IN OFFICE TABLE           
         BE    GETSJ50                                                          
         LA    R4,OFFLNQ(R4)       BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,GETSJ40                                                       
         B     GETSJX                                                           
GETSJ50  DS    0H                                                               
NEW      USING OFFD,R6                                                          
         LA    R6,OFFWRK                                                        
         MVC   OFFWRK,SPACES                                                    
         MVC   NEW.OFFCODE,OFFCODE                                              
         MVC   NEW.OFFLIST,OFFLIST                                              
*                                                                               
         LHI   R0,OFFBKCT          NO. OF BUCKETS                               
         LA    R1,NEW.OFFBKT                                                    
         ZAP   0(OFFBKLN,R1),=P'0'     CLEAR FIELDS                             
         LA    R1,OFFBKLN(R1)                                                   
         BCT   R0,*-10             NUMBER OF BUCKETS TO LOOP                    
*                                                                               
         MVC   MSG,=CL10'BINTABBFR'                                             
         GOTO1 ADUMP,DMCB,(RC),(R4),OFFLNQ                                      
*                                                                               
         ZAP   NEW.OFFSJCNT,=P'1'      UPDATE ACCOUNT COUNTER                   
         ZAP   NEW.OFFTOTAL,=P'1'      UPDATE TOTAL COUNTER                     
         DROP  NEW                                                              
*                                                                               
         MVC   MSG,=CL10'BINTAB #1'                                             
         GOTO1 ADUMP,DMCB,(RC),OFFWRK,L'OFFWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),OFFWRK,AOFFTAB   UPDATE BUCKETS                
*                                                                               
         CLI   QOPT1,C'Y'          DO WE WANT TO PRINT DETAILS                  
         BE    GETSJX                                                           
*                                                                               
         USING SRTD,R5                                                          
         LA    R5,SRTWRK                                                        
         MVC   SRTREC(SRTLNQ),SPACES      CLEAR WORK AREA                       
         MVC   SRTOFF,OFFCODE                                                   
         MVC   SRTLIST,OFFLIST                                                  
         MVC   SRTACC,ACTKULA                                                   
*                                                                               
         MVC   MSG,=CL10'SORTER-PUT'                                            
         GOTO1 ADUMP,DMCB,(RC),SRTWRK,SRTLNQ                                    
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SRTREC                                      
*                                                                               
GETSJX   B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* SORTER DEFINITIONS                                                 *          
**********************************************************************          
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,2,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=17'                                    
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
*                                                                               
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
ACCMST   DC    CL8'ACCMST'                                                      
PKROUND  DC    XL2'010C'                                                        
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(BINADD)           ROUTINE TO ADD TO BINSEARCH TABLE            
         DC    A(OFFTAB)           OFFICE CODE TABLE                            
         DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(SORTER)           PRINT DATA                                   
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R5,56,ELCODE                                                     
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         CLI   RCSUBPRG,2                                                       
         BNE   BXHK10                                                           
*                                                                               
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         MVI   BOXCOLS+(POFF-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PLIST-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PACTOTAL-PRTLINE-1),C'C'                                
         MVI   BOXCOLS+(PSJTOTAL-PRTLINE-1),C'C'                                
         MVI   BOXCOLS+(PTOTAL-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+PLINELNQ,C'R'                                            
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
         B     BXXIT                                                            
*                                                                               
BXHK10   MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         MVI   BOXCOLS+(POFF-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PACCODE-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+PLINELNQ,C'R'                                            
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS BXHOOK  NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
BINA10   GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
*                                                                               
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
*                                                                               
         L     R4,DMCB             A(RECORD FOUND)                              
         LR    RE,R3                                                            
         LHI   R6,OFFLIST-OFFD     DISPLACEMENT TO LIST BYTE                    
         AR    R4,R6               BUMP TO ERROR BYTE IN TABLE                  
         AR    RE,R6               BUMP TO ERROR BYTE IN NEW ITEM               
         MVC   0(L'OFFLIST,R4),0(RE)                                            
*                                                                               
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         SR    R6,R6                                                            
         IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
BINA20   AP    0(L'OFFBKT,R4),0(L'OFFBKT,R3)   ADD TO BUCKET                    
         LA    R3,L'OFFBKT(R3)    BUMP TO NEXT ENTRY IN NEW ITEM                
         LA    R4,L'OFFBKT(R4)    BUMP TO NEXT ENTRY IN TABLE                   
         BCT   R0,BINA20                                                        
*                                                                               
         B     BINXIT                                                           
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS  BINADD NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 APRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),        +        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS  DUMP NMOD1                                               *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,IO,DMWORK                  
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',ACCKDA,IO,DMWORK            
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
*                                                                               
* BINTABLE 1 - 1R ACCOUNT TABLE BUILT OFF RECOVERY TAPE                         
*                                                                               
         DC    C'**OFFTAB**'                                                    
OFFTAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 5               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(OFFLNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(OFFKLNQ)            KEY LENGTH                               
         DC    AL4(OFFMAX)             MAX IN TABLE                             
         DC    AL1(OFFBKCT)            NUMBER OF BUCKETS                        
         DC    AL1(OFFBKT-OFFD)        DISPLACEMENT TO FIRST BUCKET             
         DS    (OFFMAX*OFFLNQ)XL1      TABLE                                    
                                                                                
OFFMAX   EQU   2000                                                             
         SPACE 1                                                                
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACOFD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
VTYPES   DS    0A                                                               
ABINADD  DS    A                   ROUTINE TO ADD TO BINSEARCH TABLE            
AOFFTAB  DS    A                   1R ACC TABLE BUILT FROM RECOVERY             
ADUMP    DS    A                   ROUTINE TO DITTO RECORDS                     
APRNTBL  DS    V                   PRINT DATA                                   
VSORTER  DS    V                   SORT DATA                                    
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
MSG      DS    CL10                DUMP MESSAGE                                 
ELCODE   DS    XL1                                                              
*                                                                               
FLAG     DS    XL1                                                              
FLGPCPY  EQU   X'80'               PRINT COMPANY CODE ONCE                      
*                                                                               
SVKEY    DS    CL42                                                             
SVOFF    DS    CL2                                                              
SVTOTAL  DS    CL10                TOTAL NUMBER OF OFFICES                      
*                                                                               
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL12               LEVEL CODES                                  
LEVACDE  DS    CL12                LEVEL A CODE                                 
LEVBCDE  DS    CL12                LEVEL B CODE                                 
LEVCCDE  DS    CL12                LEVEL C CODE                                 
LEVDCDE  DS    CL12                LEVEL D CODE                                 
LEVSLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
LEVNMES  DS    0CL36               LEVEL NAMES                                  
LEVANME  DS    CL36                LEVEL A NAME                                 
LEVBNME  DS    CL36                LEVEL B NAME                                 
LEVCNME  DS    CL36                LEVEL C NAME                                 
LEVDNME  DS    CL36                LEVEL D NAME                                 
*                                                                               
*                                                                               
OFFWRK   DS    CL(OFFLNQ)          BINSEARCH WORK AREA - OFFICE TABLE           
SRTWRK   DS    CL(SRTLNQ)          WORK AREA FOR SORTER                         
*                                                                               
RECVIO   DS    CL(RCVRECRD-RCVRECD+2000)                                        
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
*                                                                               
         EJECT                                                                  
**********************************************************************          
* SORT RECORD DSECT                                                  *          
**********************************************************************          
         SPACE 1                                                                
SRTD     DSECT                                                                  
SRTREC   DS    0C                                                               
SRTOFF   DS    CL2                 SORT OFFICE CODE                             
SRTLIST  DS    CL1                 SORT  LIST                                   
SRTACC   DS    CL14                SORT ACCOUNT CODE                            
SRTLNQ   EQU   *-SRTREC            LENGTH OR RECORD                             
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
         DS    CL2                                                              
PCPY     DS    CL2                 COMPANY CODE                                 
         DS    CL6                                                              
POFF     DS    CL2                 OFFICE CODE                                  
         DS    CL6                                                              
PACCODE  DS    CL14                ACCOUNT CODE                                 
         DS    CL2                                                              
*                                                                               
         ORG   PACCODE                                                          
         DS    CL2                                                              
PLIST    DS    CL2                                                              
         DS    CL4                                                              
PACTOTAL DS    CL12                ACCOUNT TOTAL                                
         DS    CL2                                                              
PSJTOTAL DS    CL12                SJ ACCOUNT TOTAL                             
         DS    CL2                                                              
PTOTAL   DS    CL12                TOTAL                                        
         DS    CL10                                                             
         ORG                                                                    
PLINELNQ EQU   *-PRTLINE                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT OFFICE RECORD TABLE                                           *         
***********************************************************************         
         SPACE 1                                                                
OFFD     DSECT                                                                  
OFFCODE  DS    CL2                 OFFICE CODE                                  
OFFKLNQ  EQU   *-OFFD                                                           
OFFLIST  DS    CL1                 OFFICE IS A LIST                             
OFFBKT   DS    0PL8                BUCKET                                       
OFFACCNT DS    PL8                 NUMBER OF TIMES IN REG ACCOUNT               
OFFBKLN  EQU   *-OFFBKT            BUCKET LENGTH                                
OFFSJCNT DS    PL8                 NUMBER OF TIMES IN SJ                        
OFFTOTAL DS    PL8                 NUMBER OF TIMES IN FILE                      
OFFBKCT  EQU   (*-OFFBKT)/OFFBKLN     NUMBER OF BUCKETS                         
OFFLNQ   EQU   *-OFFD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACBIGPRINTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
* ACRCVRECD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPOF02 03/23/15'                                      
         END                                                                    
