*          DATA SET SPREPZW02  AT LEVEL 016 AS OF 03/19/12                      
*PHASE SPZW02A                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPZW02 - SCAN WORKER FILES FOR INVOICES'                        
SPZW02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPZW02,R8,RR=R2                                                
                                                                                
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
*                                                                               
         MVI   MQFLAG,C'N'                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,CTDAY)                                 
*                                                                               
* DATE RANGE HERE                                                               
*                                                                               
         XC    SDAY,SDAY                                                        
         XC    EDAY,EDAY                                                        
         CLC   QEND,SPACES                                                      
         BNH   ZW11                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(2,SDAY)                                  
         GOTO1 DATCON,DMCB,(0,QEND),(2,EDAY)                                    
*                                                                               
ZW11     DS    0H                                                               
         L     R6,VMASTC                                                        
         USING MASTD,R6                                                         
*                                                                               
         MVC   MCDUB,=CL8'T00A2C'                                               
         GOTO1 MCVLOADM,DMCB,0                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   EZWRKIO,DMCB+4                                                   
         DROP  R6                                                               
*                                                                               
         LA    RE,EZWRKIOB                                                      
         LHI   RF,WRKIOBL                                                       
         XCEFL                                                                  
*                                                                               
         MVC   WRKEZUID,RCORIGID                                                
         MVC   WRKIACOM,ACOMFACS                                                
         LA    RF,IO                                                            
         ST    RF,WRKIAREC                                                      
         MVC   WRKIABUF,AWKBUFF                                                 
         MVI   WRKIFTYP,WRKIFTEZ                                                
         MVI   WRKIACTN,WRKIANDX                                                
*                                                                               
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    ZW12                                                             
         CLI   WRKIERRS,WRKIEEOF   FAKE CALL: DON'T CARE IF EOF                 
         BE    ZW12                                                             
         DC    H'0'                                                             
*                                                                               
ZW12     DS    0H                                                               
         XC    WRKEZKEY,WRKEZKEY   START WITH 1ST INDEX                         
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         LA    R1,MYHEAD                                                        
         ST    R1,HEADHOOK                                                      
*                                                                               
         L     RE,ADCONLST                                                      
         USING SPADCONS,RE                                                      
         MVC   AMQRPT,VMQRPT                                                    
         DROP  RE                                                               
*                                                                               
         MVI   WRKIFILE+4,C'1'     ALL FILES                                    
*                                                                               
         CLI   QOPT5,C'T'          TRACE                                        
         BNE   ZW20                                                             
         MVC   P(5),WRKIFILE                                                    
         GOTO1 REPORT                                                           
*                                                                               
ZW20     DS    0H                                                               
*&&DO                                                                           
         LA    R2,WRKRINDX                                                      
         GOTO1 DATAMGR,DMCB,DINDEX,WRKFILE,WRKRINDX,IO,AWKBUFF                  
         CLI   8(R1),0                                                          
         BE    ZW30                                                             
         CLI   8(R1),X'90'         EOF?                                         
         BNE   ZW30                YES                                          
*&&                                                                             
*                                                                               
         MVI   WRKIACTN,WRKIANDX                                                
         MVI   WRKINDS,WRKIWFNQ+WRKINOXQ                                        
         XC    WRKEZUID,WRKEZUID                                                
*                                                                               
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    ZW30                                                             
         CLI   WRKIERRS,WRKIEEOF   FAKE CALL: DON'T CARE IF EOF                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   WRKIFILE+4,C'F'      DONE ALL FILES                              
         BE    ZW200                YES                                         
*                                                                               
         CLI   WRKIFILE+4,C'9'      GO TO NEXT FILE                             
         BNE   *+12                                                             
         MVI   WRKIFILE+4,C'A'                                                  
         B     ZW24                                                             
         ZIC   R1,WRKIFILE+4                                                    
         LA    R1,1(,R1)                                                        
         STC   R1,WRKIFILE+4                                                    
*                                                                               
ZW24     XC    WRKEZKEY,WRKEZKEY                                                
*                                                                               
         CLI   QOPT5,C'T'          TRACE                                        
         BNE   ZW20                                                             
         MVC   P(10),WRKIFILE                                                   
         GOTO1 REPORT                                                           
         B     ZW20                                                             
*                                                                               
*        FILTER WORKER FILE                                                     
*                                                                               
ZW30     DS    0H                                                               
         CLI   WRKEZDAY,WRKEZDYQ   IS IT AN INVOICE                             
         BNE   ZW20                                                             
*                                                                               
         OC    SDAY,SDAY                                                        
         BZ    ZW31                                                             
*                                                                               
         CLC   WRKEZBDT,SDAY                                                    
         BL    ZW20                                                             
         CLC   WRKEZBDT,EDAY                                                    
         BH    ZW20                                                             
         B     ZW50                                                             
*                                                                               
ZW31     DS    0H                                                               
         CLC   WRKEZBDT,CTDAY      RIGHT DAY?                                   
         BNE   ZW20                                                             
*                                                                               
*        FILTER OUT DDS AGENCIES                                                
*                                                                               
         DROP  R2                                                               
*                                                                               
*        PROCESS WORKER FILE                                                    
*                                                                               
ZW50     LA    R0,IO               SET 'TO' ADDRESS                             
         LA    R1,L'IO             SET 'TO' LENGTH                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*&&DO                                                                           
         GOTO1 DATAMGR,DMCB,DREAD,WRKFILE,WRKRINDX,IO,AWKBUFF                   
         CLI   8(R1),0                                                          
         BE    ZW52                                                             
         CLI   8(R1),X'90'         EOF?                                         
         BE    ZW20                NEXT INDEX                                   
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
         MVI   WRKIACTN,WRKIAGET                                                
         GOTO1 EZWRKIO,WRKIOB                                                   
         CLI   WRKIERRS,0                                                       
         BE    ZW52                                                             
         CLI   WRKIERRS,WRKIEEOF   FAKE CALL: DON'T CARE IF EOF                 
         BE    ZW20                                                             
         DC    H'0'                                                             
*                                                                               
ZW52     DS    0H                                                               
         LA    R3,IO                                                            
         CLC   IO+4(2),=C'22'      STATION INFO                                 
         BE    ZW60                                                             
         CLC   IO+4(2),=C'31'      INVOICE INFO                                 
         BE    ZW70                                                             
         B     ZW50                                                             
***                                                                             
*        PROCESS STATION HEADER RECORD - 22                                     
***              *******                                                        
ZW60     DS    0H                                                               
         MVC   EZCALL,SPACES                                                    
         MVC   EZMED,SPACES                                                     
         MVC   EZBAND,SPACES                                                    
*                                                                               
         CLI   QOPT5,C'T'          TRACE                                        
         BNE   ZW62                                                             
         MVC   P(50),IO                                                         
         GOTO1 REPORT                                                           
*                                                                               
ZW62     LA    R1,IO+7             PAST LEN, REC ID AND SEMI-COLON              
         MVC   EZCALL,0(R1)        GETS YOU THE CALL LETTERS                    
*                                                                               
         CLI   EZCALL+1,X'5E'      SYSCODES ARE GETTING SENT AS 1               
         BNE   *+10                                                             
         MVC   EZCALL+1(3),=C'   '                                              
*                                                                               
         CLI   EZCALL+2,X'5E'      SYSCODES ARE GETTING SENT AS 2               
         BNE   *+10                                                             
         MVC   EZCALL+2(2),=C'   '                                              
*                                                                               
         CLI   EZCALL+3,X'5E'      AND ANY STATION CAN BE 3                     
         BNE   *+8                                                              
         MVI   EZCALL+3,C' '                                                    
*                                                                               
         LA    R1,1(R1)            MAY BE ANY LEN 1 TO 4                        
         CLI   0(R1),X'5E'                                                      
         BE    ZW63                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
*                                                                               
ZW63     LA    R1,1(R1)            BUMP PAST SEMI TO MEDIA                      
         CLI   0(R1),X'5E'         IS THERE ANYTHING                            
         BE    ZW65                                                             
         MVC   EZMED,0(R1)         SAVE MEDIA                                   
         CLI   EZMED+1,X'5E'       WAS ONLY 1 CHAR SENT                         
         BNE   *+8                                                              
         MVI   EZMED+1,X'40'       SPACE OUT SEMICOLON                          
ZW64     LA    R1,1(R1)                                                         
         CLI   0(R1),X'5E'         NEXT SEMI-COLON                              
         BNE   ZW64                                                             
*                                                                               
ZW65     LA    R1,1(R1)            POINT TO BAND                                
         CLI   0(R1),X'5E'         IS THERE ANYTHING                            
         BE    ZW50                                                             
         MVC   EZBAND,0(R1)        SAVE BAND                                    
         CLI   EZBAND+1,X'5E'      WAS ONLY 1 CHAR SENT                         
         BNE   *+8                                                              
         MVI   EZBAND+1,X'40'      SPACE OUT SEMICOLON                          
         B     ZW50                                                             
***                                                                             
*        PROCESS INVOICE HEADER RECORD - 31                                     
***              *******                                                        
ZW70     DS    0H                                                               
*                                                                               
         MVC   EZ12STAT,IO+7       12 STATUS BYTES                              
         CLI   QOPT4,C'A'          INCLUDE NON-IM INVOICES                      
         BE    *+12                                                             
         TM    EZ12STAT,X'01'      INVOICE CAME FROM IM                         
         BNO   ZW50                SKIP                                         
**                                                                              
         CLI   QOPT5,C'T'          TRACE                                        
         BNE   ZW71                                                             
         MVC   P(80),IO                                                         
         GOTO1 REPORT                                                           
**                                                                              
*                                                                               
*        OPTIONAL FILTERS                                                       
*                                                                               
ZW71     CLI   QOPT1,C'A'          INCLUDE ALL INVOICES                         
         BE    ZW80                                                             
*                                                                               
ZW72     CLI   QOPT1,C'C'          INCLUDE CONVERTED INVOICES                   
         BE    ZW73                                                             
         TM    EZ12STAT,X'40'      CONVERTED                                    
         BO    ZW50                SKIP                                         
ZW73     CLI   QOPT2,C'D'          INCLUDE DELETED INVOICES                     
         BE    ZW74                                                             
         TM    EZ12STAT,X'08'      DELETED                                      
         BO    ZW50                SKIP                                         
ZW74     DS    0H                                                               
*                                                                               
*        GET DATA AND ADD TO FILE                                               
*                                                                               
ZW80     LA    R1,IO+20            PAST LEN,REC,12,SEMI-COLON                   
         SR    R2,R2                                                            
*                                                                               
ZW83     CLI   0(R1),X'5E'         LOOK FOR 4TH SEMI                            
         BE    ZW84                                                             
         LA    R1,1(R1)                                                         
         B     ZW83                                                             
ZW84     LA    R1,1(R1)            BUMP PAST SEMI                               
         LA    R2,1(R2)            ADD TO COUNTER                               
         CHI   R2,4                                                             
         BL    ZW83                                                             
         MVC   EZINVDAT,0(R1)                                                   
****     LA    R1,7(R1)            FIXED LEN 6 + SEMI                           
*                                                                               
         SR    R2,R2                                                            
ZW93     CLI   0(R1),X'5E'         LOOK FOR NEXT 3RD SEMI                       
         BE    ZW94                                                             
         LA    R1,1(R1)                                                         
         B     ZW93                                                             
ZW94     LA    R1,1(R1)            BUMP PAST SEMI                               
         LA    R2,1(R2)            ADD TO COUNTER                               
         CHI   R2,3                                                             
         BL    ZW93                                                             
         MVC   EZINVNUM,0(R1)                                                   
*                                                                               
         LA    R2,10               CLEAR OUT IF < 10                            
         LA    R3,EZINVNUM                                                      
ZW95     CLI   0(R3),X'5E'         CHECK IF LESS THAN LEN 10                    
         BE    ZW96                                                             
         LA    R3,1(R3)                                                         
         BCT   R2,ZW95                                                          
         B     ZW98                                                             
*                                                                               
ZW96     MVI   0(R3),C' '                                                       
         LA    R3,1(R3)                                                         
         BCT   R2,ZW96                                                          
*                                                                               
ZW98     DS    0H                                                               
         SR    R2,R2                                                            
         CLI   0(R1),X'5E'                                                      
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         LA    R1,1(R1)            BUMP PAST SEMI                               
         MVC   EZINVMOS,0(R1)                                                   
*                                                                               
ZW100    DS    0H                                                               
         SR    R2,R2                                                            
ZW105    CLI   0(R1),X'5E'         LOOK FOR NEXT 26 SEMI                        
         BE    ZW110                                                            
         LA    R1,1(R1)                                                         
         B     ZW105                                                            
ZW110    LA    R1,1(R1)            BUMP PAST SEMI                               
         LA    R2,1(R2)            ADD TO COUNTER                               
         CHI   R2,26                                                            
         BL    ZW105                                                            
         MVC   EZ12ST2,0(R1)                                                    
*                                                                               
ZW120    DS    0H                                                               
         BAS   RE,WRTIMGR          WRITE INFO OUT TO FILE                       
         B     ZW50                NEXT READ                                    
*                                                                               
*****************************************************************               
*        SORT INFO - NOT USED RIGHT NOW                                         
*****************************************************************               
*                                                                               
         LA    R4,IO2                                                           
         USING SORTRECD,R4                                                      
         XC    IO2(SRLENQ),IO2                                                  
*                                                                               
         MVC   SRMED,EZMED         MEDIA                                        
         DROP  R4                                                               
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R4)                                     
         B     ZW50                                                             
*                                                                               
*****************************************************************               
*        DONE READING WORKER FILES - CLOSE FILE AND SEND TO EDICT               
*****************************************************************               
*                                                                               
ZW200    DS    0H                                                               
*&&DO                                                                           
         CLI   WRTFLAG,C'Y'        HAS DATASET BEEN OPEN?                       
         BNE   ZW210               NO - DON'T BOTHER CLOSING IT                 
*                                                                               
         L     R2,=A(EZINVMGR)                                                  
         CLOSE ((R2),)                                                          
         FREEPOOL (R2)                                                          
*                                                                               
ZW210    DS    0H                                                               
*&&                                                                             
         CLI   MQFLAG,C'Y'                                                      
         BNE   ZW220                                                            
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ZW220    GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         GOTO1 AENDREQ                                                          
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
**********************************************************************          
*        SPECIAL AGENCY TABLE IF WORKER FILE UNDER DIFFERENT ID                 
*        AGENCY (2 CHAR) - USER-ID NUMBER                                       
**********************************************************************          
*                                                                               
         DS    0H                                                               
AGYTAB   DC    CL2'H9',H'9211'                                                  
         DC    X'FFFF'                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
**********************************************************************          
*       ALLOCATE FILE FOR WRITEBACK TO INVOICE MANAGER                          
*       - ADD INVOICE DETAILS TO FILE - EZI DATA SET                            
*       - ADD INVOICE DETAILS TO MQ                                             
**********************************************************************          
*                                                                               
WRTIMGR  NTR1                                                                   
*&&DO                                                                           
         CLI   WRTFLAG,C'Y'        IS DATASET ALLOCATED?                        
         BE    WRT10               YES - WRITE TO IT                            
*                                                                               
         MVC   WORK,SPACES                                                      
         LA    R6,WORK                                                          
         USING DSNNAMED,R6                                                      
*                                                                               
         MVC   DSNROOT,=C'EZI'                                                  
         MVI   DSNROOT+L'DSNROOT,C'.'                                           
*                                                                               
         MVI   DSNDATE,C'D'                                                     
         L     RF,VMASTC                                                        
         MVC   DSNDATE+1(2),MCDATE-MASTD(RF)   COPY MONTH                       
         MVC   DSNDATE+3(2),MCDATE-MASTD+3(RF) COPY DAY                         
         MVI   DSNDATE+L'DSNDATE,C'.'                                           
*                                                                               
         L     R2,MCJOBSTM-MASTD(,RF)       JOB START TIME IN SEC*100           
         MHI   R2,3           TIMCON WANTS IT IN SEC*300                        
         STCM  R2,15,SVIMTIME                                                   
         GOTO1 TIMCON,DMCB,(0,SVIMTIME),DUB                                     
*                                                                               
         MVI   DSNTIME,C'T'                                                     
         CLI   DUB,C' '            ONE OR TWO DIGIT HOUR?                       
         BH    *+8                 TWO DIGITS - BRANCH                          
         MVI   DUB,C'0'            ONE DIGIT - PREPEND ZERO                     
         MVC   DSNTIME+1(2),DUB      COPY HOURS                                 
         MVC   DSNTIME+3(2),DUB+3    COPY MINUTES                               
         MVI   DSNTIME+L'DSNTIME,C'.'                                           
*                                                                               
*        EXTRACT JOB NUMBER                                                     
*                                                                               
         ST    RD,SVRD                                                          
         LINK  EP=FWRGETJI,PARAM=(SVIMJOB)                                      
         L     RD,SVRD                                                          
         MVC   DSNJOB(3),=C'JOB'                                                
         MVC   DSNJOB+3(5),SVIMJOB+3                                            
*                                                                               
         MVC   SVIMDSN(DSNNMLQ),WORK                                            
*                                                                               
         CLI   QOPT3,C'F'          USE OWN FILE                                 
         BE    WRT08               YES- SKIP DYNALLOC                           
*                                                                               
         GOTO1 DYNALLOC,DMCB,(C'A',=CL8'EZINVMGR')                              
         CLI   DMCB+4,0                                                         
         BE    *+6                                                              
         DC    H'0'                DDNAME ALREADY ALLOCATED, WTF?               
*                                                                               
         GOTO1 DYNALLOC,DMCB,(X'80',=C'EZINVMGR'),                     X        
               (X'81',=XL6'000001000010'),                             X        
               (X'C0',WORK)                                                     
         OC    DMCB+12(4),DMCB+12                                               
         BZ    *+6                                                              
         DC    H'0'                DYNALLOC CALL FAILED, VERY, VERY SAD         
         DROP  R6                                                               
*                                                                               
WRT08    L     R2,=A(EZINVMGR)                                                  
         OPEN  ((R2),(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   WRTFLAG,C'Y'        INDICATE DATASET ALLOCATED                   
*                                                                               
         MVC   P(4),=C'FILE'                                                    
         MVC   P+10(DSNNMLQ),WORK                                               
         GOTO1 REPORT                                                           
*                                                                               
*        OPEN AN MQ ENTRY AS WELL                                               
*                                                                               
WRT09    DS    0H                                                               
*&&                                                                             
         CLI   QOPT3,C'F'          SKIP MQ IF USING OWN FILE -TESTING           
         BE    WRT10                                                            
         CLI   MQFLAG,C'Y'                                                      
         BE    WRT10                                                            
*                                                                               
         MVI   MQFLAG,C'Y'                                                      
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,=C'IMSTATUS********'),0,0            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        CREATE THE INFO LINE                                                   
*                                                                               
WRT10    DS    0H                                                               
         XC    IMLINE,IMLINE                                                    
         LA    R2,IMLINE+4                                                      
         USING WMGRD,R2                                                         
* LINE BREAK                                                                    
         MVC   WMGRSEP,=C'<DDSSEPERATOR>'                                       
*                                                                               
* ACTION                                                                        
         MVI   WMGRACT,C'L'        LOADED                                       
* INVOICE NUMBER                                                                
         MVC   WMGRINV,EZINVNUM                                                 
* INVOICE MOS                                                                   
         MVC   WMGRMOS,EZINVMOS                                                 
* INVOICE DATE                     WHAT TO DO WITH BAD DATE                     
         LA    R0,6                SKIP IF NOT VALID?                           
         LA    R1,EZINVDAT                                                      
WRT15    CLI   0(R1),C'0'                                                       
         BL    WRT16                                                            
         CLI   0(R1),C'9'                                                       
         BH    WRT16                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,WRT15                                                         
         GOTO1 DATCON,DMCB,(0,EZINVDAT),(5,WMGRDATE)                            
* STATION (ORIGINAL)                                                            
WRT16    MVC   WMGRSTA,EZCALL      FROM THE 22 RECORD                           
* BAND (ORIRINAL)                                                               
         MVC   WMGRBND,EZBAND      FROM THE 22 RECORD                           
* MEDIA (ORIGINAL)                                                              
         MVC   WMGRMED,EZMED       FROM THE 22 RECORD                           
* USER ID                                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+CTIKNUM-CTIKEY(2),WRKEZUID                                   
*                                                                               
         L     R6,ADAGY                                                         
         CLC   0(L'CTIKEY,R6),KEY                                               
         BE    WRT17                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,ADAGY                     
         L     R6,ADAGY                                                         
         CLC   0(L'CTIKEY,R6),KEY                                               
         BNE   WRT100                                                           
*                                                                               
WRT17    LA    R6,28(R6)           FIRST ELEM                                   
WRT18    CLI   0(R6),0                                                          
         BE    WRT100                                                           
         CLC   =X'02',0(R6)                                                     
         BE    WRT20                                                            
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     WRT18                                                            
*                                                                               
WRT20    ZIC   R1,1(R6)                                                         
         SHI   R1,3                                                             
         CHI   R1,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WMGRUID(0),2(R6)            USER ID                              
         OC    WMGRUID,SPACES                                                   
*                                                                               
         CLI   EZ12ST2+9,X'00'                                                  
         BE    WRT100                                                           
         EDIT  (B1,EZ12ST2+9),WMGRVER                                           
*                                                                               
WRT100   DS    0H                                                               
         LA    R4,WRKEZBDT                                                      
         GOTO1 DATCON,DMCB,(2,0(R4)),(X'20',WMGRBDAT)                           
*                                                                               
WRT900   DS    0H                                                               
         MVI   WMGRACT+L'WMGRACT,X'4F'                                          
         MVI   WMGRINV+L'WMGRINV,X'4F'                                          
         MVI   WMGRDATE+L'WMGRDATE,X'4F'                                        
         MVI   WMGRSTA+L'WMGRSTA,X'4F'                                          
         MVI   WMGRMED+L'WMGRMED,X'4F'                                          
         MVI   WMGRBND+L'WMGRBND,X'4F'                                          
         MVI   WMGRUID+L'WMGRUID,X'4F'                                          
         MVI   WMGRAGY+L'WMGRAGY,X'4F'                                          
         MVI   WMGRNSTA+L'WMGRNSTA,X'4F'                                        
         MVI   WMGRNMED+L'WMGRNMED,X'4F'                                        
         MVI   WMGRNBND+L'WMGRNBND,X'4F'                                        
         MVI   WMGRCLT+L'WMGRCLT,X'4F'                                          
         MVI   WMGRPRD1+L'WMGRPRD1,X'4F'                                        
         MVI   WMGRPRD2+L'WMGRPRD2,X'4F'                                        
         MVI   WMGREST+L'WMGREST,X'4F'                                          
         MVI   WMGRPREP+L'WMGRPREP,X'4F'                                        
         MVI   WMGRPKG+L'WMGRPKG,X'4F'                                          
         MVI   WMGRVER+L'WMGRVER,X'4F'                                          
         MVI   WMGRMOS+L'WMGRMOS,X'4F'                                          
         MVI   WMGRNINV+L'WMGRNINV,X'4F'                                        
         MVI   WMGRRVER+L'WMGRRVER,X'4F'                                        
         MVI   WMGRBDAT+L'WMGRBDAT,X'4F'                                        
*                                                                               
         OC    WMGRD(WMGRDLQ),SPACES                                            
*                                                                               
         LHI   R0,WMGRDLQ                                                       
         AHI   R0,4                                                             
         STCM  R0,3,IMLINE                                                      
*        L     R0,=A(EZINVMGR)                                                  
*        PUT   (0),IMLINE                                                       
*                                                                               
         MVC   P(80),IMLINE                                                     
         GOTO1 REPORT                                                           
*                                                                               
WRT60    DS    0H                                                               
         CLI   QOPT3,C'F'          SKIP MQ IF USING OWN FILE -TESTING           
         BE    WRT80                                                            
*                                                                               
         CP    MQCOUNT,=P'100'     100 ENTRIES TO A FILE                        
         BE    WRT70                                                            
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),IMLINE+4,WMGRDLQ,0                       
         CLI   DMCB+8,0                                                         
         BE    *+6                 DONE - GET OUT                               
         DC    H'0'                                                             
         AP    MQCOUNT,=P'1'                                                    
         B     WRT80                                                            
*                                                                               
*        CLOSE THIS BUFFER AND OPEN A NEW MQ ENTRY                              
*                                                                               
WRT70    GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,=C'IMSTATUS********'),0,0            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   MQCOUNT,=P'0'                                                    
         BE    WRT60                                                            
*                                                                               
WRT80    DS    0H                                                               
*                                                                               
         J     EXIT                                                             
         DROP  R2                                                               
*                                                                               
SVRD     DS    F                                                                
SVIMJOB  DC    CL8'        '                                                    
         EJECT                                                                  
**********************************************************************          
*        PRINT REPORT FROM SORTER RECORDS                                       
**********************************************************************          
*                                                                               
PRINTIT  NTR1                                                                   
*                                                                               
PR10     GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R4,15,4(R1)                                                      
         BZ    PRX                                                              
         MVC   IO2(SRLENQ),0(R4)                                                
         LA    R4,IO2                                                           
         USING SORTRECD,R4                                                      
         LA    R5,P1                                                            
         USING PRINTD,R5                                                        
*                                                                               
         CLC   SRMED,LASTMED                                                    
         BE    PRX                                                              
PR20     MVI   FORCEHED,C'Y'                                                    
         MVC   LASTMED,SRMED                                                    
*                                                                               
         MVC   PRMED,SRMED                                                      
         GOTO1 REPORT                                                           
         B     PR10                                                             
*                                                                               
PRX      J     EXIT                                                             
         DROP  R4,R5                                                            
*                                                                               
**********************************************************************          
*        HEADLINES                                                              
**********************************************************************          
*                                                                               
MYHEAD   NTR1                      HEADLINES                                    
         LA    R4,IO2                                                           
         USING SORTRECD,R4                                                      
MYHX     J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
*                                                                               
**********************************************************************          
*        ERROR TABLE AND STUFF                                                  
**********************************************************************          
*                                                                               
         GETEL R5,24,ELCODE                                                     
         GETEL2 R5,42,ELCODE                                                    
**********************************************************************          
*        LTORG                                                                  
**********************************************************************          
         LTORG                                                                  
CTFLIST  DS    0F                                                               
         DC    CL8'NGENDIR'                                                     
         DC    CL8'NGENFIL'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
DINDEX   DC    CL8'INDEX'                                                       
DREAD    DC    CL8'READ'                                                        
DCLOSE   DC    CL8'CLOSE'                                                       
WRKF     DC    CL8'WRKF'                                                        
WRKFILE  DC    CL8'WRKFILE'                                                     
DAYS     DC    CL7'MTWTFSS'                                                     
AWKBUFF  DC    A(WKBUFF)                                                        
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=150'                                   
*                                                                               
*&&DO                                                                           
         DS    0D                                                               
EZINVMGR DCB   DDNAME=EZINVMGR,                                        X        
               DSORG=PS,                                               X        
               LRECL=512,                                              X        
               BLKSIZE=5120,                                           X        
               RECFM=VB,                                               X        
               MACRF=PM                                                         
*&&                                                                             
*                                                                               
IOWORK   DS    12D                 IO WORK AREA                                 
AMQRPT   DS    A                                                                
EZWRKIO  DS    A                                                                
ELCODE   DS    X                                                                
SVSPUTL  DS    X                                                                
FLAG     DS    X                                                                
CTDAY    DS    XL2                 COMP TODAY                                   
*                                                                               
SDAY     DS    XL2                                                              
EDAY     DS    XL2                                                              
*                                                                               
WRKRID   DS    XL2                                                              
TEMPDATE DS    CL6                                                              
TEMPDAT2 DS    CL6                                                              
LASTMED  DS    CL1                                                              
*                                                                               
EZCALL   DS    CL4                                                              
EZMED    DS    CL2                                                              
EZBAND   DS    CL2                                                              
EZINVNUM DS    CL10                                                             
EZINVDAT DS    CL6                                                              
EZINVMOS DS    CL4                                                              
EZ12STAT DS    XL12                                                             
EZ12ST2  DS    XL12                                                             
*                                                                               
MQFLAG   DS    X                                                                
MQCOUNT  DC    PL4'0'                                                           
*                                                                               
EZWRKIOB DS    XL250               DMWRKIO CONTROL BLOCK (194+56 SPARE)         
         ORG   EZWRKIOB                                                         
       ++INCLUDE DDWRKIOD                                                       
         ORG                                                                    
*                                                                               
*&&DO                                                                           
WRTFLAG  DS    X                                                                
SVIMDSN  DS    0X                                                               
         ORG   SVIMDSN+DSNNMLQ                                                  
*&&                                                                             
*                                                                               
SVREMPQK DS    XL7                                                              
SVVLRMKY DS    V                                                                
SVIMTIME DS    F                                                                
         ORG                                                                    
*                                                                               
         DC    C'**INDX**'                                                      
WRKRINDX DS    CL42                                                             
         DC    C'***IO***'                                                      
IO       DS    XL256               IO AREA                                      
         DC    C'*IMLINE*'                                                      
IMLINE   DS    XL256               IO AREA                                      
IO2      DS    XL1200              IO AREA                                      
         DS    0D                                                               
WKBUFF   DS    14336C                                                           
*                                                                               
***********************************************************************         
*        DSECTS                                                                 
***********************************************************************         
*                                                                               
*        DATA SET NAME                                                          
*                                                                               
*&&DO                                                                           
DSNNAMED DSECT                                                                  
DSNROOT  DS    CL3                 C'EZI'                                       
         DS    C                   DOT                                          
DSNDATE  DS    CL5                                                              
         DS    C                   DOT                                          
DSNTIME  DS    CL5                                                              
         DS    C                   DOT                                          
DSNJOB   DS    CL8                 C'JOB'                                       
DSNNMLQ  EQU   *-DSNNAMED                                                       
*&&                                                                             
*                                                                               
*                                                                               
       ++INCLUDE SPWMGRD                                                        
*                                                                               
***********************************************************************         
*        SORT RECORD DSECT                                                      
***********************************************************************         
SORTRECD DSECT                                                                  
SRMED    DS    CL1                 MEDIA                                        
SRLENQ   EQU   *-SORTRECD                                                       
*                                                                               
***********************************************************************         
*        PRINT LINE  DSECT                                                      
***********************************************************************         
PRINTD   DSECT                                                                  
PRINDEX  DS    CL8                                                              
         DS    CL4                                                              
PRUSER   DS    CL10                                                             
         DS    CL4                                                              
PRCALL   DS    CL4                                                              
         DS    CL2                                                              
PRMED    DS    CL1                                                              
         DS    CL2                                                              
PRBAND   DS    CL1                                                              
         DS    CL3                                                              
PRINVCE  DS    CL12                INVOICE                                      
         DS    CL2                                                              
PRDATE   DS    CL6                                                              
         DS    CL2                                                              
*                                                                               
         EJECT                                                                  
                                                                                
         PRINT OFF                                                              
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDREMOTED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPZW02 03/19/12'                                      
         END                                                                    
