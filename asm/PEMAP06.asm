*          DATA SET PEMAP06    AT LEVEL 011 AS OF 05/01/02                      
*PHASE TE1B06A                                                                  
         TITLE 'TE1B06 - TASK RECORD MAINTENANCE'                               
TE1B06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**TSMN**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     LIST                                                             
         EJECT                                                                  
*              VALIDATE AND DISPLAY KEY                                         
         SPACE 3                                                                
VKEY     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         CLI   ACTEQU,1                                                         
         BNE   LKEY1                                                            
         USING MAPKEY,R4                                                        
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   MAPKTYP,X'06'                                                    
         LA    R2,TSRSYSH                                                       
         GOTO1 VALISYS                                                          
         MVC   WORK(8),SAVSYSCD    EXPAND SYSTEM                                
         BAS   RE,GENDISP                                                       
         LA    R2,TSRPRJH                                                       
         GOTO1 VALIPROJ                                                         
         LA    R2,TSRTYPEH                                                      
         GOTO1 VALITYPE                                                         
         MVC   WORK,SPACES         EXPAND TYPE                                  
         MVC   WORK(7),SAVTYPE+1                                                
         BAS   RE,GENDISP                                                       
         LA    R2,TSRCODH                                                       
         GOTO1 ANY                                                              
         MVC   TSKSYS,SAVSYSCD                                                  
         MVC   TSKPROJ,SAVPJCOD                                                 
         MVC   TSKTYPE,SAVTYPE                                                  
         MVC   TSKCODE,WORK                                                     
         B     XIT                                                              
         SPACE 1                                                                
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         LA    R2,TSRSYSH                                                       
         MVC   WORK(8),TSKSYS                                                   
         BAS   RE,GENDISP                                                       
         LA    R2,TSRPRJH                                                       
         MVC   WORK(8),TSKPROJ                                                  
         BAS   RE,GENDISP                                                       
         LA    R2,TSRTYPEH                                                      
         MVC   WORK(7),TSKTYPE+1                                                
         BAS   RE,GENDISP                                                       
         LA    R2,TSRCODH                                                       
         MVC   WORK(8),TSKCODE                                                  
         BAS   RE,GENDISP                                                       
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FOR LIST OR REPORTS                                 
         SPACE 3                                                                
LKEY1    XC    QSYS,QSYS           PRESET FIELDS                                
         XC    QPROJ,QPROJ                                                      
         XC    QTYPE,QTYPE                                                      
         XC    QTASK,QTASK                                                      
         XC    QPERS,QPERS                                                      
         XC    QSTAT,QSTAT                                                      
         XC    QDATE,QDATE                                                      
         SPACE 1                                                                
         LA    R2,TSLSYSH          SYSTEM                                       
         CLI   5(R2),0                                                          
         BE    LKEY2               NO SYSTEM, NO PROJECT                        
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,QSYSL                                                         
         GOTO1 VALISYS                                                          
         MVC   QSYS,SAVSYSCD                                                    
         SPACE 1                                                                
         LA    R2,TSLPRJH          PROJECT                                      
         CLI   5(R2),0                                                          
         BE    LKEY2                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,QPROJL                                                        
         MVC   QPROJ,TSLPRJ                                                     
         MVC   SAVPJCOD,TSLPRJ                                                  
         SPACE 1                                                                
LKEY2    LA    R2,TSLTYPEH         TYPE                                         
         CLI   5(R2),0                                                          
         BE    LKEY3                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,QTYPEL                                                        
         GOTO1 VALITYPE                                                         
         MVC   QTYPE,SAVTYPE                                                    
         SPACE 1                                                                
LKEY3    LA    R2,TSLTASKH         TASK                                         
         CLI   5(R2),0                                                          
         BE    LKEY4                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,QTASKL                                                        
         GOTO1 ANY                                                              
         MVC   QTASK,WORK                                                       
         SPACE 1                                                                
LKEY4    LA    R2,TSLSTATH         OPEN?                                        
         CLI   5(R2),0                                                          
         BE    LKEY6                                                            
         MVC   QSTAT,8(R2)                                                      
         CLI   QSTAT,C'Y'          S/B Y OR N                                   
         BE    LKEY6                                                            
         CLI   QSTAT,C'N'                                                       
         BE    LKEY6                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
LKEY6    LA    R2,TSLPERSH         PERSON                                       
         CLI   5(R2),0                                                          
         BE    LKEY8                                                            
         GOTO1 VALIPERS                                                         
         MVC   QPERS,WORK                                                       
         SPACE 1                                                                
LKEY8    LA    R2,TSLDATEH         DATE                                         
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 VALIDATE,DMCB,QDATE                                              
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE TASK DETAILS                                            
         SPACE 3                                                                
VREC     DS    0H                                                               
DREC     LA    R2,TSRWRKH          WORK DESCRIPTION FIRST                       
         MVI   OPTION,0                                                         
         MVI   MAX,3                                                            
         MVI   ELCODE,X'62'                                                     
         GOTO1 VALICHAT                                                         
         SPACE 1                                                                
         MVI   ELCODE,X'60'        NOW GET TASK ELEMENT                         
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT                                                       
         USING TSKELD,R6                                                        
         CLI   MODE,DISPREC                                                     
         BE    DREC2                                                            
         MVC   TSKEL(2),=X'6018'   PERSON SCHEDULED                             
         LA    R2,TSRPERH                                                       
         GOTO1 VALIPERS                                                         
         MVC   TSKWHO,WORK                                                      
         SPACE 1                                                                
         LA    R2,TSRSTRTH         START                                        
         CLI   5(R2),0             (CAN BE OPEN)                                
         BE    VREC3                                                            
         GOTO1 VALIDATE,DMCB,DUB                                                
         GOTO1 GETDAY,DMCB,DUB,WORK                                             
         ZIC   R3,DMCB             BACK UP TO MONDAY                            
         LA    R4,1                                                             
         SR    R4,R3                                                            
         BZ    VREC2                                                            
         GOTO1 ADDAY,DMCB,DUB,WORK,(R4)                                         
         MVC   DUB,WORK                                                         
         SPACE 1                                                                
VREC2    GOTO1 DATCON,DMCB,DUB,(1,TSKSTART)                                     
         SPACE 1                                                                
VREC3    LA    R2,TSRTIMEH         AMOUNT OF TIME (WEEKS)                       
         CLI   5(R2),0             CAN BE OPEN                                  
         BE    VREC4                                                            
         GOTO1 VALINUM                                                          
         MVC   TSKWEEKS,ACTUAL                                                  
         SPACE 1                                                                
VREC4    LA    R2,TSRPCTH          PERCENT OF TIME                              
         GOTO1 VALINUM                                                          
         MVC   TSKPCT,ACTUAL                                                    
         SPACE 1                                                                
         LA    R2,TSRCOMPH         COMPLETE PERCENT                             
         MVI   ACTUAL,0                                                         
         CLI   5(R2),0                                                          
         BE    VREC5                                                            
         GOTO1 VALINUM                                                          
         SPACE 1                                                                
VREC5    CLC   TSKOKPCT,ACTUAL     (NOTE DATE IF CHANGED)                       
         BE    VREC6                                                            
         MVC   TSKOKPCT,ACTUAL                                                  
         GOTO1 DATCON,DMCB,(5,0),(1,TSKOKDAT)                                   
         SPACE 1                                                                
VREC6    GOTO1 ADDELEM                                                          
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY TASK DETAILS                                             
         SPACE 3                                                                
DREC2    LA    R2,TSRPERH          PERSON                                       
         MVC   WORK(8),TSKWHO                                                   
         BAS   RE,GENDISP                                                       
         SPACE 1                                                                
         LA    R2,TSRSTRTH         START DATE                                   
         CLI   TSKSTART,0                                                       
         BE    DREC4                                                            
         GOTO1 DATCON,DMCB,(1,TSKSTART),(8,WORK)                                
         SPACE 1                                                                
DREC4    BAS   RE,GENDISP                                                       
         LA    R2,TSRTIMEH         TIME                                         
         CLI   TSKWEEKS,0                                                       
         BE    DREC6                                                            
         EDIT  (1,TSKWEEKS),(3,WORK),WRK=DMCB,ALIGN=LEFT                        
         SPACE 1                                                                
DREC6    BAS   RE,GENDISP                                                       
         LA    R2,TSRPCTH          PERCENT                                      
         CLI   TSKPCT,0                                                         
         BE    DREC8                                                            
         EDIT  (1,TSKPCT),(3,WORK),WRK=DMCB,ALIGN=LEFT                          
         SPACE 1                                                                
DREC8    BAS   RE,GENDISP                                                       
         LA    R2,TSRCOMPH         PERCENT COMPLETE                             
         CLI   TSKOKPCT,0                                                       
         BE    DREC10                                                           
         EDIT  (1,TSKOKPCT),(3,WORK),WRK=DMCB,ALIGN=LEFT                        
         SPACE 1                                                                
DREC10   BAS   RE,GENDISP                                                       
         LA    R2,TSRONH           ON DATE                                      
         CLI   TSKOKDAT,0                                                       
         BE    DREC12                                                           
         MVC   WORK(2),=C'ON'                                                   
         GOTO1 DATCON,DMCB,(1,TSKOKDAT),(8,WORK+3)                              
         SPACE 1                                                                
DREC12   BAS   RE,GENDISP                                                       
         B     XIT                                                              
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LIST4                                                            
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   MAPKTYP,X'06'                                                    
         MVC   TSKSYS,QSYS                                                      
         MVC   TSKPROJ,QPROJ                                                    
         MVC   TSKTYPE,QTYPE                                                    
         GOTO1 HIGH                                                             
         B     LIST4                                                            
         SPACE 1                                                                
LIST2    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST4    CLC   KEY(3),KEYSAVE      CHECKMAIN C/B                                
         BNE   XIT                                                              
         CLI   QSYS,0              IF SYSTEM WAS REQUESTED                      
         BE    LIST6                                                            
         ZIC   R1,QSYSL                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY+3(0),KEYSAVE+3  SHOULD MATCH ON THIS                         
         BNE   XIT                                                              
         CLI   QPROJ,0             IF PROJECT WAS ALSO REQUESTED                
         BE    LIST6                                                            
         ZIC   R1,QPROJL                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY+11(0),KEYSAVE+11 THIS SHOULD ALSO MATCH                      
         BL    LIST2                                                            
         SPACE 1                                                                
LIST6    CLI   QTYPE,0             TYPE SELECTED?                               
         BE    LIST7                                                            
         ZIC   R1,QTYPEL                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   QTYPE(0),TSKTYPE                                                 
         BNE   LIST2                                                            
LIST7    CLI   QTASK,0             TASK NAME FILTERING?                         
         BE    LIST8                                                            
         ZIC   R1,QTASKL                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   QTASK(0),TSKCODE                                                 
         BNE   LIST2                                                            
         SPACE 1                                                                
LIST8    GOTO1 GETREC                                                           
         CLI   MODE,PRINTREP                                                    
         BE    REPS2                                                            
         MVC   LISTAR,SPACES       SHOW CODE                                    
         LA    R3,LISTAR                                                        
         LA    R5,TSLHED                                                        
         OI    TSLHEDH+6,X'80'                                                  
         MVC   0(8,R3),TSKSYS                                                   
         MVC   0(6,R5),=C'SYSTEM'                                               
         SPACE 1                                                                
         LA    R3,10(R3)                                                        
         LA    R5,10(R5)                                                        
         MVC   0(8,R3),TSKPROJ                                                  
         MVC   0(7,R5),=C'PROJECT'                                              
         SPACE 1                                                                
         LA    R3,9(R3)                                                         
         LA    R5,9(R5)                                                         
         MVC   0(7,R3),TSKTYPE+1                                                
         MVC   0(4,R5),=C'TYPE'                                                 
         SPACE 1                                                                
         LA    R3,8(R3)                                                         
         LA    R5,8(R5)                                                         
         MVC   0(8,R3),TSKCODE                                                  
         MVC   0(4,R5),=C'TASK'                                                 
         SPACE 1                                                                
         L     R6,AIO              GET TASK DETAILS                             
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TSKELD,R6                                                        
         SPACE 1                                                                
         LA    R3,9(R3)            SHOW PERSON                                  
         LA    R5,9(R5)                                                         
         MVC   0(8,R3),TSKWHO                                                   
         MVC   0(6,R5),=C'PERSON'                                               
         CLI   QPERS,0             MAY BE FILTERING ON PERSON                   
         BE    LIST10                                                           
         CLC   QPERS,TSKWHO                                                     
         BNE   LIST2                                                            
         SPACE 1                                                                
LIST10   LA    R3,9(R3)            START DATE                                   
         LA    R5,9(R5)                                                         
         MVC   0(5,R5),=C'START'                                                
         CLI   TSKSTART,0                                                       
         BE    LIST14                                                           
         GOTO1 DATCON,DMCB,(1,TSKSTART),(8,0(R3))                               
         SPACE 1                                                                
LIST12   CLI   QDATE,0             DATE FILTER                                  
         BE    LIST14                                                           
         GOTO1 DATCON,DMCB,(1,TSKSTART),(0,WORK)                                
         ZIC   RF,TSKWEEKS                                                      
         MH    RF,=H'7'            (COMPUTE END)                                
         BCTR  RF,0                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         CLC   QDATE,WORK                                                       
         BL    LIST2                                                            
         CLC   QDATE,WORK+6                                                     
         BH    LIST2                                                            
         SPACE 1                                                                
LIST14   LA    R3,8(R3)            WEEKS                                        
         LA    R5,8(R5)                                                         
         EDIT  (1,TSKWEEKS),(2,1(R3)),ALIGN=LEFT                                
         MVC   0(3,R5),=C'WKS'                                                  
         SPACE 1                                                                
         LA    R3,4(R3)            PERCENT OF TIME                              
         LA    R5,4(R5)                                                         
         EDIT  (1,TSKPCT),(3,0(R3))                                             
         MVC   0(3,R5),=C'PCT'                                                  
         SPACE 1                                                                
         LA    R3,4(R3)            DONE PERCENT                                 
         LA    R5,4(R5)                                                         
         MVC   0(4,R5),=C'DONE'                                                 
         CLI   TSKOKPCT,0                                                       
         BE    LIST16                                                           
         EDIT  (1,TSKOKPCT),(3,(R3))                                            
         MVI   3(R3),C'%'                                                       
         CLI   TSKOKPCT,100                                                     
         BL    LIST16                                                           
         CLI   QSTAT,C'N'          DONE FILTER                                  
         BE    LIST2                                                            
         B     LIST18                                                           
         SPACE 1                                                                
LIST16   CLI   QSTAT,C'Y'                                                       
         BE    LIST2                                                            
         SPACE 1                                                                
LIST18   GOTO1 LISTMON                                                          
         B     LIST2                                                            
         EJECT                                                                  
*              REPORT ON TASKS                                                  
         SPACE 3                                                                
REPS2    MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         CLC   TSKSYS,LASTSYS      SKIP ON SYSTEM C/B                           
         BE    REPS4                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   LASTSYS,TSKSYS                                                   
         B     REPS4B                                                           
         SPACE 1                                                                
REPS4    CLC   TSKPROJ,LASTPROJ    CHECK CHANGE OF PROJECT                      
         BE    REPS5                                                            
         SPACE 1                                                                
REPS4B   MVC   LASTPROJ,TSKPROJ                                                 
         MVC   P+1(8),TSKPROJ      PROJECT                                      
         MVC   TASKKEY,KEY                                                      
         MVC   TASKKYSV,KEYSAVE                                                 
         MVI   TSKKTYP,X'05'       GO AND READ PROJECT                          
         XC    TSKTYPE(16),TSKTYPE                                              
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'50'                                                     
         LA    R3,P+10                                                          
         BAS   RE,CHATOUT                                                       
         MVC   KEY,TASKKEY                                                      
         MVC   KEYSAVE,TASKKYSV                                                 
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         SPACE 1                                                                
REPS5    MVC   P+41(7),TSKTYPE+1   TYPE                                         
         MVC   P2+41(8),TSKCODE    CODE                                         
         L     R6,AIO              GET TASK DETAILS                             
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TSKELD,R6                                                        
         SPACE 1                                                                
         LA    R3,P+50             SHOW PERSON                                  
         MVC   0(8,R3),TSKWHO                                                   
         CLI   QPERS,0             MAY BE FILTERING ON PERSON                   
         BE    REPS6                                                            
         CLC   QPERS,TSKWHO                                                     
         BNE   LIST2                                                            
         SPACE 1                                                                
REPS6    LA    R3,P2+50            START DATE                                   
         CLI   TSKSTART,0                                                       
         BE    REPS8                                                            
         GOTO1 DATCON,DMCB,(1,TSKSTART),(8,0(R3))                               
         CLI   QDATE,0             DATE FILTER                                  
         BE    REPS8                                                            
         GOTO1 DATCON,DMCB,(1,TSKSTART),(0,WORK)                                
         ZIC   RF,TSKWEEKS                                                      
         MH    RF,=H'7'            (COMPUTE END)                                
         BCTR  RF,0                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         CLC   QDATE,WORK                                                       
         BL    LIST2                                                            
         CLC   QDATE,WORK+6                                                     
         BH    LIST2                                                            
         SPACE 1                                                                
REPS8    LA    R3,P+59             WEEKS                                        
         MVC   0(7,R3),=C'ONGOING'                                              
         CLI   TSKWEEKS,0                                                       
         BE    REPS9                                                            
         MVC   0(7,R3),SPACES                                                   
         EDIT  (1,TSKWEEKS),(2,1(R3)),ALIGN=LEFT                                
         MVC   3(5,R3),=C'WEEKS'                                                
         CLI   TSKWEEKS,1                                                       
         BNE   *+8                                                              
         MVI   7(R3),C' '                                                       
         SPACE 1                                                                
REPS9    LA    R3,P2+61            PERCENT OF TIME                              
         EDIT  (1,TSKPCT),(3,0(R3))                                             
         MVI   3(R3),C'%'                                                       
         SPACE 1                                                                
         LA    R3,P+69             DONE PERCENT                                 
         CLI   TSKOKPCT,0                                                       
         BE    REPS10                                                           
         EDIT  (1,TSKOKPCT),(3,(R3))                                            
         MVI   3(R3),C'%'                                                       
         GOTO1 DATCON,DMCB,(1,TSKOKDAT),(8,P2+68)                               
         CLI   TSKOKPCT,100                                                     
         BL    REPS10                                                           
         CLI   QSTAT,C'N'          DONE FILTER                                  
         BE    LIST2                                                            
         B     REPS12                                                           
         SPACE 1                                                                
REPS10   CLI   QSTAT,C'Y'                                                       
         BE    LIST2                                                            
         SPACE 1                                                                
REPS12   MVI   ELCODE,X'62'                                                     
         LA    R3,P+78                                                          
         BAS   RE,CHATOUT                                                       
         MVI   SPACING,2                                                        
         BAS   RE,SPLAT                                                         
         B     LIST2                                                            
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
CHATOUT  NTR1                                                                   
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     CHAT4                                                            
         SPACE 1                                                                
CHAT2    BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
CHAT4    BNE   XIT                                                              
         USING CHATELD,R6                                                       
         ZIC   R4,CHATSEQ          POSITION TO RIGHT LINE                       
         CH    R4,=H'4'                                                         
         BH    CHAT2                                                            
         BCTR  R4,0                                                             
         MH    R4,=H'132'                                                       
         AR    R4,R3                                                            
         ZIC   R1,CHATLEN          PICK UP LENGTH                               
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     CHAT2                                                            
         MVC   0(0,R4),CHAT                                                     
         SPACE 1                                                                
GENDISP  ZIC   R1,0(R2)            GENERAL DISPLAY                              
         SH    R1,=H'9'                                                         
         EX    R1,GDCLC                                                         
         BE    GENDISP2                                                         
         EX    R1,GDMVC                                                         
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
GENDISP2 MVC   WORK,SPACES                                                      
         BR    RE                                                               
         SPACE 1                                                                
GDCLC    CLC   8(0,R2),WORK                                                     
GDMVC    MVC   8(0,R2),WORK                                                     
         SPACE 1                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H4+10(8),LASTSYS                                                 
         SPACE 1                                                                
         L     R4,ABOX             BOXES, IF AROUND                             
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         USING BOXD,R4                                                          
         MVI   BOXROWS+06,C'T'                                                  
         MVI   BOXROWS+09,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   00(R2),C'L'                                                      
         MVI   09(R2),C'C'                                                      
         MVI   40(R2),C'C'                                                      
         MVI   49(R2),C'C'                                                      
         MVI   58(R2),C'C'                                                      
         MVI   67(R2),C'C'                                                      
         MVI   76(R2),C'C'                                                      
         MVI   108(R2),C'R'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         B     XIT                                                              
         SPACE 2                                                                
HEDSPECS SSPEC H1,2,C'MAP SYSTEM'                                               
         SSPEC H2,2,C'----------'                                               
         SSPEC H1,40,C'TASKS SCHEDULED'                                         
         SSPEC H2,40,C'---------------'                                         
         SSPEC H1,77,REPORT                                                     
         SSPEC H1,96,REQUESTOR                                                  
         SSPEC H2,77,RUN                                                        
         SSPEC H2,103,PAGE                                                      
         SSPEC H4,2,C'SYSTEM -'                                                 
         SSPEC H8,02,C'PROJECT  DESCRIPTION OF PROJECT'                         
         SSPEC H8,42,C'TYPE     WHO?     HOW LONG COMPLETE'                     
         SSPEC H9,42,C'TASK     WHEN?    PCT TIME PCT/DATE'                     
         SSPEC H8,79,C'DESCRIPTION OF TASK'                                     
         DC    X'00'               END MARKER FOR SPECS                         
         SPACE 1                                                                
RELO     DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMAPFILE                                                      
       ++INCLUDE PEMAPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPF6D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPE6D                                                       
         PRINT OFF                                                              
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
QSYSL    DS    CL1                                                              
QPROJL   DS    CL1                                                              
QTYPEL   DS    CL1                                                              
QTASKL   DS    CL1                                                              
QSYS     DS    CL8                                                              
QPROJ    DS    CL8                                                              
QTYPE    DS    CL8                                                              
QTASK    DS    CL8                                                              
QPERS    DS    CL8                                                              
QDATE    DS    CL6                                                              
QSTAT    DS    CL8                                                              
LASTSYS  DS    CL8                                                              
LASTPROJ DS    CL8                                                              
TASKKEY  DS    CL36                                                             
TASKKYSV DS    CL36                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011PEMAP06   05/01/02'                                      
         END                                                                    
