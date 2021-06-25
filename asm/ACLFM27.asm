*          DATA SET ACLFM27    AT LEVEL 018 AS OF 05/01/02                      
*PHASE T60327A,+0                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE ADDAY                                                                  
*INCLUDE PERVERT                                                                
         TITLE 'ALLOCATE PERSONNEL SALARY DATA TO BUDGET FILE'                  
T60327   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 AC27WKLN-AC27WRKD,*LFM27**,R9,R7,RR=R5                           
         LR    R8,RC                                                            
         USING AC27WRKD,R8                                                      
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         ST    RD,SAVERD                                                        
         ST    R5,PRELOC                                                        
         L     R5,=V(ADDAY)                                                     
         A     R5,PRELOC                                                        
         ST    R5,ADDAY                                                         
         L     R5,=V(PERVERT)                                                   
         A     R5,PRELOC                                                        
         ST    R5,PERVERT                                                       
         L     R5,=V(HELLO)                                                     
         A     R5,PRELOC                                                        
         ST    R5,HELLO                                                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                     *-----------------------*                                 
*                     *   MODE  =  BUILDKEY   *                                 
*                     *-----------------------*                                 
*---------------------------------------------------------------------*         
         CLI   MODE,BUILDKEY                                                    
         BNE   DSPR                                                             
*                                                                               
         XC    PROFKEY,PROFKEY     GET PROFILE FOR MNTHLY WRK DAYS              
         MVC   PROFKEY(4),=C'A0A0'                                              
         MVC   PROFKEY+4(1),COMPANY                                             
         USING TWAD,RA                                                          
         MVC   PROFKEY+12(2),TWAAGY                                             
         USING T603FFD,RA                                                       
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,PROFPARA,PROFKEY,PROGPROF,DATAMGR                       
         OC    PROGPROF,PROGPROF                                                
         BNZ   BLDKY04                                                          
         LA    R0,12                                                            
         LA    R1,PROGPROF                                                      
         MVI   0(R1),20            DEFAULT IS 20 DAYS A MONTH                   
         LA    R1,1(R1)                                                         
         BCT   R0,*-8                                                           
*                                                                               
BLDKY04  DS    0H                                                               
         MVC   LOGHEAD,SPACES                                                   
         OI    LOGHEADH+6,X'80'                                                 
*                                                                               
         CLC   LOGSAMT(3),=C'BUD'                                               
         BNE   *+8                                                              
         MVI   BUDSW,1                                                          
*                                                                               
         MVC   KEY,SPACES          GET 1R ACCOUNT STRUCTURE                     
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'1R'                                                  
         GOTO1 READ                                                             
         MVI   ELCODE,X'16'                                                     
         LA    R4,IO                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO LEDGER ELEMENT                            
         USING ACHEIRD,R4                                                       
         ZIC   R3,ACHRLEN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SAVLEDG(0),ACHREL                                                
*                                                                               
         LA    R2,LOGDPTH          VALIDATE OFFICE / DEPT                       
         GOTO1 ANY                                                              
         ZIC   R3,LOGDPTH+5                                                     
         LA    R4,SAVLEDG                                                       
         USING ACHEIRD,R4                                                       
         ZIC   R5,ACHRLEVA                                                      
         CLI   ACHRLEVD,0                                                       
         BE    BLDKY10                                                          
         ZIC   R5,ACHRLEVB         IF 4 LEVEL A +B =OFFICE/DEPT                 
BLDKY10  STC   R5,SUBDISP           SAVE DISP SUB-DEPT                          
         CR    R3,R5                                                            
         BH    INVERR                                                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),LOGDPT                                                  
         MVC   LOGDPTN,SPACES                                                   
         OI    LOGDPTH+6,X'80'     SET TRANSMIT FOR OFF/DEPT                    
         OI    LOGDPTNH+6,X'80'                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
*                                                                               
         LR    R5,RA                                                            
         USING TWAD,R5                                                          
         CLI   TWAACCS,C'*'                                                     
         BNE   BLDKY20                                                          
         CLC   TWAACCS+1(1),KEY+3                                               
         BE    BLDKY20                                                          
         MVI   ERROR,55            SECURITY LOCKOUT                             
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
BLDKY20  TM    LOGDPTH+4,X'20'     PREVIOUSLY VALIDATED                         
         BO    BLDKY30             YES                                          
         OI    LOGDPTH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'         SET TO INDICATE A KEY INPUT CHANGE           
*                                                                               
BLDKY30  LA    R2,LOGSBDH          VALIDATE SUB-DEPT                            
         GOTO1 ANY                                                              
BLDKY31  ZIC   R3,LOGSBDH+5                                                     
         LA    R4,SAVLEDG                                                       
         USING ACHEIRD,R4                                                       
         ZIC   R6,ACHRLEVB         IF 4 LEVEL LEDGER                            
         CLI   ACHRLEVD,0          SUB DPT IS AT LEVEL C                        
         BE    *+8                                                              
         IC    R6,ACHRLEVC                                                      
         STC   R6,EMPDISP                                                       
         CR    R3,R6                                                            
         BH    INVERR                                                           
         LA    R4,KEY+3                                                         
         ZIC   R5,SUBDISP                                                       
         AR    R4,R5               R4 TO SUB-DEPT FIELD IN KEY                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),LOGSBD                                                   
BLDKY33  MVC   LOGSBDN,SPACES                                                   
         OI    LOGSBDNH+6,X'80'                                                 
         OI    LOGSBDH+6,X'80'                                                  
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
BLDKY35  TM    LOGSBDH+4,X'20'                                                  
         BO    BLDKY40                                                          
         OI    LOGSBDH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'                                                      
*                                                                               
BLDKY40  LA    R2,LOGIEMPH         VALIDATE EMPLOYEE                            
         GOTO1 ANY                                                              
BLDKY41  ZIC   R3,LOGIEMPH+5                                                    
         LA    R4,SAVLEDG                                                       
         USING ACHEIRD,R4                                                       
         ZIC   R6,ACHRLEVC        IF 4 LEVEL LEDGER STAFF                       
         CLI   ACHRLEVD,0           IS AT LEVEL 4                               
         BE    *+8                                                              
         IC    R6,ACHRLEVD                                                      
         CR    R3,R6                                                            
         BH    INVERR                                                           
         LA    R4,KEY+3                                                         
         ZIC   R5,EMPDISP                                                       
         AR    R4,R5                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),LOGIEMP                                                  
         MVC   LOGEMPN,SPACES                                                   
         OI    LOGIEMPH+6,X'80'                                                 
         OI    LOGEMPNH+6,X'80'                                                 
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         TM    LOGIEMPH+4,X'20'                                                 
         BO    BLDKY45                                                          
         OI    LOGIEMPH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'                                                      
*                                                                               
*              VALIDATE THE BUDGET CODES                                        
BLDKY45  LA    R2,LOGBUDFH                                                      
         GOTO1 ANY                                                              
         LA    R4,SAVEKEY                                                       
         XC    SAVEKEY,SAVEKEY                                                  
         USING ACKEYD,R4                                                        
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,COMPANY                                                 
         MVC   ACBTKCOD,SPACES     BUDGET CODE                                  
         ZIC   R1,LOGBUDFH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACBTKCOD(0),LOGBUDF                                              
         MVC   MYIO(42),0(R4)                                                   
         BAS   RE,MYHIGH                                                        
         LA    R4,MYIO                                                          
         CLC   SAVEKEY(ACBTKNO2-ACBTKEY),0(R4)                                  
         BE    *+12                                                             
         MVI   ERROR,76            RECORD NOT FOUND                             
         B     XIT                                                              
         MVC   BUDNUM,ACBTKNO2     SAVE FROM BUDGET NUMBER                      
         OI    LOGBUDFH+6,X'80'    TRANSMIT                                     
         TM    LOGBUDFH+4,X'20'                                                 
         BO    *+12                                                             
         OI    LOGBUDFH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'                                                      
*                                                                               
         LA    R2,LOGBUDTH                                                      
         CLI   5(R2),0             ANY INPUT TO "TO" BUDGET                     
         BE    BLDKY52             NO - THEN DEFAULT TO "FROM" BUDGET           
         LA    R4,SAVEKEY                                                       
         XC    SAVEKEY,SAVEKEY                                                  
         USING ACKEYD,R4                                                        
         MVI   ACBTKTYP,ACBTKTEQ                                                
         MVC   ACBTKCMP,COMPANY                                                 
         MVC   ACBTKCOD,SPACES     BUDGET CODE                                  
         ZIC   R1,LOGBUDTH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACBTKCOD(0),LOGBUDT                                              
         MVC   MYIO(42),0(R4)                                                   
         BAS   RE,MYHIGH                                                        
         LA    R4,MYIO                                                          
         CLC   SAVEKEY(ACBTKNO2-ACBTKEY),0(R4)                                  
         BE    *+12                                                             
         MVI   ERROR,76            RECORD NOT FOUND                             
         B     XIT                                                              
*                                                                               
         CLI   LOGACT,C'I'         IF ACTION INQUIRY                            
         BE    *+10                USE FROM BUDGET                              
         MVC   BUDNUM,ACBTKNO2     SAVE BUDGET NUMBER                           
         OI    LOGBUDTH+6,X'80'    TRANSMIT                                     
         TM    LOGBUDTH+4,X'20'                                                 
         BO    *+12                                                             
         OI    LOGBUDTH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'                                                      
*                                                                               
BLDKY52  DS    0H                                                               
         CLI   BUDSW,1                                                          
         BNE   BLDKY54                                                          
         BAS   RE,BUDDATS                                                       
         B     XIT                                                              
*                                                                               
BLDKY54  LA    R2,LOGSTRTH         EDIT START                                   
         CLI   5(R2),0             ANY INPUT                                    
         BNE   BLDKY56             YES                                          
BLDKY55  LA    R3,MSGFL                                                         
         CLI   LOGACT,C'N'         IF ACTION NEW                                
         BE    ERRMSG              ERROR                                        
         MVI   ANYKEY,C'Y'                                                      
         B     XIT                                                              
*                                                                               
BLDKY56  CLC   8(5,R2),=C'FIRST'                                                
         BE    BLDKY58                                                          
         CLC   8(4,R2),=C'PREV'                                                 
         BE    BLDKY57                                                          
         CLC   8(4,R2),=C'NEXT'                                                 
         BNE   BLDKY59                                                          
BLDKY57  OC    DISPTOEL,DISPTOEL                                                
         BNZ   BLDKY58                                                          
         LA    R3,MSG10L                                                        
         B     ERRMSG                                                           
BLDKY58  CLC   LOGWSAL(6),=C'DELETE'                                            
         BNE   BLDKY55                                                          
         LA    R3,MSG11L                                                        
         B     ERRMSG                                                           
*                                                                               
*                                  VALIDATE START DATE INPUT                    
BLDKY59  LA    R6,LOGSTRT          START DATE                                   
         LA    R3,MYSTART          START DATE SAVE AREA                         
         MVI   DATESW,C'S'                                                      
         BAS   RE,DATEVAL                                                       
*                                                                               
BLDKY60  LA    R2,LOGENDH                                                       
         GOTO1 ANY                                                              
*                                                                               
         LA    R6,LOGEND           END DATE                                     
         LA    R3,MYEND            END DATE SAVE AREA                           
         LA    R5,MYSTART          START DATE                                   
         MVI   DATESW,C'E'                                                      
         BAS   RE,DATEVAL                                                       
*                                                                               
         CLC   MYSTART,MYEND       START MUST BE LOWER THAN END                 
         BNL   INVDTE                                                           
         BAS   RE,WEEK             FIGURE OUT WEEKS IN SALARY PERIOD            
*                                                                               
         LA    R2,LOGCLTSH         CLT BUDGET  START DATE                       
         CLI   5(R2),0             DID THEY INPUT ONE                           
         BNE   BLDKY62             YES - GO VALIDATE                            
         CLI   LOGCLTEH+5,0                                                     
         BE    BLDKY61                                                          
         LA    R2,LOGCLTEH         NO - THEN CANT HAVE BUDGET END DATE          
         LA    R3,MSG17L                                                        
         B     ERRMSG                                                           
BLDKY61  MVC   MYBUDST,MYSTART      MAKE IT THE SAME AS SAL START               
         MVC   MYBUDED,MYEND                                                    
         B     BLDKY64                                                          
         SPACE  1                                                               
BLDKY62  LA    R6,LOGCLTS          CLT BUDGET START DATE                        
         LA    R3,MYBUDST          BUDGET START  SAVE AREA                      
         MVI   DATESW,C'S'                                                      
         BAS   RE,DATEVAL                                                       
         LA    R2,LOGCLTEH                                                      
         CLI   LOGCLTEH+5,0        MUST HAVE BUDGET END                         
         BNE   BLDKY63                                                          
         LA    R3,MSG18L                                                        
         B     ERRMSG                                                           
BLDKY63  LA    R6,LOGCLTE          CLT BUDGET END DATE                          
         LA    R3,MYBUDED          BUDGET END SAVE AREA                         
         LA    R5,MYBUDST          BUDGET START                                 
         MVI   DATESW,C'E'                                                      
         BAS   RE,DATEVAL                                                       
         CLC   MYBUDST,MYBUDED     START MUST BE LOWER THAN END                 
         BNL   INVDTE                                                           
BLDKY64  BAS   RE,DATEINIT         SETS YR/MNTH TABLE PACKED FORMAT             
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
* SET UP REQUEST DATE RANGE TABLE-PACKED YR/MNTH (FOR 1D BDG ELEMENTS)          
* STORE NUMBER OF MONTHS IN REQUEST.                                            
*---------------------------------------------------------------------*         
DATEINIT NTR1                                                                   
         GOTO1 DATCON,DMCB,(1,MYBUDST),(0,WORK)                                 
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+10,F'-5'                             
         XC    WORK(5),WORK                                                     
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,WORK)                                 
         MVC   BACK5YR,WORK                                                     
*        PACK  BACK5YR,WORK(2)                                                  
*        SP    BACK5YR,=P'5'                                                    
*        SRP   BACK5YR,1,0                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(1,MYBUDST),(0,WORK)                                 
         SR    R0,R0                                                            
         LA    R2,DATAB                                                         
*                                                                               
DATINT1  GOTO1 DATCON,DMCB,(0,WORK),(1,FULL)                                    
         AH    R0,=H'1'            COUNT NUMBER OF MONTHS                       
*                                                                               
         MVC   0(2,R2),FULL        PACKED YYMM TO DATAB                         
         MVI   2(R2),X'FF'         NEW END OF TABLE                             
         CLC   0(2,R2),MYBUDED                                                  
         BNL   DATINT3                                                          
*                                                                               
         MVC   WORK+4(2),=C'28'    GET TO NEXT MONTH                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'6'                                      
         MVC   WORK(6),WORK+6                                                   
         LA    R2,2(R2)                                                         
         B     DATINT1                                                          
*                                                                               
DATINT3  STC   R0,MNTHCNT                                                       
         GOTO1 DATCON,DMCB,(1,MYBUDST),(2,SVSTRT) COMPRESSED START              
         GOTO1 DATCON,DMCB,(1,MYBUDED),(2,SVEND) COMPRESSED END                 
*                                                                               
         TM    LOGCLTSH+4,X'20'                                                 
         BO    *+12                                                             
         OI    LOGCLTSH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'                                                      
         TM    LOGCLTEH+4,X'20'                                                 
         BO    *+12                                                             
         OI    LOGCLTEH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'                                                      
*                                                                               
         CLI   BUDSW,1                                                          
         BE    XIT                                                              
         OI    LOGCLTSH+6,X'80'    TRANSMIT DATES                               
         OI    LOGCLTEH+6,X'80'                                                 
         GOTO1 DATCON,DMCB,(1,MYBUDST),(8,LOGCLTS)                              
         GOTO1 DATCON,DMCB,(1,MYBUDED),(8,LOGCLTE)                              
         LA    R2,LOGACTH                                                       
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                                                                               
*                                                                               
*---------------------------------------------------------------------*         
BUDDATS  NTR1                                                                   
         XC    MYSTART,MYSTART                                                  
         XC    MYEND,MYEND                                                      
         CLI   LOGSTRTH+5,0                                                     
         BE    BUDD200                                                          
         LA    R6,LOGSTRT                                                       
         LA    R3,MYSTART                                                       
         MVI   DATESW,C'S'                                                      
         BAS   RE,DATEVAL                                                       
         MVC   MYEND,MYSTART                                                    
*                                                                               
         CLI   LOGENDH+5,0                                                      
         BE    BUDD200                                                          
         LA    R6,LOGEND                                                        
         LA    R3,MYEND                                                         
         LA    R5,MYSTART                                                       
         MVI   DATESW,C'E'                                                      
         BAS   RE,DATEVAL                                                       
         MVC   LOGEND,SPACES                                                    
         OI    LOGENDH+6,X'80'                                                  
*                                                                               
BUDD200  LA    R2,LOGCLTSH                                                      
         GOTO1 ANY                                                              
         LA    R6,LOGCLTS                                                       
         LA    R3,MYBUDST                                                       
         MVI   DATESW,C'S'                                                      
         BAS   RE,DATEVAL                                                       
*                                                                               
BUDD240  LA    R2,LOGCLTEH                                                      
         GOTO1 ANY                                                              
         LA    R6,LOGCLTE                                                       
         LA    R3,MYBUDED                                                       
         LA    R5,MYBUDST                                                       
         MVI   DATESW,C'E'                                                      
         BAS   RE,DATEVAL                                                       
*                                                                               
         BAS   RE,DATEINIT                                                      
         GOTO1 DATCON,DMCB,(1,MYBUDST),(2,SVSTRT) COMPRESSED START              
         GOTO1 DATCON,DMCB,(1,MYBUDED),(2,SVEND) COMPRESSED END                 
*                                                                               
         ZAP   WEEKS,=P'52'                                                     
         SR    R1,R1                                                            
         LA    R2,12                                                            
         LA    R3,PROGPROF                                                      
BUDD300  ZIC   R4,0(R3)                                                         
         AR    R1,R4                                                            
         LA    R3,1(R3)                                                         
         BCT   R2,BUDD300                                                       
         ST    R1,TOTDAYS                                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        NUMBER OF WEEKS IN DATE RANGE AND                                      
*        NUMBER OF WORKING DAYS IN DATE RANGE                                   
*---------------------------------------------------------------------*         
WEEK     NTR1                                                                   
         GOTO1 DATCON,DMCB,(1,MYSTART),(0,WORK) PACKED DATES TO EBCIDIC         
         GOTO1 DATCON,DMCB,(1,MYEND),(0,WORK+6)                                 
         GOTO1 PERVERT,DMCB,WORK,WORK+6   GET NUMBER OF WEEKS                   
         LH    R0,DMCB+10          REMAINDER                                    
         LH    R6,DMCB+12          NUMBER OF WEEKS                              
         LH    R3,DMCB+14          NUMBER OF MONTHS IN SALARY RANGE             
         CH    R0,=H'4'                                                         
         BL    *+8                                                              
         LA    R6,1(R6)            ROUND UP                                     
         CVD   R6,DUB                                                           
         ZAP   WEEKS,DUB+6(2)      SAVE # OF WEEKS                              
*                                                                               
         SR    R5,R5                                                            
         XC    TOTDAYS,TOTDAYS                                                  
         ZIC   RF,MYSTART+1        STARTING MONTH OF SALARY START DATE          
         BCTR  RF,0                REDUCED BY ONE (PROGPROF+0)                  
         CLI   MYSTART+1,10        MONTHS 1 THRU 9   OK                         
         BL    *+8                                                              
         SH    RF,=H'6'            ADJ X'10'(OCT) IS NOW A X'09'                
         STC   RF,BYTE                                                          
         LA    RF,PROGPROF(RF)                                                  
         B     WEEK01                                                           
*                                                                               
WEEK0A   CLI   BYTE,X'0C'                                                       
         BL    WEEK01                                                           
         MVI   BYTE,0                                                           
         LA    RF,PROGPROF                                                      
WEEK01   ZIC   R1,0(RF)            WORKING DAYS THIS MTH INTO R1                
         AR    R5,R1               ADD TO TOTAL WRK DAYS                        
         LA    RF,1(RF)            BUMP TO NEXT WRK DAYS PROFILE                
         ZIC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         BCT   R3,WEEK0A           FOR NUMBER OF MTHS IN SALARY PERIOD          
         ST    R5,TOTDAYS          STORE TOTAL                                  
*                                                                               
         TM    LOGSTRTH+4,X'20'    PREVIOUSLY VALIDATED                         
         BO    *+12                YES                                          
         OI    LOGSTRTH+4,X'20'                                                 
         MVI   ANYKEY,C'Y'         SET TO INDICATE A KEY INPUT CHANGE           
         TM    LOGENDH+4,X'20'                                                  
         BO    *+12                                                             
         OI    LOGENDH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'                                                      
         OI    LOGSTRTH+6,X'80'    TRANSMIT DATES                               
         OI    LOGENDH+6,X'80'                                                  
         GOTO1 DATCON,DMCB,(1,MYSTART),(8,LOGSTRT)                              
         GOTO1 DATCON,DMCB,(1,MYEND),(8,LOGEND)                                 
         LA    R2,LOGACTH                                                       
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        VALIDATE DATE FIELD INPUT                                              
*              R6= ADDR OF SCREEN INPUT                                         
*              R3= ADDR OF DATE SAVE AREA                                       
*              R5= ADDR OF SAL OR BUD START DATE                                
*---------------------------------------------------------------------*         
DATEVAL  NTR1                                                                   
         MVI   BYTE,0              M/D/Y                                        
         B     *+8                                                              
DATEVL1  MVI   BYTE,2              M/Y                                          
         GOTO1 DATVAL,DMCB,(BYTE,(R6)),WORK                                     
         CLI   DMCB+3,0            =LENGTH OF VALID FIELD                       
         BNE   DATEVL2                                                          
         CLI   BYTE,2              TRIED BOTH                                   
         BE    INVDTE              INVALID DATE                                 
         B     DATEVL1                                                          
         SPACE 2                                                                
DATEVL2  CLI   DATESW,C'S'         IS THIS A START DATE                         
         BNE   DATEVL6             NO - MUST BE END DATE                        
         CLC   WORK+4(2),=C'00'    CHECK TO SUPPLY DEFAULT DAY                  
         BNE   DATEVL4             INPUT WAS M/D/Y                              
         MVC   WORK+4(2),=C'01'    DFLT DAY IS THE 1ST                          
DATEVL4  GOTO1 DATCON,DMCB,(0,WORK),(1,(R3)) SAVE PACKED START                  
         B     DATEVL99                                                         
*                                                                               
DATEVL6  CLC   WORK+4(2),=C'00'    CHECK TO SUPPLY DEFAULT DAY                  
         BNE   DATEVL9             INPUT WAS M/D/Y                              
*                                                                               
         MVC   WORK+4(2),=C'28'    DFLT DAY IS LAST DAY OF MONTH                
DATEVL8  GOTO1 ADDAY,DMCB,WORK,WORK+6,F'1'  ADD TILL MONTH INCREMENT            
         CLC   WORK+2(2),WORK+8    HAS MONTH CHANGED                            
         BNE   DATEVL9             YES                                          
         MVC   WORK+4(2),WORK+10   HIGHER DAY                                   
         B     DATEVL8                                                          
*                                                                               
DATEVL9  GOTO1 DATCON,DMCB,(0,WORK),(1,(R3)) SAVE PACKED END                    
*                                                                               
*        CHECK FOR MAX OF 12 MONTHS                                             
         GOTO1 DATCON,DMCB,(1,(R5)),(0,WORK)  START DATE (R5) TO EBCDIC         
         SR    R0,R0                                                            
DATEVL10 GOTO1 DATCON,DMCB,(0,WORK),(1,FULL)  PACK THE DATE FOR COMPARE         
         AH    R0,=H'1'                       COUNT NUMBER OF MONTHS            
         CH    R0,=H'12'                                                        
         BNH   *+12                                                             
         LA    R3,MSG19L                                                        
         B     ERRMSG                         MORE THAN 12 MONTHS               
*                                                                               
         CLC   FULL(2),0(R3)                  COMPARE FOR END DATE(R3)          
         BNL   DATEVL99                                                         
*                                                                               
         MVC   WORK+4(2),=C'28'               GET TO NEXT MONTH                 
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'6'                                      
         MVC   WORK(6),WORK+6                                                   
         B     DATEVL10                                                         
*                                                                               
DATEVL99 B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                     *----------------------*                                  
*                     *   MODE = DSPLYREC    *                                  
*                     *----------------------*                                  
*---------------------------------------------------------------------*         
DSPR     DS    0H                                                               
         CLI   MODE,DSPLYREC                                                    
         BNE   BLDR                                                             
         LA    R4,IO                                                            
         BAS   RE,DISPLAY                                                       
         LA    R2,LOGDPTH                                                       
         CLI   LOGACT,C'A'                                                      
         BNE   XIT                                                              
         LA    R2,LOGWSALH                                                      
         B     XIT                                                              
*                                                                               
DISPLAY  NTR1                                                                   
         ST    R4,AREC                                                          
         ZAP   TOTPCT,=P'0'                                                     
         MVC   NONCLI,=C'999-100'                                               
         CLC   LOGSAMT(3),=C'BUD'                                               
         BE    D05                                                              
         CLI   BUDSW,1                                                          
         BE    D05                                                              
*                                                                               
         BAS   RE,CLRFLDS                                                       
         TWAXC LOGWSALH            TURNS TRANSMIT BITS ON                       
         MVI   ELCODE,X'5D'        SALARY BUDGET ELEMENT                        
         USING ACBSALD,R4                                                       
         BAS   RE,GETEL                                                         
         BNE   D08                                                              
D04      CLC   ACBSDGNO,BUDNUM     MATCH ON BUDGET NUMBER                       
         BNE   *+12                                                             
         TM    ACBSTA1,ACBSMON     SKIP MONTHLY ELEMS                           
         BNO   D10                                                              
         BAS   RE,NEXTEL                                                        
         BE    D04                                                              
         B     D08                                                              
*                                                                               
D05      DS    0H                                                               
         BAS   RE,CLRFLDS                                                       
         TWAXC LOGWSALH            TURNS TRANSMIT BITS ON                       
         MVC   LOGSAMT(3),=C'BUD'                                               
         OI    LOGSAMTH+6,X'80'                                                 
         MVI   ELCODE,X'52'        SALARY BUDGET ELEMENT                        
         USING ACSALRYD,R4                                                      
         BAS   RE,GETEL                                                         
         BNE   D08                                                              
         B     *+12                                                             
D05E     BAS   RE,NEXTEL                                                        
         BNE   D08                                                              
         CLI   ACSALTYP,X'08'                                                   
         BNE   D05E                                                             
*                                                                               
         CLI   LOGSTRTH+5,0                                                     
         BE    D06                                                              
         CLC   ACSALBEG,MYSTART                                                 
         BE    D06                                                              
         BH    D05E                                                             
         BL    D05G                                                             
*                                                                               
D05G     EQU   *                                                                
         CLI   LOGENDH+5,0                                                      
         BE    D06                                                              
         OC    ACSALEND,ACSALEND                                                
         BZ    D07                                                              
         CLC   ACSALEND,MYEND                                                   
         BE    D06A                                                             
         BH    D06A                                                             
         B     D05E                                                             
*                                                                               
D06      DS    0H                                                               
         MVC   MYSTART(2),ACSALBEG                                              
         MVI   MYSTART+2,X'01'                                                  
D06A     OC    ACSALEND,ACSALEND                                                
         BNZ   D06F                                                             
         GOTO1 DATCON,DMCB,(5,WORK),(1,MYEND)                                   
         B     D07                                                              
D06F     MVC   TSTDATP(2),ACSALEND                                              
         MVI   TSTDATP+2,X'01'                                                  
         GOTO1 DATCON,DMCB,(1,TSTDATP),(0,TSTDATE)                              
         MVI   TSTDATV+2,C'/'                                                   
         MVI   TSTDATV+5,C'/'                                                   
         MVC   TSTDATV(2),TSTDATE+2                                             
         MVC   TSTDATV+6(2),TSTDATE                                             
         MVC   TSTDATV+3(2),=C'31'                                              
         GOTO1 DATVAL,DMCB,(0,TSTDATV),TSTDATE                                  
         OC    DMCB(4),DMCB                                                     
         BNZ   D06H                                                             
         MVC   TSTDATV+3(2),=C'30'                                              
         GOTO1 DATVAL,DMCB,(0,TSTDATV),TSTDATE                                  
         OC    DMCB(4),DMCB                                                     
         BNZ   D06H                                                             
         MVC   TSTDATV+3(2),=C'28'                                              
         GOTO1 DATVAL,DMCB,(0,TSTDATV),TSTDATE                                  
         OC    DMCB(4),DMCB                                                     
         BNZ   D06H                                                             
         MVI   TSTDATE+2,X'29'                                                  
D06H     GOTO1 DATCON,DMCB,(0,TSTDATE),(1,MYEND)                                
         OI    LOGENDH+6,X'80'                                                  
         GOTO1 DATCON,DMCB,(1,MYEND),(6,LOGEND)                                 
*                                                                               
D07      DS    0H                                                               
         ZAP   WEEKS,=P'52'                                                     
         SR    R1,R1                                                            
         LA    R2,12                                                            
         LA    R3,PROGPROF                                                      
D07A     ZIC   R4,0(R3)                                                         
         AR    R1,R4                                                            
         LA    R3,1(R3)                                                         
         BCT   R2,D07A                                                          
         ST    R1,TOTDAYS                                                       
*                                                                               
***      BAS   RE,BUDDATS                                                       
         GOTO1 DATCON,DMCB,(1,MYSTART),(6,LOGSTRT)                              
         GOTO1 DATCON,DMCB,(1,MYBUDST),(6,LOGCLTS)                              
         GOTO1 DATCON,DMCB,(1,MYBUDED),(6,LOGCLTE)                              
         OI    LOGSTRTH+6,X'80'                                                 
         OI    LOGCLTSH+6,X'80'                                                 
         OI    LOGCLTEH+6,X'80'                                                 
         B     D54                                                              
*                                                                               
D08      LA    R3,MSG2L            NO X'5D' NO X'5E'                            
         LA    R2,LOGWSALH                                                      
         CLI   BUDSW,1                                                          
         BNE   *+8                                                              
         LA    R2,LOGSTRTH                                                      
         B     ERRMSG                                                           
*                                                                               
         USING ACBSALD,R4                                                       
D10      L     R6,AREC                                                          
         USING ACKEYD,R6                                                        
         CLI   LOGSTRTH+5,0        USER DESIRES MOST RECENT EL BY DATE          
         BNE   D11                 NO                                           
D10A     LR    RF,R4                                                            
D10B     ZIC   R1,1(RF)                                                         
         AR    RF,R1               NEXT ELEMENT                                 
         CLI   0(RF),X'5D'         ANOTHER SALARY EL                            
         BNE   D11G                NO-R4=A(1ST OF MOST RECENT 5D)               
         CLC   ACBSDGNO-ACBSALD(L'ACBSDGNO,RF),BUDNUM                           
         BNE   D10B                                                             
         TM    ACBSTA1-ACBSALD(RF),ACBSMON       SKIP MONTHLY ELEMS             
         BO    D10B                                                             
         CLC   2(2,RF),2(R4)       MORE RECENT DATE                             
         BE    D10B                NO                                           
         LR    R4,RF               YES, SEE IF ITS THE LATEST                   
         B     D10A                                                             
*                                                                               
D11      CLC   LOGSTRT(5),=C'FIRST' DISPLAY FIRST(EARLIEST)                     
         BE    D11G                YES                                          
*                                                                               
         SR    R0,R0               PREV OR NEXT INDICATOR                       
         CLC   LOGSTRT(4),=C'PREV' DISPLAY PREVIOUS                             
         BE    DISOPT              NO                                           
         CLC   LOGSTRT(4),=C'NEXT' DISPLAY NEXT                                 
         BNE   D14                                                              
         LR    R0,RB               SIGNIFY NEXT                                 
*                                                                               
DISOPT   ZIC   R1,DISPTOEL         DISP FROM ACRECORD TO LAST ELEMENT           
         LA    RF,ACRECORD                                                      
         LR    R4,RF                                                            
         AR    R4,R1               A(LAST ELEMENT)                              
*                                                                               
D11A     CLI   0(RF),X'5D'         ADVANCE TO FIRST 5D ELEMENT                  
         BNE   D11ANX                                                           
         TM    ACBSTA1-ACBSALD(RF),ACBSMON     SKIP MONTHLY ELEMS               
         BO    D11ANX                                                           
         CLC   ACBSDGNO-ACBSALD(L'ACBSDGNO,RF),BUDNUM                           
         BE    D11B                                                             
D11ANX   ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     D11A                                                             
*                                                                               
D11B     CR    RF,R4               IF EQUAL                                     
         BNE   D11D                THERE IS NO PREVIOUS                         
         LTR   R0,R0                                                            
         BNZ   D11E                                                             
D11C     LA    R3,MSG12L           DESIRED ELEMENT NOT EXISTENT                 
         LA    R2,LOGSTRTH                                                      
         B     ERRMSG                                                           
D11D     ST    RF,FULL                                                          
         ZIC   R1,1(RF)            ADVANCE TO A(EL DISPLAYED LAST TIME)         
         AR    RF,R1                                                            
         CLI   0(RF),X'5D'         ANOTHER ELEMENT                              
         BNE   D11C                NO                                           
         TM    ACBSTA1-ACBSALD(RF),ACBSMON     SKIP MONTHLY ELEMS               
         BO    D11D                                                             
         CLC   ACBSDGNO-ACBSALD(L'ACBSDGNO,RF),BUDNUM                           
         BNE   D11D                                                             
         CR    RF,R4                                                            
         BNE   D11D                                                             
         LTR   R0,R0               IF PREVIOUS                                  
         BZ    D11F                GOT IT                                       
*                                                                               
D11E     ZIC   R1,1(RF)            ADVANCE TO NEXT                              
         AR    RF,R1                                                            
         CLI   0(RF),X'5D'         ANOTHER ELEMENT                              
         BNE   D11C                NO                                           
         TM    ACBSTA1-ACBSALD(RF),ACBSMON     SKIP MONTHLY ELEMS               
         BO    D11E                                                             
         CLC   ACBSDGNO-ACBSALD(L'ACBSDGNO,RF),BUDNUM                           
         BNE   D11E                                                             
         CLC   0(6,R4),0(RF)       MORE RECENT DATES                            
         BNL   D11E                                                             
         ST    RF,FULL                                                          
*                                                                               
D11F     L     R4,FULL             A(PREVIOUS ELEMENT)                          
D11G     MVC   MYSTART(6),ACBSSTRT START-END TO TDA FLDS                        
         BAS   RE,WEEK             INIT STORAGE FLDS WITH EL DATES              
         B     D20                                                              
*                                                                               
D14      CLC   ACBSSTRT,MYSTART                                                 
         BNE   D15                                                              
         CLC   ACBSENDT,MYEND                                                   
         BNE   D15                                                              
         TM    ACBSTA1,ACBSMON     SKIP MONTHLY ELEMS                           
         BO    D15                                                              
         CLC   ACBSDGNO,BUDNUM                                                  
         BE    D20                                                              
D15      BAS   RE,NEXTEL                                                        
**T      BNE   D05                 NO ELEMENT FOR I OR A                        
         BNE   D08                 NO ELEMENT FOR I OR A                        
         B     D10                                                              
*                                                                               
D20      LR    R1,R4               STORE FOR DISPLAY OPTIONS                    
         LA    RF,ACRECORD                                                      
         SR    R1,RF                                                            
         STC   R1,DISPTOEL                                                      
         DROP  R6                                                               
         ZAP   SVWSAL,ACBSWKLY     SAVE IN EVENT OF DELETE OPTION               
         ZAP   DUB,ACBSWKLY        WEEKLY SALARY                                
         LA    R2,LOGWSALH                                                      
         BAS   RE,NUMDIS                                                        
*                                                                               
         ZAP   DUB,ACBSYRLY        YEARLY SALARY                                
         LA    R2,LOGYSALH                                                      
         BAS   RE,NUMDIS                                                        
*                                                                               
         ZAP   HOLDINC1,=P'0'                                                   
         ZAP   HOLDINC2,=P'0'                                                   
         OC    ACBSQTR1,ACBSQTR1                                                
         BZ    D50                 NO INCREASE DATA TO DISP                     
*                                                                               
D35      MVC   LOGQTR1(1),ACBSQTR1 QUARTER 1                                    
         ZAP   DUB,ACBSINC1        INCREASE 1                                   
         LA    R2,LOGINC1H                                                      
         BAS   RE,NUMDIS2                                                       
*                                                                               
         LA    R0,4                                                             
         LA    R3,QTRTBL                                                        
D37      CLC   ACBSQTR1,0(R3)                                                   
         BE    D38                                                              
         LA    R3,3(R3)            NXT TBL ENT                                  
         BCT   R0,D37                                                           
*                                                                               
D38      ZAP   HOLDINC1,ACBSINC1   CALCULATE INCREASE 1                         
         ZAP   DUB(2),WEEKS                                                     
         SP    DUB(2),1(2,R3)      NUMBER OF WEEKS OF RNG-STRT WK QTR           
         MP    HOLDINC1,DUB(2)     AMOUNT OF WEEKS FOR QUARTER                  
*                                                                               
         ZAP   DUB,HOLDINC1                                                     
         LA    R2,LOGAMT1H                                                      
         BAS   RE,NUMDIS2          1ST INCREASE                                 
*                                                                               
         OC    ACBSQTR2,ACBSQTR2                                                
         BZ    D47                 NO INCREASE DATA TO DISP                     
*                                                                               
         MVC   LOGQTR2(1),ACBSQTR2 QUARTER 2                                    
         ZAP   DUB,ACBSINC2                                                     
         LA    R2,LOGINC2H                                                      
         BAS   RE,NUMDIS2          INCREASE 2                                   
*                                                                               
         LA    R0,4                                                             
         LA    R3,QTRTBL                                                        
D40      CLC   ACBSQTR2,0(R3)                                                   
         BE    D45                                                              
         LA    R3,3(R3)            NXT TBL ENT                                  
         BCT   R0,D40                                                           
*                                                                               
D45      ZAP   HOLDINC2,ACBSINC2   CALCULATE INCREASE 2                         
         ZAP   DUB(2),WEEKS                                                     
         SP    DUB(2),1(2,R3)      NUMBER OF WEEKS OF RNG-STRT WK QTR           
         MP    HOLDINC2,DUB(2)     AMOUNT OF WEEKS                              
*                                                                               
         ZAP   DUB,HOLDINC2                                                     
         LA    R2,LOGAMT2H                                                      
         BAS   RE,NUMDIS2          INCREASE 2                                   
*                                                                               
D47      AP    HOLDINC1,HOLDINC2                                                
         CP    HOLDINC1,=P'0'                                                   
         BE    D50                                                              
         ZAP   DUB,HOLDINC1                                                     
         LA    R2,LOGIAMTH         BDGTED YRLY INC                              
         BAS   RE,NUMDIS2                                                       
*                                                                               
D50      ZAP   DUB,ACBSBSAL                                                     
         LA    R2,LOGSAMTH                                                      
         BAS   RE,NUMDIS           BUDGETED YEARLY SALARY                       
*                                                                               
D50B     LA    R2,LOGCLTSH         CLT BUDGET  START DATE                       
         CLI   5(R2),0             DID THEY INPUT ONE                           
         BNE   D52                 YES - GO VALIDATE                            
         CLI   LOGCLTEH+5,0                                                     
         BE    D51                                                              
         LA    R2,LOGCLTEH         NO - THEN CANT HAVE BUDGET END DATE          
         LA    R3,MSG17L                                                        
         B     ERRMSG                                                           
D51      MVC   MYBUDST,MYSTART      MAKE IT THE SAME AS SAL START               
         MVC   MYBUDED,MYEND                                                    
         B     D54                                                              
         SPACE  1                                                               
D52      LA    R6,LOGCLTS          CLT BUDGET START DATE                        
         LA    R3,MYBUDST          BUDGET START  SAVE AREA                      
         MVI   DATESW,C'S'                                                      
         BAS   RE,DATEVAL                                                       
         LA    R2,LOGCLTEH                                                      
         CLI   LOGCLTEH+5,0        MUST HAVE BUDGET END                         
         BNE   D53                                                              
         LA    R3,MSG18L                                                        
         B     ERRMSG                                                           
D53      LA    R6,LOGCLTE          CLT BUDGET END DATE                          
         LA    R3,MYBUDED          BUDGET END SAVE AREA                         
         LA    R5,MYBUDST          BUDGET START                                 
         MVI   DATESW,C'E'                                                      
         BAS   RE,DATEVAL                                                       
*                                                                               
         CLC   MYBUDST,MYSTART     BUD PERIOD MUST BE WITHIN SAL PERIOD         
         BL    *+14                                                             
         CLC   MYBUDED,MYEND                                                    
         BNH   D54                                                              
         LA    R3,MSG7L                                                         
         LA    R2,LOGCLTSH                                                      
         B     ERRMSG                                                           
*                                                                               
D54      BAS   RE,DATEINIT         SETS YR/MNTH TABLE PACKED FORMAT             
         MVI   ELCODE,X'5E'        CLT/PCT ELEMENT                              
         L     R4,AREC                                                          
         USING ACCLPCTD,R4         FIND EL DATES = TO DATE FLD INPUT            
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SR    R3,R3               REGULATE LOOP FOR 2 ELEMENTS                 
*                                                                               
D55      CLC   SVSTRT,ACCLPCST                                                  
         BNE   D60                                                              
         CLC   SVEND,ACCLPCEN                                                   
         BNE   D60                                                              
         CLC   BUDNUM,ACCLBDNO                                                  
         BE    D65                                                              
D60      ZIC   R1,ACCLPCLN                                                      
         AR    R4,R1                                                            
         CLI   0(R4),X'5E'                                                      
         BE    D55                                                              
         LTR   R3,R3               IF R3 IS 0 - NO CLT ALLOC EL'S FOUND         
         BNZ   D62                                                              
         LA    R2,LOGCLTSH                                                      
         LA    R3,MSG20L                                                        
         B     ERRMSG                                                           
*                                                                               
D62      CLI   LOGACT,C'I'                                                      
         BE    XIT                                                              
*                                                                               
D63      CP    TOTPCT,=P'10000'                                                 
         BE    XIT                 FULLY ALLOCATED                              
         ZAP   DUB,=P'10000'                                                    
         SP    DUB,TOTPCT          GET NON CLIENT PERCENT                       
         MVC   8(7,R2),NONCLI      CODE FOR NON CLIENT                          
         OI    6(R2),X'80'         TRANSMIT BIT                                 
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2 TO PERCENT HEADER                         
         EDIT  (P8,DUB),(5,8(R2)),2,ALIGN=LEFT                                  
         OI    6(R2),X'80'         TRANSMIT BIT                                 
         B     XIT                                                              
*                                                                               
D65      LTR   R3,R3               WHICH PASS                                   
         BNZ   D67                 SECOND                                       
*                                                                               
         LA    R2,LOGCLT1H         1ST CLT INPUT HDR                            
D67      LA    R3,ACCLTPRD         1ST ELEMENT CLIENT                           
         ZIC   R0,ACCLPCLN         ELEMENT LENGTH                               
         SH    R0,=H'8'            DEDUCT FOR CODE-LEN-DATES                    
         ST    R0,ELLEN                                                         
*                                                                               
D70      MVC   8(3,R2),0(R3)       CLIENT                                       
         LA    R6,8(R2)                                                         
DI73     CLI   0(R6),C' '          FIND 1ST AVAILABLE POS FOR DILIMETER         
         BE    DI73B                                                            
         CLI   0(R6),0                                                          
         BE    DI73B                                                            
         LA    R6,1(R6)                                                         
         B     DI73                                                             
DI73B    MVI   0(R6),C'-'          DILIMETER                                    
         LA    R6,1(R6)            ADVANCE REC. FLD                             
         LA    R3,3(R3)            A(PRD)                                       
         MVC   0(3,R6),0(R3)                                                    
*                                                                               
DI73C    LA    R3,3(R3)            TO PCT                                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               TO PCT HDR                                   
         AP    TOTPCT,0(3,R3)                                                   
         CP    0(3,R3),=P'10000'                                                
         BNE   DI74                                                             
         MVC   8(3,R2),=C'100'                                                  
         B     DI74A                                                            
DI74     EDIT  (P3,0(R3)),(5,8(R2)),2,ALIGN=LEFT                                
*                                                                               
DI74A    ZIC   R1,0(R2)                                                         
         AR    R2,R1               NEXT CLT HDR                                 
*                                                                               
         L     R0,ELLEN                                                         
         SH    R0,=H'9'            CHECK IF END OF ELEMENT                      
         LTR   R0,R0                                                            
         BZ    D60                 CHECK FOR 2ND ELEMENT                        
         ST    R0,ELLEN                                                         
*                                                                               
         LA    R3,3(R3)            NEXT CLIENT                                  
         B     D70                                                              
         EJECT                                                                  
*--------------------------------------------*                                  
*        NUMDIS-COMMON ROUTINE TO DISPLAY    *                                  
*        NUMERICS                            *                                  
*        DUB=AMOUNT                          *                                  
*        R2 =A(FIELD HEADER)                 *                                  
*--------------------------------------------*                                  
NUMDIS   SRP   DUB,62,5                                                         
NUMDIS2  OI    6(R2),X'80'                                                      
         TM    1(R2),X'20'                                                      
         BNO   ND10                                                             
         EDIT  (P8,DUB),(9,8(R2)),MINUS=YES                                     
         BR    RE                                                               
ND10     EDIT  (P8,DUB),(9,BLOCK),ALIGN=LEFT,MINUS=YES                          
         ZIC   R3,0(R2)                                                         
         SH    R3,=H'9'                                                         
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   8(0,R2),BLOCK                                                    
*--------------------------------*                                              
*        CLEAR INCREASE FIELDS                                                  
*--------------------------------*                                              
CLRFLDS  MVC   LOGAMT1,SPACES                                                   
         MVC   LOGAMT2,SPACES                                                   
         MVC   LOGIAMT,SPACES                                                   
         OI    LOGAMT1H+6,X'80'                                                 
         OI    LOGAMT2H+6,X'80'                                                 
         OI    LOGIAMTH+6,X'80'                                                 
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*                *------------------------*                                     
*                *    MODE = BUILDREC     *                                     
*                *------------------------*                                     
*--------------------------------------------------------------------*          
BLDR     CLI   MODE,NEWELEM                                                     
         BE    *+16                                                             
         CLI   MODE,CHNGELEM                                                    
         BE    *+8                                                              
         B     XIT                                                              
*                                                                               
         CLI   BUDSW,1                                                          
         BE    BLDR06                                                           
         CLC   MYBUDST,MYSTART     BUD PERIOD MUST BE WITHIN SAL PERIOD         
         BL    *+14                                                             
         CLC   MYBUDED,MYEND                                                    
         BNH   BLDR06                                                           
         LA    R3,MSG7L                                                         
         LA    R2,LOGCLTSH                                                      
         B     ERRMSG                                                           
BLDR06   LA    R4,IO2              SALARY/BUDGET ELEMENT                        
         USING ACKEYD,R4                                                        
         LA    R2,IO2              ON AMENDS MOVE RECORD TO IO2                 
         LA    R3,IOLENQ                                                        
         XR    R5,R5                                                            
         LH    R5,ACLENGTH         CURRENT LENGTH                               
         MVCL  R2,R4                                                            
*                                                                               
         CLI   BUDSW,1                                                          
         BE    BLDR15                                                           
*                                                                               
         LA    R4,IO2              SALARY/BUDGET ELEMENT                        
         MVI   ELCODE,X'5D'        BUILD NEW ELEMENT(SAL-BDG)                   
         BAS   RE,GETEL                                                         
         BNE   BLDR21              NO X'5D' NO X'5E'S'                          
         USING ACBSALD,R4          COVERS SALARY/BUDGET ELEMENT                 
BLDR10   CLC   ACBSDGNO,BUDNUM     FOR THIS BUDGET TYPE                         
         BNE   BLDR12                                                           
         CLC   ACBSSTRT,MYSTART                                                 
         BNE   BLDR10A                                                          
         CLC   ACBSENDT,MYEND                                                   
         BNE   BLDR10A                                                          
         MVI   ACBSALEL,X'FF'      MARK TO DELETE                               
         B     BLDR12                                                           
*                                                                               
BLDR10A  CLC   MYSTART,ACBSENDT    CHECK ELEMENT FOR OVERLAPPING DATES          
         BH    BLDR12                                                           
         CLC   MYEND,ACBSSTRT                                                   
         BL    BLDR12                                                           
         LA    R3,MSG5L            1ST PASS NOTIFY USER OF IMPENDING            
         LA    R2,LOGSTRTH         DELETE                                       
         B     ERRMSG                                                           
*                                                                               
BLDR12   BAS   RE,NEXTEL                                                        
         BE    BLDR10                                                           
*                                                                               
         USING ACCLPCTD,R4         SCAN FOR CLT-PCT ELEMENTS TO DELETE          
BLDR15   LA    R4,IO2                                                           
         MVI   ELCODE,X'5E'                                                     
         BAS   RE,GETEL                                                         
         BNE   BLDR21                                                           
*                                                                               
BLDR17   CLC   BUDNUM,ACCLBDNO                                                  
         BNE   BLDR19                                                           
         CLC   SVSTRT,ACCLPCST     COMPARE COMPRESSED DATES                     
         BNE   BLDR17A                                                          
         CLC   SVEND,ACCLPCEN                                                   
         BNE   BLDR17A                                                          
         MVI   ACCLPCEL,X'FF'      DELETE THIS ONE                              
         B     BLDR19                                                           
*                                                                               
BLDR17A  CLC   SVSTRT,ACCLPCEN     CHECK ELEMENT FOR OVERLAPPING DATES          
         BH    BLDR19                                                           
         CLC   SVEND,ACCLPCST                                                   
         BL    BLDR19                                                           
***      LA    R3,MSG5L            1ST PASS NOTIFY USER OF IMPENDING            
         GOTO1 DATCON,DMCB,(2,ACCLPCST),(6,MSG22+26)                            
         GOTO1 DATCON,DMCB,(2,ACCLPCEN),(6,MSG22+36)                            
         LA    R3,MSG22L           1ST PASS NOTIFY USER OF IMPENDING            
         LA    R2,LOGCLTSH         DELETE                                       
         B     ERRMSG                                                           
*                                                                               
BLDR19   BAS   RE,NEXTEL                                                        
         BE    BLDR17                                                           
*                                                                               
BLDR21   GOTO1 DELEL,DMCB,(X'FF',IO2),0 DELETE MARKED ELEMENTS                  
*                                                                               
         BAS   RE,ELVAL            VALIDATE DETAIL PORTION OF SCREEN            
*--------------------------------------------------------------*                
*        ANALYSE EXISTENT BUDGET FILE-AMEND TO REFLECT                          
*        NEW PERSONNEL/BUDGET DATA INPUT.                                       
*--------------------------------------------------------------*                
BLDR25   BAS   RE,BDGTPROC                                                      
         CLC   LOGWSAL(6),=C'DELETE' IF DELETE MODE                             
         BNE   BLDR26                                                           
         LA    RF,MYIO                                                          
         LA    RE,IO2                                                           
         LA    R1,IOLENQ                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
         BAS   RE,MYWRITE          WRITE 1R REC WITHOUT 5D-5E ELS               
         ZAP   DUB,SVWSAL          REDISPLAY WEEKLY SALARY                      
         LA    R2,LOGWSALH                                                      
         BAS   RE,NUMDIS                                                        
         MVI   DISPTOEL,0          CLEAR DISPLAY OPTION DISPLACEMENT            
         LA    R3,MSG13L           DATA DELETED MSG                             
         LA    R2,LOGACTH                                                       
         B     ERRMSG                                                           
*                                                                               
BLDR26   MVI   ELCODE,X'5E'        MAY HAVE BEEN IN DELETE MODE FOR             
         LA    R4,IO2                                                           
         USING ACCLPCTD,R4         ALL CLT-PRD'S ENTERED                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
BLDR28   BAS   RE,NEXTEL           THERE COULD BE 2                             
         BNE   BLDR30              NONE FOUND DELETE 5D                         
         CLC   BUDNUM,ACCLBDNO     BUDGET NUMBER                                
         BNE   BLDR28                                                           
         CLC   SVSTRT,ACCLPCST     COMPARE COMPRESSED DATES                     
         BNE   BLDR28                                                           
         CLC   SVEND,ACCLPCEN                                                   
         BNE   BLDR28                                                           
         B     BLDRDIS                                                          
*                                                                               
BLDR30   GOTO1 DELEL,DMCB,(X'5D',IO2),(8,MYSTART)                               
         LA    R3,LOGWSALH         CLEAR 5D-5E ELEMENT INPUT                    
BLDR32   ZIC   R0,0(R3)                                                         
         CH    R0,=H'9'                                                         
         BE    XIT                                                              
         TM    1(R3),X'20'                                                      
         BO    BLDR35                                                           
         LA    R1,8(R3)                                                         
         LR    R6,R0                                                            
         SH    R6,=H'9'                                                         
         OI    6(R3),X'80'                                                      
         EX    R6,CLEARIPT                                                      
         B     BLDR35                                                           
CLEARIPT MVC   0(0,R1),SPACES                                                   
*                                                                               
BLDR35   AR    R3,R0                                                            
         B     BLDR32                                                           
*                                                                               
*        DISPLAY PERSONNEL RECORD JUST PROCESSED                                
BLDRDIS  LA    R4,IO2                                                           
         BAS   RE,DISPLAY                                                       
         B     XIT                 BUDGET DATA PROCESSED-NO ERRORS              
         EJECT                     BACK TO BASE TO ADD IO2 CONTENTS             
*-------------------------------------------------*                             
*  EDIT BUDGET-SALARY INPUT BUILD                 *                             
*  ACBSALEL (X'5D') AND ACCLPCEL (X'5E') ELEMENTS *                             
*-------------------------------------------------*                             
ELVAL    NTR1                                                                   
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING ACBSALD,R4          COVERS SALARY BUDGET ELEMENT                 
         MVI   ACBSALEL,X'5D'      ELEMENT CODE                                 
         MVI   ACBSALEN,ACBSLNEQ   ELEMENT LENGTH                               
         MVC   ACBSSTRT,MYSTART    START DATE                                   
         MVC   ACBSENDT,MYEND      END DATE                                     
         MVC   ACBSDGNO,BUDNUM     BUDGET NUMBER                                
         ZAP   ACBSWKLY,=P'0'                                                   
         ZAP   ACBSYRLY,=P'0'                                                   
         ZAP   ACBSINC1,=P'0'                                                   
         ZAP   ACBSINC2,=P'0'                                                   
         ZAP   ACBSBSAL,=P'0'                                                   
         ZAP   HOLDINC1,=P'0'                                                   
         ZAP   HOLDINC2,=P'0'                                                   
*                                                                               
         CLI   BUDSW,1                                                          
         BNE   ELV120                                                           
         BAS   RE,BUDWSAL                                                       
         ZAP   ACBSWKLY,SVWSAL                                                  
         ZAP   SVYSAL,SVWSAL                                                    
***      MP    SVYSAL,=P'52'                                                    
         MP    SVYSAL,WEEKS                                                     
         ZAP   ACBSYRLY,SVYSAL                                                  
         ZAP   ACBSBSAL,SVYSAL                                                  
         ZAP   HOLDINC1,SVYSAL                                                  
         MVC   LOGSAMT(3),=C'BUD'                                               
         OI    LOGSAMTH+6,X'80'                                                 
         B     BR62A                                                            
*                                                                               
ELV120   LA    R2,LOGWSALH         A(WEEKLY CURRENT SALARY INPUT)               
         GOTO1 ANY                                                              
*                                                                               
ELV130   CLC   LOGWSAL(6),=C'DELETE' IF DELETE MODE                             
         BNE   ELV140                                                           
         ZAP   ACBSWKLY,SVWSAL     SAVED SAL FROM DISP PASS TO EL FLD           
         B     BR03                                                             
*                                                                               
ELV140   GOTO1 IPTVAL,DMCB,(R2)    COMMON ROUTINE NUMERIC VAL                   
         CP    DUB+2(6),=P'9999900'                                             
         BH    INVERR                                                           
         ZAP   ACBSWKLY,DUB+4(4)   WEEKLY CURRENT SALARY                        
*-------------------------------------------------------------------*           
*        VERIFY YEARLY SALARY INPUT MULTIPLY WEEKLY SALARY INPUT BY             
*        THE NUMBER OF WEEKS REPRESENTED BY MYSTART TO MYEND                    
*-------------------------------------------------------------------*           
BR03     ZAP   DUB,WEEKS           # OF WEEKS                                   
         ZAP   WORK(10),ACBSWKLY                                                
         GOTO1 CALCIT,DMCB,DUB+5,WORK,1,0                                       
         BZ    BR05A               GOOD CC                                      
         LA    R3,MSG8L                                                         
         B     ERRMSG                                                           
BR05A    ZAP   HOLDINC1,WORK+4(6)  WKLY SAL * WEEKS = YEARLY SALARY             
*                                                                               
         LA    R2,LOGYSALH         A(YEARLY CURRENT SALARY INPUT)               
         GOTO1 ANY                                                              
*                                                                               
         GOTO1 IPTVAL,DMCB,(R2)    COMMON ROUTINE NUMERIC VAL                   
         ZAP   ACBSYRLY,DUB+3(5)   YEARLY CURRENT SALARY                        
         CP    ACBSYRLY,HOLDINC1   VERIFY                                       
         BE    BR08                                                             
         ZAP   DUB,HOLDINC1                                                     
         LA    R3,MSG6L                                                         
         B     CALCERR                                                          
*                                                                               
BR08     ZAP   QWK1,=P'0'          INIT QWK1 AND 2 FIELDS                       
         ZAP   QWK2,=P'0'                                                       
         MVI   QTR1,0                                                           
         MVI   QTR2,0                                                           
         BAS   RE,CLRFLDS          CLEAR INCREASE FIELDS                        
*                                                                               
         SR    R0,R0               INDICATE 1ST QTR EDIT                        
         LA    R2,LOGQTR1H                                                      
         BAS   RE,QTRVAL                                                        
         LTR   R0,RB                                                            
         LA    R2,LOGQTR2H                                                      
         BAS   RE,QTRVAL                                                        
         LA    R2,LOGQTR1H                                                      
         CLI   QTR2,0                                                           
         BE    BR10                                                             
         CLC   QTR2,QTR1                                                        
         LA    R2,LOGQTR2H                                                      
         BNH   INVERR                                                           
*                                                                               
BR10     CP    QWK1,=P'0'          ANY INCREASE                                 
         BNE   BR32                YES                                          
         ZAP   HOLDINC1,=P'0'                                                   
         B     BR61                                                             
*                                                                               
BR32     MVC   ACBSQTR1,QTR1       INCREASE 1                                   
*        DP    HOLDINC1,=P'100'    GET RID OF 00 PENNIES                        
         ZAP   ACBSINC1,HOLDINC1+3(3)                                           
         CP    QWK2,=P'0'                                                       
         BE    BR50                NO 2ND INCREASE                              
         MVC   ACBSQTR2,QTR2       INCREASE 2                                   
*        DP    HOLDINC2,=P'100'                                                 
         ZAP   ACBSINC2,HOLDINC2+3(3)                                           
*                                                                               
BR50     ZAP   WORK(10),ACBSINC1   CALCULATE INCREASE 1                         
         ZAP   FULL(3),QWK1                                                     
         GOTO1 CALCIT,DMCB,FULL,WORK,1,0       QTR1 * INC1                      
         BZ    BR50A               GOOD CC                                      
         LA    R3,MSG8L                                                         
         B     ERRMSG                                                           
BR50A    ZAP   HOLDINC1(6),WORK+4(6)                                            
*                                                                               
         OC    ACBSQTR2,ACBSQTR2   2ND INCREASE                                 
         BZ    BR60                NO                                           
*                                                                               
BR53     ZAP   WORK(10),ACBSINC2   CALCULATE INCREASE 2                         
         ZAP   FULL(3),QWK2                                                     
         GOTO1 CALCIT,DMCB,FULL,WORK,1,0        QTR2 * INC2                     
         BZ    BR53A               GOOD CC                                      
         LA    R3,MSG8L                                                         
         B     ERRMSG                                                           
BR53A    ZAP   HOLDINC2(6),WORK+4(6)                                            
*                                                                               
BR60     ZAP   DUB,HOLDINC1        DISPLAY INCREASE 1                           
         LA    R2,LOGAMT1H                                                      
         BAS   RE,NUMDIS2                                                       
*                                                                               
         CP    ACBSINC2,=P'0'                                                   
         BE    BR60A                                                            
         ZAP   DUB,HOLDINC2        DISPLAY INCREASE 2                           
         LA    R2,LOGAMT2H                                                      
         BAS   RE,NUMDIS2                                                       
*                                                                               
         AP    HOLDINC1(6),HOLDINC2   INCREASE + INCREASE                       
*                                                                               
BR60A    ZAP   DUB,HOLDINC1        DISPLAY TOTAL INCREASE                       
         LA    R2,LOGIAMTH                                                      
         BAS   RE,NUMDIS2                                                       
*                                                                               
BR61     MP    HOLDINC1,=P'100'    GET BACK 2 DEC                               
         AP    HOLDINC1(6),ACBSYRLY   TOTAL INCREASE + YRLY CUR SALARY          
*                                  SAVE FOR VERIFICATION                        
         LA    R2,LOGSAMTH         BUDGETED YEARLY SALARY                       
         GOTO1 ANY                                                              
*                                                                               
         GOTO1 IPTVAL,DMCB,(R2)    COMMON ROUTINE NUMERIC VAL                   
         CP    HOLDINC1,DUB+2(6)   INPUT MUST MATCH PREVIOUS CALC               
         BE    BR61B                                                            
         ZAP   DUB,HOLDINC1                                                     
         LA    R3,MSG9L                                                         
         B     CALCERR                                                          
*                                                                               
BR61B    ZAP   ACBSBSAL,DUB+3(5)    BDG YRLY SAL TO REC                         
*                                                                               
         CLC   LOGWSAL(6),=C'DELETE' IN DELET MODE                              
         BE    BR62A               DON'T ADD                                    
BR61J    LA    R4,IO2                      TO PREVENT RECORD OVERFLOW:          
         SR    R3,R3                                                            
         LH    R3,ACLENGTH-ACKEYD(R4)      IF ADDING THIS NEW 5D ELEM           
         LA    R2,ACBSLNEQ                 WILL GO OVER MAX RECORD              
         AR    R2,R3                       LENGTH DELETE OLDEST 5D              
         CH    R2,=H'1000'                 ELEMENTS                             
         BNH   BR61M                                                            
         MVI   ELCODE,X'5D'                                                     
         BAS   RE,GETEL                                                         
         BNE   BR61M                                                            
         MVI   0(R4),X'FF'                                                      
         GOTO1 DELEL,DMCB,(X'FF',IO2),0                                         
         B     BR61J                                                            
BR61M    GOTO1 ADDEL,DMCB,IO2,ELEMENT      ADD BUDGET SALARY ELEMENT            
*---------------------------------------------------------*                     
*        VALIDATE CLIENT/PERCENT INPUT                                          
*---------------------------------------------------------*                     
BR62A    LA    R4,ELEMENT                                                       
         ZAP   HOLDINC1,ACBSBSAL   STORE FOR CALC OF BDGT PCTS                  
         LA    R3,CLTTBLLN         CLEAR COST CODE-PCT TBL AREA                 
         LA    R2,CLTPCTTB                                                      
         LA    R1,0                                                             
         MVCL  R2,R0                                                            
*                                                                               
         LA    R1,LOGLSTH          A(LAST FLD)                                  
         ST    R1,LASTFLD                                                       
         LA    R6,CLTPCTTB         KEEPS LIST SJ CLT 1C COST CODES              
         USING CLTPCTD,R6                                                       
         ZAP   PCTTOT,=P'0'        ACCUM PERCENTAGE FLD                         
         LA    R2,LOGCLT1H         A(1ST CLIENT FIELD)                          
*                                                                               
BR63     XC    ELEMENT,ELEMENT     CLEAR ELEMENT BUILD AREA                     
         LA    R4,ELEMENT                                                       
         USING ACCLPCTD,R4                                                      
*                                                                               
         MVI   ACCLPCEL,X'5E'      CLIENT PERCENT ELEMENT CODE                  
         MVI   ACCLPCLN,8          INITIAL ELEMENT LENGHT                       
         MVC   ACCLPCST,SVSTRT     COMPRESSED START                             
         MVC   ACCLPCEN,SVEND      COMPRESSED END                               
         MVC   ACCLBDNO,BUDNUM     BUDGET NUMBER                                
         LA    R5,ACCLTPRD                                                      
*                                                                               
BR65     CLI   5(R2),0                                                          
         BNE   BR70                                                             
         LR    RE,R2                                                            
         ZIC   R1,0(RE)                                                         
         AR    RE,R1               TO PCT PARTNER                               
         CLI   5(RE),0                                                          
         BNE   INVERR              MISSING CLT INPUT                            
         LR    R2,RE                                                            
         B     BR95                                                             
*                                                                               
BR70     ZIC   R0,5(R2)            CLT CODE LEN                                 
         GOTO1 RDCLNT,DMCB,8(R2),(R0) VALIDATE CLT INPUT                        
*                                                                               
         MVC   0(6,R5),CLTIPT      CLT/PRD INPUT TO CLT/PCT ELEMENT             
*                                                                               
         BAS   RE,CHKIPT           EDIT FOR DUPLICATE CLT-PRD INPUT             
         MVC   CLTCSTCD,WORK       COST CODE TO WS TABLE                        
         MVC   CLTSJCD,CLTIPT                                                   
*                                  AND DUPLICATE COST CODES                     
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               TO A(PCT HDR)                                
         GOTO1 ANY                                                              
*                                                                               
         GOTO1 IPTVAL,DMCB,(C'P',(R2)) COMMON ROUTINE NUMERIC VAL               
         AP    PCTTOT(6),DUB+2(6)  ACCUMALATE PERCENT                           
         CP    PCTTOT,=P'10000'                                                 
         BNH   BR80                                                             
         ZAP   DUB,PCTTOT                                                       
         LA    R3,MSG4L                                                         
         B     CALCERR                                                          
*--------------------------------------------------------------------*          
* ALLOW FOR ZERO PCT AMOUNTS - CLT-PRD'S WITH 0 AMOUNTS ARE NOT ADDED           
* TO EITHER PERSONNEL OR BUDGET FILE. THESE AMOUNTS ACT AS INDICATORS           
* TO DELETE AT RECORD ADD TIMES.                                                
*--------------------------------------------------------------------*          
BR80     ZAP   6(3,R5),DUB+5(3)    PCT TO X'5E' ELEMENT                         
*                                                                               
* CALC. PCT * BUDGETED YRLY SAL.                                                
         ZAP   WORK(10),HOLDINC1   SAVED YRLY SAL                               
         GOTO1 CALCIT,DMCB,DUB+5,WORK,1,1       PCT * YRLY SAL                  
         SRP   WORK(10),64-4,5     ROUND UP 4 POSITIONS                         
         ZAP   CLTPCAMT,WORK+4(6)                                               
*                                                                               
         LA    R6,CLTPCTLN(R6)     NEXT COST CODE PCT AMNT POSITION             
         CP    WORK+4(6),=P'0'     IF 0, DON'T ADD TO X'5E' ELEMENT             
         BE    BR95                                                             
*                                                                               
         LA    R5,9(R5)            NEXT X'5E' CLT/PCT                           
         LA    RE,9                                                             
         ZIC   R1,ACCLPCLN                                                      
         AR    R1,RE                                                            
         STC   R1,ACCLPCLN         INCREMENT ELEMENT LENGTH                     
         CLI   ACCLPCLN,X'FB'                                                   
         BNE   BR95                                                             
BR93     CLC   LOGWSAL(6),=C'DELETE'                                            
         BE    BR94                DON'T ADD IN DELETE MODE                     
BR93C    LA    R4,IO2                      TO PREVENT RECORD OVERFLOW:          
         SR    R1,R1                                                            
         LH    R1,ACLENGTH-ACKEYD(R4)      IF ADDING THIS NEW 5D ELEM           
         ZIC   R3,ELEMENT+1                WILL GO OVER MAX RECORD              
         AR    R1,R3                       LENGTH DELETE OLDEST 5D              
         CH    R1,=H'1000'                 ELEMENTS                             
         BNH   BR93M                                                            
         MVI   ELCODE,X'5E'                                                     
         BAS   RE,GETEL                                                         
         BNE   BR93M                                                            
         MVI   0(R4),X'FF'                                                      
         GOTO1 DELEL,DMCB,(X'FF',IO2),0                                         
         B     BR93C                                                            
BR93M    GOTO1 ADDEL,DMCB,IO2,ELEMENT      ADD 1ST OF POSS 2 X'5E' ELS          
BR94     LA    R4,ELEMENT                                                       
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               NEXT CLIENT INPUT HDR                        
         L     R1,LASTFLD                                                       
         CR    R1,R2      IF MORE FLDS TO EDIT-CHK F/ MORE IPT NXT EL           
         BNE   BR63                                                             
         B     BR100                                                            
*                                                                               
BR95     ZIC   RE,0(R2)                                                         
         AR    R2,RE               NEXT CLIENT INPUT HDR                        
         L     R1,LASTFLD                                                       
         CR    R2,R1                                                            
         BNE   BR65                                                             
         CLI   CLTPCTTB,0          CHECK FOR ENTRIES IN TABLE                   
         BNE   BR97                YES                                          
*                                                                               
         LA    R2,LOGCLT1H                                                      
         LA    R3,MSGEL                                                         
         B     ERRMSG                                                           
*                                                                               
BR97     CLI   ACCLPCLN,8                                                       
         BNH   BR100                                                            
         SR    R2,RE                                                            
         B     BR93                GO ADD IT                                    
*                                                                               
BR100    CP    PCTTOT,=P'10000' FINAL ACCUM OF PCT INPUT MUST=                  
         BE    XIT                                                              
         CP    PCTTOT,=P'0'                                                     
         BE    XIT                 ZERO IS OK TO DELETE DATA                    
         ZAP   DUB,PCTTOT                                                       
         LA    R3,MSG4L                                                         
         LA    R2,LOGPCT1H                                                      
         B     CALCERR                                                          
         EJECT                                                                  
*-----------------------------------------------------------*                   
*        CALCULATE WEEKLY SALARY FROM BUD '52' ELEMENT                          
*        (FILL SVWSAL)                                                          
*-----------------------------------------------------------*                   
BUDWSAL  NTR1                                                                   
         ZAP   SVWSAL,=P'0'                                                     
         ZAP   WRKWKLY,=P'0'                                                    
         LA    R4,IO                                                            
         MVI   ELCODE,X'52'        SALARY BUDGET ELEMENT                        
         USING ACSALRYD,R4                                                      
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
BUDW105  CLI   ACSALTYP,X'08'                                                   
         BE    BUDW120                                                          
BUDW110  BAS   RE,NEXTEL                                                        
         BE    BUDW105                                                          
         LA    R3,MSG2L                                                         
         LA    R2,LOGSTRTH                                                      
         B     ERRMSG                                                           
*                                                                               
BUDW120  DS    0H                                                               
         CLI   LOGSTRTH+5,0                                                     
         BE    BUDW150                                                          
         CLC   ACSALBEG,MYSTART                                                 
         BE    BUDW150                                                          
         BL    BUDW130                                                          
         BH    BUDW110                                                          
         CLI   LOGENDH+5,0                                                      
         BE    BUDW150                                                          
BUDW130  OC    ACSALEND,ACSALEND                                                
         BZ    BUDW150                                                          
         CLC   ACSALEND,MYEND                                                   
         BE    BUDW150                                                          
         BH    BUDW150                                                          
         B     BUDW110                                                          
*                                                                               
BUDW150  CLI   ACSALBAS,C'A'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    WRKWKLY,ACSALARY                                                 
         ZAP   HALF,=P'52'                                                      
         DP    WRKWKLY,HALF                                                     
         ZAP   SVWSAL,WRKWKLY(6)                                                
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        QTRVAL-EDITS FOR QUARTER AND INCREASE INPUT. STORES VALID              
*        QUARTER AND NUMBER OF WEEKS EFFECTED BY INCREASE IN PRG.               
*        STORAGE FIELDS QTR1 AND QTR2, QWK1 AND QWK2. STORES INPUT              
*        WEEKLY INCREASE AMOUNTS IN HOLDINC1 AND HOLDINC2.                      
*        UPON ENTRY:                                                            
*        R0=0 FOR FIRST INCREASE FLDS NON-ZERO FOR 2ND                          
*        R2=CURRENT QARTER FIELD SCREEN HEADER                                  
*        WEEKS=NUMBER OF WEEKS REPRESENTING KEY INPUT DATE RNG.                 
*---------------------------------------------------------------------*         
QTRVAL   NTR1                                                                   
         CLI   5(R2),0             INPUT IN QTR FLD                             
         BNE   QT10                YES                                          
QT05     ZIC   R1,0(R2)            INVALID IF INPUT IN REMAINING FLDS           
         AR    R2,R1                                                            
         CLI   5(R2),0                                                          
         BNE   INVERR                                                           
         LTR   R0,R0               1ST OR 2ND QTR                               
         BZ    *+8                 1ST, CHK 2ND                                 
         B     XIT                                                              
         LA    R2,LOGQTR2H                                                      
         CLI   5(R2),0                                                          
         BNE   INVERR                                                           
         LR    R0,RB                                                            
         B     QT05                GO CHK 2ND AMNT FLD                          
*                                                                               
QT10     CLI   5(R2),1             EDIT QTR INPUT                               
         BNE   INVERR                                                           
         LA    R3,QTRTBL                                                        
QT20     CLC   0(1,R3),8(R2)                                                    
         BE    QT30                                                             
         LA    R3,3(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BE    INVERR                                                           
         B     QT20                                                             
*                                  STARTING WEEK OF INPUT QUARTER               
QT30     ZAP   DUB(2),WEEKS                                                     
         SP    DUB(2),1(2,R3)      FROM WEEKS OF KEY INPUT DATE RNG             
         BP    QT35                                                             
         LA    R3,MSG16L                                                        
         B     ERRMSG                                                           
*                                                                               
QT35     LTR   R0,R0               1ST OR 2ND                                   
         BNZ   *+20                                                             
         MVC   QTR1,8(R2)                                                       
         ZAP   QWK1,DUB(2)                                                      
         B     *+16                                                             
         MVC   QTR2,8(R2)                                                       
         ZAP   QWK2,DUB(2)                                                      
*                                                                               
         ZIC   R1,0(R2)            ADVANCE TO CURRENT AMNT FLD                  
         AR    R2,R1                                                            
         GOTO1 ANY                                                              
         GOTO1 IPTVAL,DMCB,(R2)    NUMERIC INPUT VAL                            
         SRP   DUB,64-2,0                                                       
         LTR   R0,R0               1ST OR 2ND                                   
         BNZ   *+14                                                             
         ZAP   HOLDINC1,DUB+2(6)                                                
         B     *+10                                                             
         ZAP   HOLDINC2,DUB+2(6)                                                
         B     XIT                                                              
*                                                                               
QTRTBL   DC    C'1',PL2'0'         QUARTER-START WEEK OF QUARTER-               
         DC    C'2',PL2'13'                                                     
         DC    C'3',PL2'26'                                                     
         DC    C'4',PL2'39'                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        RDCLNT - VALIDATES CLIENT CODE INPUT                                   
*              P1 - A(INPUT FIELD)                                              
*              P2 - INPUT FIELD LENGTH                                          
*              RETURNS IN WORK CORRESPONDING COST CODE FOR SJ-CLIENT            
*              ERROR NE X'FF' INDICATES INVALID INPUT                           
*--------------------------------------------------------------------*          
RDCLNT   NTR1                                                                   
         LM    R3,R4,0(R1)                                                      
         MVC   CLTIPT,SPACES                                                    
         MVC   PRDIPT,SPACES                                                    
*                                                                               
         LA    R6,0                                                             
         LA    R1,4                                                             
         LR    R5,R3                                                            
RC01     CLI   0(R5),C'-'          SCAN FOR DILIMETER                           
         BE    RC03                                                             
         CLI   0(R5),C','                                                       
         BE    RC03                                                             
         CLI   0(R5),C' '                                                       
         BE    RC03                                                             
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)            COUNT CLT LEN                                
         BCT   R1,RC01                                                          
         LA    R3,MSGAL                                                         
         B     ERRMSG                                                           
*                                                                               
RC03     LA    R5,1(R5)            POINTS TO PRD INPUT                          
         BCTR  R6,0                                                             
         LTR   R6,R6                                                            
         BM    INVERR                                                           
         EX    R6,TOCLT                                                         
         B     RC05                                                             
TOCLT    MVC   CLTIPT(0),0(R3)                                                  
RC05     LR    R1,R4               =LENGTH OF INPUT                             
         LA    R6,2(R6)            CLT INPUT + DILIMITER                        
         SR    R1,R6               INPUT MINUS CLT + DILIMITER                  
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BM    INVERR                                                           
         EX    R1,TOPRD                                                         
         B     RC07                                                             
TOPRD    MVC   PRDIPT(0),0(R5)                                                  
*                                                                               
RC07     MVC   DUB(6),CLTIPT       CLT/PRD                                      
         BAS   RE,SETKY                                                         
         CLC   WORK,SPACES                                                      
         BE    RC08                TRY FOR DEFAULT                              
         B     XIT                                                              
RC08     MVC   DUB,SPACES                                                       
         MVC   DUB(3),CLTIPT       CLT                                          
         BAS   RE,SETKY                                                         
         CLC   WORK,SPACES                                                      
         BE    INVERR              TRY FOR DEFAULT                              
         B     XIT                                                              
*------------------------------------------*                                    
*        READ FOR CLT/PRD OR CLT REC                                            
*        RETRIEVE COST CODE RETURN IN                                           
*        WORK                                                                   
*------------------------------------------*                                    
SETKY    NTR1                                                                   
         MVC   MYIO(42),SPACES     SET CLT/PRD KEY                              
         MVC   MYIO(1),COMPANY                                                  
         MVC   MYIO+1(2),=C'SJ'                                                 
         MVC   MYIO+3(6),DUB       CLT/PRD OR PRD                               
         BAS   RE,MYREAD                                                        
         CLI   DMCB+8,0                                                         
         BNE   INVERR                                                           
*                                                                               
         LA    R6,MYIO                                                          
         AH    R6,DATADISP         GET COSTIONG CODE                            
         USING ACPROFD,R6                                                       
RC10     CLI   0(R6),0             END OF REC                                   
         BE    NOCODE                                                           
         CLI   0(R6),X'24'                                                      
         BE    RC20                                                             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     RC10                                                             
*                                                                               
RC20     CLC   ACPRCOST,SPACES                                                  
         BE    NOCODE                                                           
         OC    ACPRCOST,ACPRCOST                                                
         BZ    NOCODE                                                           
         MVC   WORK(15),ACPRCOST                                                
         B     XIT                                                              
NOCODE   MVC   WORK,SPACES                                                      
         B     XIT                                                              
         DROP  R6                                                               
*------------------------------------------------------*                        
*        CHKIPT-EDITS FOR DUPLICATE CLIENT-PRODUCT                              
*        INPUT OR CLIENT-PRODUCTS WHICH POINT TO                                
*        THE SAME COST CODE                                                     
*        WORK = COST CODE RETRIEVED IN RDCLT RTN                                
*------------------------------------------------------*                        
CHKIPT   NTR1                                                                   
         LA    RE,CLTPCTTB   CHECK THAT EACH CLT/PRD HAS OWN COST CODE          
         USING CLTPCTD,RE                                                       
CI01     OC    CLTCSTCD,CLTCSTCD                                                
         BZ    XIT                                                              
         CLC   CLTCSTCD,WORK                                                    
         BE    DUPERR                                                           
         LA    RE,CLTPCTLN(RE)                                                  
         B     CI01                                                             
*                                                                               
DUPERR   LA    R3,MSGDL                                                         
         B     ERRMSG                                                           
         DROP  RE                                                               
         EJECT                                                                  
*-------------------------------------------------*                             
*        COMMON CALL FOR DATAMGR                                                
*-------------------------------------------------*                             
MYHIGH   MVC   COMMAND,=C'DMRDHI  '                                             
         B     DM20                                                             
MYREAD   MVC   COMMAND,=C'DMREAD  '                                             
         B     DM20                                                             
MYADD    MVC   COMMAND,=C'DMADD   '                                             
         B     DM20                                                             
MYWRITE  MVC   COMMAND,=C'DMWRT   '                                             
         B     DM20                                                             
MYSEQ    MVC   COMMAND,=C'DMRSEQ  '                                             
*                                                                               
DM20     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',MYIO,MYIO             
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------*                       
*        IPTVAL-NUMERIC INPUT VALIDATION                                        
*              P1-HI ORDER BYTE = P = PERCENT VAL                               
*              REST HAS A(OF HDR FLD)                                           
*              ON RETURN DUB+2   = PACKED NUMBER                                
*-------------------------------------------------------*                       
IPTVAL   NTR1                                                                   
         MVC   BYTE,0(R1)                                                       
         MVI   0(R1),0                                                          
         L     R3,0(R1)                                                         
         ZIC   R4,5(R2)                                                         
         CLI   BYTE,C'P'           PCT EDIT                                     
         BE    IV05                                                             
         LR    RF,R4                                                            
         LA    R1,8(R2)                                                         
IV02     CLI   0(R1),C'.'                                                       
         BNE   IV03                                                             
         LA    R3,MSG15L                                                        
         B     ERRMSG              NO PENNIES                                   
IV03     LA    R1,1(R1)                                                         
         BCT   RF,IV02                                                          
*                                                                               
IV05     GOTO1 CASHVAL,DMCB,8(R3),(R4)                                          
         CLI   DMCB,X'FF'                                                       
         BE    INVERR                                                           
         L     R6,4(R1)                                                         
         CVD   R6,DUB                                                           
         CP    DUB,=P'0'                                                        
         BL    INVERR              NO MINUS AMOUNTS                             
         CLI   BYTE,C'P'           MONEY OR PCT INPUT                           
         BNE   IV10                IT'S MONEY                                   
         CP    DUB,=P'10000'       PCT GREATER THAN 100                         
         BH    INVERR                                                           
         B     XIT                                                              
IV10     OC    DUB(2),DUB          IS IT TO BIG                                 
         BNZ   INVERR              YES                                          
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        CALCIT-ROUTINE PERFORMS ARITHMETIC CACULATIONS                         
*        P1 = MULTIPLIER, DIVISOR                                               
*        P2 = MULTIPLICAN, DIVIDEND                                             
*        P3 = ARITHMETIC FUNCTION                                               
*              1-MULTIPLICATION                                                 
*              2-DIVIDE                                                         
*        P4 = 0 = SCREEN INPUT-2 DEC                                            
*              NZ  CALCULATING WITH PCT * PENNIES                               
*        ANSWER RETURNED IN WORK                                                
*---------------------------------------------------------------------*         
CALCIT   NTR1                                                                   
         LM    R3,R6,0(R1)                                                      
         CH    R5,=H'2'                                                         
         BE    DIVIDE                                                           
*                                                                               
MULTPLY  MP    0(10,R4),0(3,R3)                                                 
         LTR   R6,R6               MORE THAN 2 DEC                              
         BNZ   XIT                 YES                                          
         OC    WORK(4),WORK        CHECK PRODUCT TO LARGE-SET RETURN CC         
         B     XIT                                                              
*                                                                               
DIVIDE   MP    0(8,R3),=P'10'                                                   
         DP    0(8,R3),0(2,R4)                                                  
         ZAP   DUB(6),0(6,R3)                                                   
         SRP   DUB(6),64-1,0                                                    
         ZAP   WORK(6),DUB(6)                                                   
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        RIDEL-MARKS 1D BUDGET ELEMENTS FOR DELETION                            
*-------------------------------------------------------------------*           
RIDEL    NTR1                                                                   
         LA    R4,MYIO             A(RECORD)                                    
         MVI   ELCODE,X'1D'        ELEMENT CODE                                 
*                                                                               
         LA    R3,DATAB            LIST OF BDG EL DATES TO DELETE               
         ZIC   R0,MNTHCNT          NUMBER OF DATES                              
         BAS   RE,GETEL                                                         
RID15    BNE   XIT                 NO 1D ELEMENTS OR END OF ELEMENT             
*                                                                               
RID17    DS    0H                                                               
         CLC   2(1,R4),BACK5YR                                                  
         BH    *+8                                                              
         MVI   0(R4),X'FF'                                                      
         CLC   2(2,R4),0(R3)                                                    
         BE    RID20                                                            
         BL    RID25                                                            
         LA    R3,2(R3)            NEXT DATE IN DATAB                           
         CLI   0(R3),X'FF'         END OF DATAB                                 
         BE    XIT                 YES                                          
         B     RID17                                                            
*                                                                               
RID20    MVI   0(R4),X'FF'         DATES MATCH MARK TO DELETE                   
RID21    BCT   R0,RID25                                                         
         B     XIT                                                              
*                                                                               
RID25    BAS   RE,NEXTEL                                                        
         B     RID15                                                            
*                                                                               
*------------------------------------------------*                              
*        ROUTINE TO DELETE AN ELEMENT            *                              
*        P1   BYTE 0    ELEMENT CODE             *                              
*             BYTE 1-3  A(RECORD)                *                              
*        P2   BYTE 0    LENGTH OF SEARCH ARGUMENT*                              
*             BYTE 1-3  A(SEARCH ARGUMENT)       *                              
*------------------------------------------------*                              
DELEL    NTR1                                                                   
         LM    R3,R4,0(R1)                                                      
         ZIC   R5,0(R1)                                                         
         ZIC   R6,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT '),((R5),(R3)),((R6),(R4))           
         B     XIT                                                              
*                                                                               
         PRINT GEN                                                              
         GETEL R4,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
*---------------------------------------*                                       
*        ROUTINE TO ADD AN ELEMENT                                              
*                                                                               
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
*---------------------------------------*                                       
ADDEL    NTR1                                                                   
         LM    R3,R4,0(R1)                                                      
         GOTO1 HELLO,ELIST,(C'P',=C'ACCOUNT '),(R3),(R4),0                      
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         EJECT                                                                  
*--------------------------------------------------------*                      
*        ROUTINE AMENDS BUDGET FILE AS DICTATED                                 
*        BY CLIENT-PRODUCT INPUT                                                
*--------------------------------------------------------*                      
BDGTPROC NTR1                                                                   
         LA    R4,MYIO                                                          
         USING ACKEYD,R4                                                        
         LA    R2,CLTPCTTB                                                      
         USING CLTPCTD,R2          COVERS COST CODE TABLE                       
*                                                                               
*        CONSTRUCT BUDGET KEY THRU U/L OF CONTRA                                
         XC    ACBTKEY(50),ACBTKEY                                              
         MVC   ACBTKEY(33),SPACES                                               
         MVI   ACBTKTYP,X'1B'                                                   
         MVC   ACBTKACC,IO2        1R, OFF/DEPT, SUB-DEPT, EMPLOYEE             
         MVC   ACBTKCON(3),CLTCSTCD CO, 1C CONTRA                               
*                                                                               
BREC03   MVC   SAVEKEY,ACBTKTYP                                                 
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         BAS   RE,MYHIGH                                                        
         NI    DMINBTS,X'FF'-X'88'   TURN'EM  OFF                               
         B     BREC10                                                           
*                                                                               
BREC05   OI    DMINBTS,X'80'       READ FOR UPDATE                              
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         BAS   RE,MYSEQ                                                         
         NI    DMINBTS,X'FF'-X'88'                                              
*                                                                               
BREC10   MVI   PROCSW,0            INNER LOOP FOR BDG REC PARTNERS              
         MVI   DMACT,0             DATAMGR ADD OR WRITE                         
*                                                                               
         CLC   SAVEKEY(21),ACBTKTYP COMPARE 1R ACT, 1C U/L OF CONTRA            
         BNE   BRECNEW             GO CHECK FOR NEW (ADDS) BDG RECS             
*                                                                               
         CLC   ACBTKBNO,BUDNUM     BUDGET NUMBER                                
         BNE   BREC05                                                           
         MVC   SAVEKEY,ACBTKTYP    SAVE ENTIRE KEY                              
*                                                                               
BREC12   BAS   RE,RIDEL            MARK ELEMENTS TO BE DELETED                  
         GOTO1 DELEL,DMCB,(X'FF',MYIO),0 DELETE MARKED ELEMENTS                 
*                                                                               
         CLI   PROCSW,1            IS IT 2ND PASS THIS REC                      
         BNE   BREC15              NO, TRY TO MATCH IN TABLE                    
         OC    CLTCSTCD,CLTCSTCD   YES, WAS A MATCH FOUND                       
         BZ    BREC25              NO, WRITE REC AS IS AFTER EL DELETES         
         B     BREC21              YES, ADD NEW ELEMENTS                        
*                                                                               
BREC15   CLC   ACBTKCON,CLTCSTCD   COMPARE REC TO COST CODE TBL ENTS            
         BE    BREC20              A MATCH, ADD NEW ELEMENTS                    
         OC    CLTCSTCD,CLTCSTCD   ANYMORE                                      
         BZ    BREC25              NO, WRITE OUT REC WITHOUT ADDING ELS         
         LA    R2,CLTPCTLN(R2)                                                  
         B     BREC15                                                           
*                                                                               
BREC20   MVI   CLTUPDT,X'FF'       MARK THIS ENTRY AS UPDATED                   
*                                                                               
BREC21   OC    CLTCSTCD,CLTCSTCD   END OF TABLE                                 
         BZ    BREC21A                                                          
         CP    CLTPCAMT,=P'0'      IF 0 THIS EL NOT TO BE ADDED                 
         BE    BREC21A                                                          
         CLC   LOGWSAL(6),=C'DELETE'                                            
         BE    BREC21A                                                          
         BAS   RE,ADD1DS           ADD NEW BUDGET X'1D' ELEMENTS                
*                                                                               
BREC21A  CLI   DMACT,C'A'          AM I ADDING                                  
         BNE   BREC25                                                           
         BAS   RE,CHKFOR1D         ANY BDGT AMOUNT DATA                         
         BNE   BREC25C             NO                                           
         BAS   RE,MYADD                                                         
         B     BREC25C                                                          
*                                                                               
BREC25   NI    ACSTATUS,X'7F'                                                   
         BAS   RE,CHKFOR1D         ANY BDGT AMOUNT DATA                         
         BE    BREC25A             YES                                          
         OI    ACSTATUS,X'80'      NO BDG DATA                                  
BREC25A  BAS   RE,MYWRITE          WRITE NEW RECORD                             
*                                                                               
BREC25B  CLI   PROCSW,0            PROCESSED 1C-1R BDG REC PARTNER              
         BE    BREC30              NO                                           
*                                                                               
BREC25C  MVC   ACBTKTYP(42),SAVEKEY                                             
         OI    DMINBTS,X'08'       PASS DELETED                                 
         BAS   RE,MYHIGH           RESTORE SEQUENCE OF READS                    
         NI    DMINBTS,X'7F'                                                    
         LA    R2,CLTPCTTB         RESTOR A(COST CODE TABLE)                    
         B     BREC05              LOOP TO READ FOR NEXT BDG RECORD             
*                                                                               
BREC30   MVI   PROCSW,1                                                         
*        REVERSE ACT-CONTRA READ FOR 1C-1R BUDG REC PARTNER                     
         MVC   ACBTKACC,SAVEKEY+18    1C CONTRA TO ACT                          
         MVC   ACBTKCON,SAVEKEY+1     1R ACT TO CONTRA                          
         MVC   WORK(42),ACBTKTYP                                                
         OI    DMINBTS,X'88'                                                    
         BAS   RE,MYREAD                                                        
         NI    DMINBTS,X'FF'-X'88'                                              
         CLI   DMCB+8,X'10'                                                     
         BE    BREC30A             RECORD NOT FOUND                             
         NI    ACSTATUS,X'7F'      CAN'T BE DELETED                             
         B     BREC12                                                           
*                                                                               
BREC30A  MVC   ACBTKTYP(42),WORK   RESTORE KEY                                  
         XC    ACRECORD(2),ACRECORD                                             
         MVI   ACLENGTH+1,49                                                    
         MVI   DMACT,C'A'                                                       
         B     BREC20                                                           
*                                                                               
BRECNEW  LA    R2,CLTCSTCD         SCAN TABLE FOR RECS TO ADD                   
BREC31   OC    CLTCSTCD,CLTCSTCD   ANYMORE                                      
         BZ    XIT                                                              
         CLI   CLTUPDT,X'FF'       PREVIOUSLY WRITTEN OUT                       
         BNE   BREC35              YES, TRY NEXT                                
BREC33   LA    R2,CLTPCTLN(R2)                                                  
         B     BREC31                                                           
BREC35   MVI   PROCSW,0                                                         
         MVI   DMACT,0                                                          
*                                                                               
*              CONSTRUCT NEW 1R/1C BDG REC KEY                                  
         XC    ACBTKEY(50),ACBTKEY                                              
         MVC   ACBTKEY(33),SPACES                                               
         MVI   ACBTKTYP,X'1B'                                                   
         MVC   ACBTKACC,IO2        1R, OFF/DEPT, SUB-DEPT, EMPLOYEE             
         MVC   ACBTKCON,CLTCSTCD   1C CONTRA                                    
         MVC   ACBTKBNO(2),BUDNUM     HARD CODE BUDGET NUMBER                   
BREC40   MVI   ACLENGTH+1,49                                                    
*                                                                               
BREC40A  CP    CLTPCAMT,=P'0'      NOT TO BE ADDED                              
         BE    BREC40B                                                          
         CLC   LOGWSAL(6),=C'DELETE'                                            
         BE    BREC40B                                                          
         BAS   RE,ADD1DS           ADD 1D BUDGET ELEMENTS                       
*                                                                               
BREC40B  CLI   DMACT,C'W'          AM I WRITING                                 
         BE    BREC41              YES                                          
         BAS   RE,CHKFOR1D         ANY BDGT AMOUNT DATA                         
         BNE   BREC43              NO                                           
         BAS   RE,MYADD                                                         
         B     BREC43                                                           
*                                                                               
BREC41   BAS   RE,CHKFOR1D         ANY BDGT AMOUNT DATA                         
         BE    BREC42              YES                                          
         OI    ACSTATUS,X'80'      NO BDG DATA                                  
BREC42   BAS   RE,MYWRITE          WRITE NEW RECORD                             
*                                                                               
BREC43   CLI   PROCSW,0            NEED TO ADD 1C/1R PARTNER                    
         BNE   BREC33                                                           
         MVI   PROCSW,1                                                         
         MVC   ACBTKACC,CLTCSTCD   1C ACCOUNT                                   
         MVC   ACBTKCON,IO2        1R CONTRA                                    
         MVC   WORK(42),ACBTKTYP                                                
         BAS   RE,MYREAD                                                        
         CLI   DMCB+8,0            IF FOUND-WRITE IT                            
         BNE   BREC45                                                           
         BAS   RE,RIDEL            MARK ELEMENTS TO BE DELETED                  
         GOTO1 DELEL,DMCB,(X'FF',MYIO),0 DELETE MARKED ELEMENTS                 
         MVI   DMACT,C'W'                                                       
         B     BREC40A                                                          
BREC45   XC    ACBTKEY(50),ACBTKEY                                              
         MVC   ACBTKTYP(42),WORK                                                
         B     BREC40                                                           
         EJECT                                                                  
*                                                                               
*        SCAN REC FOR 1D ELEMENTS PRIOR TO MARKING FILE                         
*        IF NONE FOUND RETURN WITH NONZERO CC                                   
CHKFOR1D NTR1                                                                   
         LA    R4,MYIO             A(RECORD)                                    
         MVI   ELCODE,X'1D'        ELEMENT CODE                                 
         BAS   RE,GETEL                                                         
         B     XIT                 NONE FOUND                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        CALCULATE BUDGET AMOUNTS. DIVIDE WORK DAYS FOR EACH MONTH BY           
*        THE TOTAL WORK DAYS REPRESENTED BY KEY DATE INPUT. MULTIPLY            
*        PERCENTAGE ANSWER BY CLIENT-PRODUCT AMOUNT. BUILD REMAINDER            
*        OF BUDGET AMOUNT ELEMENT, ADD TO CURRENT RECORD.                       
*---------------------------------------------------------------------*         
ADD1DS   NTR1                                                                   
         ZAP   WORK+10(6),=P'0'    INIT ACCUM FOR BDG AMOUNTS                   
         XC    MY1DEL,MY1DEL       CLEAR ELEMENT BUILD AREA                     
         LA    R3,DATAB            YR/MONTH DATES                               
         ZIC   R0,MNTHCNT          COUNT OF YEAR MONTHS                         
         LA    R6,MY1DEL                                                        
         USING ACBAD,R6            COVERS BUDGET AMOUNT ELEMENTS                
*                                                                               
ADD03    LA    R4,PROGPROF         MONTHLY WORKING DAYS                         
**T      ZIC   R5,DATAB+1          1ST MONTH                                    
         ZIC   R5,1(R3)                                                         
         CH    R5,=H'10'           ADJUST FOR OCT, NOV, DEC                     
         BL    ADD05                                                            
         SH    R5,=H'6'                                                         
ADD05    BCTR  R5,0                                                             
         AR    R4,R5               POINT TO WORKING DAYS OF 1ST MONTH           
*                                                                               
ADD07    ZIC   R1,0(R4)            WORKING DAYS FOR CURRENT MONTH               
         LTR   R1,R1               PREVENT 0 BUDGET ELEMENTS                    
         BZ    ADD30                                                            
*                                                                               
*        MULTIPLY WORK DAYS FOR MONTH BY CLT-PRD AMOUNT                         
         CVD   R1,DUB              MONTHLY WORK DAYS TO PACKED FORMAT           
         ZAP   WORK(10),CLTPCAMT   MOVE CLT-PRD AMT TO MULTIPLICAN              
         MP    WORK(10),DUB+6(2)   WORK DAYS * AMOUNT                           
         MP    WORK(10),=P'10000'  INCREASE BY 4 DECIMALS                       
*                                                                               
*        DIVIDE ANSWER IN WORK BY TOTAL WORK DAYS                               
         L     R1,TOTDAYS          CONVERT TOT WORK DAYS TO PACKED              
         CVD   R1,DUB                                                           
         DP    WORK(10),DUB+6(2)                                                
         SRP   WORK(8),64-6,5                                                   
*                                                                               
ADD10    MVC   ACBAEL(2),=X'1D0C'  ELEMENT CODE/LENGTH                          
         MVC   ACBAMNTH,0(R3)      YR/MNTH                                      
         ZAP   ACBABUDG,WORK+2(6)  AMOUNT                                       
*                                                                               
         AP    WORK+10(6),WORK+2(6)                                             
         CH    R0,=H'1'            IS THIS THE LAST PASS                        
         BH    ADD25               NO                                           
*                                                                               
         CLI   BUDSW,1                                                          
         BNE   ADD11                                                            
         CLI   MNTHCNT,X'0C'                                                    
         BE    ADD11A                                                           
         B     ADD25                                                            
ADD11    CLC   MYSTART,MYBUDST     DO ROUNDING ONLY IF WE BUDGET                
         BNE   ADD25               THE WHOLE SALARY PERIOD                      
         CLC   MYEND,MYBUDED                                                    
         BNE   ADD25                                                            
*                                                                               
*        ADJUST LAST EL AMOUNT DUE TO ROUNDING + OR -                           
ADD11A   ZAP   WORK(8),CLTPCAMT    GET RID OF 2 DEC IN CLT-PRD AMNT             
         DP    WORK(8),=P'100'                                                  
         ZAP   DUB(6),WORK(6)                                                   
         CP    WORK+10(6),DUB(6)   MAKE UP F/ RNDING + OR - ON LAST EL          
         BE    ADD25                                                            
         BNH   ADD12                                                            
         SP    DUB(6),WORK+10(6)   LIGHT                                        
         AP    ACBABUDG,DUB(6)     ADD THE DIFFERENCE                           
         CLI   BUDSW,1                                                          
         BNE   ADD25                                                            
         AP    ACBABUDG,=P'1'                                                   
         B     ADD25                                                            
*                                                                               
ADD12    SP    WORK+10(6),DUB(6)   TOO MUCH                                     
         SP    ACBABUDG,WORK+10(6) SUBTRACT                                     
         CLI   BUDSW,1                                                          
         BNE   ADD25                                                            
         AP    ACBABUDG,=P'1'                                                   
*                                                                               
ADD25    MP    ACBABUDG,=P'100'                                                 
         GOTO1 ADDEL,DMCB,MYIO,MY1DEL                                           
*                                                                               
ADD30    LA    R3,2(R3)            NEXT DATE                                    
**T      LA    R4,1(R4)            NEXT MONTHS WORK DAYS                        
**T      BCT   R0,ADD07                                                         
         BCT   R0,ADD03                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        ROUTINE TO OUTPUT ERROR MESSAGES                                       
*--------------------------------------------------------------------*          
INVERR   MVI   ERROR,INVALID                                                    
         B     XITERR                                                           
*                                                                               
INVDTE   MVI   ERROR,DATERR                                                     
         B     XITERR                                                           
*                                                                               
CALCERR  MVC   ERRBLK,SPACES                                                    
         LA    R6,ERRBLK                                                        
         ZIC   R4,0(R3)            R4=MSG LEN                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   ERRBLK(0),1(R3)                                                  
         AR    R6,R4                                                            
         DP    DUB,=P'100'                                                      
         EDIT  (P6,DUB),(7,(R6)),ALIGN=LEFT                                     
         AR    R4,R0               =TOTAL LENGTH OF MESSAGE                     
         STC   R4,MSG14L                                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   MSG14(0),ERRBLK                                                  
         LA    R3,MSG14L                                                        
*                                                                               
ERRMSG   DS    0H                  R3=A(MSG)                                    
         MVI   ERROR,X'FE'                                                      
         XR    R4,R4                                                            
         IC    R4,0(R3)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     XITERR                                                           
         MVC   LOGHEAD(0),1(R3)                                                 
XITERR   L     RD,SAVERD           EXIT VIA 1ST RD CHAIN                        
XIT      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              TABLE OF ERROR MESSAGES                                          
*--------------------------------------------------------------------*          
MSG2L    DC    AL1(L'MSG2)                                                      
MSG2     DC    C'REC DOESNT CONTAIN ALLOCATION DATA FOR INPUT PERIOD'           
MSG4L    DC    AL1(L'MSG4)                                                      
MSG4     DC    C'ERROR-100% TOTAL REQUIRED, INPUT = '                           
MSG5L    DC    AL1(L'MSG5)                                                      
MSG5     DC    C'BDGT/SALARY DATA EXISTS WITH OVERLAPPING DATES'                
MSG6L    DC    AL1(L'MSG6)                                                      
MSG6     DC    C'ERROR-WEEKLY SALARY * WEEKS = '                                
MSG7L    DC    AL1(L'MSG7)                                                      
MSG7     DC    C'ERROR-CLT BUDGET PERIOD MUST BE WITHIN SALARY PERIOD'          
MSG8L    DC    AL1(L'MSG8)                                                      
MSG8     DC    C'ERROR-AMOUNT IS TOO LARGE'                                     
MSG9L    DC    AL1(L'MSG9)                                                      
MSG9     DC    C'ERROR CURRENT SALARY PLUS INCREASE(S) = '                      
MSGAL    DC    AL1(L'MSGA)                                                      
MSGA     DC    C'LACKING DASH (-) BETWEEN CLIENT-PRODUCT.'                      
MSGBL    DC    AL1(L'MSGB)                                                      
MSGB     DC    C'CLIENT RECORD LACKS COST ACCOUNT CODE'                         
MSGCL    DC    AL1(L'MSGC)                                                      
MSGC     DC    C'DUPLICATE CLIENT-PRODUCT ENTRY'                                
MSGDL    DC    AL1(L'MSGD)                                                      
MSGD     DC    C'CLIENT-PRODUCT INPUT POINTS TO DUPLICATE COST CODE'            
MSGEL    DC    AL1(L'MSGE)                                                      
MSGE     DC    C'CLIENT-PRODUCT INPUT REQUIRED'                                 
MSGFL    DC    AL1(L'MSGF)                                                      
MSGF     DC    C'INVALID INPUT FOR ACTION NEW'                                  
MSG10L   DC    AL1(L'MSG10)                                                     
MSG10    DC    C'OPTION REQUIRES A PRIOR DISPLAY'                               
MSG11L   DC    AL1(L'MSG11)                                                     
MSG11    DC    C'DELETE AND DATE OPTIONS ARE MUTUALLY EXCLUSIVE'                
MSG12L   DC    AL1(L'MSG12)                                                     
MSG12    DC    C'DESIRED ELEMENT DOES NOT EXIST'                                
MSG13L   DC    AL1(L'MSG13)                                                     
MSG13    DC    C'PERSONNEL AND RELATED BUDGET DATA DELETED'                     
MSG14L   DC    AL1(L'MSG14)                                                     
MSG14    DC    CL54' '                                                          
MSG15L   DC    AL1(L'MSG15)                                                     
MSG15    DC    C'ERROR-AMOUNTS INPUT WITH PENNIES ARE INVALID'                  
MSG16L   DC    AL1(L'MSG16)                                                     
MSG16    DC    C'ERROR-KEY DATE RANGE AND INCREASE INPUT CONTRADICT'            
MSG17L   DC    AL1(L'MSG17)                                                     
MSG17    DC    C'ERROR-CLT BUD END DATE INVALID WITHOUT BUD START DATE'         
MSG18L   DC    AL1(L'MSG18)                                                     
MSG18    DC    C'ERROR-CLT BUDGET PERIOD END DATE MISSING'                      
MSG19L   DC    AL1(L'MSG19)                                                     
MSG19    DC    C'ERROR-DATE RANGE MORE THAN 12 MONTHS'                          
MSG20L   DC    AL1(L'MSG20)                                                     
MSG20    DC    C'ERROR-NO CLT BUDGET INFO FOUND FOR THIS PERIOD'                
MSG21L   DC    AL1(L'MSG21)                                                     
MSG21    DC    C'NEW RECORD ADDED -- ENTER NEXT'                                
MSG22L   DC    AL1(L'MSG22)                                                     
MSG22    DC    C'ERROR OVERLAPPING DATES - XXX/XX TO XXX/XX'                    
         DS    0H                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
AC27WRKD DSECT                                                                  
ADDAY    DS    V                   EXSTERNAL RTN ADDRESSES                      
PERVERT  DS    V                                                                
HELLO    DS    V                                                                
PRELOC   DS    F                                                                
QWKS     DS    PL3                                                              
QWK1     DS    PL3                 # OF WKS REPED BY QTR1 FLD                   
QWK2     DS    PL3                 # OF WKS REPED BY QTR2 FLD                   
QTR1     DS    C                   QTR 1 IPT                                    
QTR2     DS    C                   QTR 2 IPT                                    
HOLDINC1 DS    PL6                 GENERAL PACKED WORK FIELDS                   
HOLDINC2 DS    PL6                                                              
SJCLTLEN DS    C                   SJ-CLT KEY LENGTH                            
PCTTOT   DS    PL6                 ACCUM PCT FOR EDIT OF 100.00 MAX             
MY1DEL   DS    CL12                BUILD AREA FOR BUDG AMNT ELS                 
DMACT    DS    C                   WRITE OR ADD WITH DATAMGR                    
PROCSW   DS    C                   INDICATE FILE ADD/WRITE PENDING              
BYTE     DS    C                   GENERAL WORK FIELD                           
CLTIPT   DS    CL3                 SJ CLIENT                                    
PRDIPT   DS    CL3                 SJ PRODUCT                                   
CLTSV    DS    CL3                 SAVED CLIENT                                 
AREC     DS    F                   A(REC TO DISPLAY)                            
SAVERD   DS    F                   EXIT SUB RTNS VIA 1ST RD                     
HOLDPCT  DS    PL3                 PCT SAVE FLD                                 
SAVEKEY  DS    CL49                KEY SAVE FOR BDG FILE ANALYSIS               
ELLEN    DS    F                   ELEMENT LEN SAVE                             
DATESW   DS    C                   S = START  E= END                            
TOTPCT   DS    PL4                                                              
NONCLI   DS    CL7                                                              
*                                                                               
ELIST    DS    3F                  FOR ADDEL-DELEL                              
ELERR    DS    CL1                                                              
         ORG   ELERR                                                            
ELADDR   DS    F                   A(ELEMENT FROM HELLO)                        
         DS    2F                                                               
PROFKEY  DS    CL14                PROFILE KEY                                  
PROFPARA DS    3F                  P'S FOR GETPROF CALL                         
*                                                                               
ERRBLK   DS    CL60                ERROR MSG WORK AREA                          
*                                                                               
CLTPCTTB DS    33CL(CLTPCTLN)      ROOM FOT 32 CODE-PCT PAIRS PLUS              
CLTTBLLN EQU   *-CLTPCTTB          EXTRA MEMBER OF ZEROS                        
MYIO     DS    2000C               SJ-CLT IO AREA                               
MYIOLN   EQU   *-MYIO                                                           
AC27WKLN EQU   *                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
CLTPCTD  DSECT                     COVERS CLTPCTTB CONTENTS                     
CLTCSTCD DS    CL15                COST CODE                                    
CLTPCAMT DS    PL6                 PERCENT                                      
CLTSJCD  DS    CL6                 CLIENT/PRODUCT                               
CLTUPDT  DS    C                   INDICATE ENTRY HAS UPDATED FILE              
CLTPCTLN EQU   *-CLTPCTD                                                        
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACLFMFFD                                                       
         EJECT                                                                  
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMDCD                                                       
SAVLEDG  DS    CL100                                                            
ELCODE   DS    CL1                                                              
SUBDISP  DS    CL1                                                              
EMPDISP  DS    CL1                                                              
MYSTART  DS    CL3                 START DATE OF SALARY                         
MYEND    DS    CL3                 END DATE OF SALARY                           
MYBUDST  DS    CL3                                                              
MYBUDED  DS    CL3                                                              
BACK5YR  DS    X                   YEAR-5 (PACKED)                              
BUDNUM   DS    CL2                 BUDGET NUMBER                                
DATAB    DS    CL24                TABLE OF MONTHS-MYSTART TO MYEND             
ENTAB    DS    C                   12 MO PROC-ENTAB=X'FF'(END OF TAB)           
MNTHCNT  DS    C                   COUNT OF MONTHS                              
SVSTRT   DS    CL2                 SAVED COMPRESSED START DATE                  
SVEND    DS    CL2                 SAVED COMPRESSED END DATE                    
PROGPROF DS    CL16                RTRNED WRK DAYS FOR 12 MONTHS                
TOTDAYS  DS    F                   TOTAL WORK DAYS FORM PROGPROF                
LASTFLD  DS    F                   ADDR OF LAST FIELD ON SCREEN                 
SV52EL   DS    F                   ADDR OF 52 SAL ELEMENT USING                 
DISPTOEL DS    C                   DISPLACEMENT TO LAST EL DISPED               
SVWSAL   DS    PL6                 WEEKLY SALARY FOR REDIS IN DLT MODE          
WEEKS    DS    PL2                 NUMBER OF WEEKS                              
BUDSW    DS    CL1                                                              
ENDVAL   DS    CL3                                                              
WRKWKLY  DS    PL8                                                              
SVYSAL   DS    PL8                 WEEKLY SALARY FOR REDIS IN DLT MODE          
TSTDATE  DS    CL6                                                              
TSTDATP  DS    PL3                                                              
TSTDATV  DS    CL8                                                              
*                                                                               
*                                                                               
*ACLFMEQU                                                                       
*DDFLDIND                                                                       
*FATWA                                                                          
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*DDCOMFACS                                                                      
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATWA                                                          
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACLFM27   05/01/02'                                      
         END                                                                    
