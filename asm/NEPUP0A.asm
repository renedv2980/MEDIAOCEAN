*          DATA SET NEPUP0A    AT LEVEL 005 AS OF 05/01/02                      
*          DATA SET NEPUP01    AT LEVEL 034 AS OF 03/07/97                      
*PHASE T3220AA,*                                                                
         TITLE 'T3220A - PLAN MAINTENANCE'                                      
T3220A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T3220A**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    VREC                                                             
         CLI   MODE,DISPKEY                                                     
         BE    DKEY                                                             
         CLI   MODE,DISPREC                                                     
         BE    DREC                                                             
         CLI   MODE,XRECDEL                                                     
         BE    AFTERDEL                                                         
         CLI   MODE,XRECREST                                                    
         BE    AFTERDEL                                                         
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     LA    R2,PUPCLIH          CLIENT                                       
         GOTO1 VVALCLT                                                          
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         SPACE 1                                                                
         MVC   PLANCODE,WORK                                                    
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 ANY                                                              
         MVC   PLANCODE,WORK                                                    
*        CLI   ACTNUM,ACTREST      IF ACTION IS RESTORE                         
*        BE    VKEY50                                                           
*        GOTO1 VVALPLAN                                                         
VKEY50   LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPLKEY,R4                                                        
         MVI   NPLKTYPE,X'20'      FILL PLAN KEY                                
         MVC   NPLKAM,BINAGYMD                                                  
         MVC   NPLKCLT,CLTCOMP                                                  
         MVC   NPLKNET,NETWORK                                                  
         MVC   NPLKDPT,DPTCODE                                                  
         MVC   NPLKPLAN,PLANCODE                                                
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     L     R4,AIO                                                           
         USING NPLRECD,R4                                                       
         MVI   NPLNEL,X'01'        PLAN ELEMENT                                 
         MVI   NPLNLEN,72                                                       
         SPACE 1                                                                
         LA    R2,PUPYEARH         PLAN YEAR                                    
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK)                                      
         L     R1,BLOCK+4                                                       
         LTR   R1,R1                                                            
         BZ    BADYEAR                                                          
         CH    R1,=H'1900'                                                      
         BL    *+8                                                              
         SH    R1,=H'1900'                                                      
         STC   R1,NPLNYEAR                                                      
         SPACE 1                                                                
         MVC   NPLNNAME,SPACES     PLAN NAME (OPTIONAL)                         
         LA    R2,PUPNAMEH                                                      
         CLI   5(R2),0                                                          
         BE    VREC2                                                            
         GOTO1 ANY                                                              
         MVC   NPLNNAME,WORK                                                    
         SPACE 1                                                                
VREC2    XC    NPLNUNIV,NPLNUNIV   UNIVERSE CODE (OPTIONAL)                     
         LA    R2,PUPUNIVH                                                      
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK)                                      
         L     R1,BLOCK+4                                                       
         LTR   R1,R1                                                            
         BZ    BADUNIV                                                          
         CH    R1,=H'9999'                                                      
         BH    BADUNIV                                                          
         CVD   R1,DUB                                                           
         L     R1,DUB+4                                                         
         SRL   R1,4                                                             
         ST    R1,DUB                                                           
         MVC   NPLNUNIV,DUB+2                                                   
         SPACE 1                                                                
VREC4    GOTO1 VLUPUNIV            CHECK IF UNIVS NOW OK                        
         CLI   ERROR,X'FF'                                                      
         BE    BADUNIV                                                          
         SPACE 1                                                                
         LA    R2,PUPLENSH         UP TO 4 LENGTHS                              
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(4,BLOCK)                                      
         MVI   FIELDERR,1                                                       
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    BADLENS                                                          
         STC   R0,NPLNNLEN         (RECORD N'LENGTHS)                           
         LA    R3,BLOCK                                                         
         LA    R5,NPLNLENS                                                      
         XC    NPLNLENS,NPLNLENS                                                
         SPACE 1                                                                
VREC6    MVC   0(1,R5),7(R3)       MOVE OUT A LENGTH                            
         LA    R1,LENLIST          CHECK FOR A REASONABLE LENGTH                
         SPACE 1                                                                
VREC8    CLI   0(R1),X'FF'                                                      
         BE    BADLENS                                                          
         CLC   0(1,R1),0(R5)                                                    
         BE    VREC10                                                           
         LA    R1,1(R1)                                                         
         B     VREC8                                                            
         SPACE 1                                                                
LENLIST  DC    AL1(10,15,20,30,40,45,60,90,120)                                 
         DC    X'FF'                                                            
         SPACE 1                                                                
VREC10   LA    R3,32(R3)                                                        
         LA    R5,1(R5)                                                         
         AI    FIELDERR,1                                                       
         BCT   R0,VREC6                                                         
         EJECT                                                                  
*              VALIDATE IF IT IS A CABLE PLAN                                   
VCBL     CLC   CONACT(3),=CL3'ADD'                                              
         BNE   VPER                                                             
         LA    R2,PUPCPLNH                                                      
         NI    NPLNOPTS,X'7F'                                                   
         CLI   8(R2),C'N'                                                       
         BE    VPER                                                             
         OI    NPLNOPTS,X'80'                                                   
         CLI   8(R2),C'Y'                                                       
         BNE   BADYORN                                                          
         EJECT                                                                  
*              VALIDATE PERIOD                                                  
         SPACE 3                                                                
VPER     LA    R2,PUPPTYPH         PERIOD TYPE                                  
         MVC   NPLNPERT,8(R2)                                                   
         CLI   5(R2),0                                                          
         BE    VPER2                                                            
         CLI   NPLNPERT,C'Q'       TEST VALUES                                  
         BE    VPER1                                                            
         CLI   NPLNPERT,C'M'       TEST VALUES                                  
         BE    VPER1                                                            
         CLI   NPLNPERT,C'W'                                                    
         BNE   BADPTYP2                                                         
         SPACE 1                                                                
VPER1    CLI   5(R2),1             SECOND CHARACTER IS PERIOD FLAVOR            
         BE    VPER2                                                            
         MVC   NPLNPRFL,9(R2)                                                   
         CLI   9(R2),C'B'          THIS CAN BE BROADCAST                        
         BE    VPER2                                                            
         CLI   9(R2),C'C'          OR CALENDAR                                  
         BE    VPER2                                                            
         B     BADPTYP2                                                         
         SPACE 1                                                                
VPER2    CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   VPEREND                                                          
         CLC   LASTPER,8(R2)       CAN'T CHANGE THIS FIELD                      
         BE    VPEREND             IF PROGRAMS HAVE BEEN ADDED                  
         MVC   PLANKEY,KEY                                                      
         MVC   AIO,AIO2                                                         
         MVI   KEY,X'22'                                                        
         LA    R4,KEY                                                           
         USING NPURECD,R4                                                       
         LA    R5,PLANKEY                                                       
         USING NPLRECD,R5                                                       
         MVC   NPUKNET,NPLKNET     BUILD PROGRAM KEY                            
         MVC   NPUKDPT,NPLKDPT                                                  
         MVC   NPUKPLAN,NPLKPLAN                                                
         XC    NPUKPROG,NPUKPROG                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     CHECK MATCH ON PLAN                          
         BE    BADPTYP4                                                         
         MVC   AIO,AIO1                                                         
         MVC   KEY,PLANKEY                                                      
         GOTO1 HIGH                                                             
         L     R4,AIO                                                           
         USING NPLRECD,R4                                                       
         DROP  R5                                                               
         SPACE 1                                                                
VPEREND  DS    0H                                                               
         EJECT                                                                  
*              VALIDATE HUT FIELDS                                              
         SPACE 3                                                                
VHUT2    LA    R2,PUPHTYRH         HUT YEAR(,BACK)                              
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK)                                      
         MVI   FIELDERR,1                                                       
         ZIC   R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BZ    BADHTYR                                                          
         L     R1,BLOCK+4                                                       
         CH    R1,=H'80'                                                        
         BL    BADHTYR                                                          
         CH    R1,=H'1900'                                                      
         BL    *+8                                                              
         SH    R1,=H'1900'                                                      
         STC   R1,NPLNHTYR                                                      
         MVI   NPLNHTNO,1          PRESET N'YEARS TO 1                          
         CH    R0,=H'1'                                                         
         BE    VHUT4                                                            
         MVI   FIELDERR,2                                                       
         L     R1,BLOCK+32+4                                                    
         LTR   R1,R1                                                            
         BZ    BADHTYR                                                          
         CH    R1,=H'5'            MAX 5 YEARS                                  
         BH    BADHTYR                                                          
         STC   R1,NPLNHTNO                                                      
         SPACE 1                                                                
VHUT4    LA    R2,PUPHTSCH         OPTIONAL AGENCY HUT SCHEME                   
         MVI   NPLNHTSC,0                                                       
         CLI   5(R2),0                                                          
         BE    VHUT6                                                            
         MVC   NPLNHTSC,8(R2)                                                   
         SPACE 1                                                                
VHUT6    LA    R2,PUPHTAVH         HUT AVERAGEING                               
         MVC   NPLNHAVE,N0PROF     PRESET FROM N0 PROFILE                       
         CLI   NPLNHAVE,0                                                       
         BNE   *+8                                                              
         MVI   NPLNHAVE,C'Q'       OR SET TO QUARTERLY                          
         CLI   5(R2),0                                                          
         BE    VHUT8                                                            
         MVC   NPLNHAVE,8(R2)                                                   
         CLI   8(R2),C'Q'                                                       
         BE    VHUT7                                                            
         CLI   8(R2),C'M'          CAN BE MONTHLY                               
         BE    VHUT7                                                            
         CLI   8(R2),C'W'          OR WEEKLY                                    
         BE    VHUT8                                                            
         B     BADHTAV                                                          
         SPACE 1                                                                
VHUT7    CLI   5(R2),1             SECOND CHARACTER IS HUT FLAVOR               
         BE    VHUT8                                                            
         MVC   NPLNHTFL,9(R2)                                                   
         CLI   9(R2),C'N'          THIS CAN BE NTI                              
         BE    VHUT8                                                            
         CLI   9(R2),C'B'          OR BROADCAST                                 
         BE    VHUT8                                                            
         CLI   9(R2),C'C'          OR CALENDAR                                  
         BE    VHUT8                                                            
         B     BADHTFL                                                          
         SPACE 1                                                                
VHUT8    LA    R2,PUPHTYPH         HUT BOOK TYPE                                
         MVI   NPLNHTBT,C'A'       DEFAULT IS ASCRIBED                          
         CLI   5(R2),0                                                          
         BE    VHUT10                                                           
         CLI   8(R2),C'D'                                                       
         BE    VHUT10                                                           
         MVC   NPLNHTBT,8(R2)                                                   
         CLI   8(R2),C'I'          INTEGRATED IS ALLOWED                        
         BE    VHUT10                                                           
         CLI   8(R2),C'A'          AND SO IS ASCRIBED                           
         BE    VHUT10                                                           
         B     BADHTYP                                                          
         SPACE 1                                                                
VHUT10   LA    R2,PUPHTOVH         HUT PERCENTAGE OVERRIDE                      
         CLI   5(R2),0                                                          
         BE    VCOMB                                                            
         ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R0)                                          
         CLI   DMCB,X'FF'                                                       
         BE    BADHTOV                                                          
         MVC   NPLNHTPO,DMCB+6                                                  
         EJECT                                                                  
*              CHECK PERIOD AND HUT COMBOS                                      
         SPACE 3                                                                
VCOMB    LA    R2,PUPPTYPH                                                      
         CLI   NPLNPERT,0          IF PERIOD IS MISSING                         
         BNE   VCOMB2                                                           
         MVC   NPLNPERT,NPLNHAVE   COPY FROM HUT AVERAGE                        
         B     VCOMBEND                                                         
         SPACE 1                                                                
VCOMB2   CLI   NPLNHAVE,C'Q'       ALL PERIODS CAN USE QUARTERLY                
         BE    VCOMBEND                                                         
         CLI   NPLNPERT,C'Q'                                                    
         BE    BADPTYP3                                                         
         CLI   NPLNHAVE,C'M'       MONTHS/WEEKS CAN USE MONTHLY                 
         BE    VCOMBEND                                                         
         CLI   NPLNPERT,C'M'                                                    
         BE    BADPTYP3                                                         
         SPACE 1                                                                
VCOMBEND DS    0H                                                               
         EJECT                                                                  
*              VALIDATE DEMO FIELDS                                             
         SPACE 3                                                                
VDEM2    LA    R2,PUPGCPMH                                                      
         XC    NPLNGCPM,NPLNGCPM   OPTIONAL GUARANTEED CPM                      
         CLI   5(R2),0                                                          
         BE    VDEM3                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   DMCB,X'FF'                                                       
         BE    BADGCPM                                                          
         MVC   NPLNGCPM,DMCB+4                                                  
         SPACE 1                                                                
VDEM3    LA    R2,PUPADJAH                                                      
         XC    NPLNADJP,NPLNADJP   OPTIONAL ADJUSTMENT FACTOR                   
         CLI   5(R2),0                                                          
         BE    VDEM3B                                                           
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,8(R2)),(R3)                                      
         CLI   DMCB,X'FF'                                                       
         BE    BADADJ                                                           
         MVC   NPLNADJP,DMCB+4                                                  
         SPACE 1                                                                
VDEM3B   LA    R2,PUPADJDH                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        REMOVE NEW GENERAL ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   VDEM3BB                                                          
         USING NPNELEM,R6                                                       
         MVC   FULL,NPNDEMO        SAVE DEMO GUAR DEMO CATEGORY                 
         DROP  R6                                                               
VDEM3BB  GOTO1 REMELEM                                                          
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(2),=X'0514'                                              
         CLI   5(R2),0                                                          
         BE    VDEM3C                                                           
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(4,8(R2)),(R3)                                      
         CLI   DMCB,X'FF'                                                       
         BE    BADADJ                                                           
         MVC   ELEMENT+2(4),DMCB+4                                              
         LA    R1,ELEMENT           SET SAVED DEMO CATEGORY                     
         USING NPNELEM,R1                                                       
         MVC   NPNDEMO,FULL                                                     
         DROP  R1                                                               
*                                                                               
VDEM3C   LA    R2,PUPDCPMH                                                      
         CLI   5(R2),0                                                          
         BE    VDEM3D                                                           
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   DMCB,X'FF'                                                       
         BE    BADGCPM                                                          
         MVC   ELEMENT+6(4),DMCB+4                                              
*                                                                               
VDEM3D   OC    ELEMENT+2(8),ELEMENT+2    ANY INPUT ON ELEMENT                   
         BZ    VDEM4                     NO DONT BOTHER TO ADD                  
         LA    R6,ELEMENT                                                       
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
VDEM4    LA    R2,PUPDEMOH                                                      
         GOTO1 ANY                                                              
         MVI   MAX,6                                                            
         GOTO1 VVALDEM                                                          
         MVC   NPLNNDEM,NDEMOS                                                  
         MVC   NPLNDEMS,DEMOS                                                   
         EJECT                                                                  
*              VALIDATE FILTERS AND TIME ZONE                                   
         SPACE 3                                                                
VFILT    LA    R2,PUPFILTH         PLAN FILTERS                                 
         XC    NPLNFILT,NPLNFILT                                                
         CLI   5(R2),0                                                          
         BE    VZONE                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NPLNFILT(0),8(R2)                                                
         SPACE 1                                                                
VZONE    LA    R2,PUPZONEH         TIME ZONE                                    
         MVI   NPLNZONE,0                                                       
         CLI   5(R2),0                                                          
         BE    VBUDG2                                                           
         MVC   NPLNZONE,8(R2)                                                   
         CLI   NPLNZONE,C'C'       (MUST BE C, M OR PACIFIC)                    
         BE    VBUDG2                                                           
         CLI   NPLNZONE,C'M'                                                    
         BE    VBUDG2                                                           
         CLI   NPLNZONE,C'P'                                                    
         BE    VBUDG2                                                           
         B     BADZONE                                                          
         EJECT                                                                  
*              VALIDATE BUDGETS                                                 
         SPACE 3                                                                
VBUDG2   MVI   ELCODE,X'04'        REMOVE PREVIOUS                              
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT          SET UP PROTOTYPE ELEMENT                     
         XC    ELEMENT,ELEMENT                                                  
         USING NPBELD,R6                                                        
         MVI   NPBELEM,X'04'                                                    
         MVI   NPBLEN,20                                                        
         ZIC   R1,NPLNYEAR         (BACK UP TO 4TH Q LAST YEAR)                 
         BCTR  R1,0                                                             
         STC   R1,NPBPER                                                        
         MVI   NPBPER+1,4                                                       
         LA    R2,PUPBUD4H                                                      
         SR    R3,R3               (QUARTER # IN R3)                            
         LA    R0,4                (UP TO 4 PERIODS)                            
         SPACE 1                                                                
VBUDG4   BAS   RE,VBUDG6                                                        
         MVC   NPBPER(1),NPLNYEAR                                               
         LA    R3,1(R3)                                                         
         STC   R3,NPBPER+1                                                      
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         BCT   R0,VBUDG4                                                        
         B     VCOMM                                                            
         SPACE 1                                                                
VBUDG6   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),(NPLNLENS,BLOCK)                               
         MVI   FIELDERR,1                                                       
         CLI   DMCB+4,0                                                         
         BE    BADBUDG                                                          
         LA    R3,BLOCK                                                         
         ZIC   R0,DMCB+4                                                        
         LA    R5,NPLNLENS                                                      
         SPACE 1                                                                
VBUDG8   MVC   NPBSEC,0(R5)        MOVE IN SECOND LENGTH                        
         CLI   DMCB+4,1                                                         
         BNE   VBUDG10             IF ONLY ONE BUDGET INPUT                     
         CLI   NPLNNLEN,1          AND THERE ARE MORE SECOND LENGTHS            
         BE    VBUDG10                                                          
         MVI   NPBSEC,0            THEN BUDGET IS FOR ALL LENGTHS               
         SPACE 1                                                                
VBUDG10  MVC   NPBUDGET,4(R3)                                                   
         CLI   0(R3),0             ZERO LENGTH - BUDGET OMITTED                 
         BE    VBUDG12                                                          
         TM    2(R3),X'80'         CHECK FOR NUMERIC                            
         BNO   BADBUDG                                                          
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
VBUDG12  LA    R3,32(R3)                                                        
         LA    R5,1(R5)                                                         
         AI    FIELDERR,1                                                       
         BCT   R0,VBUDG8                                                        
         B     XIT                                                              
*                                                                               
* - GET COMMENT                                                                 
VCOMM    MVI   ELCODE,X'11'        REMOVE COMMENT ELEMENT                       
         GOTO1 REMELEM                                                          
         LA    R2,PUPCOMMH                                                      
         CLI   5(R2),0                                                          
         BE    VINT0                                                            
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         USING NPLCOM,R6                                                        
         MVI   NPLCMEL,X'11'                                                    
         ZIC   R1,5(R2)                                                         
         LA    R1,3(R1)                                                         
         STCM  R1,1,NPLCMLEN                                                    
         MVI   NPLCMLIN,X'01'                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NPLCMCOM(0),8(R2)                                                
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
* - GET INTEGRATION                                                             
VINT0    B     DREC                                                             
         MVI   ELCODE,X'08'        REMOVE PREVIOUS                              
         GOTO1 REMELEM                                                          
**       LA    R2,PUPINTGH                                                      
         CLI   5(R2),0                                                          
         BE    VINT20                                                           
         ZIC   R3,5(R2)                                                         
***      GOTO1 CASHVAL,DMCB,(0,PUPINTG),(R3)                                    
         L     R3,4(R1)                                                         
         LTR   R3,R3                                                            
         BZ    BADBUDG                                                          
         SR    R2,R2               XXX                                          
         D     R2,=F'100'          XXX                                          
         STCM  R3,15,4(R1)         XXX TEMPORARY                                
         LA    R6,ELEMENT          SET UP PROTOTYPE ELEMENT                     
         XC    ELEMENT,ELEMENT                                                  
         USING NPLNEL2,R6                                                       
         MVI   NPL2ELEM,X'08'                                                   
         MVI   NPL2LEN,30                                                       
         MVC   NPL2INT,4(R1)                                                    
         GOTO1 ADDELEM                                                          
VINT20   B     DREC                                                             
         EJECT                                                                  
*              DISPLAY KEY                                                      
         SPACE 3                                                                
DKEY     L     R4,AIO                                                           
         MVC   GENCKEY,KEY                                                      
         USING NPLRECD,R4                                                       
         GOTO1 CLUNPK,DMCB,NPLKCLT,PUPCLI                                       
         LA    R2,PUPCLIH                                                       
         MVI   5(R2),2                                                          
         CLI   PUPCLI+2,X'41'                                                   
         BL    *+8                                                              
         MVI   5(R2),3                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 VVALCLT                                                          
         MVC   AIO,AIO1                                                         
         OI    PUPCLIH+6,X'80'                                                  
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
         MVC   PUPNET(4),NPLKNET                                                
         OI    PUPNETH+6,X'80'                                                  
         MVC   WORK(1),NPLKDPT                                                  
         GOTO1 VLUPDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         MVC   PUPPLAN,NPLKPLAN                                                 
         OI    PUPPLANH+6,X'80'                                                 
         MVC   KEY,GENCKEY                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
         SPACE 3                                                                
DREC     GOTO1 VEXTPLAN                                                         
         LA    R2,PUPYEAR          PLAN YEAR                                    
         ZIC   R3,PLANYEAR                                                      
         AH    R3,=H'1900'                                                      
         EDIT  (R3),(4,0(R2))                                                   
*        MVC   0(2,R2),=C'19'                                                   
*        EDIT  (1,PLANYEAR),(2,2(R2))                                           
         OI    PUPYEARH+6,X'80'                                                 
         SPACE 1                                                                
         MVC   PUPNAME,PLANNAME    PLAN NAME                                    
         OI    PUPNAMEH+6,X'80'                                                 
         MVC   PUPUNIV,SPACES      UNIVERSE CODE                                
         OI    PUPUNIVH+6,X'80'                                                 
         OC    PLANUNIV,PLANUNIV                                                
         BZ    DREC2                                                            
         XC    DUB,DUB                                                          
         MVC   DUB+6(2),PLANUNIV                                                
         L     R1,DUB+4                                                         
         SLL   R1,4                                                             
         ST    R1,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R2,DUB                                                           
         EDIT  (R2),(4,PUPUNIV),ALIGN=LEFT                                      
         SPACE 1                                                                
DREC2    LA    R2,PUPLENS          LENGTHS                                      
         XC    PUPLENS,PUPLENS                                                  
         OI    PUPLENSH+6,X'80'                                                 
         LA    R3,PLANLENS                                                      
         ZIC   R4,PLANNLEN                                                      
         SPACE 1                                                                
DREC4    EDIT  (1,0(R3)),(2,0(R2))                                              
         MVI   2(R2),C','                                                       
         LA    R2,3(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,DREC4                                                         
         BCTR  R2,0                                                             
         MVI   0(R2),C' '                                                       
         SPACE 1                                                                
         LA    R2,PUPHTYR          HUT YEAR                                     
         XC    PUPHTYR,PUPHTYR                                                  
         OI    PUPHTYRH+6,X'80'                                                 
         EDIT  (1,PLANHTYR),(2,0(R2))                                           
         CLI   PLANHTNO,2                                                       
         BL    DREC6                                                            
         MVI   2(R2),C','                                                       
         EDIT  (1,PLANHTNO),(1,3(R2))                                           
         SPACE 1                                                                
DREC6    XC    PUPHTSC,PUPHTSC                                                  
         MVC   PUPHTSC(1),PLANHTSC                                              
         OI    PUPHTSCH+6,X'80'                                                 
         XC    PUPHTAV,PUPHTAV                                                  
         MVC   PUPHTAV(1),PLANHTAV                                              
         MVC   PUPHTAV+1(1),PLANHTFL                                            
         OI    PUPHTAVH+6,X'80'                                                 
         MVC   PUPHTYP(1),PLANHTYP                                              
         OI    PUPHTYPH+6,X'80'                                                 
         EDIT  (2,PLANHTPO),(8,PUPHTOV),2,ALIGN=LEFT,ZERO=BLANK                 
         OI    PUPHTOVH+6,X'80'                                                 
         MVC   PUPPTYP,SPACES                                                   
         MVC   PUPPTYP(1),PLANPERT                                              
         MVC   PUPPTYP+1(1),PLANPRFL                                            
         OC    PUPPTYP,SPACES                                                   
         OI    PUPPTYPH+6,X'80'                                                 
         MVC   PUPCPLN(1),PLANPRCB                                              
         OI    PUPCPLNH+6,X'80'                                                 
         SPACE 1                                                                
DREC7    MVC   LASTPER,PUPPTYP     SAVE THIS TO CHECK ON CHANGE                 
         EDIT  (4,GUARCPM),(8,PUPGCPM),2,ALIGN=LEFT,ZERO=BLANK                  
         OI    PUPGCPMH+6,X'80'                                                 
         EDIT  (4,PLANADJP),(8,PUPADJA),4,ALIGN=LEFT,ZERO=BLANK                 
         OI    PUPADJAH+6,X'80'                                                 
         EDIT  (4,PLANDADJ),(8,PUPADJD),4,ALIGN=LEFT,ZERO=BLANK                 
         OI    PUPADJDH+6,X'80'                                                 
         XC    PUPDCPM,PUPDCPM                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   DREC7C                                                           
         USING NPNELD,R6                                                        
         EDIT  (4,NPNDCPM),(8,PUPDCPM),2,ALIGN=LEFT,ZERO=BLANK                  
DREC7C   OI    PUPDCPMH+6,X'80'                                                 
         MVC   PUPFILT,PLANFILT                                                 
         OI    PUPFILTH+6,X'80'                                                 
         MVC   PUPZONE,PLANZONE                                                 
         OI    PUPZONEH+6,X'80'                                                 
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY DEMOS                                                    
         SPACE 3                                                                
         LA    R2,PUPDEMO                                                       
         OI    PUPDEMOH+6,X'80'                                                 
         XC    PUPDEMO,PUPDEMO                                                  
         LA    R3,DEMOS                                                         
         ZIC   R4,NDEMOS                                                        
         LTR   R4,R4                                                            
         BZ    DREC12                                                           
         SPACE 1                                                                
DREC8    GOTO1 VSETDB                                                           
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         GOTO1 DEMOCON,DMCB,(0,0(R3)),(10,WORK),(C'S',DBLOCK)                   
         MVC   0(10,R2),WORK                                                    
         CH    R4,=H'1'                                                         
         BE    DREC11                                                           
         SPACE 1                                                                
DREC10   LA    R2,1(R2)                                                         
         CLI   0(R2),X'41'                                                      
         BH    DREC10                                                           
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         LA    R3,3(R3)                                                         
         BCT   R4,DREC8                                                         
*--MOVE OUT DEMO GUARANTEE                                                      
DREC11   XC    PUPGDEM,PUPGDEM                                                  
         OI    PUPGDEMH+6,X'80'                                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   DREC12                                                           
         USING NPNELD,R6                                                        
         GOTO1 VSETDB                                                           
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         GOTO1 DEMOCON,DMCB,(0,NPNDEMO),(10,PUPGDEM),(C'S',DBLOCK)              
         B     DREC12                                                           
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY BUDGETS                                                  
         SPACE 3                                                                
DREC12   MVI   LENGTH,0                                                         
         GOTO1 VEXTBUDG                                                         
         LA    R2,PUPBUD4H                                                      
         LA    R3,BUDGETS                                                       
         LA    R4,4                SET UP FOR 4 QUARTERS                        
         SPACE 1                                                                
DREC14   XC    8(30,R2),8(R2)                                                   
         BAS   RE,DREC16                                                        
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         LA    R3,20(R3)                                                        
         BCT   R4,DREC14                                                        
* - COMMENT                                                                     
         LA    R2,PUPCOMMH                                                      
         XC    PUPCOMM,PUPCOMM                                                  
         OI    6(R2),X'80'                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRECX                                                            
         USING NPLCOM,R6                                                        
         ZIC   R1,NPLCMLEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PUPCOMM(0),NPLCMCOM                                              
         OI    6(R2),X'80'                                                      
* - INTEGRATION                                                                 
         B     DRECX                                                            
**       LA    R2,PUPINTGH                                                      
**       XC    PUPINTG,PUPINTG                                                  
         OI    6(R2),X'80'                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRECX                                                            
         USING NPLNEL2,R6                                                       
**       EDIT  (B4,NPL2INT),(10,PUPINTG),ALIGN=LEFT                             
         OI    6(R2),X'80'                                                      
DRECX    B     XIT                                                              
         SPACE 1                                                                
DREC16   NTR1                      SUBROUTINE FOR 1 QUARTER                     
         LA    R2,8(R2)                                                         
         LA    R4,1                                                             
         OC    4(16,R3),4(R3)      CHECK IF BUDGETS ANALYZED BY LENGTH          
         BZ    DREC18                                                           
         LA    R3,4(R3)                                                         
         ZIC   R4,PLANNLEN                                                      
         SPACE 1                                                                
DREC18   EDIT  (4,0(R3)),(9,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                      
         AR    R2,R0                                                            
         CH    R4,=H'1'                                                         
         BE    XIT                                                              
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,DREC18                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE AFTER DELETE OR RESTORE OF PLAN                          
*              ROUTINE DELETES OR RESTORES PROGRAMS                             
*              AND CABLE PROGRAM RECORDS                                        
         SPACE 3                                                                
AFTERDEL L     R4,AIO              CREATE PROGRAM KEY FROM PLAN                 
         USING NPLRECD,R4                                                       
*                                                                               
*--FIRST DELETE CABLE PLAN                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING NPCKEY,R5                                                        
         MVI   NPCKTYPE,X'28'                                                   
         MVC   NPCKAM,NPLKAM                                                    
         MVC   NPCKCLT,NPLKCLT                                                  
         MVC   NPCKNET,NPLKNET                                                  
         MVC   NPCKDPT,NPLKDPT                                                  
         MVC   NPCKPLAN,NPLKPLAN                                                
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
*        SPACE 1                                                                
         CLC   KEY(20),KEYSAVE                                                  
         BE    AFTER20                                                          
         NI    DMINBTS,X'F7'                                                    
         MVC   AIO,AIO1            RESET AIO                                    
         B     AFTER60                                                          
         SPACE 1                                                                
AFTER20  XI    KEY+20,X'80'        REVERSE DELETE BIT                           
         GOTO1 WRITE                                                            
         MVC   AIO,AIO2            NOW READ FOR PROGRAM RECS                    
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         XI    22(R1),X'80'                                                     
         GOTO1 PUTREC                                                           
         DROP  R5                                                               
*                                                                               
*--DELETE PUP PROGRAM RECORDS                                                   
*                                                                               
AFTER60  XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING NPUKEY,R5                                                        
         MVI   NPUKTYPE,X'22'                                                   
         MVC   NPUKAM,NPLKAM                                                    
         MVC   NPUKCLT,NPLKCLT                                                  
         MVC   NPUKNET,NPLKNET                                                  
         MVC   NPUKDPT,NPLKDPT                                                  
         MVC   NPUKPLAN,NPLKPLAN                                                
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         B     AFTER80                                                          
         SPACE 1                                                                
AFTER70  GOTO1 SEQ                                                              
         SPACE 1                                                                
AFTER80  CLC   KEY(13),KEYSAVE                                                  
         BE    AFTER90                                                          
         NI    DMINBTS,X'F7'                                                    
         MVC   AIO,AIO1            RESET AIO                                    
         B     XIT                                                              
         SPACE 1                                                                
AFTER90  XI    KEY+20,X'80'        REVERSE DELETE BIT                           
         GOTO1 WRITE                                                            
         MVC   AIO,AIO2            NOW READ FOR PROGRAM RECS                    
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         XI    22(R1),X'80'                                                     
         GOTO1 PUTREC                                                           
         B     AFTER70                                                          
         EJECT                                                                  
*              ERROR EXITS                                                      
         SPACE 3                                                                
BADYEAR  MVC   CONHEAD(L'YEARERR),YEARERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADUNIV  MVC   CONHEAD(L'UNIVERR),UNIVERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADLENS  MVC   CONHEAD(L'LENSERR),LENSERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADHTYR  MVC   CONHEAD(L'HTYRERR),HTYRERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADHTAV  MVC   CONHEAD(L'HTAVERR),HTAVERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADHTFL  MVC   CONHEAD(L'HTFLERR),HTFLERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADYORN  MVC   CONHEAD(L'YORNERR),YORNERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADHTYP  MVC   CONHEAD(L'HTYPERR),HTYPERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADHTOV  MVC   CONHEAD(L'HTOVERR),HTOVERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADGCPM  MVC   CONHEAD(L'GCPMERR),GCPMERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADADJ   MVC   CONHEAD(L'ADJERR),ADJERR                                         
         B     MYEND                                                            
         SPACE 1                                                                
BADBUDG  MVC   CONHEAD(L'BUDGERR),BUDGERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADPTYP2 MVC   CONHEAD(L'PTYP2ERR),PTYP2ERR                                     
         B     MYEND                                                            
         SPACE 1                                                                
BADPTYP3 MVC   CONHEAD(L'PTYP3ERR),PTYP3ERR                                     
         B     MYEND                                                            
         SPACE 1                                                                
BADPTYP4 MVC   CONHEAD(L'PTYP4ERR),PTYP4ERR                                     
         B     MYEND                                                            
         SPACE 1                                                                
BADZONE  MVC   CONHEAD(L'ZONEERR),ZONEERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
YEARERR  DC    C'** ERROR ** NEED PLAN YEAR'                                    
UNIVERR  DC    C'** ERROR ** UNIVERSE INVALID'                                  
LENSERR  DC    C'** ERROR ** UP TO 4 LENGTHS'                                   
HTYRERR  DC    C'** ERROR ** HUT YEAR EXPRESSION INVALID'                       
HTAVERR  DC    C'** ERROR ** SHOULD BE Q(UARTER) M(ONTH) OR W(EEK)'             
HTFLERR  DC    C'SECOND CHARACTER S/B BE N(TI) C(ALENDAR) B(ROADCAST)'          
HTYPERR  DC    C'** ERROR ** SHOULD BE D(IARY) OR I(NTEGRATED)'                 
YORNERR  DC    C'** ERROR ** "Y" OR "N" ARE THE ONLY VALID INPUTS'              
HTOVERR  DC    C'** ERROR ** SHOULD BE NUMERIC UP TO 2 DEC PLACES'              
ZONEERR  DC    C'** ERROR ** SHOULD BE C(ENTRAL) M(OUNTAIN) P(ACIFIC)'          
GCPMERR  DC    C'** ERROR ** CPM VALUE NOT VALID'                               
ADJERR   DC    C'** ERROR ** ADJUSTMENT FACTOR/TYPE INVALID'                    
BUDGERR  DC    C'** ERROR ** BUDGETS NOT VALID'                                 
PTYP2ERR DC    C'** ERROR ** INVALID PERIOD TYPE'                               
PTYP3ERR DC    C'** ERROR ** PERIOD INCONSISTENT WITH HUT PERIOD'               
PTYP4ERR DC    C'* ERROR * CANT CHANGE PERIODS AFTER PROGRAMS ADDED'            
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 3                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 1                                                                
MYCURSOR MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         GOTO1 VCURSERR            AND POSITIONING CURSOR                       
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
         SPACE 1                                                                
ERREND   GOTO1 VERRXIT                                                          
         SPACE 1                                                                
*  SET END OF SCREEN                                                            
XIT      LA    RE,PUPCOMMH                                                      
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         MVC   0(3,RE),=XL3'000101'                                             
         XIT1                                                                   
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPFAD                                                       
GENCKEY  DS    CL32                                                             
PLANKEY  DS    CL32                                                             
LASTPER  DS    CL1                 SAVE PERIOD                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEPUP0A   05/01/02'                                      
         END                                                                    
