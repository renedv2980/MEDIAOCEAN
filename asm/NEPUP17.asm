*          DATA SET NEPUP17    AT LEVEL 091 AS OF 05/01/02                      
*PHASE T32217A,*                                                                
         TITLE 'T32217 - PROGRAM WEEK LIST STEREO ONLY'                         
T32217   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32217**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FOR LIST                                            
         SPACE 3                                                                
         MVC   GENCKEY,KEY         SAVE GENCON'S KEY                            
         LA    R4,KEY                                                           
         USING NPUKEY,R4                                                        
         SPACE 1                                                                
         LA    R2,PUPCLIH          CLIENT (REQUIRED)                            
         GOTO1 VVALCLT                                                          
         SPACE 1                                                                
         LA    R2,PUPNETH          NETWORK                                      
         GOTO1 VVALNET                                                          
         SPACE 1                                                                
         LA    R2,PUPDPTH          DAYPART                                      
         GOTO1 VVALDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         SPACE 1                                                                
         LA    R2,PUPPLANH         PLAN                                         
         GOTO1 VVALPLAN                                                         
         SPACE 1                                                                
*                                                                               
*--DEMO FIELD                                                                   
         CLI   CONACT,C'V'         ONLY USED FOR VPH REQUEST                    
         BE    *+12                                                             
         CLI   CONACT,C'D'         ONLY USED FOR VPH REQUEST                    
         BNE   TIT60                                                            
         LA    R2,PUPDEMH          DEMO                                         
         CLI   5(R2),0             FIELD INPUTTED                               
         BNE   *+14                                                             
         MVC   ACTDEMO,TARGET     USE TARGET IF NO DEMO INPUTTED                
         B     TIT40                                                            
*                                                                               
         CLI   5(R2),1             WAS DEMO INPUTTED                            
         BH    TIT50               YES VALIDATE THE VALUE                       
*                                                                               
         XC    WORK,WORK                                                        
         CLI   8(R2),C'R'                                                       
         BNE   *+14                                                             
         MVC   WORK(5),=CL5'HOMES'                                              
         B     TIT45                                                            
*                                                                               
         CLI   8(R2),C'S'                                                       
         BNE   *+14                                                             
         MVC   WORK(5),=CL5'SHARE'                                              
         B     TIT45                                                            
*                                                                               
         CLI   8(R2),C'H'                                                       
         BNE   *+14                                                             
         MVC   WORK(5),=CL3'HUT'                                                
         B     TIT45                                                            
*                                                                               
         LA    RF,DEMOS                                                         
         CLI   8(R2),C'1'          IS FIELD NUMERIC                             
         BL    BADDNUM                                                          
         BE    TIT30                                                            
         CLI   8(R2),C'6'                                                       
         BH    BADDNUM                                                          
         NI    8(R2),X'0F'                                                      
         CLC   8(1,R2),NDEMOS      DOES PLAN RECORD HAVE THIS MANY DEMS         
         BH    BADDNUM             NO ERROR                                     
         ZIC   RE,8(R2)                                                         
         BCTR  RE,0                                                             
         MH    RE,=H'3'                                                         
         AR    RF,RE                                                            
TIT30    MVC   ACTDEMO,0(RF)                                                    
*                                                                               
TIT40    XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         GOTO1 DEMOCON,DMCB,(0,ACTDEMO),(10,WORK),(C'S',DBLOCK)                 
TIT45    MVC   8(10,R2),WORK                                                    
         OC    8(10,R2),SPACES                                                  
         MVI   5(R2),10                                                         
         OI    6(R2),X'80'                                                      
         B     TIT60                                                            
*                                                                               
TIT50    MVC   SVDEMOS,DEMOS                                                    
         MVI   MAX,1                                                            
         GOTO1 VVALDEM                                                          
         MVC   ACTDEMO,DEMOS                                                    
         MVC   DEMOS,SVDEMOS                                                    
         LA    RE,6                                                             
         LA    RF,DEMOS                                                         
TIT55    CLC   0(1,RF),ACTDEMO                                                  
         BNE   *+14                                                             
         CLC   2(1,RF),ACTDEMO+2                                                
         BE    TIT60                                                            
         LA    RF,3(RF)                                                         
         BCT   RE,TIT55                                                         
         B     BADDPLAN                                                         
*                                                                               
TIT60    CLI   PLANPERT,C'W'                                                    
         BE    *+16                                                             
         MVI   PUPQURT,X'40'                                                    
         OI    PUPQURTH+6,X'80'    QUARTER                                      
         B     TIT70                                                            
         ZIC   R1,PUPQURT                                                       
         SLL   R1,28                                                            
         SRL   R1,28                                                            
         STC   R1,PROGPERQ                                                      
         CLI   PROGPERQ,1                                                       
         BL    BADQUART                                                         
         CLI   PROGPERQ,4                                                       
         BH    BADQUART                                                         
         BNE   *+8                                                              
         MVI   PROGPERQ,0                                                       
*                                                                               
TIT70    OI    PUPTITLH+6,X'80'    QUARTER                                      
         MVC   PUPTITL,SPACES                                                   
*                                                                               
         CLI   CONACT,C'U'         UNIT LEVEL DISPLAY                           
         BNE   *+12                                                             
         BAS   RE,SETUHEAD                                                      
         B     TITEND                                                           
*                                                                               
         CLI   PLANPERT,C'Q'                                                    
         BNE   *+22                                                             
         MVC   PUPTITL,QTITLES     QUARTERLY                                    
         MVI   HEADCNT,4           NUMBER OF COLUMNS                            
         BAS   RE,QURTDATS         SET WEEK DATES                               
         B     TITEND                                                           
         CLI   PLANPERT,C'M'                                                    
         BNE   *+22                                                             
         MVC   PUPTITL,MTITLES     MONTHLY                                      
         MVI   HEADCNT,13          NUMBER OF COLUMNS                            
         BAS   RE,MNTHDATS         SET WEEK DATES                               
         B     TITEND                                                           
         SPACE 1                                                                
*                                                                               
         MVI   HEADCNT,16          NUMBER OF COLUMNS                            
         BAS   RE,WEEKDATS         SET WEEK DATES                               
         LA    R1,PUPTITL          WEEKLY                                       
         LA    RE,16                                                            
         LA    RF,OVWORK                                                        
TIT90    CLC   0(5,RF),=C'TOTAL'                                                
         BE    TITEND                                                           
         MVC   0(1,R1),0(RF)       BYPASS SECOND CHAR OF MONTH                  
         MVC   1(3,R1),2(RF)       QUARTERLY                                    
         LA    R1,4(R1)                                                         
         LA    RF,5(RF)                                                         
         BCT   RE,TIT90                                                         
*                                                                               
         SPACE 1                                                                
TITEND   MVI   LINECNT,0                                                        
*                                                                               
         MVC   KEY,GENCKEY         RESTORE PREVIOUS GENCON KEY                  
         CLC   LASTACT,CONACT      HAS ACTION CHANGED                           
         BNE   *+12                                                             
         CLI   KEY,X'22'           FIRST PASS NO                                
         BE    LIST2                                                            
         MVC   LASTACT,CONACT                                                   
         XC    KEY,KEY                                                          
         MVI   NPUKTYPE,X'22'      FILL PROGRAM KEY                             
         MVC   NPUKAM,BINAGYMD                                                  
         MVC   NPUKCLT,CLTCOMP                                                  
         MVC   NPUKNET,NETWORK                                                  
         MVC   NPUKDPT,DPTCODE                                                  
         MVC   NPUKPLAN,PLANCODE                                                
         MVC   SAVEKEY,NPUKTYPE                                                 
         EJECT                                                                  
*              HANDLE I/O FOR LIST                                              
         SPACE 3                                                                
LIST2    GOTO1 HIGH                                                             
         B     LIST6                                                            
         SPACE 1                                                                
LIST4    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST6    CLC   KEY(13),SAVEKEY     MUST HAVE MATCH ON PLAN                      
         BNE   LISTEX                                                           
         CLI   PLANPERT,C'W'       CHECK FOR WEEKLY                             
         BNE   *+14                                                             
         CLC   KEY+19(1),PROGPERQ  CHECK FOR PROPER QUARTER                     
         BNE   LIST4                                                            
         MVC   LISTAR,SPACES                                                    
         GOTO1 GETREC                                                           
         GOTO1 VEXTPROG                                                         
*                                                                               
         BAS   RE,CHKNLIST                                                      
         CLI   CONACT,C'U'         UNIT LEVEL DISPLAY                           
         BNE   LIST20                                                           
         BAS   RE,SETSPOTS         SET SPOTS IN OUTGRID TABLE                   
         BAS   RE,DISSPOTS         DISPLAY SPOTS ON SCREEN                      
         B     LIST4                                                            
*                                                                               
LIST20   CLI   CONACT,C'V'         UNIT LEVEL DISPLAY                           
         BNE   LIST30                                                           
         BAS   RE,SETVPHS          SET VPHS IN OUTGRID TABLE                    
         BAS   RE,DISVPHS          DISPLAY VPHS ON SCREEN                       
         B     LIST4                                                            
*                                                                               
LIST30   CLI   CONACT,C'H'         HUT DISPLAY                                  
         BNE   LIST40                                                           
         BAS   RE,SETHOMS          SET HUTS IN OUTGRID TABLE                    
         BAS   RE,DISHOMS          DISPLAY HUTS ON SCREEN                       
         B     LIST4                                                            
*                                                                               
LIST40   CLI   CONACT,C'R'         RATING DISPLAY                               
         BNE   LIST50                                                           
         BAS   RE,SETHOMS          SET RATINGS IN OUTGRID TABLE                 
         BAS   RE,DISHOMS          DISPLAY RATINGS ON SCREEN                    
         B     LIST4                                                            
*                                                                               
LIST50   CLI   CONACT,C'S'         SHARE DISPLAY                                
         BNE   LIST60                                                           
         BAS   RE,SETHOMS          SET SHARE IN OUTGRID TABLE                   
         BAS   RE,DISHOMS          DISPLAY SHARE ON SCREEN                      
         B     LIST4                                                            
*                                                                               
LIST60   CLI   CONACT,C'C'         COST DISPLAY                                 
         BNE   LISTEX                                                           
         BAS   RE,SETCOST          SET COST IN OUTGRID TABLE                    
         BAS   RE,DISCOST          DISPLAY COST ON SCREEN                       
         B     LIST4                                                            
*                                                                               
LISTEX   B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK IF WHOLE RECORD WILL                            
*              FIT ON SCREEN                                                    
         SPACE 3                                                                
CHKNLIST NTR1                                                                   
         MVI   NLISTS,14           MOVE DEFAULT                                 
         ZIC   RE,LINECNT          CURRENT NUMBER OF LINES OF DISPLAY           
         ZIC   RF,PLANNLEN         NEEDED LINES FOR THIS DISPLAY                
         AR    RE,RF                                                            
         CH    RE,=H'14'           WILL IT OVERFLOW                             
         BH    CHKN020             YES                                          
         STC   RE,LINECNT          UPDATE LINE COUNT                            
         B     CHKNEX                                                           
*                                                                               
CHKN020  MVC   NLISTS,LINECNT      FOR SCREEN BREAK                             
         MVI   LINECNT,0                                                        
CHKNEX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MOVES THE UNIT COUNT WEEKLY                           
*              TO THE RIGHT SPOT ON THE OUTGRID TABLE                           
         SPACE 3                                                                
SETSPOTS NTR1                                                                   
         L     RE,AIO                                                           
         USING NPURECD,RE                                                       
         USING NPUAD,R6                                                         
         LA    R6,NPGDEL                                                        
         ZIC   RF,NPGDLEN                                                       
         AR    R6,RF                                                            
*                                                                               
         XC    OUTGRID,OUTGRID                                                  
*                                                                               
         DROP  RE                                                               
*                                                                               
SETSP020 CLI   0(R6),X'02'                                                      
         BE    SETSP050                                                         
         CLI   0(R6),X'12'                                                      
         BE    SETSP050                                                         
         BH    SETSPEX                                                          
         CLI   0(R6),X'00'                                                      
         BE    SETSPEX                                                          
*                                                                               
SETSP050 LA    RE,CMONDAYS                                                      
         LA    RF,OUTGRID                                                       
         PRINT GEN                                                              
         ZIC   R1,HEADCNT          NUMBER OF COLUMNS                            
         PRINT NOGEN                                                            
*                                                                               
SETSP080 CLC   NPUAPER,0(RE)                                                    
         BE    SETSP120                                                         
         LA    RE,2(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,SETSP080                                                      
         B     SETSP150                                                         
*                                                                               
SETSP120 MVC   0(1,RF),NPUAUNS                                                  
         MVC   16(1,RF),NPUAUNS+1                                               
         MVC   32(1,RF),NPUAUNS+2                                               
         MVC   48(1,RF),NPUAUNS+3                                               
*                                                                               
SETSP150 ZIC   RF,NPUALEN                                                       
         AR    R6,RF                                                            
         B     SETSP020                                                         
*                                                                               
SETSPEX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT UNITS STORED IN                                
*              OUTGRID TO THE SCREEN                                            
         SPACE 3                                                                
DISSPOTS NTR1                                                                   
         CLI   PLANNLEN,0                                                       
         BE    DISSPEX                                                          
*                                                                               
         LA    R2,PLANLENS         LENGTHS                                      
         ZIC   R3,PLANNLEN         NUMBER OF LENGTHS                            
         LA    R4,OUTGRID                                                       
DISSP020 LA    R5,LISTAR+27                                                     
         LA    R6,16                                                            
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(6),PROGCODE                                               
         MVC   LISTAR+11(16),PROGNAME                                           
         EDIT  (1,(R2)),(3,LISTAR+7)     MOVE LENGTH OUT                        
         LA    R2,1(R2)                                                         
*                                                                               
DISSP080 EDIT  (1,0(R4)),(3,0(R5))                                              
         LA    R4,1(R4)                                                         
         LA    R5,3(R5)                                                         
         BCT   R6,DISSP080                                                      
         GOTO1 LISTMON                                                          
*                                                                               
         BCT   R3,DISSP020                                                      
*                                                                               
DISSPEX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MOVES THE VPH'S OR HOUSEHOLD RATING                   
*              TO THE RIGHT SPOT ON THE OUTGRID TABLE                           
         SPACE 3                                                                
SETVPHS  NTR1                                                                   
         L     RE,AIO                                                           
         USING NPURECD,RE                                                       
         USING NPUAD,R6                                                         
         LA    R6,NPGDEL                                                        
         ZIC   RF,NPGDLEN                                                       
         AR    R6,RF                                                            
*                                                                               
         XC    OUTGRID,OUTGRID                                                  
         MVC   GDDEMO,ACTDEMO                                                   
         CLI   CONACT,C'R'                                                      
         BNE   SETVP020                                                         
         MVC   GDDEMO(3),=XL3'00D901'    GET HOMES                              
*                                                                               
         DROP  RE                                                               
*                                                                               
SETVP020 CLI   0(R6),X'02'                                                      
         BE    SETVP050                                                         
         CLI   0(R6),X'12'                                                      
         BE    SETVP050                                                         
         BH    SETVPEX                                                          
         CLI   0(R6),X'00'                                                      
         BE    SETVPEX                                                          
*                                                                               
SETVP050 MVC   PERIOD,2(R6)                                                     
         GOTO1 VGETDEM                                                          
*                                                                               
         LA    RE,CMONDAYS                                                      
         LA    RF,OUTGRID                                                       
         PRINT GEN                                                              
         ZIC   R1,HEADCNT          NUMBER OF COLUMNS                            
         PRINT NOGEN                                                            
*                                                                               
SETVP080 CLC   NPUAPER,0(RE)                                                    
         BE    SETVP120                                                         
         LA    RE,2(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R1,SETVP080                                                      
         B     SETVP150                                                         
*                                                                               
SETVP120 MVC   0(4,RF),GDVPH                                                    
         CLI   CONACT,C'R'                                                      
         BNE   *+10                                                             
         MVC   0(4,RF),GDGRP                                                    
*                                                                               
SETVP150 ZIC   RF,NPUALEN                                                       
         AR    R6,RF                                                            
         B     SETVP020                                                         
*                                                                               
SETVPEX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT IMPRESSIONS                                    
*              STORED IN OUTGRID TO THE SCREEN                                  
         SPACE 3                                                                
DISVPHS  NTR1                                                                   
         CLI   PLANNLEN,0                                                       
         BE    DISVPEX                                                          
*                                                                               
         LA    R2,PLANLENS         LENGTHS                                      
         ZIC   R3,PLANNLEN         NUMBER OF LENGTHS                            
         LA    R4,OUTGRID                                                       
DISVP020 LA    R5,LISTAR+11                                                     
         LA    R6,16                                                            
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(6),PROGCODE                                               
*                                                                               
DISVP080 EDIT  (4,0(R4)),(4,0(R5))                                              
         CLI   CONACT,C'R'                                                      
         BNE   DISVP100                                                         
         OC    0(4,R4),0(R4)                                                    
         BZ    DISVP100                                                         
         EDIT  (4,0(R4)),(4,0(R5)),1                                            
DISVP100 LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,DISVP080                                                      
         GOTO1 LISTMON                                                          
*                                                                               
DISVPEX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MOVES THE VPH'S OR HOUSEHOLD RATING                   
*              TO THE RIGHT SPOT ON THE OUTGRID TABLE                           
         SPACE 3                                                                
SETHOMS  NTR1                                                                   
         L     RE,AIO                                                           
         USING NPURECD,RE                                                       
         USING NPUAD,R6                                                         
         LA    R6,NPGDEL                                                        
         ZIC   RF,NPGDLEN                                                       
         AR    R6,RF                                                            
*                                                                               
         XC    OUTGRID,OUTGRID                                                  
         DROP  RE                                                               
*                                                                               
SETHM020 CLI   0(R6),X'02'                                                      
         BE    SETHM050                                                         
         CLI   0(R6),X'12'                                                      
         BE    SETHM050                                                         
         BH    SETHMEX                                                          
         CLI   0(R6),X'00'                                                      
         BE    SETHMEX                                                          
*                                                                               
SETHM050 LA    RE,CMONDAYS                                                      
         LA    R5,OUTGRID                                                       
         ZIC   R1,HEADCNT          NUMBER OF COLUMNS                            
*                                                                               
SETHM080 CLC   NPUAPER,0(RE)                                                    
         BE    SETHM120                                                         
         LA    RE,2(RE)                                                         
         LA    R5,4(R5)                                                         
         BCT   R1,SETHM080                                                      
         B     SETHM150                                                         
*                                                                               
SETHM120 CLI   CONACT,C'S'                                                      
         BNE   *+10                                                             
         MVC   2(2,R5),NPUASHR                                                  
         CLI   CONACT,C'H'                                                      
         BNE   *+10                                                             
         MVC   2(2,R5),NPUAHUT                                                  
         CLI   CONACT,C'R'                                                      
         BNE   *+10                                                             
         MVC   2(2,R5),NPUARTG                                                  
*                                                                               
SETHM150 ZIC   RF,NPUALEN                                                       
         AR    R6,RF                                                            
         B     SETHM020                                                         
*                                                                               
SETHMEX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT IMPRESSIONS                                    
*              STORED IN OUTGRID TO THE SCREEN                                  
         SPACE 3                                                                
DISHOMS  NTR1                                                                   
         MVI   RTGSW,0                                                          
*                                                                               
         CLI   PLANNLEN,0                                                       
         BE    DISHMEX                                                          
*                                                                               
         LA    R2,PLANLENS         LENGTHS                                      
         ZIC   R3,PLANNLEN         NUMBER OF LENGTHS                            
         LA    R4,OUTGRID                                                       
DISHM020 LA    R5,LISTAR+11                                                     
         LA    R6,16                                                            
*                                                                               
         CLI   CONACT,C'H'                                                      
         BNE   *+10                                                             
         MVC   LISTAR+7(3),=CL3'HUT'                                            
         CLI   CONACT,C'S'                                                      
         BNE   *+10                                                             
         MVC   LISTAR+7(3),=CL3'SHR'                                            
         CLI   CONACT,C'R'                                                      
         BNE   *+10                                                             
         MVC   LISTAR+7(3),=CL3'RTG'                                            
         MVC   LISTAR(6),PROGCODE                                               
*                                                                               
DISHM080 OC    0(4,R4),0(R4)                                                    
         BZ    DISHM120                                                         
         EDIT  (4,0(R4)),(4,0(R5)),1                                            
DISHM120 LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,DISHM080                                                      
         GOTO1 LISTMON                                                          
*                                                                               
DISHMEX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO MOVES TCOST TO OUTGRID TABLE                          
         SPACE 3                                                                
SETCOST  NTR1                                                                   
         L     RE,AIO                                                           
         USING NPURECD,RE                                                       
         USING NPUBD,R6                                                         
         LA    R6,NPGDEL                                                        
         ZIC   RF,NPGDLEN                                                       
         AR    R6,RF                                                            
*                                                                               
         XC    OUTGRID,OUTGRID                                                  
         DROP  RE                                                               
*                                                                               
SETCT020 CLI   0(R6),X'12'                                                      
         BE    SETCT050                                                         
         BH    SETCTEX                                                          
         CLI   0(R6),X'00'                                                      
         BE    SETCTEX                                                          
*                                                                               
SETCT050 LA    RE,CMONDAYS                                                      
         LA    R5,OUTGRID                                                       
         ZIC   R1,HEADCNT          NUMBER OF COLUMNS                            
*                                                                               
SETCT080 CLC   NPUBPER,0(RE)                                                    
         BE    SETCT120                                                         
         LA    RE,2(RE)                                                         
         LA    R5,4(R5)                                                         
         BCT   R1,SETCT080                                                      
         B     SETCT150                                                         
*                                                                               
SETCT120 MVC   0(4,R5),NPUBAMT                                                  
         MVC   64(4,R5),NPUBAMT+4                                               
         MVC   128(4,R5),NPUBAMT+8                                              
         MVC   192(4,R5),NPUBAMT+12                                             
*                                                                               
SETCT150 ZIC   RF,NPUBLEN                                                       
         AR    R6,RF                                                            
         B     SETCT020                                                         
*                                                                               
SETCTEX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO OUTPUT COST IN HEX TO SCREEN                          
DISCOST  NTR1                                                                   
         CLI   PLANNLEN,0                                                       
         BE    DISCTEX                                                          
*                                                                               
         LA    R2,PLANLENS         LENGTHS                                      
         ZIC   R3,PLANNLEN         NUMBER OF LENGTHS                            
         LA    R4,OUTGRID                                                       
DISCT020 LA    R5,LISTAR+11                                                     
         LA    R6,16                                                            
*                                                                               
         MVC   LISTAR,SPACES                                                    
         MVC   LISTAR(6),PROGCODE                                               
         EDIT  (1,(R2)),(3,LISTAR+7)     MOVE LENGTH OUT                        
         LA    R2,1(R2)                                                         
*                                                                               
DISCT080 MVC   0(4,R5),0(R4)                                                    
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,DISCT080                                                      
         GOTO1 LISTMON                                                          
*                                                                               
         BCT   R3,DISCT020                                                      
*                                                                               
DISCTEX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET WEEK START DATES                                  
*              USED FOR WEEKLY PLANS                                            
         SPACE 3                                                                
WEEKDATS NTR1                                                                   
         ZIC   R1,PROGPERQ         GET END OF QUARTER FROM QUARTER NO.          
         SLL   R1,2                                                             
         LA    R1,QENDDATE(R1)                                                  
         MVC   WORK+2(4),0(R1)                MMDD FROM TABLE                   
         ZIC   R2,PLANYEAR         PLAN YEAR                                    
         CLI   PROGPERQ,0                                                       
         BNE   *+6                                                              
         BCTR  R2,0                OR PREVIOUS FOR FOURTH QUARTER               
         EDIT  (R2),(2,WORK),WRK=DMCB,FILL=0                                    
         SPACE 1                                                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6       FIND PREVIOUS SUNDAY               
         ZIC   R0,DMCB             (END OF QUARTER DAY IN R0)                   
         ZIC   R1,ZEROPROF+8       PICK UP PROFILE START OF WEEK                
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                (DEFAULT IS MONDAY)                          
         BCT   R1,*+8              BACK UP TO END OF WEEK                       
         LA    R1,7                MONDAY BACKS UP TO SUNDAY                    
         SR    R1,R0                                                            
         BZ    WKD3                                                             
         BM    *+8                                                              
WKD1     SH    R1,=H'7'                                                         
         CLI   N0PROF+3,C'C'       IF WE ARE USING CALENDAR                     
         BNE   *+8                    ADD IN ANOTHER WEEK                       
         AH    R1,=H'7'                                                         
         ST    R1,DMCB+8                                                        
* THIRD QUARTER YEAR 2000 FIX                                                   
         CLC   WORK(4),=C'0010'    ,,IF 3D QRT 00                               
         BNE   WKD1A                                                            
         CLI   N0PROF+3,C'C'       ,,IF CALENDAR                                
         BNE   WKD1A                                                            
         A     R1,=F'-7'           ,,DROP ONE WEEK                              
         ST    R1,DMCB+8                                                        
* 1ST QUARTER YEAR 2001 FIX                                                     
WKD1A    CLC   WORK(4),=C'0103'    ,,IF 1ST QRT 01                              
         BNE   WKD1B                                                            
         CLI   N0PROF+3,C'B'       ,,IF BROADCAST                               
         BNE   WKD1B                                                            
         A     R1,=F'7'            ,,ADD ONE WEEK                               
         ST    R1,DMCB+8                                                        
* THIRD QUARTER YEAR 2001 FIX                                                   
WKD1B    CLC   WORK(4),=C'0110'    ,,IF 3D QRT 01                               
         BNE   WKD2                                                             
         CLI   N0PROF+3,C'C'       ,,IF CALENDAR                                
         BNE   WKD2                                                             
         A     R1,=F'-7'           ,,DROP ONE WEEK                              
         ST    R1,DMCB+8                                                        
*                                                                               
WKD2     GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         MVC   WORK(6),WORK+6                                                   
         SPACE 1                                                                
*-- NEXT FOUR LINES ARE NEEDED TO CORRECT A FLAW IN THE 2000                    
*-- YEAR DATE CALCULATIONS, DEALING WITH THE THIRD QUARTER.                     
WKD3     CLC   WORK(6),=CL6'001001'                                             
         BNE   *+12                                                             
         CLI   N0PROF+3,C'B'       IF WE ARE USING BROADCAST                    
         BE    WKD1                   ADD IN ANOTHER WEEK                       
*                                                                               
         L     R1,=F'-111'         BACK UP 16 WEEKS TO FIRST MONDAY             
         ST    R1,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         LA    R2,OVWORK                                                        
         LA    R3,CMONDAYS                                                      
         LA    R0,16                                                            
         SPACE 1                                                                
WKD4     MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(7,0(R2))                                   
         GOTO1 DATCON,DMCB,(0,WORK),(2,0(R3))                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'7'                                      
         LA    R2,5(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,WKD4                                                          
         MVC   OVWORK+80(5),=C'TOTAL'                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET MONTH DATES                                       
*              USED FOR MONTHLY PLANS                                           
         SPACE 3                                                                
MNTHDATS NTR1                                                                   
         SPACE 3                                                                
         ZIC   RE,PLANYEAR                                                      
         BCTR  RE,0                SUBTRACT 1 FROM YEAR                         
         LA    RF,9                                                             
         LA    R2,CMONDAYS                                                      
         LA    R3,4                                                             
*                                                                               
MND050   STC   RE,0(R2)            YEAR                                         
         STC   RF,1(R2)            MONTH                                        
         LA    RF,1(RF)                                                         
         LA    R2,2(R2)                                                         
         BCT   R3,MND050                                                        
*                                                                               
         ZIC   RE,PLANYEAR                                                      
         LA    RF,1                                                             
         LA    R3,9                                                             
*                                                                               
MND100   STC   RE,0(R2)            YEAR                                         
         STC   RF,1(R2)            MONTH                                        
         LA    RF,1(RF)                                                         
         LA    R2,2(R2)                                                         
         BCT   R3,MND100                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET QUARTER DATES                                     
*              USED FOR QUARTLY PLANS                                           
         SPACE 3                                                                
QURTDATS NTR1                                                                   
         SPACE 3                                                                
         ZIC   RE,PLANYEAR                                                      
         BCTR  RE,0                SUBTRACT 1 FROM YEAR                         
         STC   RE,CMONDAYS         YEAR QUARTER                                 
         MVI   CMONDAYS+1,X'04'                                                 
*                                                                               
         ZIC   RE,PLANYEAR                                                      
         LA    R2,CMONDAYS+2                                                    
         LA    RF,1                                                             
         LA    R3,3                                                             
*                                                                               
QTD100   STC   RE,0(R2)            YEAR                                         
         STC   RF,1(R2)            QUARTER                                      
         LA    RF,1(RF)                                                         
         LA    R2,2(R2)                                                         
         BCT   R3,QTD100                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*--SET HEADLINE FOR UNIT LEVEL REPORT                                           
*                                                                               
SETUHEAD NTR1                                                                   
         CLI   PLANPERT,C'Q'                                                    
         BNE   *+22                                                             
         MVC   PUPTITL+15(40),QTITLESU    QUARTERLY                             
         MVI   HEADCNT,4           NUMBER OF COLUMNS                            
         BAS   RE,QURTDATS         SET WEEK DATES                               
         B     SETUEND                                                          
         CLI   PLANPERT,C'M'                                                    
         BNE   *+22                                                             
         MVC   PUPTITL+15(40),MTITLESU     MONTHLY                              
         MVI   HEADCNT,13          NUMBER OF COLUMNS                            
         BAS   RE,MNTHDATS         SET WEEK DATES                               
         B     SETUEND                                                          
         SPACE 1                                                                
*                                                                               
         MVI   HEADCNT,16          NUMBER OF COLUMNS                            
         BAS   RE,WEEKDATS         SET WEEK DATES                               
         LA    R1,PUPTITL+16       WEEKLY                                       
         LA    RE,16                                                            
         LA    RF,OVWORK                                                        
SETU20   CLC   0(5,RF),=C'TOTAL'                                                
         BE    SETUEND                                                          
*                                                                               
         LA    R5,WEEKCNV                                                       
SETU60   CLI   0(R5),C' '                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(3,RF),0(R5)                                                    
         BE    SETU80                                                           
         LA    R5,4(R5)                                                         
         B     SETU60                                                           
SETU80   MVC   0(1,R1),3(R5)       MONTH CODE                                   
         MVC   1(2,R1),3(RF)       WEEK DATE                                    
         LA    R1,3(R1)                                                         
         LA    RF,5(RF)                                                         
         BCT   RE,SETU20                                                        
*                                                                               
SETUEND  B     XIT                                                              
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
XIT      XIT1                                                                   
         SPACE 1                                                                
QENDDATE DC    C'1231033106301001'                                              
         SPACE 1                                                                
BADQUART MVC   CONHEAD(L'QURTERR),QURTERR                                       
         B     MYEND                                                            
BADDPLAN MVC   CONHEAD(L'NOPLAN),NOPLAN                                         
         B     MYEND                                                            
BADDNUM  MVC   CONHEAD(L'BADDVAL),BADDVAL                                       
         B     MYEND                                                            
         SPACE 1                                                                
QURTERR  DC    C'** ERROR ** NEED QUARTER NUMBER (1-4) FOR WEEKLIES'            
NOPLAN   DC    C'** ERROR ** DEMO NOT ON THE PLAN RECORD'                       
BADDVAL  DC    C'** ERROR ** THERE IS NO DEMO TO MATCH THIS NUMBER'             
*                                                                               
QTITLES  DC    CL64'  Q4  Q1  Q2  Q3'                                           
MTITLES  DC    CL64' SEP OCT NOV DEC JAN FEB MAR APR MAY JUN JUL AUG SEX        
               P'                                                               
*                                                                               
QTITLESU DC    CL40'  Q4 Q1 Q2 Q3'                                              
MTITLESU DC    CL40' SEPOCTNOVDECJANFEBMARAPRMAYJUNJULAUGSEP'                   
*                                                                               
WEEKCNV  DC    CL49'JAN1FEB2MAR3APR4MAY5JUN6JUL7AUG8SEP9OCTANOVBDECC '          
         EJECT                                                                  
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
       ++INCLUDE NEPUPE8D                                                       
         SPACE 1                                                                
GENCKEY  DS    CL32                                                             
         DS    0F                                                               
UNITACCS DS    CL52                                                             
********                                                                        
CMONDAYS DS    CL32                COMPRESSED MONDAY DATES                      
OUTGRID  DS    CL256               TABLE FOR WEEKLY OUTPUT                      
SAVEKEY  DS    CL13                COMPARE KEY                                  
HEADCNT  DS    XL1                 NUMBER OF COLUMNS                            
LINECNT  DS    XL1                 LINES CURRENTLY ON SCREEN                    
ACTDEMO  DS    XL3                 DEMO TOO BE USED                             
SVDEMOS  DS    XL18                SAVE DEMOS AREAS                             
RTGSW    DS    XL1                 OUTPUT RATING SWITCH                         
LASTACT  DS    CL1                 LAST ACTION                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'091NEPUP17   05/01/02'                                      
         END                                                                    
