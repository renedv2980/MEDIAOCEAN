*          DATA SET NEPUP08    AT LEVEL 146 AS OF 05/01/02                      
*PHASE T32208A,*                                                                
         TITLE 'T32208 - CABLE PROGRAM MAINTENANCE'                             
T32208   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T32208**,RA                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         OI    CONSERVH+6,X'81'    SCREEN ALWAYS MODIFIED                       
         OI    GENSTAT2,RETEQSEL                                                
*                                                                               
         CLI   PFKEY,5                                                          
         BNE   *+8                                                              
         BAS   RE,CLRSCRN                                                       
*                                                                               
         CLI   MODE,PROCPFK                                                     
         BE    PF                                                               
         CLI   MODE,VALKEY                                                      
         BE    VKEY                                                             
         CLI   MODE,DISPKEY                                                     
         BE    DKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    VREC                                                             
         CLI   MODE,DISPREC                                                     
         BE    DREC                                                             
         CLI   MODE,LISTRECS                                                    
         BE    LR                                                               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
         SPACE 3                                                                
VKEY     XC    NETWORK,NETWORK                                                  
         XC    DPTCODE,DPTCODE                                                  
         XC    PLANCODE,PLANCODE                                                
         LA    R2,PUPCLIH          CLIENT                                       
         TM    4(R2),X'20'         FIELD BEEN PREVIOUSLY VALIDATED              
         BO    *+16                YES BYPASS VALIDATION                        
         XC    FRSTELEM,FRSTELEM   CLEAR ELEMENT POINTER                        
         XC    LASTELEM,LASTELEM   CLEAR ELEMENT POINTER                        
         GOTO1 VVALCLT                                                          
         OI    4(R2),X'20'         FIELD HAS BEEN VALIDATED                     
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
*                                                                               
*                                                                               
         SPACE 1                                                                
*                                                                               
*                                                                               
VK10     LA    R2,PUPNETH          NETWORK                                      
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         CLC   CONACT(4),=CL4'LIST'                                             
         BE    VK40                                                             
VK20     TM    4(R2),X'20'         FIELD BEEN PREVIOUSLY VALIDATED              
         BO    *+16                YES BYPASS VALIDATION                        
         XC    FRSTELEM,FRSTELEM   CLEAR ELEMENT POINTER                        
         XC    LASTELEM,LASTELEM   CLEAR ELEMENT POINTER                        
         GOTO1 VVALNET                                                          
         OI    4(R2),X'20'         FIELD HAS BEEN VALIDATED                     
         SPACE 1                                                                
*                                                                               
*                                                                               
VK40     LA    R2,PUPDPTH          DAYPART                                      
         CLI   5(R2),0                                                          
         BNE   VK60                                                             
         CLC   CONACT(4),=CL4'LIST'                                             
         BE    VK80                                                             
VK60     TM    4(R2),X'20'         FIELD BEEN PREVIOUSLY VALIDATED              
         BO    *+16                YES BYPASS VALIDATION                        
         XC    FRSTELEM,FRSTELEM   CLEAR ELEMENT POINTER                        
         XC    LASTELEM,LASTELEM   CLEAR ELEMENT POINTER                        
         GOTO1 VVALDPT                                                          
         OI    4(R2),X'20'         FIELD HAS BEEN VALIDATED                     
         MVC   PUPDPT,DPTNAME                                                   
         OI    PUPDPTH+6,X'80'                                                  
         SPACE 1                                                                
*                                                                               
*                                                                               
VK80     LA    R2,PUPPLANH         PLAN                                         
         CLI   5(R2),0                                                          
         BNE   VK100                                                            
         CLC   CONACT(4),=CL4'LIST'                                             
         BE    VK200                                                            
VK100    TM    4(R2),X'20'         FIELD BEEN PREVIOUSLY VALIDATED              
         BO    *+16                YES BYPASS VALIDATION                        
         XC    FRSTELEM,FRSTELEM   CLEAR ELEMENT POINTER                        
         XC    LASTELEM,LASTELEM   CLEAR ELEMENT POINTER                        
         GOTO1 VVALPLAN                                                         
         CLI   PLANPRCB,C'Y'                                                    
         BNE   NOCABERR                                                         
         OI    4(R2),X'20'         FIELD HAS BEEN VALIDATED                     
*                                                                               
*                                                                               
         CLC   CONACT(4),=CL4'LIST'                                             
         BE    VK200                                                            
         BAS   RE,STSCREEN         MOVE THE DEMOS TO THE SCREEN                 
*        CLI   ACTNUM,ACTREST      IF ACTION IS RESTORE                         
*        BE    VKEY200                                                          
*        GOTO1 VVALPLAN                                                         
VK200    LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPCKEY,R4                                                        
         MVI   NPCKTYPE,X'28'      FILL CABLE PROGRAM KEY                       
         MVC   NPCKAM,BINAGYMD                                                  
         MVC   NPCKCLT,CLTCOMP                                                  
         MVC   NPCKNET,NETWORK                                                  
         MVC   NPCKDPT,DPTCODE                                                  
         MVC   NPCKPLAN,PLANCODE                                                
*                                                                               
         CLC   KEY(20),SAVEKEY                                                  
         BE    *+16                                                             
         XC    FRSTELEM,FRSTELEM   KEY CHANGE                                   
         XC    LASTELEM,LASTELEM   RESET POINTERS                               
         MVC   SAVEKEY(20),KEY                                                  
*                                                                               
         CLC   CONACT(4),=CL4'LIST'                                             
         BE    VKEX                                                             
         CLC   CONACT(3),=CL3'ADD'                                              
         BE    VKEX                                                             
         CLI   ACTNUM,ACTREST      IF ACTION IS RESTORE                         
         BE    VKEX                                                             
*                                                                               
*  GET THE CABLE PROGRAM RECORD                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   NORECORD                                                         
         GOTO1 GETREC                                                           
*                                                                               
VKEX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY KEY                                                      
*                                                                               
         SPACE 3                                                                
DKEY     L     R4,AIO                                                           
         USING NPCRECD,R4                                                       
*                                                                               
*                                                                               
         GOTO1 CLUNPK,DMCB,NPCKCLT,PUPCLI                                       
         LA    R2,PUPCLIH                                                       
         MVI   5(R2),2                                                          
         CLI   PUPCLI+2,X'41'                                                   
         BL    *+8                                                              
         MVI   5(R2),3                                                          
*                                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 VVALCLT                                                          
         MVC   AIO,AIO1                                                         
         OI    PUPCLIH+6,X'80'                                                  
         MVC   PUPCLIN,CLTNAME                                                  
         OI    PUPCLINH+6,X'80'                                                 
*                                                                               
*                                                                               
         MVC   PUPNET(4),NPCKNET                                                
         OI    PUPNETH+6,X'80'                                                  
         MVI   PUPNETH+5,3                                                      
         CLI   NPCKNET+4,X'40'                                                  
         BNH   *+8                                                              
         MVI   PUPNETH+5,4                                                      
*                                                                               
*                                                                               
         MVC   WORK(1),NPCKDPT                                                  
         GOTO1 VLUPDPT                                                          
         MVC   PUPDPT,DPTNAME                                                   
         OC    PUPDPT,SPACES                                                    
         OI    PUPDPTH+6,X'80'                                                  
         MVI   PUPDPTH+5,8                                                      
*                                                                               
*                                                                               
         MVC   PUPPLAN,NPCKPLAN                                                 
         OC    PUPPLAN,SPACES                                                   
         OI    PUPPLANH+6,X'80'                                                 
         MVI   PUPPLANH+5,4                                                     
*                                                                               
*                                                                               
         NI    PUPCLIH,X'DF'       TURN OFF VALIDATION BITS                     
         NI    PUPNETH,X'DF'                                                    
         NI    PUPDPTH,X'DF'                                                    
         NI    PUPPLANH,X'DF'                                                   
         MVC   KEY,SAVEKEY                                                      
         B     VKEY                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*  STSCREEN MOVES THE PLANS DEMO NAMES TO THE HEADLINES                         
*  AND PROTECTS THE FIELDS THAT ARE NOT IN USE.                                 
*              VALIDATE RECORD                                                  
STSCREEN NTR1                                                                   
         LA    R2,HEADNAD                                                       
         LA    R3,HEADDEM                                                       
*                                                                               
         LA    R4,DEMOS                                                         
         LA    R5,6                                                             
         SR    R6,R6                                                            
         SPACE 1                                                                
         GOTO1 VSETDB                                                           
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
*                                                                               
STS050   CH    R6,=H'4'            DO WE ALREADY HAVE 4 DEMOS                   
         BNL   STS200                                                           
         CLI   1(R4),0             ANY MORE DEMOS                               
         BE    STS200              NOPE, GET OUT                                
         CLI   0(R4),1             CHECK NADS                                   
         BNH   STS070                                                           
         EDIT  (1,0(R4)),(3,0(R2))                                              
         MVI   0(R4),0             ZERO NAD FIELD                               
         B     STS090                                                           
STS070   CLI   2(R4),1             BYPASS HOMES                                 
         BE    STS120                                                           
STS090   GOTO1 DEMOCON,DMCB,(0,0(R4)),(10,WORK),(C'S',DBLOCK)                   
         MVC   0(6,R3),WORK                                                     
         CLI   WORK+6,X'40'                                                     
         BE    *+10                                                             
         MVC   0(6,R3),WORK+1                                                   
         SPACE 1                                                                
         LA    R6,1(R6)            ADD TO COUNT                                 
STS120   LA    R2,3(R2)            BUMP NAD HOLD                                
         LA    R3,6(R3)            BUMP DEMO HOLD                               
         LA    R4,3(R4)            GO TO NEXT DEMO                              
         BCT   R5,STS050                                                        
*                                                                               
*  MOVE THE HEADLINES OUT                                                       
*                                                                               
STS200   LA    R2,PUPHDEMH                                                      
         LA    R3,PUPHLNEH                                                      
         LA    R4,PUPHNADH                                                      
         LA    R5,4                                                             
*                                                                               
*  CLEAR THE HEADLINES                                                          
*                                                                               
STS220   MVC   8(6,R2),SPACES                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               GET NEXT DEMO FIELD                          
         MVC   8(6,R3),SPACES                                                   
         OI    6(R3),X'80'         TRANSMIT                                     
         ZIC   RE,0(R3)                                                         
         AR    R3,RE               GET NEXT UNDERLINE FIELD                     
         MVC   8(3,R4),SPACES                                                   
         OI    6(R4),X'80'         TRANSMIT                                     
         ZIC   RE,0(R4)                                                         
         AR    R4,RE               GET NEXT NAD FIELD                           
         BCT   R5,STS220                                                        
*                                                                               
         STCM  R6,1,DEMCNT         SAVE NUMBER OF DEMOS                         
         LTR   R6,R6                                                            
         BZ    STS400                                                           
         LA    R2,PUPHNADH                                                      
         LA    R3,HEADNAD          FIRST MOVE NAD INFO                          
*                                                                               
STS240   OC    0(3,R3),0(R3)                                                    
         BZ    STS260                                                           
         MVC   8(3,R2),0(R3)                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
STS260   BAS   RE,NXTFIELD                                                      
         LA    R3,3(R3)                                                         
         BCT   R6,STS240                                                        
*                                                                               
STS300   ZIC   R6,DEMCNT           NUMBER OF DEMOS                              
         LA    R2,PUPHDEMH                                                      
         LA    R3,PUPHLNEH                                                      
         LA    R4,HEADDEM          MOVE THE DEMO'S                              
*                                                                               
STS320   MVC   8(6,R2),0(R4)                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         MVC   8(6,R3),=CL6'______'                                             
         OI    6(R3),X'80'         TRANSMIT                                     
         BAS   RE,NXTFIELD                                                      
         ZIC   RE,0(R3)                                                         
         AR    R3,RE               GET NEXT UNDERLINE FIELD                     
         LA    R4,6(R4)            GET NEXT DEMO FIELD                          
         BCT   R6,STS320                                                        
*                                                                               
*  SET ALL THE DEMO FEILDS TO UNPROTECT                                         
*                                                                               
STS400   LA    R2,PUPDM1H          FIRST DEMO FIELD                             
         LA    R4,PUPDM1H          FIRST DEMO FIELD                             
         LA    R3,PUPNAMH          FIRST FIELD ON A LINE                        
         LA    R5,4                                                             
*                                                                               
STS420   NI    1(R2),X'DF'         UNPROTECT THE FIELD                          
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NXTFIELD                                                      
         BCT   R5,STS420                                                        
         LA    R3,PUPNAM2H-PUPNAMH(R3)   GET NEXT LINE                          
         LA    R4,PUPNAM2H-PUPNAMH(R4)   DEMO POINTER NEXT LINE                 
         LR    R2,R4               RESET DEMO POINTER IN R2                     
         CLI   0(R3),17            FIRST FIELD IS 17 NYTES LONG                 
         BNE   STS500              IF NOT EQUAL,END OF SCREEN                   
         LA    R5,4                UNPROTECT NEXT LINE                          
         B     STS420                                                           
*                                                                               
*  PROTECT THE FIELDS NOT BEING USED                                            
*                                                                               
STS500   ZIC   R6,DEMCNT           NUMBER OF DEMOS IN HEADLINE                  
         LA    R5,4                MAXIMUM DEMOS ALLOWED                        
         SR    R5,R6               R5=UNUSED DEMO COLUMNS                       
         LTR   R5,R5                                                            
         BZ    STSEX               ALL FIELDS ARE USED, EXIT                    
         STCM  R5,1,NDEMCNT                                                     
         LA    R2,PUPDM1H          FIRST DEMO FIELD                             
         LTR   R6,R6                ANY DEMOS IN HEADLINES                      
         BZ    STS540                                                           
*                                                                               
*  POINT TO FIRST UNUSED DEMO FIELD                                             
*                                                                               
STS520   BAS   RE,NXTFIELD                                                      
         BCT   R6,STS520                                                        
         LR    R4,R2               SAVE LOCATION IN R4                          
         LA    R3,PUPNAMH          FIRST FIELD ON A LINE                        
*                                                                               
STS540   ZIC   R5,NDEMCNT          NNUMBER OF UNUSED FIELDS ON A LINE           
STS560   OI    1(R2),X'20'         PROTECT THE FIELD                            
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NXTFIELD                                                      
         BCT   R5,STS560                                                        
         LA    R3,PUPNAM2H-PUPNAMH(R3)   GET NEXT LINE                          
         LA    R4,PUPNAM2H-PUPNAMH(R4)   DEMO POINTER NEXT LINE                 
         LR    R2,R4               RESET DEMO POINTER IN R2                     
         CLI   0(R3),17            FIRST FIELD IS 17 NYTES LONG                 
         BE    STS540              IF EQUAL WE ARE STILL ON THE SCREEN          
*                                                                               
STSEX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
*   VALIDATE THE RECORD                                                         
*                                                                               
         SPACE 3                                                                
*        OI    GENSTAT2,RETEQSEL                                                
VREC     CLC   CONACT(3),=CL3'ADD' IF ADD ACTION IGNORE PF KEYS                 
         BE    VREC10                                                           
         CLI   PFKEY,6             GET NEXT SCREEN                              
         BE    DREC                                                             
         CLI   PFKEY,5             GET NEXT SCREEN                              
         BE    VRECEX                                                           
*                                                                               
VREC10   L     R4,AIO                                                           
         USING NPCRECD,R4                                                       
         MVC   COUNT,NPCNNUM       CURRENT COUNT                                
         CLC   NPCNEL(2),=XL2'0114'  IS ELEMENT THERE                           
         BE    *+22                                                             
         XC    NPCNEL(20),NPCNEL     CLEAR ELEMENT AREA                         
         MVC   NPCNEL(2),=XL2'0114'  SET UP 01 ELEMENT                          
         XC    COUNT,COUNT         ELEMENT COUNT                                
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT                                                  
         XC    HOLDNAME,HOLDNAME                                                
         XC    HOLDDEM,HOLDDEM                                                  
         XC    HOLDRTSH,HOLDRTSH                                                
         LA    R2,PUPNAMH          FIRST SCREEN FIELD                           
         LA    R3,12                                                            
         LA    R5,ELEMENT                                                       
         USING NPPELD,R5                                                        
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVC   NPPELEM(2),=XL2'022C'                                            
*                                                                               
*  PROGRAM NAME                                                                 
*                                                                               
VREC50   ST    R2,LINESVE                                                       
         CLI   5(R2),0                                                          
         BE    VREC70                                                           
         CLI   5(R2),1                                                          
         BNE   VREC55                                                           
         CLI   8(R2),C'='                                                       
         BNE   VREC55                                                           
         OC    HOLDNAME,HOLDNAME                                                
         BZ    BADDROP                                                          
         MVC   NPPNAME,HOLDNAME                                                 
         B     VREC70                                                           
VREC55   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NPPNAME(0),8(R2)                                                 
         MVC   HOLDNAME,NPPNAME                                                 
*                                                                               
*  PROGRAM CODE                                                                 
*                                                                               
VREC70   BAS   RE,NXTFIELD                                                      
         CLI   5(R2),0                                                          
         BE    VREC80                                                           
         CLI   5(R2),1                                                          
         BNE   VREC75                                                           
         CLI   8(R2),C'='                                                       
         BE    BADCODE                                                          
VREC75   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NPPCODE(0),8(R2)                                                 
         OC    NPPCODE,SPACES                                                   
*                                                                               
         CLC   NPPNAME(7),=CL7'*DELETE'   CHECK TO DELETE LINE                  
         BE    VREC400                                                          
*                                                                               
*  CHECK IF ELEMENT EXISTS                                                      
         MVC   DUB(4),NPPCODE                                                   
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'02',AIO),(4,DUB)                    
         CLI   12(R1),0            TEST IF OVERRIDE FOUND                       
         BNE   VREC80              NOT FOUND                                    
         TM    4(R2),X'20'         WAS FIELD CHANGED                            
         BZ    DUPCODE             YES DUPPERR                                  
*                                                                               
*  DELETE THE OLD ELEMENT                                                       
         GOTO1 HELLO,DMCB,(C'D',UNTFILE),(X'02',AIO),(4,DUB)                    
         ZIC   RE,COUNT            SUBTRACT 1 FROM ELEMENT COUNT                
         SH    RE,=H'1'                                                         
         STCM  RE,1,COUNT                                                       
*                                                                               
*  DAY                                                                          
*                                                                               
VREC80   BAS   RE,NXTFIELD                                                      
         CLI   5(R2),0                                                          
         BE    VREC150                                                          
         CLI   5(R2),1                                                          
         BNE   VREC90                                                           
         CLI   8(R2),C'='                                                       
         BNE   VREC90                                                           
         OC    HOLDDAY,HOLDDAY                                                  
         BZ    BADDROP                                                          
         MVC   NPPDAY,HOLDDAY                                                   
         B     VREC150                                                          
*                                                                               
VREC90   ZIC   R6,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R6),8(R2)),DAYHLD,BYTE                             
         CLI   DAYHLD,0                                                         
         BE    BADDAY                                                           
         MVI   NPPDAY,X'FF'                                                     
         CLI   DAYHLD,X'7C'        M-F                                          
         BE    VREC140                                                          
         MVI   NPPDAY,X'08'        REP DAY M-S                                  
         CLI   DAYHLD,X'7F'        M-SU                                         
         BE    VREC140                                                          
*                                                                               
         MVI   WORK,X'40'          ELSE CAN ONLY BE ONE DAY                     
         MVI   WORK+1,0                                                         
         MVI   WORK+2,X'01'        MON - REP CODE                               
         LA    R6,7                FOR BCT - CHECK EACH DAY M-SUN               
*                                                                               
VREC100  MVC   ALMSK+1(1),WORK     ALTER MASK                                   
ALMSK    TM    DAYHLD,X'00'                                                     
         BNO   VREC120                                                          
         CLI   WORK+1,0            SEE IF I ALREADY HAVE A DAY                  
         BNE   BADDAY              YES                                          
         MVI   WORK+1,1                                                         
         MVC   NPPDAY,WORK+2       REP DAY                                      
*                                                                               
VREC120  ZIC   R0,WORK                                                          
         SRL   R0,1                FOR NEXT MASK                                
         STC   R0,WORK                                                          
         ZIC   R1,WORK+2                                                        
         LA    R1,1(R1)                                                         
         STC   R1,WORK+2           NEXT REP DAY                                 
         BCT   R6,VREC100                                                       
*                                                                               
VREC140  MVC   HOLDDAY,NPPDAY                                                   
*                                                                               
*  TIME                                                                         
*                                                                               
VREC150  BAS   RE,NXTFIELD                                                      
         CLI   5(R2),0                                                          
         BE    VREC200                                                          
         CLI   5(R2),1                                                          
         BNE   VREC160                                                          
         CLI   8(R2),C'='                                                       
         BNE   VREC160                                                          
         OC    HOLDTIME,HOLDTIME                                                
         BZ    BADDROP                                                          
         MVC   NPPTIME,HOLDTIME                                                 
         B     VREC200                                                          
*                                                                               
VREC160  ZIC   R6,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R6),8(R2)),NPPTIME                                 
         MVC   HOLDTIME,NPPTIME                                                 
         CLI   DMCB,X'FF'          INVALID TIME                                 
         BE    BADTIME                                                          
         CLC   NPPTIME,=C'NONE'                                                 
         BE    BADTIME                                                          
         CLC   NPPTIME,=C'VARY'                                                 
         BE    BADTIME                                                          
         CLC   NPPTIME+2(2),=2X'00'                                             
         BE    BADTIME             END TIME REQUIRED                            
         CLC   NPPTIME+2(2),=C'CC'                                              
         BE    BADTIME                                                          
*                                                                               
*  RATING/SHARE                                                                 
*                                                                               
VREC200  BAS   RE,NXTFIELD         POINT TO HOMES                               
         BAS   RE,GETRTSH                                                       
*                                                                               
*  DEMOS                                                                        
*                                                                               
VREC240  BAS   RE,NXTFIELD         POINT TO FIRST DEMO                          
         BAS   RE,GETDEMS                                                       
         BAS   RE,CKFIELDS         CHECK ALL REQ FIELDS ARE INPUTTED            
VREC260  BAS   RE,NXTFIELD         POINT TO NEXT LINE                           
         CLI   0(R2),9             LAST FIELD                                   
         BE    VREC320                                                          
         CLI   0(R2),17            NEXT LINE                                    
         BNE   VREC260                                                          
*                                                                               
*  ADD THE ELEMENT                                                              
*                                                                               
VREC320  OC    NPPCODE,NPPCODE                                                  
         BZ    VREC360                                                          
         GOTO1 HELLO,DMCB,(C'P',UNTFILE),AIO,ELEMENT,0                          
         CLI   12(R1),0            TEST IF OK                                   
         BE    VREC340             YES                                          
         CLI   12(R1),5            TEST FOR RECORD OVERFLOW                     
         BE    *+6                                                              
         DC    H'0'                                                             
         B     TOOLARGE                                                         
VREC340  ZIC   RE,COUNT                                                         
         AH    RE,=H'1'                                                         
         STCM  RE,1,COUNT                                                       
VREC360  XC    ELEMENT,ELEMENT                                                  
         MVC   NPPELEM(2),=XL2'022C'                                            
         BCT   R3,VREC50                                                        
         B     VREC500                                                          
*                                                                               
*  DELETE THE ELEMENT                                                           
*                                                                               
VREC400  MVC   DUB(4),ELEMENT+2                                                 
         GOTO1 HELLO,DMCB,(C'D',UNTFILE),(X'02',AIO),(4,DUB)                    
         ZIC   RE,COUNT                                                         
         SH    RE,=H'1'                                                         
         STCM  RE,1,COUNT                                                       
*  GET TO NEXT LINE                                                             
         LA    R1,8                                                             
VREC420  BAS   RE,NXTFIELD                                                      
         BCT   R1,VREC420                                                       
*                                                                               
         B     VREC360             PROCESS NEXT RECORD                          
*                                                                               
VREC500  CLI   COUNT,X'28'         40 ELEMENTS MAX                              
         BH    TOOLARGE                                                         
         MVC   NPCNNUM,COUNT       LOAD CURRNET ELEMENT COUNT                   
         BAS   RE,DISPCNT          DISPLAY COUNT                                
VRECEX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  GETDEMS VALIDATES THE 5 DEMO FIELDS ON EACH LINE                             
*                                                                               
GETDEMS  NTR1                                                                   
         LA    R3,4                                                             
         LA    R5,ELEMENT                                                       
         LA    R5,NPPDEM1-NPPELEM(R5)                                           
         LA    R6,HOLDDEM                                                       
*                                                                               
GETD050  CLI   5(R2),0                                                          
         BE    GETD100                                                          
         CLI   5(R2),1                                                          
         BNE   GETD060                                                          
         CLI   8(R2),C'='                                                       
         BE    GETD200                                                          
*                                                                               
GETD060  ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R0)                                          
         CLI   DMCB,0                                                           
         BNE   BADDEM                                                           
         L     R0,DMCB+4                                                        
         LTR   R0,R0                                                            
         BL    BADDEM              CAN'T BE NEGATIVE                            
*                                                                               
         CVD   R0,DUB              VALIDATE VPH INPUT                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   BADDEM              NO DECIMAL                                   
*                                                                               
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,2(R5)          MOVE TO ELEMENT                              
         MVC   0(4,R6),0(R5)       MOVE TO HOLDDEM                              
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         BAS   RE,NXTFIELD                                                      
         BCT   R3,GETD050                                                       
         B     XIT                                                              
*                                                                               
GETD100  XC    0(4,R5),0(R5)       ZERO FIELD                                   
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         BAS   RE,NXTFIELD                                                      
         BCT   R3,GETD050                                                       
         B     XIT                                                              
*                                                                               
GETD200  MVC   0(4,R5),0(R6)       MOVE IN FROM HOLDDDEM                        
         LA    R5,4(R5)                                                         
         LA    R6,4(R6)                                                         
         BAS   RE,NXTFIELD                                                      
         BCT   R3,GETD050                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*  GETRTSH VALIDATES THE RATING SHARE FIELD                                     
*                                                                               
GETRTSH  NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    GETRSEX                                                          
*                                                                               
         LA    R5,ELEMENT                                                       
*                                                                               
         CLI   5(R2),1                                                          
         BNE   GETRS060                                                         
         CLI   8(R2),C'='                                                       
         BNE   GETRS060                                                         
         OC    HOLDRTSH,HOLDRTSH                                                
         BZ    BADDROP                                                          
         MVC   NPPRSIN(3),HOLDRTSH                                              
         B     GETRSEX                                                          
*                                                                               
GETRS060 ZIC   R4,5(R2)                                                         
         MVI   NPPRSIN,0                                                        
         LA    R6,8(R2)                                                         
         CLI   0(R6),C'R'          RATING                                       
         BNE   GETRS080                                                         
         CLI   5(R2),2                                                          
         BL    BADDEM              MUST BE AT LEAST 2                           
         MVI   NPPRSIN,C'R'                                                     
         LA    R6,1(R6)                                                         
         BCTR  R4,0                                                             
*                                                                               
GETRS080 DS    0H                                                               
         GOTO1 CASHVAL,DMCB,0(R6),(R4)                                          
         CLI   DMCB,0                                                           
         BNE   BADDEM                                                           
         L     R4,DMCB+4                                                        
         C     R4,=F'10000'        MAX IS 100.00                                
         BH    BADDEM                                                           
         LTR   R4,R4                                                            
         BL    BADDEM                                                           
         CVD   R4,DUB                                                           
         LA    R3,DUB+2                                                         
*                                                                               
*        CLI   NPPRSIN,C'R'                                                     
*        BE    GETRS100                                                         
         LA    R3,DUB                                                           
         DP    DUB,=P'10'                                                       
         CP    DUB+6(2),=P'0'      MUST GET ZERO REMAINDER                      
         BNE   BADDEM                                                           
GETRS100 MVC   WORK(6),0(R3)                                                    
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   NPPRTSH,HALF                                                     
         MVC   HOLDRTSH,NPPRSIN                                                 
GETRSEX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*  CKFIELDS CHECKS THE ELEMENT TO SEE ALL REQUIRED FIELDS ARE INPUTTED          
*                                                                               
CKFIELDS NTR1                                                                   
         LA    R5,ELEMENT                                                       
         USING NPPELD,R5                                                        
*                                                                               
         OC    NPPCODE,NPPCODE     WAS CODE INPUTTED                            
         BNZ   CKF100              YES CHECK DAY AND TIME                       
*  CODE NOT INPUTTED DAY AND TIME CANNOT BE INPUTTED EITHER                     
         CLI   NPPDAY,0                                                         
         BNE   CKFERR              DAY INPUTTED ERROR                           
         OC    NPPTIME,NPPTIME                                                  
         BNZ   CKFERR              TIME INPUTTED ERROR                          
         B     CKFEX                                                            
*  CODE INPUTTED DAY AND TIME MUST BE INPUTTED                                  
CKF100   CLI   NPPDAY,0                                                         
         BE    CKFERR              DAY NOT INPUTTED ERROR                       
         OC    NPPTIME,NPPTIME                                                  
         BZ    CKFERR              TIME NOT INPUTTED ERROR                      
         B     CKFEX                                                            
*                                                                               
CKFERR   L     R2,LINESVE                                                       
         B     BADLINE                                                          
*                                                                               
CKFEX    CLI   NPPDAY,X'FF'        IF FAKE M-F IS SET                           
         BNE   *+8                                                              
         MVI   NPPDAY,0            CORRECT AND RESET TO CORRECT M-F             
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*   DISPLAY THE RECORD                                                          
*                                                                               
         SPACE 3                                                                
DREC     CLI   PFKEY,5                                                          
         BE    DRECEX                                                           
*                                                                               
         L     R4,AIO                                                           
         USING NPCRECD,R4                                                       
         USING NPPELD,R3                                                        
         LA    R2,PUPNAMH                                                       
*                                                                               
         BAS   RE,DISPCNT          DISPLAY THE COUNT                            
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
*                                                                               
         CLI   PFKEY,6                                                          
         BNE   *+10                                                             
         MVC   FRSTELEM,LASTELEM   BUMP TO NEXT SCREEN                          
*                                                                               
*  GET TO THE LAST ELEMENT                                                      
*                                                                               
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'02',AIO),0                          
         CLI   12(R1),0            TEST IF OVERRIDE FOUND                       
         BNE   DRECEX                                                           
         L     R3,12(R1)                                                        
         OC    FRSTELEM,FRSTELEM   FIRST PASS                                   
         BNZ   DREC20              NO POSITION POINTER                          
         MVC   FRSTELEM,NPPCODE                                                 
         B     DREC60                                                           
*                                                                               
DREC20   CLI   0(R3),X'02'                                                      
         BE    DREC30                                                           
         XC    LASTELEM,LASTELEM                                                
         B     DRECEX                                                           
DREC30   CLC   NPPCODE,FRSTELEM    ARE WE POSITIONED RIGHT                      
         BNL   DREC60                                                           
         ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     DREC20                                                           
*                                                                               
DREC40   ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'02'                                                      
         BE    DREC60                                                           
         XC    LASTELEM,LASTELEM                                                
         B     DRECEX                                                           
*                                                                               
DREC60   MVC   LASTELEM,NPPCODE                                                 
         CLI   0(R2),9             END OF SCREEN                                
         BE    DRECEX                                                           
*                                                                               
         MVC   8(9,R2),NPPNAME     PROGRAM NAME                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NXTFIELD                                                      
*                                                                               
         MVC   8(4,R2),NPPCODE     PROGRAM CODE                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         MVI   5(R2),4             SET DUMMY LENGTH                             
         BAS   RE,NXTFIELD                                                      
*                                                                               
         ZIC   R1,NPPDAY           DAY                                          
         MH    R1,=H'4'                                                         
         LA    R1,DAYLIST(R1)                                                   
         MVC   8(4,R2),0(R1)                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NXTFIELD                                                      
*                                                                               
         XC    8(11,R2),8(R2)      TIME                                         
         GOTO1 UNTIME,DMCB,NPPTIME,8(R2)                                        
         OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NXTFIELD                                                      
*                                                                               
         CLI   NPPRSIN,C'R'        RATING/SHARE                                 
         BNE   DREC070                                                          
         EDIT  (B2,NPPRTSH),(5,8(R2)),1,ALIGN=LEFT,FLOAT=R                      
         B     DREC080                                                          
DREC070  EDIT  (B2,NPPRTSH),(5,8(R2)),1,ALIGN=LEFT                              
DREC080  OI    6(R2),X'80'         TRANSMIT                                     
         BAS   RE,NXTFIELD                                                      
*                                                                               
*  DEMOS LOOP WILL OUTPUT THE 4 DEMO FIELDS                                     
*                                                                               
         LA    R5,NPPDEM1                                                       
         LA    R6,4                                                             
DREC120  OC    0(4,R5),0(R5)                                                    
         BZ    DREC140                                                          
         EDIT  (B4,0(R5)),(6,8(R2))                                             
         OI    6(R2),X'80'         TRANSMIT                                     
DREC140  BAS   RE,NXTFIELD                                                      
         LA    R5,4(R5)                                                         
         BCT   R6,DREC120                                                       
         B     DREC40              GET NEXT LINE                                
*                                                                               
DRECEX   BAS   RE,PROTCODE         PROTECT THE CODE FIELDS                      
         B     XIT                                                              
*                                                                               
DAYLIST  DC    C'M-F MON TUE WED THU FRI SAT SUN M-SUVAR'                       
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*  PROTECT THE PROGRAM CODE FIELDS THAT HAVE INPUT                              
*                                                                               
PROTCODE NTR1                                                                   
         LA    R2,PUPCODH                                                       
         LA    R3,PUPNAMH                                                       
*                                                                               
PRCD020  CLI   0(R3),17            END OF SCREEN                                
         BNE   PRCDEX              YES EXIT                                     
         CLI   5(R2),0                                                          
         BE    *+8                                                              
         OI    1(R2),X'20'         PROTECT                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R3,PUPNAM2H-PUPNAMH(R3)   GET NEXT LINE                          
         LA    R2,PUPNAM2H-PUPNAMH(R2)   GET NEXT CODE                          
         B     PRCD020                                                          
*                                                                               
PRCDEX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  DISPLAY THE COUNT FIELD                                                      
*                                                                               
DISPCNT  NTR1                                                                   
         L     R4,AIO                                                           
         USING NPCRECD,R4                                                       
         EDIT  (1,NPCNNUM),(2,PUPPCNT)                                          
         OI    PUPPCNTH+6,X'80'                                                 
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*  CLEAR SCREEN                                                                 
*                                                                               
CLRSCRN  NTR1                                                                   
         CLC   CONACT(4),=CL4'LIST'                                             
         BE    CLRSEX                                                           
*                                                                               
         LA    R2,PUPNAMH                                                       
*                                                                               
CLRS050  CLI   0(R2),9                                                          
         BE    CLRS100                                                          
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
*                                                                               
         XC    8(0,R2),8(R2)       CLEAR THE FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    4(R2),X'20'         SET PREVIOSLY SET                            
         MVI   5(R2),0                                                          
         BAS   RE,NXTFIELD                                                      
         B     CLRS050                                                          
*                                                                               
*  UNPROTECT THE PROGRAM CODE FIELDS                                            
*                                                                               
CLRS100  LA    R2,PUPCODH                                                       
         LA    R3,PUPNAMH                                                       
*                                                                               
CLRS120  CLI   0(R3),17            END OF SCREEN                                
         BNE   CLRSEX              YES EXIT                                     
         NI    1(R2),X'DF'         UNPROTECT                                    
         LA    R3,PUPNAM2H-PUPNAMH(R3)   GET NEXT LINE                          
         LA    R2,PUPNAM2H-PUPNAMH(R2)   GET NEXT CODE                          
         B     CLRS120                                                          
*                                                                               
CLRSEX   B     XIT                                                              
*                                                                               
*   LIST RECORDS                                                                
*                                                                               
LR       DS    0H                                                               
         LA    R5,LISTAR                                                        
         USING PLINED,R5                                                        
*                                                                               
         OC    KEY(17),KEY                                                      
         BNZ   LR100                                                            
*                                                                               
         SPACE                                                                  
         LA    R6,KEY                                                           
         USING NPCRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(20),SAVEKEY                                                  
LR100    GOTO1 HIGH                                                             
         B     LR220                                                            
*                                                                               
LR200    GOTO1 SEQ                                                              
LR220    LA    R6,KEY                                                           
         CLC   SAVEKEY(4),KEY                                                   
         BNE   LRX                                                              
         CLI   NETWORK,0           NETWORK FILTER                               
         BE    LR240                                                            
         CLC   NPCKNET,NETWORK                                                  
         BNE   LR200                                                            
         CLI   DPTCODE,0           DAYPART FILTER                               
         BE    LR240                                                            
         CLC   NPCKDPT,DPTCODE                                                  
         BNE   LR200                                                            
         CLI   PLANCODE,0          PLANCODE FILTER                              
         BE    LR240                                                            
         CLC   NPCKPLAN,PLANCODE                                                
         BNE   LR200                                                            
LR240    MVC   SAVEKEY(20),KEY                                                  
         L     R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   PNET,NPCKNET        NETWORK                                      
         MVC   PDAYPART,NPCKDPT    DAYPART                                      
         MVC   PPLAN,NPCKPLAN      PLAN                                         
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR200               GOTO READ SEQ                                
*                                                                               
LRX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
PF       CLI   PFKEY,7                                                          
         BE    PFK40                                                            
*                                                                               
         CLI   PFKEY,5                                                          
         BNE   PFK20                                                            
         BAS   RE,CLRSCRN                                                       
         OI    GENSTAT2,RETEQSEL                                                
         B     PFEX                                                             
*                                                                               
PFK20    CLI   PFKEY,6                                                          
         BNE   PFEX                                                             
         MVC   FRSTELEM,LASTELEM                                                
         OI    GENSTAT2,RETEQSEL                                                
         CLI   THISLSEL,C'C'       IS SELECT CHANGE                             
         BE    PFK25                                                            
         CLI   THISLSEL,CHASELQ    IS SELECT CHANGE                             
         BNE   PFEX                                                             
*  SPECIAL CHANGE ACTION CODE                                                   
*  REREAD THE RECORD                                                            
PFK25    MVC   KEY(20),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(20),SAVEKEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         B     DREC                                                             
*                                                                               
PFK40    NI    GENSTAT2,X'FF'-RETEQSEL                                          
         OI    GENSTAT2,NEXTSEL                                                 
         B     PFEX                                                             
*                                                                               
PFEX     B     XIT                                                              
         EJECT                                                                  
*              ERROR EXITS                                                      
         SPACE 3                                                                
BADCODE  MVC   CONHEAD(L'CODEERR),CODEERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADDROP  MVC   CONHEAD(L'DROPERR),DROPERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
BADDAY   MVC   CONHEAD(L'DAYERR),DAYERR                                         
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADTIME  MVC   CONHEAD(L'TIMEERR),TIMEERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADLINE  MVC   CONHEAD(L'LINEERR),LINEERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
BADDEM   MVC   CONHEAD(L'DEMOERR),DEMOERR                                       
         B     MYCURSOR                                                         
         SPACE 1                                                                
DUPCODE  MVC   CONHEAD(L'DCODEERR),DCODEERR                                     
         B     MYEND                                                            
         SPACE 1                                                                
TOOLARGE MVC   CONHEAD(L'LARGEERR),LARGEERR                                     
         B     MYEND                                                            
         SPACE 1                                                                
NORECORD MVC   CONHEAD(L'FOUNDERR),FOUNDERR                                     
         B     MYEND                                                            
NOCABERR MVC   CONHEAD(L'CABLERR),CABLERR                                       
         B     MYEND                                                            
         SPACE 1                                                                
CODEERR  DC    C'** ERROR ** PROGRAM CODE INVALID OR MISSING'                   
DROPERR  DC    C'** ERROR ** NOTHING TO DROP DOWN'                              
DAYERR   DC    C'** ERROR ** INVALID OR MISSING DAY FIELD'                      
TIMEERR  DC    C'** ERROR ** INVALID OR MISSING TIME FIELD'                     
LINEERR  DC    C'** ERROR ** MISSING INPUT IN REQUIRED FIELDS'                  
DEMOERR  DC    C'** ERROR ** INVALID DEMO INPUT'                                
DCODEERR DC    C'** ERROR ** PROGRAM CODE ALREADY EXISTS'                       
LARGEERR DC    C'** ERROR ** MAX OF 40 PROGRAMS ALLOWED'                        
FOUNDERR DC    C'** ERROR ** RECORD NOT FOUND'                                  
CABLERR  DC    C'** ERROR ** PLAN NOT SET UP FOR CABLE PROGRAMS'                
         EJECT                                                                  
*              SUPPORTING SUBROUTINES                                           
         SPACE 3                                                                
NXTFIELD ZIC   RF,0(R2)                                                         
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
XIT      XIT1                                                                   
         EJECT                                                                  
UNTFILE  DC    CL8'UNTFIL'                                                      
CODEORD  DC    CL8'ADD=CODE'                                                    
*              LTORG FOR THIS PHASE                                             
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEPUPALLN                                                      
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEPUPF9D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEPUPF8D                                                       
         EJECT                                                                  
LINESVE  DS    F                                                                
*                                                                               
GENCKEY  DS    CL32                                                             
SAVEKEY  DS    CL32                                                             
*                                                                               
HOLDRTSH DS    CL3                                                              
HOLDDEM  DS    CL16                                                             
HOLDNAME DS    CL9                                                              
HOLDDAY  DS    CL1                                                              
HOLDTIME DS    CL4                                                              
*                                                                               
DAYHLD   DS    CL1                                                              
COUNT    DS    CL1                                                              
*                                                                               
HEADNAD  DS    CL12                                                             
HEADDEM  DS    CL24                                                             
*                                                                               
DEMCNT   DS    CL1                 NUMBER OF USED DEMOS FROM PLAN               
NDEMCNT  DS    CL1                 NUMBER OF UNUSED DEMOS FROM PLAN             
*                                                                               
FRSTELEM DS    CL4                 PROGRAM CODE OF FIRST ELEM TO LIST           
LASTELEM DS    CL4                 PROGRAM CODE OF LAST ELEMENT READ            
*                                                                               
PLINED   DSECT                                                                  
PNET     DS    CL4                                                              
         DS    CL6                                                              
PDAYPART DS    CL1                                                              
         DS    CL8                                                              
PPLAN    DS    CL8                                                              
PLENGTH  EQU   *-PNET                                                           
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'146NEPUP08   05/01/02'                                      
         END                                                                    
