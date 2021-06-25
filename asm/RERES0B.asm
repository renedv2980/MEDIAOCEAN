*          DATA SET RERES0B    AT LEVEL 067 AS OF 05/01/02                      
*PHASE T8190BA                                                                  
         TITLE 'T8190B - RERES0B - SETS'                                        
***********************************************************************         
*                                                                     *         
*  RERES0B (T8190B) --- INPUT OF SET RECORDS                          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
*   OCT95 (BOB) DATE OF BIRTH IN RESEARCH                             *         
*                                                                     *         
*HERE******************************************************************         
T8190B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T8190B*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
                                                                                
         MVI   ACTELOPT,C'N'       DON'T WANT ACTIVITIES ELEMENT                
         MVI   IOOPT,C'Y'          DO MY OWN I/O'S                              
         OI    GENSTAT4,NODELLST   DON'T ALLOW DELETE IN LIST                   
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    INVLACT             DISPLAY ONLY HERE                            
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         CLI   MODE,PRINTREP       REPORT?                                      
         BE    PR                                                               
         CLI   MODE,RECDEL         DELETE?                                      
         BE    INVLFLD                                                          
                                                                                
NOXIT    LA    R1,1                                                             
         B     *+6                                                              
YESXIT   SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
*                                                                               
*        VALIDATE REP                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
*                                                                               
         USING RREPKEY,R6                                                       
         MVI   RREPKTYP,X'01'      REP RECORD                                   
         MVC   RREPKREP,AGENCY                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
*                                                                               
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
*                                                                               
         USING RREPELEM,R6                                                      
*                                                                               
         MVC   CPAREP,RREPPAR      SAVE PARENT REP CODE                         
*                                                                               
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSETKEY,R6                                                       
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,CPAREP                                                  
                                                                                
         CLI   ACTNUM,ACTREP       FOR REPORT, TYPE FILTER IS OPTIONAL          
         BNE   VK10                                                             
         LA    R2,SETTYPEH         VALIDATE SET TYPE                            
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         B     VK20                                                             
                                                                                
VK10     DS    0H                                                               
         LA    R2,SETTYPEH         VALIDATE SET TYPE                            
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
                                                                                
VK20     DS    0H                                                               
         LA    R3,TYPELIST                                                      
                                                                                
VK30     DS    0H                                                               
         USING TYPLSTD,R3          ESTABLISH TABLE ENTRY                        
*                                                                               
         CLC   TPLSETID,8(R2)                                                   
         BE    VK40                                                             
*                                                                               
         LA    R3,TPLENTL(R3)      BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         CLI   0(R3),X'FF'         CHECK FOR END OF TABLE                       
         BNE   VK30                                                             
         B     INVLFLD                                                          
                                                                                
VK40     DS    0H                  CHECK REP MASTER/SUBSIDIARY                  
         MVC   VALSVKEY,KEY                                                     
         GOTO1 CHECKSUB,DMCB,8(R2)                                              
         MVC   KEY,VALSVKEY                                                     
*                                                                               
*        CLI   REPTYPE,REPSUBS                                                  
*        BNE   VK50                                                             
*        CLC   =C'SP',8(R2)        ALLOW SP AND ST FOR SUBSIDIARIES             
*        BE    VK60                ALSO OF                                      
*        B     INVLSUB                                                          
*                                                                               
VK50     DS    0H                  ONLY ALLOW GS, AD AND AG FOR MASTERS         
*        CLI   REPTYPE,REPMAST     ALSO PP CT DT                                
*        BNE   VK60                                                             
*        CLC   =C'GS',8(R2)                                                     
*        BE    VK60                                                             
*        BNE   INVLMAS                                                          
*                                                                               
VK60     DS    0H                                                               
         MVC   RSETKSET,8(R2)                                                   
         MVC   SETTTL,TPLTTL       DISPLAY TITLE                                
         OI    SETTTLH+6,X'80'     FORCE TRANSMISSON                            
                                                                                
         LA    R2,SETIDENH         VALIDATE SET IDENTIFIER                      
                                                                                
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK70                                                             
         CLI   ACTNUM,ACTREP                                                    
         BE    VK70                                                             
                                                                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
                                                                                
VK70     DS    0H                                                               
         MVC   RSETKID,8(R2)                                                    
         OC    RSETKID,SPACES                                                   
                                                                                
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                  SEE IF ANY CHANGES WERE MADE                 
         CLI   SETDESCH+5,0        MUST BE ENTERED                              
         BE    MISSFLD                                                          
*                                                                               
         TM    SETDESCH+4,X'20'                                                 
         BZ    VR30                                                             
                                                                                
VR10     DS    0H                                                               
         LA    R2,SETFRSTH                                                      
                                                                                
VR20     DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BZ    VR30                                                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    RF,SETENDH                                                       
         CR    R2,RF                                                            
         BNH   VR20                                                             
         B     VRX                 NOTHING WAS CHANGED                          
                                                                                
VR30     DS    0H                  VALIDATE MEMBERS                             
         OI    GENSTAT2,RETEQSEL   RETURN SCREEN TO SHOW CHANGES                
         MVC   VALSVKEY,KEY        VALIDATION ROUTINE CLOBBERS DATAMGR          
         LA    R2,SETFRSTH          INFO                                        
         MVI   NOBLANK,C'N'                                                     
                                                                                
VR40     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VR90                                                             
         MVI   NOBLANK,C'Y'                                                     
                                                                                
         LA    R3,TYPELIST                                                      
                                                                                
VR50     DS    0H                                                               
         USING TYPLSTD,R3          ESTABLISH TABLE ENTRY                        
*                                                                               
         CLC   TPLSETID,SETTYPE                                                 
         BE    VR60                                                             
         LA    R3,TPLENTL(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   VR50                                                             
         B     INVLFLD                                                          
                                                                                
VR60     DS    0H                                                               
*                                                                               
         CLC   =C'MS',SETTYPE      SPECIAL LENGTH CHECK FOR STATION             
         BNE   VR70                                                             
*                                                                               
         CLI   5(R2),7             MUST ACCOUNT FOR '-'                         
         BH    INVLFLD                                                          
         B     VR80                                                             
*                                                                               
VR70     DS    0H                                                               
         CLC   5(1,R2),TPLIDLN     CHECK INPUT LENGTH                           
         BH    INVLFLD                                                          
                                                                                
VR80     DS    0H                                                               
         L     R3,TPLVALA          A(VALIDATION ROUTINE)                        
         GOTO1 (R3),DMCB,0(R2),RR=Y                                             
         BNZ   INVLFLD                                                          
                                                                                
VR90     DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    RF,SETENDH                                                       
         CR    R2,RF                                                            
         BNH   VR40                                                             
                                                                                
         LA    R2,SETFRSTH                                                      
         CLI   NOBLANK,C'Y'                                                     
         BNE   INVLFLD             MUST HAVE AT LEAST ONE ENTRY                 
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         BE    VR100                                                            
                                                                                
         MVC   KEY,VALSVKEY        REESTABLISH RECORD                           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,RSETDCDQ                                                  
         GOTO1 REMELEM                                                          
                                                                                
VR100    DS    0H                  DESCRIPTION                                  
         LA    R6,ELEM                                                          
         USING RSETDESD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RSETDCDE,RSETDCDQ   ELEMENT TYPE                                 
         MVI   RSETDELN,RSETDOV    ELEMENT OVERHEAD LENGTH                      
                                                                                
         LA    R2,SETDESCH                                                      
         CLI   5(R2),0                                                          
         BE    VR110                                                            
                                                                                
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RSETDESC(0),SETDESC                                              
                                                                                
         ZIC   RF,RSETDELN         ADD VARIABLE LENGTH FROM                     
         LA    RF,1(R1,RF)         DESCRIPTION FIELD                            
         STC   RF,RSETDELN                                                      
         DROP  R6                                                               
                                                                                
VR110    DS    0H                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VR120    DS    0H                  YES, SOMETHING WAS CHANGED                   
         CLI   ACTNUM,ACTADD                                                    
         BE    VR130                                                            
         L     R6,AIO                                                           
         MVI   ELCODE,RSETMCDQ                                                  
         GOTO1 REMELEM                                                          
                                                                                
VR130    DS    0H                                                               
         LA    R6,ELEM                                                          
         USING RSETMEMD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RSETMCDE,RSETMCDQ   ELEMENT TYPE                                 
         MVI   RSETMELN,RSETMTOV   ELEMENT OVERHEAD LENGTH                      
                                                                                
         LA    R3,TYPELIST                                                      
         USING TYPLSTD,R3          ESTABLISH TABLE ENTRY                        
                                                                                
VR140    DS    0H                                                               
         CLC   SETTYPE,TPLSETID                                                 
         BE    VR150                                                            
         LA    R3,TPLENTL(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   VR140                                                            
         B     INVLFLD                                                          
                                                                                
VR150    DS    0H                                                               
         MVC   RSETMLEN,TPLIDLN    LENGTH OF EACH MEMBER                        
                                                                                
         LA    R2,SETFRSTH                                                      
         LA    R5,RSETMEMB                                                      
                                                                                
VR160    DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VR210                                                            
                                                                                
         CLC   =C'MS',SETTYPE      SPECIAL FOR STATION                          
         BNE   VR170               MUST OMIT '-' IF ANY                         
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(7),8(R2)                                                    
         OC    WORK,SPACES                                                      
                                                                                
         BAS   RE,COLLAPSE         COLLAPSE AGY/STA CODE FROM SCREEN            
         MVC   0(6,R5),WORK+10                                                  
         B     VR200                                                            
                                                                                
VR170    DS    0H                                                               
         ZIC   R1,RSETMLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,VR180                                                         
         EX    R1,VR190                                                         
         B     VR200                                                            
VR180    MVC   0(0,R5),8(R2)                                                    
VR190    OC    0(0,R5),SPACES                                                   
                                                                                
VR200    DS    0H                                                               
         BAS   RE,CHECKDUP         CHECK IF FIELD IS A DUPLICATE                
         BNZ   DUPLIC8                                                          
                                                                                
         ZIC   R1,RSETMLEN                                                      
         LA    R5,0(R5,R1)                                                      
         ZIC   R0,RSETMELN                                                      
         AR    R0,R1                                                            
         STC   R0,RSETMELN         NEW ELEMENT LENGTH                           
                                                                                
VR210    DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    RF,SETENDH                                                       
         CR    R2,RF                                                            
         BNH   VR160                                                            
         DROP  R6                                                               
                                                                                
         L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,SETFRSTH                                                      
                                                                                
VR220    DS    0H                  SET VALIDATED                                
         OI    4(R2),X'20'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    RF,SETENDH                                                       
         CR    R2,RF                                                            
         BNH   VR220                                                            
                                                                                
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR230                                                            
                                                                                
         GOTO1 ADDREC              ADD THE RECORD/KEY                           
         CLI   DMCB+8,0                                                         
         BE    VRX                                                              
         DC    H'0'                                                             
                                                                                
VR230    DS    0H                                                               
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VRX      DS    0H                                                               
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         LA    R6,KEY                                                           
         USING RSETKEY,R6                                                       
         MVC   SETTYPE,RSETKSET                                                 
         OI    SETTYPEH+6,X'80'    XMIT                                         
         MVC   SETIDEN,RSETKID                                                  
         OI    SETIDENH+6,X'80'    XMIT                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         TWAXC SETDESCH,PROT=Y     CLEAR SCREEN                                 
                                                                                
         L     R6,AIO                                                           
         USING RSETDESD,R6                                                      
                                                                                
         MVI   ELCODE,RSETDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DR10                                                             
                                                                                
         CLI   RSETDELN,RSETDOV    SKIP IF NO DESCRIPTION                       
         BNH   DR10                                                             
         ZIC   R1,RSETDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RSETDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SETDESC(0),RSETDESC                                              
         OI    SETDESCH+6,X'80'    XMIT                                         
         DROP  R6                                                               
                                                                                
DR10     DS    0H                  DISPLAY MEMBERS                              
         L     R6,AIO                                                           
         USING RSETMEMD,R6                                                      
         MVI   ELCODE,RSETMCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R2,SETFRSTH                                                      
         LA    R5,RSETMEMB                                                      
         CLI   RSETMELN,RSETMTOV   MUST HAVE AT LEAST ONE MEMBER                
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   R4,RSETMELN                                                      
                                                                                
DR20     DS    0H                                                               
                                                                                
         CLC   =C'MS',SETTYPE      SPECIAL FOR STATION                          
         BNE   DR30                                                             
         MVC   8(4,R2),0(R5)                                                    
         MVI   5(R2),7             SET FIELD LENGTH                             
                                                                                
DR26     DS    0H                                                               
                                                                                
         MVI   12(R2),C' '                                                      
         LA    RE,8(R2)                                                         
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'          YES, USE XXXX-XX FORMAT                      
                                                                                
         MVC   1(2,RE),=C'TV'                                                   
                                                                                
         CLI   4(R5),C'T'                                                       
         BE    DR40                                                             
         CLI   4(R5),C' '                                                       
         BE    DR40                                                             
                                                                                
         MVC   1(1,RE),4(R5)                                                    
                                                                                
         MVI   2(RE),C' '                                                       
                                                                                
         CLI   1(RE),C'A'          IF RADIO                                     
         BE    *+8                                                              
         CLI   1(RE),C'F'                                                       
         BE    *+8                                                              
         CLI   1(RE),C'C'                                                       
         BNE   *+12                                                             
         MVI   2(RE),C'M'             SET SECOND CHARACTER                      
         B     DR40                                                             
*                                                                               
         CLI   1(RE),C'L'          LOW FREQUENCY                                
         BNE   *+12                                                             
         MVI   2(RE),C'F'                                                       
         B     DR40                                                             
*                                                                               
         B     DR40                                                             
*                                                                               
DR30     DS    0H                                                               
         ZIC   R1,RSETMLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R5)                                                    
                                                                                
DR40     DS    0H                                                               
         OI    6(R2),X'80'         XMIT                                         
                                                                                
         LA    R3,TYPELIST                                                      
         USING TYPLSTD,R3          ESTABLISH TABLE ENTRY                        
                                                                                
DR50     DS    0H                                                               
         CLC   SETTYPE,TPLSETID                                                 
         BE    DR60                                                             
         LA    R3,TPLENTL(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   DR50                                                             
         B     DR80                                                             
                                                                                
DR60     DS    0H                  GET EXPANDED NAME                            
         L     R3,TPLVALA                                                       
         GOTO1 (R3),DMCB,0(R2),RR=Y                                             
         BZ    DR65                                                             
         ZIC   R0,0(R2)            NONE FOUND, BUMP FIELD AND SKIP              
         AR    R2,R0                                                            
         B     DR80                                                             
                                                                                
DR65     DS    0H                                                               
         ZIC   R0,0(R2)            DISPLAY EXPANDED NAME                        
         AR    R2,R0                                                            
                                                                                
         CLC   =C'MS',SETTYPE      SPECIAL FOR STATION                          
         BNE   *+14                                                             
         MVC   8(20,R2),WORK+10       MARKET NAME                               
         B     DR80                                                             
                                                                                
         MVC   8(20,R2),WORK                                                    
                                                                                
DR80     DS    0H                                                               
         ZIC   RF,RSETMLEN         CHECK IF WE'VE DISPLAYED ALL MEMBERS         
         SR    R4,RF                                                            
         LA    RF,RSETMTOV                                                      
         CR    RF,R4                                                            
         BNL   DRX                                                              
                                                                                
         ZIC   R1,RSETMLEN                                                      
         LA    R5,0(R5,R1)                                                      
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    RF,SETENDH                                                       
         CR    R2,RF                                                            
         BNH   DR20                                                             
                                                                                
DRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LIST THE RECORD                                                               
***********************************************************************         
LR       DS    0H                                                               
*                                                                               
         MVI   NLISTS,14           NUMBER OF LINES ON A SCREEN                  
*                                                                               
         OC    SAVEKEY,SAVEKEY     DISPLAY FROM SELECT?                         
         BZ    LR05                                                             
         MVC   KEY,SAVEKEY                                                      
         XC    SAVEKEY,SAVEKEY                                                  
         B     LR10                                                             
                                                                                
LR05     DS    0H                                                               
         OC    KEY(L'RSETKEY),KEY                                               
         BNZ   LR10                                                             
                                                                                
         LA    R6,KEY                                                           
         USING RSETKEY,R6                                                       
                                                                                
         XC    KEY,KEY                                                          
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,CPAREP                                                  
         MVC   RSETKSET,SETTYPE                                                 
                                                                                
         CLI   SETIDENH+5,0                                                     
         BE    LR10                                                             
         MVC   RSETKID,SETIDEN                                                  
         OC    RSETKID,SPACES                                                   
         DROP  R6                                                               
                                                                                
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
LR20     DS    0H                                                               
         CLC   KEY(23),KEYSAVE                                                  
         BNE   LRX                                                              
         CLC   SETTYPE,RSETKSET-RSETKEY+KEY  SET TYPE BREAK                     
         BNE   LRX                                                              
                                                                                
         MVC   LISTAR,SPACES                                                    
                                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSETREC,R6                                                       
         MVC   LSETIDEN,RSETKID                                                 
         DROP  R6                                                               
                                                                                
         USING RSETDESD,R6                                                      
         MVI   ELCODE,RSETDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LRSEQ                                                            
                                                                                
         CLI   RSETDELN,RSETDOV    SKIP IF NO DESCRIPTION                       
         BNH   LR30                                                             
         ZIC   R1,RSETDELN                                                      
         LA    RF,RSETDOV                                                       
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSETDESC(0),RSETDESC                                             
         DROP  R6                                                               
                                                                                
LR30     DS    0H                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
                                                                                
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
                                                                                
LRX      B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       DS    0H                                                               
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
                                                                                
         LA    R6,KEY                                                           
         USING RSETKEY,R6                                                       
                                                                                
         XC    KEY,KEY                                                          
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,CPAREP                                                  
         MVC   RSETKSET,SETTYPE                                                 
                                                                                
         CLI   SETIDENH+5,0                                                     
         BE    PR10                                                             
         MVC   RSETKID,SETIDEN                                                  
         OC    RSETKID,SPACES                                                   
         DROP  R6                                                               
                                                                                
PR10     DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
PR20     DS    0H                                                               
         CLI   SETTYPEH+5,0        NO FILTER ON TYPE                            
         BNE   PR30                                                             
         CLC   KEY(21),KEYSAVE                                                  
         BNE   PRX                                                              
*        CLI   REPTYPE,REPMAST     IF REP IS MASTER                             
*        BNE   PR40                                                             
*        CLC   =C'OF',KEY+21       LOCK OUT THE OFFICE                          
*        BE    PRSEQ                                                            
*        B     PR40                                                             
*                                                                               
PR30     DS    0H                                                               
*        CLC   KEY(23),KEYSAVE                                                  
*        BNE   PRX                                                              
*                                                                               
PR40     DS    0H                                                               
         MVC   VALSVKEY,KEY        VALIDATION ROUTINE CLOBBERS DATAMGR          
                                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSETREC,R6                                                       
                                                                                
         MVC   SVTYPE,RSETKSET                                                  
                                                                                
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(4),=C'TYPE'                                                    
         MVC   P+6(L'SETTYPE),RSETKSET                                          
         MVC   P+17(10),=C'IDENTIFIER'                                          
         MVC   P+29(L'SETIDEN),RSETKID                                          
         BAS   RE,PRINT                                                         
         MVC   P(11),=C'DESCRIPTION'                                            
                                                                                
         L     R6,AIO                                                           
         USING RSETDESD,R6                                                      
                                                                                
         MVI   ELCODE,RSETDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR50                                                             
                                                                                
         CLI   RSETDELN,RSETDOV    SKIP IF NO DESCRIPTION                       
         BNH   PR50                                                             
         ZIC   R1,RSETDELN                                                      
         LA    RF,RSETDOV                                                       
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+13(0),RSETDESC                                                 
         DROP  R6                                                               
                                                                                
PR50     DS    0H                                                               
         BAS   RE,PRINT                                                         
         BAS   RE,PRINT                                                         
                                                                                
         MVC   P(8),=28C'-'                                                     
         MVC   P+9(28),=28C'-'                                                  
         MVC   P+40(8),=28C'-'                                                  
         MVC   P+49(28),=28C'-'                                                 
         BAS   RE,PRINT                                                         
                                                                                
         L     R6,AIO                                                           
         USING RSETMEMD,R6                                                      
         MVI   ELCODE,RSETMCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   PCOUNT,2            TWO MEMBERS PER LINE                         
         LA    R2,P                                                             
         LA    R5,RSETMEMB                                                      
         CLI   RSETMELN,RSETMTOV   MUST HAVE AT LEAST ONE MEMBER                
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    ELEM,ELEM           INIT DUMMY SCREEN FIELD                      
                                                                                
         ZIC   R4,RSETMELN                                                      
                                                                                
PR60     DS    0H                                                               
         ZIC   R1,RSETMLEN                                                      
         STC   R1,ELEM+5           DATA LENGTH                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),0(R5)                                                  
                                                                                
         CLC   =C'MS',SETTYPE                                                   
         BNE   PR68                                                             
                                                                                
PR63     DS    0H                                                               
         MVC   ELEM+8+4(2),SPACES                                               
         LA    RE,ELEM+8                                                        
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'          YES, USE XXXX-XX FORMAT                      
         MVI   ELEM+5,7                                                         
                                                                                
PR65     DS    0H                                                               
         MVC   1(2,RE),=C'TV'                                                   
         CLI   4(R5),C'T'                                                       
         BE    PR68                                                             
         CLI   4(R5),C' '                                                       
         BE    PR68                                                             
         MVC   1(1,RE),4(R5)                                                    
         MVI   2(RE),C'M'                                                       
                                                                                
PR68     DS    0H                                                               
         LA    R3,TYPELIST                                                      
         USING TYPLSTD,R3           ESTABLISH TABLE ENTRY                       
                                                                                
PR70     DS    0H                                                               
         CLC   SVTYPE,TPLSETID                                                  
         BE    PR80                                                             
         LA    R3,TPLENTL(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   PR70                                                             
         B     PR100                                                            
                                                                                
PR80     DS    0H                                                               
         MVC   0(8,R2),ELEM+8      PRINT                                        
         L     R3,TPLVALA                                                       
         GOTO1 (R3),DMCB,ELEM,RR=Y                                              
         BNZ   PR100                                                            
                                                                                
         MVC   9(20,R2),WORK                                                    
                                                                                
PR100    DS    0H                                                               
         MVC   KEY,VALSVKEY        REESTABLISH RECORD                           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
                                                                                
         ZIC   RF,RSETMLEN         CHECK IF WE'VE DISPLAYED ALL MEMBERS         
         SR    R4,RF                                                            
         LA    RF,RSETMTOV                                                      
         CR    RF,R4                                                            
         BNL   PR110                                                            
                                                                                
         ZIC   R1,RSETMLEN         DO TWO MEMBERS PER LINE                      
         LA    R5,0(R5,R1)                                                      
         LA    R2,40(R2)                                                        
         CLI   PCOUNT,1                                                         
         BE    PR105                                                            
         MVI   PCOUNT,1                                                         
         B     PR60                                                             
                                                                                
PR105    BAS   RE,PRINT                                                         
         LA    R2,P                                                             
         MVI   PCOUNT,2            TWO MEMBERS PER LINE                         
         B     PR60                                                             
                                                                                
PR110    DS    0H                                                               
         BAS   RE,PRINT                                                         
                                                                                
PRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     PR20                                                             
                                                                                
PRX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LINE                                                                  
***********************************************************************         
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REPORT HEADLINE SPECS                                                         
***********************************************************************         
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,1,AGYNAME                                                     
         PSPEC H1,76,RUN                                                        
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,76,REQUESTOR                                                  
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* REPORT HEADHOOK ROUTINE                                                       
***********************************************************************         
HOOK     NTR1                                                                   
         L     R6,AIO                                                           
         USING RSETREC,R6                                                       
         CLC   =C'DS',RSETKSET                                                  
         BNE   HOOK10                                                           
         MVC   H6(19),=C'DAYPART MENU REPORT'                                   
         B     HOOK80                                                           
                                                                                
HOOK10   DS    0H                                                               
                                                                                
         CLC   =C'MS',RSETKSET                                                  
         BNE   HOOK20                                                           
         MVC   H6(25),=C'STATIONS IN MARKET REPORT'                             
         B     HOOK80                                                           
                                                                                
HOOK20   DS    0H                                                               
                                                                                
HOOK80   DS    0H                                                               
         GOTO1 CENTER,DMCB,H6,88                                                
                                                                                
HOOKX    B     EXIT                                                             
         DROP  R6                                                               
         TITLE 'T8190B - RERES0B - SETS - DAYPART SET - VALDPT'                 
***********************************************************************         
* VALIDATE DAYPART CODE                                                         
* P1 HAS DPT CODE                                                               
* WORK HAS SHORT AND LONG DAYPART CODES CL3,CL15                                
* USES AIO2                                                                     
***********************************************************************         
VALDPT   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         XC    KEY,KEY             GET DAYPART DESCRIPTIONS                     
         LA    R6,KEY                                                           
         USING RRDPREC,R6                                                       
         MVI   RRDPKTYP,RRDPKIDQ                                                
         MVC   RRDPKREP,CPAREP                                                  
         MVC   RRDPKDPT,8(R2)                                                   
         OC    RRDPKDPT,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RRDPKEY),KEYSAVE                                           
         BNE   NOXIT                                                            
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RRDPREC,R6                                                       
         MVC   WORK(L'RRDPSNAM),RRDPSNAM                                        
         MVC   WORK+L'RRDPSNAM+5(L'RRDPLNAM),RRDPLNAM                           
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YESXIT                                                           
                                                                                
         TITLE 'T8101B - RERES0B - SETS - DAYPART SET - VALSTA'                 
***********************************************************************         
*                                                                     *         
*        VALIDATE STATION CALL LETTERS                                *         
*        P1 HAS STATION CALL LETTERS                                  *         
*                                                                     *         
***********************************************************************         
VALSTA   NTR1  LABEL=*                                                          
*                                  **********************************           
*                                  *                                *           
*                                  *           STATION              *           
*                                  *                                *           
*                                  **********************************           
*                                                                               
*        READ STATION FILE TO VALIDATE STATION                                  
*                                                                               
         L     R2,0(R1)            POINT TO STATION FIELD                       
*                                                                               
         XC    KEY,KEY             ESTABLISH STATION RECORD KEY                 
         LA    R3,KEY                                                           
         USING RSTAKEY,R3                                                       
*                                                                               
         MVI   RSTAKTYP,X'02'      RECORD TYPE                                  
         MVC   RSTAKREP,AGENCY     REP ID                                       
         MVC   RSTAKSTA,8(R2)      STATION                                      
         OC    RSTAKSTA,SPACES                                                  
*                                                                               
         LA    RE,8(R2)            POINT TO STATION CALL LETTERS                
*                                                                               
         CLI   0(RE),C'-'                                                       
         BE    *+20                                                             
         CLI   0(RE),C' '                                                       
         BE    VSTASTE5            NO BROADCAST BAND = TV                       
         LA    RE,1(RE)                                                         
         B     *-20                                                             
*                                  BROADCAST BAND PRESENT                       
         MVC   RSTAKSTA+4(1),1(RE) COPY BROADCAST BAND                          
*                                                                               
         CLI   RSTAKSTA+4,C'T'     MEDIA IS BLANK FOR ANY TV STATION            
         BE    VSTASTE5                                                         
         CLI   RSTAKSTA+4,X'F0'    IE. MEDIA= T,1-9                             
         BL    VSTASTE9                                                         
         CLI   RSTAKSTA+4,X'F9'                                                 
         BH    VSTASTE9                                                         
*                                                                               
VSTASTE5 DS    0H                                                               
*                                                                               
         MVI   RSTAKSTA+4,C' '                                                  
*                                                                               
VSTASTE9 DS    0H                                                               
*                                                                               
         GOTO1 HIGH                READ FOR STATION POINTER                     
*                                                                               
         CLC   RSTAKEY,KEYSAVE     MUST FIND STATION                            
         BNE   VSTASTNV                                                         
*                                                                               
         B     VALSTAX                                                          
*                                                                               
*        ERROR ROUTINES                                                         
*                                                                               
VSTASTNV DS    0H                  STATION NOT ON FILE                          
*                                                                               
         MVC   CONHEAD(L'VSTASTNM),VSTASTNM                                     
*                                                                               
         B     VSTAERR                                                          
*                                                                               
VSTASTNM DC    C'** ERROR ** STATION NOT ON FILE'                               
*                                                                               
VSTASTXE DS    0H                  STATION ID TOO LONG                          
*                                                                               
         MVC   CONHEAD(L'VSTASTXM),VSTASTXM                                     
*                                                                               
         B     VSTAERR                                                          
*                                                                               
VSTASTXM DC    C'** ERROR ** STATION CALL LETTERS ARE 3-4 LONG'                 
*                                                                               
VSTAERR  DS    0H                                                               
*                                                                               
         MVI   ERROR,SUPPLIED      ERROR MESSAGE SUPPLIED                       
*                                                                               
         GOTO1 ERREX2                                                           
*                                                                               
VALSTAX  DS    0H                                                               
         CR    RB,RB                                                            
         XIT1                                                                   
*                                                                               
         TITLE 'T8190B - RERES0B - SETS '                                       
***********************************************************************         
* COLLAPSES AGENCY/STATION XXXX-XX TO XXXXXX                                    
* WORK HAS INPUT                                                                
* WORK+10 HAS OUTPUT                                                            
***********************************************************************         
COLLAPSE NTR1                                                                   
         LA    RE,WORK                                                          
COLASP10 CLI   0(RE),C'-'                                                       
         BE    COLASP20                                                         
         CLI   0(RE),C' '                                                       
         BE    COLASP40                                                         
         LA    RE,1(RE)                                                         
         B     COLASP10                                                         
                                                                                
COLASP20 DS    0H                                                               
         CLC   =C'MS',SETTYPE      SPECIAL FOR STATION                          
         BNE   COLASP23                                                         
         CLC   =C'TV',1(RE)                                                     
         BE    COLASP25                                                         
         CLI   1(RE),C'T'                                                       
         BE    COLASP25                                                         
         MVC   WORK+14(1),1(RE)       BAND                                      
         B     COLASP25                                                         
                                                                                
COLASP23 DS    0H                                                               
         MVC   WORK+14(2),1(RE)       OFFICE                                    
                                                                                
COLASP25 DS    0H                                                               
         LA    RF,WORK+1                                                        
         SR    RE,RF                                                            
         EX    RE,COLASP30                                                      
         B     COLASPX                                                          
                                                                                
COLASP30 MVC   WORK+10(0),WORK                                                  
                                                                                
COLASP40 DS    0H                                                               
         CLC   =C'MS',SETTYPE      SPECIAL FOR STATION                          
         BE    COLASP50                                                         
         MVC   WORK+10(4),WORK                                                  
         B     COLASPX                                                          
                                                                                
COLASP50 DS    0H                                                               
         MVC   WORK+10(5),WORK                                                  
                                                                                
COLASPX  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK IF FIELD INPUT IS A DUPLICATE                                           
* R2 POINTS TO FIELD HEADER                                                     
***********************************************************************         
CHECKDUP NTR1                                                                   
         LA    R3,SETFRSTH                                                      
                                                                                
CHKDUP10 DS    0H                                                               
         CR    R3,R2                                                            
         BNL   YESXIT                                                           
         CLC   8(L'SETFRST,R3),8(R2)                                            
         BE    NOXIT                                                            
         ZIC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         ZIC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     CHKDUP10                                                         
         EJECT                                                                  
***********************************************************************         
* CHECK IF REP IS A SUBSIDIARY                                                  
* ONLY ALLOW SP ACTION FOR SUBSIDIARIES AND ONLY ALLOW                          
* GS, AD AND AG FOR MASTERS                                                     
* P1=A(SET TYPE)                                                                
* USES AIO2                                                                     
***********************************************************************         
CHECKSUB NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         MVI   REPTYPE,REPREP      DEFAULT TO NEITHER MASTER NOR SUBS           
         XC    KEY,KEY                                                          
                                                                                
         LA    R6,KEY                                                           
         USING RREPKEY,R6                                                       
         MVI   RREPKTYP,X'01'      REP RECORD                                   
         MVC   RREPKREP,AGENCY                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RREPREC,R6                                                       
                                                                                
         OC    RREPMAST,RREPMAST                                                
         BZ    CHKSUBX                                                          
         CLC   RREPMAST,SPACES                                                  
         BE    CHKSUBX                                                          
                                                                                
         MVI   REPTYPE,REPSUBS                                                  
         CLC   RREPMAST,=X'FFFF'                                                
         BNE   CHKSUBX                                                          
         MVI   REPTYPE,REPMAST                                                  
                                                                                
CHKSUBX  DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREND                                                           
*                                                                               
INVLACT  MVI   ERROR,INVRCACT      INVALID RECORD/ACTION COMBO                  
         LA    R2,CONACTH          CURSOR TO ACTION FIELD                       
         B     ERREND                                                           
*                                                                               
DUPLIC8  MVI   ERROR,DUPLICAT                                                   
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*  TYPE                       CL2                                               
*  TYPE LENGTH                XL1                                               
*  SPARE                      XL1                                               
*  TYPE VALIDATION ROUTINE    A                                                 
*  TYPE DECRIPTION            CL20                                              
         DS    0D                                                               
TYPELIST DS    0CL28                                                            
         DC    C'DP',AL1(1),XL1'00',AL4(VALDPT),CL20'    DAYPART MENU'          
TYPELSTL EQU   *-TYPELIST          LENGTH OF TABLE ENTRY                        
         DC    C'MS',AL1(5),XL1'00',AL4(VALSTA)                                 
         DC    CL20' STATIONS IN MARKET'                                        
*                                                                               
         DC    X'FF'               EOT                                          
         TITLE 'T8190B - RERES0B - SETS - DAYPART SET - TYPLSTD'                
***********************************************************************         
*                                                                     *         
*        TYPELIST DSECT                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TYPLSTD  DSECT                                                                  
TPLENTRY DS    0X                  TABLE ENTRY                                  
TPLSETID DS    CL2                 SET TYPE IDENTIFIER                          
TPLIDLN  DS    AL1                 SET ID LENGTH                                
         DS    XL1                 SPARE                                        
TPLVALA  DS    A                   A(VALIDATION ROUTINE)                        
TPLTTL   DS    CL20                SCREEN TITLE                                 
TPLENTL  EQU   *-TPLENTRY          TABLE ENTRY LENGTH                           
*                                                                               
         TITLE 'T8190B - RERES0B - SETS - DAYPART SET'                          
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RERESWRK                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RERESFBD          (OUR MAINTENANCE SCREEN OVERLAY)             
         ORG   CONTAGH                                                          
       ++INCLUDE RERESEBD          (OUR LIST SCREEN OVERLAY)                    
RSETRECD DSECT                                                                  
       ++INCLUDE REGENSET                                                       
RRDPRECD DSECT                                                                  
       ++INCLUDE REGENRDP                                                       
RESTARECD      DSECT                                                            
*REGENSTA                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
*                                                                               
* APPLICATION WORK AREA                                                         
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
SAVEKEY  DS    CL(L'KEY)                                                        
VALSVKEY DS    CL(L'KEY)           KEY SAVED IN VALIDATION ROUTINES             
NOBLANK  DS    X                   Y/N                                          
SVTYPE   DS    CL2                                                              
REPTYPE  DS    C                                                                
PCOUNT   DS    X                                                                
REPMAST  EQU   C'M'                THIS IS A MASTER                             
REPSUBS  EQU   C'S'                THIS IS A SUBSIDIARY                         
REPREP   EQU   C'R'                THIS IS NEITHER                              
         EJECT                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSETIDEN DS    CL5                                                              
         DS    CL7                                                              
LSETDESC DS    CL60                                                             
*                                                                               
* OFFLINE LIST LINE                                                             
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067RERES0B   05/01/02'                                      
         END                                                                    
