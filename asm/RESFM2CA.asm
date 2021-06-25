*          DATA SET RESFM2CA   AT LEVEL 070 AS OF 05/01/02                      
*PHASE T8182CA                                                                  
         TITLE 'T8182C - RESFM2C - SETS'                                        
***********************************************************************         
*                                                                     *         
*  RESFM2C (T8182C) --- INPUT OF SET RECORDS                          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 25OCT93 (SKU) DATE OF BIRTH                                         *         
*                                                                     *         
* 08APR94 (SKU) ADD STATION TO SET                                    *         
*                                                                     *         
* 18JAN95 (SKU) FIX DISPLAY OF NO DESCRIPTION                         *         
*                                                                     *         
* 21FEB95 (SKU) FIX BUG OF NOT FINDING EXPANDED NAME                  *         
*                                                                     *         
* 10OCT96 (SEP) CHANGED TO DISPLAY LOW POWER TV STATIONS              *         
*                                                                     *         
* 10APR97 (JRD) SET OF SETS                                           *         
*                                                                     *         
* 14APR97 (BU ) ACCEPT 'GR' AS IF IT IS 'GS'                          *         
*                                                                     *         
* 24APR98 (BU ) MOVE S/P SET TO MASTER LEVEL                          *         
*                                                                     *         
* 08OCT98 (BU ) ACCEPT STATION SETS FROM MASTER AND COMPANY LEVEL     *         
*                                                                     *         
* 18JAN99 (SKU) ALLOW DDS TO DELETE                                   *         
*                                                                     *         
* 21APR99 (RHV) SETS OF MARKET                                        *         
*                                                                     *         
* 27OCT00 (BU ) FORCE CLEARCHANNEL (NU) TO KRG (K3)                   *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
T8182C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T8182C*,R7,RR=R3                                              
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
         CLI   DDS,C'Y'            IF NOT DDS,                                  
         BE    MAIN00                                                           
         OI    GENSTAT4,NODELLST   DON'T ALLOW DELETE IN LIST                   
                                                                                
MAIN00   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         CLI   MODE,PRINTREP       REPORT?                                      
         BE    PR                                                               
         CLI   DDS,C'Y'                                                         
         BNE   MAIN10                                                           
         CLI   MODE,RECDEL         DELETE?                                      
         BE    DELR                                                             
         B     NO                                                               
MAIN10   DS    0H                                                               
         CLI   MODE,RECDEL         DELETE?                                      
         BE    CANNOTD                                                          
                                                                                
NO       LA    R1,1                                                             
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSETKEY,R6                                                       
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,AGENCY                                                  
                                                                                
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
         CLC   =C'GR',8(R2)        FORCE 'GR' (TEMP) TO 'GS'                    
         BNE   VK25                                                             
         MVC   8(2,R2),=C'GS'                                                   
VK25     EQU   *                                                                
         LA    R3,TYPELIST                                                      
                                                                                
VK30     DS    0H                                                               
         CLC   0(2,R3),8(R2)                                                    
         BE    VK40                                                             
         LA    R3,L'TYPELIST(R3)                                                
         CLI   0(R3),X'FF'                                                      
         BNE   VK30                                                             
         B     INVLFLD                                                          
                                                                                
VK40     DS    0H                  CHECK REP MASTER/SUBSIDIARY                  
         MVC   VALSVKEY,KEY                                                     
         GOTO1 CHECKSUB,DMCB,8(R2)                                              
         MVC   KEY,VALSVKEY                                                     
*                                                                               
         CLC   =C'ST',8(R2)        ALLOW ST FOR MASTER AND SUBS                 
         BE    VK60                                                             
*                                                                               
         CLI   REPTYPE,REPSUBS                                                  
         BNE   VK50                                                             
***      CLC   =C'SP',8(R2)                                                     
***      BE    VK60                                                             
         CLC   =C'OF',8(R2)        ALLOW OF (OFFICE) FOR SUBSIDIARIES           
         BE    VK60                                                             
         CLC   =C'CT',8(R2)        ALLOW CT (CON TYPE) FOR SUBS                 
         BE    VK60                                                             
         B     INVLSUB                                                          
                                                                                
VK50     DS    0H                  FOLLOWING TYPES ARE ALLOWED                  
         CLI   REPTYPE,REPMAST        AS MASTERS ONLY                           
         BNE   VK60                                                             
         CLC   =C'GS',8(R2)                                                     
         BE    VK60                                                             
         CLC   =C'AD',8(R2)                                                     
         BE    VK60                                                             
         CLC   =C'AG',8(R2)                                                     
         BE    VK60                                                             
         CLC   =C'MK',8(R2)                                                     
         BE    VK60                                                             
         CLC   =C'PP',8(R2)                                                     
         BE    VK60                                                             
         CLC   =C'RE',8(R2)                                                     
         BE    VK60                                                             
         CLC   =C'OF',8(R2)        ALLOW OF (OFFICE) FOR MASTER ALSO            
         BE    VK60                                                             
         CLC   =C'CT',8(R2)        ALLOW CT (CONTYP) FOR MASTER ALSO            
         BE    VK60                                                             
         CLC   =C'SP',8(R2)        ALLOW SALESPERSON FOR MASTER ALSO            
         BE    VK60                                                             
         CLC   =C'DT',8(R2)                                                     
         BNE   INVLMAS                                                          
                                                                                
VK60     DS    0H                                                               
         MVC   RSETKSET,8(R2)                                                   
                                                                                
         LA    R2,SETIDENH         VALIDATE SET IDENTIFIER                      
                                                                                
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK70                                                             
         CLI   ACTNUM,ACTREP                                                    
         BE    VK70                                                             
                                                                                
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
                                                                                
         CLC   =C'CT',RSETKSET     CONTRACT TYPE?                               
         BNE   VK70                NO                                           
                                                                                
         CLI   5(R2),2             LONGER THAN TYPE?                            
         BL    INVLFLD                                                          
         CLI   8(R2),C'*'                                                       
         BE    INVLFLD                                                          
                                                                                
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
         LA    R2,SETDESCH                                                      
         CLI   SETDESCH+5,0        MUST BE ENTERED                              
         BE    MISSFLD                                                          
*                                                                               
         TM    SETDESCH+4,X'20'                                                 
         BZ    VR30                                                             
                                                                                
         TM    SETSOFSH+4,X'20'                                                 
         BZ    VR30                                                             
                                                                                
         TM    SETEXCSH+4,X'20'                                                 
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
                                                                                
VR30     DS    0H                                                               
         LA    R2,SETSOFSH                                                      
         CLI   5(R2),0                                                          
         BE    VR32                                                             
         CLI   SETSOFS,C'Y'                                                     
         BE    VR32                                                             
         CLI   SETSOFS,C'N'                                                     
         BNE   INVLFLD                                                          
*                                                                               
VR32     DS    0H                                                               
         LA    R2,SETEXCSH                                                      
         CLI   5(R2),0                                                          
         BE    VR34                                                             
         CLI   SETEXCS,C'Y'                                                     
         BE    VR34                                                             
         CLI   SETEXCS,C'N'                                                     
         BNE   INVLFLD                                                          
*                                                                               
VR34     DS    0H                                                               
         MVC   VALSVKEY,KEY        VALIDATION ROUTINE CLOBBERS DATAMGR          
         LA    R2,SETFRSTH          INFO                                        
         MVI   NOBLANK,C'N'                                                     
                                                                                
VR40     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VR90                                                             
         MVI   NOBLANK,C'Y'                                                     
                                                                                
         CLI   SETSOFS,C'Y'        SET OF SETS?                                 
         BNE   VR48                NO                                           
                                                                                
         CLI   5(R2),4             LENGTH OF IDENTIFIER                         
         BH    INVLFLD                                                          
                                                                                
         GOTO1 =A(VALSOFS),DMCB,8(R2),SETTYPE,RR=Y                              
         BNZ   INVLFLD                                                          
         B     VR90                                                             
                                                                                
VR48     DS    0H                                                               
         LA    R3,TYPELIST                                                      
                                                                                
VR50     DS    0H                                                               
         CLC   0(2,R3),SETTYPE                                                  
         BE    VR60                                                             
         LA    R3,L'TYPELIST(R3)                                                
         CLI   0(R3),X'FF'                                                      
         BNE   VR50                                                             
         B     INVLFLD                                                          
                                                                                
VR60     DS    0H                                                               
         CLC   =C'AG',SETTYPE      SPECIAL LENGTH CHECK FOR AGENCY              
         BE    VR65                                                             
         CLC   =C'ST',SETTYPE      SPECIAL LENGTH CHECK FOR STATION             
         BNE   VR70                                                             
                                                                                
VR65     DS    0H                                                               
         CLI   5(R2),7             MUST ACCOUNT FOR '-'                         
         BH    INVLFLD                                                          
         B     VR80                                                             
                                                                                
VR70     DS    0H                                                               
         CLC   5(1,R2),2(R3)                                                    
         BH    INVLFLD                                                          
                                                                                
VR80     DS    0H                                                               
         L     R3,3(R3)                                                         
         GOTO1 (R3),DMCB,8(R2),RR=Y                                             
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
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
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
                                                                                
         LA    R6,ELEM                                                          
         USING RSET1DES,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   RSET1CDE,X'01'      ELEMENT TYPE                                 
         MVI   RSET1ELN,16         ELEMENT LENGTH                               
         CLI   SETSOFS,C'Y'        SET OF SETS?                                 
         BNE   *+8                 NO                                           
         OI    RSET1FLG,X'80'                                                   
         CLI   SETEXCS,C'Y'        EXCLUSION SET?                               
         BNE   *+8                 NO                                           
         OI    RSET1FLG,X'08'                                                   
                                                                                
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
                                                                                
         CLI   SETSOFS,C'Y'        SET OF SETS?                                 
         BNE   *+12                NO                                           
         MVI   RSETMLEN,4          LENGTH OF EACH MEMBER                        
         B     VR152                                                            
                                                                                
         LA    R3,TYPELIST                                                      
                                                                                
VR140    DS    0H                                                               
         CLC   SETTYPE,0(R3)                                                    
         BE    VR150                                                            
         LA    R3,L'TYPELIST(R3)                                                
         CLI   0(R3),X'FF'                                                      
         BNE   VR140                                                            
         B     INVLFLD                                                          
                                                                                
VR150    DS    0H                                                               
         MVC   RSETMLEN,2(R3)      LENGTH OF EACH MEMBER                        
                                                                                
VR152    DS    0H                                                               
         LA    R2,SETFRSTH                                                      
         LA    R5,RSETMEMB                                                      
                                                                                
VR160    DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VR210                                                            
         CLC   =C'AG',SETTYPE      SPECIAL FOR AGENCY                           
         BE    VR165               MUST OMIT '-' IF ANY                         
         CLC   =C'ST',SETTYPE      SPECIAL FOR STATION                          
         BNE   VR170               MUST OMIT '-' IF ANY                         
                                                                                
VR165    DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(7),8(R2)                                                    
         OC    WORK,SPACES                                                      
                                                                                
         BAS   RE,COLLASPE         COLLASPE AGY/STA CODE FROM SCREEN            
         MVC   0(6,R5),WORK+10                                                  
         B     VR200                                                            
                                                                                
VR170    DS    0H                                                               
         ZIC   R1,RSETMLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,VR180                                                         
         CLC   =C'MK',SETTYPE      FOR MARKET CODES                             
         BNE   VR175                                                            
         ZIC   R1,5(R2)            NULL PADDED, SO USE FLD LEN FOR EX           
         BCTR  R1,0                                                             
VR175    EX    R1,VR190                                                         
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
         XC    SETDESC,SETDESC                                                  
         XC    SETSOFS,SETSOFS                                                  
         XC    SETEXCS,SETEXCS                                                  
                                                                                
         TWAXC SETFRSTH,PROT=Y     CLEAR SCREEN                                 
                                                                                
         L     R6,AIO                                                           
         USING RSET1DES,R6                                                      
                                                                                
         MVI   SETSOFS,C'N'                                                     
         OI    SETSOFSH+6,X'80'                                                 
         MVI   SETEXCS,C'N'                                                     
         OI    SETEXCSH+6,X'80'                                                 
                                                                                
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR02                                                             
                                                                                
         TM    RSET1FLG,X'80'                                                   
         BNO   *+8                                                              
         MVI   SETSOFS,C'Y'                                                     
                                                                                
         TM    RSET1FLG,X'08'                                                   
         BNO   *+8                                                              
         MVI   SETEXCS,C'Y'                                                     
                                                                                
DR02     DS    0H                                                               
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
         CLI   SETSOFS,C'Y'                                                     
         BNE   DR22                                                             
                                                                                
         MVC   8(4,R2),0(R5)                                                    
         B     DR40                                                             
                                                                                
DR22     DS    0H                                                               
         CLC   =C'AG',SETTYPE      SPECIAL FOR AGENCY                           
         BNE   DR25                                                             
         MVC   8(4,R2),0(R5)                                                    
         CLC   4(2,R5),SPACES      IS THERE AN OFFICE?                          
         BE    DR40                                                             
         B     DR26                                                             
                                                                                
DR25     DS    0H                                                               
         CLC   =C'ST',SETTYPE      SPECIAL FOR STATION                          
         BNE   DR30                                                             
         MVC   8(4,R2),0(R5)                                                    
                                                                                
DR26     DS    0H                                                               
         MVI   12(R2),C' '                                                      
         LA    RE,8(R2)                                                         
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'          YES, USE XXXX-XX FORMAT                      
                                                                                
         CLC   =C'ST',SETTYPE                                                   
         BE    DR28                                                             
         MVC   1(2,RE),4(R5)                                                    
         B     DR40                                                             
                                                                                
DR28     DS    0H                                                               
         MVC   1(2,RE),=C'TV'                                                   
         CLI   4(R5),C' '                                                       
         BE    DR40                                                             
         MVC   1(2,RE),=C'L '                                                   
         CLI   4(R5),C'L'                                                       
         BE    DR40                                                             
         MVC   1(1,RE),4(R5)                                                    
         MVI   2(RE),C'M'                                                       
         B     DR40                                                             
                                                                                
DR30     DS    0H                                                               
         ZIC   R1,RSETMLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R5)                                                    
                                                                                
DR40     DS    0H                                                               
         OI    6(R2),X'80'         XMIT                                         
                                                                                
         CLI   SETSOFS,C'Y'        SET OF SETS?                                 
         BNE   DR48                NO                                           
                                                                                
         GOTO1 =A(VALSOFS),DMCB,8(R2),SETTYPE,RR=Y                              
         BZ    DR65                                                             
         ZIC   R0,0(R2)            NONE FOUND, BUMP FIELD AND SKIP              
         AR    R2,R0                                                            
         B     DR80                                                             
                                                                                
DR48     DS    0H                                                               
         LA    R3,TYPELIST                                                      
                                                                                
DR50     DS    0H                                                               
         CLC   SETTYPE,0(R3)                                                    
         BE    DR60                                                             
         LA    R3,L'TYPELIST(R3)                                                
         CLI   0(R3),X'FF'                                                      
         BNE   DR50                                                             
         B     DR80                                                             
                                                                                
DR60     DS    0H                  GET EXPANDED NAME                            
         L     R3,3(R3)                                                         
         GOTO1 (R3),DMCB,8(R2),RR=Y                                             
         BZ    DR65                                                             
         ZIC   R0,0(R2)            NONE FOUND, BUMP FIELD AND SKIP              
         AR    R2,R0                                                            
         B     DR80                                                             
                                                                                
DR65     DS    0H                                                               
         ZIC   R0,0(R2)            DISPLAY EXPANDED NAME                        
         AR    R2,R0                                                            
                                                                                
         CLI   SETSOFS,C'Y'        SET OF SETS?                                 
         BE    DR70                YES                                          
                                                                                
         CLC   =C'GS',SETTYPE                                                   
         BNE   DR70                                                             
         MVC   8(L'RGRPNAME,R2),WORK                                            
         MVI   18(R2),C'/'                                                      
         MVC   19(L'RGRPSBNM,R2),WORK+10                                        
         B     DR80                                                             
                                                                                
DR70     DS    0H                                                               
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
         MVC   RSETKREP,AGENCY                                                  
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
         MVI   BYTE,0                                                           
                                                                                
         LA    R6,KEY                                                           
         USING RSETKEY,R6                                                       
                                                                                
         XC    KEY,KEY                                                          
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,AGENCY                                                  
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
         CLI   REPTYPE,REPMAST     IF REP IS MASTER                             
         BNE   PR40                                                             
***>>    CLC   =C'SP',KEY+21       LOCK OUT THE SALESPERSON                     
***>>    BE    PRSEQ                                                            
         CLC   =C'ST',KEY+21       LOCK OUT THE STATION                         
         BE    PRSEQ                                                            
         CLC   =C'OF',KEY+21       LOCK OUT THE OFFICE                          
         BE    PRSEQ                                                            
         B     PR40                                                             
                                                                                
PR30     DS    0H                                                               
         CLC   KEY(23),KEYSAVE                                                  
         BNE   PRX                                                              
                                                                                
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
                                                                                
         L     R6,AIO                                                           
         USING RSET1DES,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   PR48                                                             
                                                                                
         LA    RE,P+40                                                          
         TM    RSET1FLG,X'08'      EXCLUSION?                                   
         BNO   *+14                                                             
         MVC   0(09,RE),=C'EXCLUSION'                                           
         LA    RE,15(RE)                                                        
                                                                                
         TM    RSET1FLG,X'80'      SET OF SETS?                                 
         BNO   *+14                                                             
         MVC   0(11,RE),=C'SET OF SETS'                                         
         MVI   BYTE,C'Y'                                                        
                                                                                
PR48     DS    0H                                                               
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
                                                                                
         MVI   PCOUNT,2            TOW MEMBERS PER LINE                         
         LA    R2,P                                                             
         LA    R5,RSETMEMB                                                      
         CLI   RSETMELN,RSETMTOV   MUST HAVE AT LEAST ONE MEMBER                
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         ZIC   R4,RSETMELN                                                      
                                                                                
PR60     DS    0H                                                               
         ZIC   R1,RSETMLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R5)                                                    
                                                                                
         CLI   BYTE,C'Y'                                                        
         BE    PR68                                                             
                                                                                
         CLC   =C'AG',SETTYPE                                                   
         BNE   PR62                                                             
         CLC   4(2,R5),SPACES                                                   
         BE    PR68                                                             
         B     PR63                                                             
                                                                                
PR62     DS    0H                                                               
         CLC   =C'ST',SETTYPE                                                   
         BNE   PR68                                                             
                                                                                
PR63     DS    0H                                                               
         MVC   4(2,R2),SPACES                                                   
         LA    RE,0(R2)                                                         
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'          YES, USE XXXX-XX FORMAT                      
                                                                                
         CLC   =C'ST',SETTYPE                                                   
         BE    PR65                                                             
         MVC   1(2,RE),4(R5)                                                    
         B     PR68                                                             
                                                                                
PR65     DS    0H                                                               
         MVC   1(2,RE),=C'TV'                                                   
         CLI   4(R5),C' '                                                       
         BE    PR68                                                             
         MVC   1(2,RE),=C'L '                                                   
         CLI   4(R5),C'L'                                                       
         BE    PR68                                                             
         MVC   1(1,RE),4(R5)                                                    
         MVI   2(RE),C'M'                                                       
                                                                                
PR68     DS    0H                                                               
         CLI   BYTE,C'Y'                                                        
         BNE   PR69                                                             
                                                                                
         GOTO1 =A(VALSOFS),DMCB,0(R2),SVTYPE,RR=Y                               
         BNZ   PR100                                                            
         B     PR90                                                             
                                                                                
PR69     DS    0H                                                               
         LA    R3,TYPELIST                                                      
                                                                                
PR70     DS    0H                                                               
         CLC   SVTYPE,0(R3)                                                     
         BE    PR80                                                             
         LA    R3,L'TYPELIST(R3)                                                
         CLI   0(R3),X'FF'                                                      
         BNE   PR70                                                             
         B     PR100                                                            
                                                                                
PR80     DS    0H                                                               
         L     R3,3(R3)                                                         
         GOTO1 (R3),DMCB,0(R2),RR=Y                                             
         BNZ   PR100                                                            
                                                                                
         CLC   =C'GS',SVTYPE                                                    
         BNE   PR90                                                             
         MVC   9(L'RGRPNAME,R2),WORK                                            
         MVI   19(R2),C'/'                                                      
         MVC   20(L'RGRPSBNM,R2),WORK+10                                        
         B     PR100                                                            
                                                                                
PR90     DS    0H                                                               
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
         CLC   =C'GS',RSETKSET                                                  
         BNE   HOOK10                                                           
         MVC   H6(26),=C'GROUP/SUBGROUP SETS REPORT'                            
         B     HOOK80                                                           
                                                                                
HOOK10   DS    0H                                                               
         CLC   =C'SP',RSETKSET                                                  
         BNE   HOOK20                                                           
         MVC   H6(23),=C'SALESPERSON SETS REPORT'                               
         B     HOOK80                                                           
                                                                                
HOOK20   DS    0H                                                               
         CLC   =C'AD',RSETKSET                                                  
         BNE   HOOK30                                                           
         MVC   H6(22),=C'ADVERTISER SETS REPORT'                                
         B     HOOK80                                                           
                                                                                
HOOK30   DS    0H                                                               
         CLC   =C'AG',RSETKSET                                                  
         BNE   HOOK35                                                           
         MVC   H6(18),=C'AGENCY SETS REPORT'                                    
         B     HOOK80                                                           
                                                                                
HOOK35   DS    0H                                                               
         CLC   =C'MK',RSETKSET                                                  
         BNE   HOOK40                                                           
         MVC   H6(18),=C'MARKET SETS REPORT'                                    
         B     HOOK80                                                           
                                                                                
HOOK40   DS    0H                                                               
         CLC   =C'ST',RSETKSET                                                  
         BNE   HOOKX                                                            
         MVC   H6(19),=C'STATION SETS REPORT'                                   
                                                                                
HOOK80   DS    0H                                                               
         GOTO1 CENTER,DMCB,H6,88                                                
                                                                                
HOOKX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
DELR     NTR1                                                                   
         CLC   =C'DEL',CONACT      DIRECTLY??                                   
         BNE   DELR20                                                           
         L     R6,AIO                                                           
         MVC   KEY(27),0(R6)                                                    
         B     DELR30                                                           
*                                                                               
DELR20   DS    0H                  FROM LIST??                                  
         MVC   KEY,SAVEKEY                                                      
*                                                                               
DELR30   DS    0H                                                               
         CLI   KEY,X'38'                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RSETREC,R6                                                       
         OI    RSETCNTL,X'80'                                                   
         DROP  R6                                                               
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
                                                                                
DELRX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE GROUP/SUBGROUP CODE                                                  
* P1 HAS GROUP CODE                                                             
* WORK HAS EXPANDED GROUP/SUBGROUP NAME                                         
* USES AIO2                                                                     
***********************************************************************         
VALGRP   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RGRPKEY,R6                                                       
         MVI   RGRPKTYP,7                                                       
         MVC   RGRPKREP,AGENCY     REP                                          
         MVC   RGRPKGRP,0(R2)                                                   
         OC    RGRPKGRP,SPACES                                                  
         DROP  R6                                                               
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RGRPREC,R6                                                       
         MVC   WORK(10),RGRPNAME   GROUP NAME                                   
         MVC   WORK+10(10),RGRPSBNM   SUB GROUP NAME                            
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE SALESPERSON CODE                                                     
* P1 HAS SALESPERSON CODE                                                       
* WORK HAS EXPANDED SALESPERSON NAME                                            
* USES AIO2                                                                     
***********************************************************************         
VALSAL   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         XC    KEY,KEY             GET SALESPERSON NAME                         
         LA    R6,KEY                                                           
         USING RSALREC,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,0(R2)                                                   
         OC    RSALKSAL,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSALKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSALREC,R6                                                       
         MVC   WORK(L'RSALNAME),RSALNAME                                        
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION CODE                                                         
* P1 HAS STATION CODE                                                           
* WORK HAS MARKET NAME                                                          
* USES AIO2                                                                     
***********************************************************************         
VALSTA   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(7),0(R2)       LENGTH FOR XXXX-XX                           
         OC    WORK,SPACES                                                      
                                                                                
         XC    KEY,KEY             GET STATION                                  
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,WORK                                                    
                                                                                
         CLI   WORK+4,C'-'         BAND?                                        
         BNE   *+14                                                             
         MVC   RSTAKSTA+4(1),WORK+5                                             
         B     *+16                                                             
         CLI   WORK+3,C'-'                                                      
         BNE   *+8                                                              
         MVI   RSTAKSTA+3,C' '                                                  
                                                                                
         CLI   RSTAKSTA+4,C'T'     TV?                                          
         BNE   *+8                                                              
         MVI   RSTAKSTA+4,C' '                                                  
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'51'        CHECK FOR A MASTER RECORD                    
         BAS   RE,GETEL            MASTER?                                      
         BNZ   VSTA0060            NO  - NOT MASTER                             
         MVC   KEY+RSTAKREP-RSTAREC(2),RSTAMCRC-RSTAMCEL(R6)                    
*                                  MOVE CURRENT REP INTO KEY                    
*                                                                               
         GOTO1 HIGH                READ STA WITH CURRENT REP                    
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   NO                                                               
         MVI   RDUPDATE,C'N'       GET CURRENT REP RECORD                       
         GOTO1 GETREC                                                           
VSTA0060 EQU   *                                                                
         L     R6,AIO                                                           
         USING RSTAREC,R6                                                       
         MVC   WORK(L'RSTAMKT),RSTAMKT                                          
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE MARKET CODE                                                          
* P1 HAS ADVERTISER CODE                                                        
* WORK HAS EXPANDED MARKET NAME                                                 
* USES AIO2                                                                     
***********************************************************************         
VALMKT   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         XC    KEY,KEY             GET ADVERTISER NAME                          
         LA    R6,KEY                                                           
         USING RMKTREC,R6                                                       
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKMKT,0(R2)                                                   
         OC    RMKTKMKT,SPACES                                                  
         MVC   RMKTKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         LA    RF,RMKTKMKT+3       NULL PAD CODE                                
         CLI   0(RF),C' '                                                       
         BNE   *+14                                                             
         MVI   0(RF),0                                                          
         BCTR  RF,0                                                             
         B     *-14                                                             
*                                                                               
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RMKTKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RMKTREC,R6                                                       
         MVC   WORK(L'RMKTNAME),RMKTNAME                                        
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADVERTISER CODE                                                      
* P1 HAS ADVERTISER CODE                                                        
* WORK HAS EXPANDED ADVERTISER NAME                                             
* USES AIO2                                                                     
***********************************************************************         
VALADV   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         XC    KEY,KEY             GET ADVERTISER NAME                          
         LA    R6,KEY                                                           
         USING RADVREC,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,0(R2)                                                   
         OC    RADVKADV,SPACES                                                  
         MVC   RADVKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RADVREC,R6                                                       
         MVC   WORK(L'RADVNAME),RADVNAME                                        
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY CODE                                                          
* P1 HAS AGENCY CODE                                                            
* WORK HAS EXPANDED AGENCY NAME                                                 
* USES AIO2                                                                     
***********************************************************************         
VALAGY   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(7),0(R2)       LENGTH FOR XXXX-XX                           
         OC    WORK,SPACES                                                      
                                                                                
         XC    KEY,KEY             GET ADVERTISER NAME                          
         LA    R6,KEY                                                           
         USING RAGYREC,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
                                                                                
         MVC   RAGYKAGY(6),SPACES                                               
                                                                                
         LA    RE,WORK                                                          
VALAGY10 CLI   0(RE),C'-'                                                       
         BE    VALAGY30                                                         
         CLI   0(RE),C' '          NO '-' MEANS INPUT SHOULD HAVE               
         BE    VALAGY20              NO OFFICE                                  
         LA    RE,1(RE)                                                         
         B     VALAGY10                                                         
                                                                                
VALAGY20 DS    0H                  INCASE USER INPUT XXXXXX                     
         CLC   WORK+4(2),SPACES      INSTEAD OF XXXX-XX                         
         BNE   NO                                                               
                                                                                
VALAGY30 DS    0H                                                               
         BAS   RE,COLLASPE         COLLASPE AGY CODE FROM SCREEN                
         MVC   RAGYKAGY(6),WORK+10                                              
                                                                                
         MVC   RAGYKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RAGYREC,R6                                                       
         MVC   WORK(L'RAGYNAM1),RAGYNAM1                                        
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
                                                                                
         TITLE 'T8182C - RESFM2C - SETS - CONTRACT TYPE - VALCTY'               
***********************************************************************         
* VALIDATE CONTRACT TYPE                                              *         
* P1 HAS CONTRACT TYPE CODE                                           *         
* WORK HAS EXPANDED CONTRACT TYPE DESCRIPTION                         *         
* USES AIO2                                                           *         
***********************************************************************         
VALCTY   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
                                                                                
         XC    KEY,KEY             GET CONTRACT DESCRIPTION                     
         LA    R6,KEY                                                           
         USING RCTYREC,R6                                                       
         MVI   RCTYKTYP,X'32'                                                   
         MVC   RCTYKREP,AGENCY                                                  
         MVC   RCTYKCTY,0(R2)                                                   
         OC    RCTYKCTY,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCTYKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RCTYREC,R6                                                       
*                                                                               
         MVC   WORK(L'RCTYDESC),RCTYDESC RETURN DESCRIPTION                     
                                                                                
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
                                                                                
         TITLE 'T8182C - RESFM2C - SETS - DEVELOP CONT TYP - VALDCT'            
***********************************************************************         
* VALIDATE DEVELOPMENTAL CONTRACT TYPE                                *         
* P1 HAS DEVELOPMENTAL CONTRACT TYPE CODE                             *         
* WORK HAS EXPANDED DEVELOPMENTAL CONTRACT TYPE DESCRIPTION           *         
* USES AIO2                                                           *         
***********************************************************************         
VALDCT   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
                                                                                
         XC    KEY,KEY             GET DEVELOPMENTAL CONTRACT DESC              
         LA    R6,KEY                                                           
         USING RDCTREC,R6                                                       
         MVI   RDCTKTYP,X'3B'                                                   
         MVC   RDCTKREP,AGENCY                                                  
         MVC   RDCTKCTY,0(R2)                                                   
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDCTKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RDCTREC,R6                                                       
*                                                                               
         MVC   WORK(L'RDCTDESC),RDCTDESC   RETURN DESCRIPTION                   
                                                                                
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
                                                                                
         TITLE 'T8182C - RESFM2C - SETS - POINT PERSON - VALPTP'                
***********************************************************************         
* VALIDATE POINT PERSON CODE                                                    
* P1 HAS POINT PERSON CODE                                                      
* WORK HAS EXPANDED POINT PERSON NAME                                           
* USES AIO2                                                                     
***********************************************************************         
VALPTP   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         XC    KEY,KEY             GET POINT PERSON NAME                        
         LA    R6,KEY                                                           
         USING RPTPREC,R6                                                       
         MVI   RPTPKTYP,X'31'                                                   
         MVC   RPTPKREP,AGENCY                                                  
         MVC   RPTPKREC,0(R2)                                                   
         OC    RPTPKREC,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RPTPKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RPTPREC,R6                                                       
         MVC   WORK(L'RPTPNAME),RPTPNAME                                        
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
                                                                                
         TITLE 'T8182C - RESFM2C - SETS - OFFICE - VALREP'                      
***********************************************************************         
* VALIDATE OFFICE CODE                                                          
* P1 HAS OFFICE CODE                                                            
* WORK HAS EXPANDED OFFICE NAME                                                 
* USES AIO2                                                                     
***********************************************************************         
VALOFF   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         XC    KEY,KEY             GET OFFICE NAME                              
         LA    R6,KEY                                                           
         USING ROFFREC,R6                                                       
         MVI   ROFFKTYP,X'04'                                                   
         MVC   ROFFKREP,AGENCY                                                  
         MVC   ROFFKOFF,0(R2)                                                   
         OC    ROFFKOFF,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'ROFFKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING ROFFREC,R6                                                       
         MVC   WORK(L'ROFFNAME),ROFFNAME                                        
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
                                                                                
         TITLE 'T8182C - RESFM2C - SETS - REP - VALREP'                         
***********************************************************************         
* VALIDATE REP CODE                                                             
* P1 HAS REP CODE                                                               
* WORK HAS EXPANDED REP NAME                                                    
* USES AIO2                                                                     
***********************************************************************         
VALREP   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         XC    KEY,KEY             GET REP NAME                                 
         LA    R6,KEY                                                           
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         MVC   RREPKREP,0(R2)                                                   
         OC    RREPKREP,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RREPREC,R6                                                       
         MVC   WORK(L'RREPNAME),RREPNAME                                        
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
                                                                                
         TITLE 'T8182C - RESFM2C - SETS '                                       
***********************************************************************         
* COLLASPES AGENCY/STATION XXXX-XX TO XXXXXX                                    
* WORK HAS INPUT                                                                
* WORK+10 HAS OUTPUT                                                            
***********************************************************************         
COLLASPE NTR1                                                                   
         LA    RE,WORK                                                          
COLASP10 CLI   0(RE),C'-'                                                       
         BE    COLASP20                                                         
         CLI   0(RE),C' '                                                       
         BE    COLASP40                                                         
         LA    RE,1(RE)                                                         
         B     COLASP10                                                         
                                                                                
COLASP20 DS    0H                                                               
         CLC   =C'ST',SETTYPE      SPECIAL FOR STATION                          
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
         CLC   =C'ST',SETTYPE      SPECIAL FOR STATION                          
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
         BNL   YES                                                              
         CLC   8(L'SETFRST,R3),8(R2)                                            
         BE    NO                                                               
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
*                                                                               
*   CLEARCHANNEL HARDCODE:  REP 'NU' IS TO BE CONSIDERED A 'SUBSID'             
*        OF KATZ RADIO, EVEN THOUGH THE REP RECORD INDICATES OTHER-             
*        WISE.                                                                  
*                                                                               
         CLC   =C'NU',AGENCY       CLEARCHANNEL RADIO?                          
         BNE   CSUB0020            NO                                           
         MVC   RREPMAST,=C'K3'     YES - SET MASTER TO KATZ RADIO               
CSUB0020 EQU   *                                                                
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
***********************************************************************         
* VALIDATE SET NAME                                                             
* P1 HAS SET NAME                                                               
* P2 HAS SET TYPE                                                               
* WORK HAS EXPANDED SET DESCRIPTION                                             
* USES AIO2                                                                     
***********************************************************************         
VALSOFS  NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
                                                                                
         XC    WORK,WORK                                                        
         XC    KEY,KEY             GET SET                                      
         LA    R6,KEY                                                           
         USING RSETREC,R6                                                       
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,AGENCY                                                  
         MVC   RSETKSET,0(R3)                                                   
         MVC   RSETKID,0(R2)                                                    
         OC    RSETKID,SPACES                                                   
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSETKEY),KEYSAVE                                           
         BNE   NO                                                               
                                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSET1DES,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   VALSOFS2                                                         
                                                                                
         TM    RSET1FLG,X'80'      SET OF SETS?                                 
         BO    SOFSINV             YES - INVALID                                
                                                                                
VALSOFS2 DS    0H                                                               
         L     R6,AIO                                                           
         USING RSETDESD,R6                                                      
         MVI   ELCODE,RSETDCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VALSOFSX            NO DESCRIPTION - BAIL OUT                    
                                                                                
         ZIC   R1,RSETDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RSETDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),RSETDESC                                                 
         DROP  R6                                                               
                                                                                
VALSOFSX DS    0H                                                               
         MVC   AIO,AIO1            RESTORE IOAREA                               
         B     YES                                                              
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
SOFSINV  MVC   RERROR,=AL2(700)                                                 
         B     ERREND                                                           
*                                                                               
DUPLIC8  MVC   RERROR,=AL2(401)                                                 
         B     ERREND                                                           
*                                                                               
INVLSUB  MVC   RERROR,=AL2(402)                                                 
         B     ERREND                                                           
*                                                                               
INVLMAS  MVC   RERROR,=AL2(403)                                                 
         B     ERREND                                                           
*                                                                               
CANNOTD  MVC   RERROR,=AL2(405)                                                 
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*  TYPE                       CL2                                               
*  TYPE LENGTH                XL1                                               
*  TYPE VALIDATION ROUTINE    XL4                                               
TYPELIST DS    0CL7                                                             
         DC    C'GS',AL1(2),AL4(VALGRP)                                         
         DC    C'SP',AL1(3),AL4(VALSAL)                                         
         DC    C'AD',AL1(4),AL4(VALADV)                                         
         DC    C'AG',AL1(6),AL4(VALAGY)                                         
         DC    C'ST',AL1(5),AL4(VALSTA)                                         
         DC    C'PP',AL1(3),AL4(VALPTP)                                         
         DC    C'CT',AL1(1),AL4(VALCTY)                                         
         DC    C'DT',AL1(2),AL4(VALDCT)                                         
         DC    C'OF',AL1(2),AL4(VALOFF)                                         
         DC    C'RE',AL1(2),AL4(VALREP)                                         
         DC    C'MK',AL1(4),AL4(VALMKT)                                         
         DC    X'FF'                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMEDD          (OUR MAINTENANCE SCREEN OVERLAY)             
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMEED          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE REGENSET                                                       
       ++INCLUDE REGENGRP                                                       
       ++INCLUDE REGENSAL                                                       
       ++INCLUDE REGENADV                                                       
       ++INCLUDE REGENAGY                                                       
       ++INCLUDE REGENMKT                                                       
       ++INCLUDE REGENSTA                                                       
       ++INCLUDE REGENPTP                                                       
       ++INCLUDE REGENCTY                                                       
       ++INCLUDE REGENDCT                                                       
       ++INCLUDE REGENOFF                                                       
         PRINT ON                                                               
*                                                                               
* APPLICATION WORK AREA                                                         
*                                                                               
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
**PAN#1  DC    CL21'070RESFM2CA  05/01/02'                                      
         END                                                                    
