*          DATA SET PPBUY04    AT LEVEL 056 AS OF 02/26/20                      
*PHASE T41104B                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY04 - EDIT POOL ALLOCATIONS'                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* JSAY 08/30/19 SUPPORT EXTENDED ALLOCATIONS ON ES2 SCREEN                      
*                                                                               
* KWAN 10/11/11 ACTIVITY IS NOT RECORDED WHEN ALLOCATION NOT CHANGED            
*                                                                               
* KWAN 10/06/06 PURCHASE ORDER #                                                
*                                                                               
* KWAN 09/23/05 ADBUYER - DRAFT INS UPLOAD MODE, DON'T ADD RECORD               
*                                                                               
* BPLA 03/01    CHANGES FOR SERIAL NUMBERS                                      
*                                                                               
* KWAN 03/05/01 RELINK WITH MODIFIED PPBUYWRK1 (4000K BUY REC)                  
*                                                                               
* BPLA 10/00    DISALLOW ALLOCATIONS TO PRD AAA                                 
*                                                                               
* BPLA 12/95    CHANGE VDTCNV GOTO'S TO VDATCON                                 
*               NOTE THAT PARAMETER LIST CHANGES                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41104   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORK04X-WORK04D,T41104,RR=R2,CLEAR=YES                           
*                                                                               
         LR    R9,RC                                                            
         USING WORK04D,R9          R9 = A(GLOBAL STORAGE)                       
*                                                                               
         BASR  R8,0                                                             
         AHI   R8,GLOBALS-*                                                     
         USING GLOBALS,R8          R8 = A(GLOBAL LITERALS)                      
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
******** L     RF,ACOMFACS                                                      
******** L     RF,(CUNSCAN-COMFACSD)(RF)                                        
******** STCM  RF,15,VUSCAN                                                     
*                                                                               
         MVI   ZZZTPASS,C'N'                                                    
         CLI   4(R1),C'T'         SET IN PPBUY14 FOR TEST PASS                  
         BNE   *+8                                                              
         MVI   ZZZTPASS,C'Y'                                                    
*                                                                               
* BUILD ALLOCATION TABLE WITH PAID/BILLED STATUS, THEN DELETE PRD ELEMS         
*                                                                               
         LA    R6,SVALLOCA        INIT SAVED ALLOCATIONS                        
         LHI   R7,L'SVALLOCA                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R6,RE                                                            
*                                                                               
         LA    R5,NEWREC+33                                                     
         ST    R5,SV1STELM                                                      
         OC    INSDA,INSDA         NEW BUY?                                     
         BZ    POL4                                                             
         LA    R5,REC+33                                                        
         ST    R5,SV1STELM                                                      
*                                                                               
         MVI   BYTE,0              RESET SWITCH                                 
         MVI   ELCODE,X'25'        FIND PAY ELEM                                
         L     R5,SV1STELM                                                      
         BRAS  RE,NXTELEM                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    2(3,R5),2(R5)       PAID?                                        
         BZ    *+8                                                              
         MVI   BYTE,C'P'           SET SWITCH                                   
*                                                                               
         LA    R6,SVALLOCA                                                      
POL2A    MVI   ELCODE,X'21'                                                     
         L     R5,SV1STELM         POINT TO FIRST ELEM                          
         BRAS  RE,NXTELEM                                                       
         BNE   POL4                                                             
         MVC   0(4,R6),2(R5)       MOVE PRD CODE + COST SHARE                   
*                                                                               
         GOTO1 VRECUP,DMCB,(1,REC),(R5)                                         
         CLI   BYTE,C'P'                                                        
         BNE   POL2B                                                            
         OI    4(R6),X'80'         SET IND                                      
*                                                                               
POL2B    MVI   ELCODE,X'26'        FIND BILL ELEM                               
         L     R5,SV1STELM                                                      
POL2C    BRAS  RE,NXTELEM                                                       
         BNE   POL2D                                                            
         CLC   2(3,R5),0(R6)       RIGHT PRD?                                   
         BNE   POL2C                                                            
         OC    2+3(3,R5),2+3(R5)   BILLED?                                      
         BZ    POL2C                                                            
         OI    4(R6),X'40'         SET BILLED IND                               
*                                                                               
POL2D    LA    R6,5(R6)            NEXT ENTRY IN ALLOCATION TABLE               
         B     POL2A                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
POL4     L     R2,TRADDR                                                        
         CLC   =C'DL',TRCODE       INSERTION DELETION?                          
         BNE   *+14                                                             
         XC    SVINSDAT,SVINSDAT   CLEAR NEW INSERTION DATE IND                 
         B     POL16               GO DELETE PASSIVE POINTERS                   
*                                                                               
         CLI   SVSCRN,X'FE'        WSJ SCREEN?                                  
         BNE   *+12                                                             
         LHI   RE,9                9 FIELDS TO ALLOCATION LINE                  
         B     *+10                                                             
         LH    RE,SVNTRNS                                                       
         BCTR  RE,0                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT R2 TO ALLOCATION DATA                  
         BCT   RE,*-6                                                           
         LA    R3,NOALLERR                                                      
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
*                                                                               
         MVI   ALLOMOD,0                                                        
         TM    4(R2),X'20'         FIELD MODIFIED?                              
         BO    *+8                                                              
         MVI   ALLOMOD,C'Y'        SET ALLOCATIONS MODIFIED                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LA    R1,PARS             SET USCAN PARMS TO EDIT ALLOCATION           
         XC    0(20,R1),0(R1)                                                   
*                                                                               
         CLC   =C'C ',TRCODE       CHANGING ZZZ?                                
         JE    POL4E                                                            
*                                                                               
         TM    GENBYSW1,ALLOEXTQ   HAVE EXTENDED ALLOCATION?                    
         JZ    POL4E                                                            
*                                                                               
         CLI   REC+3,X'07'         EST REC AVAILABLE?                           
         JNE   *+12                NO , READ EST REC                            
         LA    R5,REC+33                                                        
         J     POL4A                                                            
*                                                                               
         XC    KEY,KEY             READ EST RECORD                              
         MVC   KEY(7),NEWREC       A/M/CLT                                      
         MVI   KEY+3,7             REC TYPE                                     
         MVC   KEY+7(3),=C'ZZZ'    PRD                                          
         MVC   KEY+10(2),BEST      ESTIMATE                                     
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEYSAVE(25),KEY     KEY NOT MATCHED?                             
         BNE   POL4E               YES, CONTINUE                                
         MVC   AREC,AWRKREC        GET EST RECORD                               
         BRAS  RE,PRT_GETR                                                      
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         L     R5,AWRKREC                                                       
         LA    R5,33(R5)                                                        
*                                                                               
POL4A    MVC   WKESTALO,SPACES     INIT TEMP WORK EST ALLOCS                    
         MVC   WKESTALX,SPACES                                                  
         MVC   WKESTALO(L'SVESTALO),SVESTALO                                    
*                                                                               
         USING PESTALLX,R5                                                      
POL4B    MVI   ELCODE,X'66'        FIND ALLOCATION EXTENSION ELEMENT            
         BRAS  RE,NXTELEM                                                       
         BNE   POL4E               IF NOT FOUND, CONTINUE                       
         CLC   PEALLZZZ,SPACES                                                  
         JNH   POL4E                                                            
         MVC   WKESTALO+L'SVESTALO(L'PEALLZZZ),PEALLZZZ                         
         DROP  R5                                                               
*                                                                               
         LA    RF,L'WKESTALO       COMBINE ALLOCATIONS FROM SVESTALO            
         LA    RE,WKESTALO         AND PEALLZZZ AND CALCULATE TOTAL             
         LA    R5,WKESTALX         LENGTH OF DATA                               
         SR    R0,R0                                                            
POL4C    CLI   0(RE),X'40'                                                      
         JNH   POL4D                                                            
         MVC   0(1,R5),0(RE)                                                    
         LA    R5,1(R5)                                                         
         AHI   R0,1                                                             
*                                                                               
POL4D    LA    RE,1(RE)                                                         
         BCT   RF,POL4C                                                         
*                                                                               
         STC   R0,USTRNG+1         STORE LENGTH OF DATA                         
*                                                                               
         LA    RE,WKESTALX         STORE ADDRESS OF INPUT DATA                  
         ST    RE,UADDR-1                                                       
*                                                                               
         MVI   UFRST,X'FF'                                                      
*                                                                               
         J     POL4F                                                            
*                                                                               
POL4E    LA    RE,8(R2)            SET INPUT ADDRESS                            
         ST    RE,UADDR-1                                                       
         MVI   UFRST,X'FF'                                                      
         MVC   USTRNG+1(1),5(R2)   SET STRING LENGTH                            
*                                                                               
POL4F    MVI   BYTE,0              RESET SWITCH                                 
         LA    R1,PARS             SET USCAN PARMS TO EDIT ALLOCATION           
         MVI   UVAL,X'80'                                                       
         MVI   USCN1,C'/'                                                       
         L     RF,VUSCAN           IF NO DIGIT, EQUAL COST/SPACE SHARES         
         BASR  RE,RF                                                            
         CLI   USTOP,C'/'                                                       
         BE    *+12                                                             
         MVI   UFRST,X'FF'                                                      
         B     EDT2                                                             
*                                                                               
         LA    R3,TOTSHERR                                                      
         BRAS  RE,EDTSHR2          VALIDATE FIELD                               
         STC   R0,BYTE                                                          
         OI    BYTE,X'80'                                                       
*                                                                               
EDT2     MVC   USCN1(2),=X'6B60'   STOP ON , OR -                               
*                                                                               
         XC    WORK,WORK           BUILD ELEMENT                                
         MVC   WORK(2),=X'2107'                                                 
*                                                                               
         LA    R3,PRERR            VALIDATE PRD CODE                            
         MVI   UVAL,0                                                           
         LA    R1,PARS             SET USCAN PARMS TO EDIT ALLOCATION           
         L     RF,VUSCAN                                                        
         BASR  RE,RF                                                            
         L     R4,UADDR-1                                                       
         CLC   =C'ZZZ',0(R4)                                                    
         BE    ERROR                                                            
         CLC   =C'AAA',0(R4)       CAN'T ALLOCATE TO AAA                        
         BE    ERROR                                                            
         CLI   0(R4),C'*'          NO OTHER AGENCY BUYS                         
         BE    ERROR                                                            
         MVC   WORK+2(3),0(R4)                                                  
         MVI   WORK+5,1            SET DEFAULT SHARES                           
         MVI   WORK+6,1                                                         
         CLI   ULNGTH+1,3                                                       
         BH    ERROR                                                            
         BE    *+16                                                             
         CLI   ULNGTH+1,2                                                       
         BL    ERROR                                                            
         MVI   WORK+4,C' '                                                      
         TM    UVAL,X'01'                                                       
         BO    ERROR                                                            
*                                                                               
         TM    BYTE,X'80'          COST SHARE REQUIRED?                         
         BNZ   EDT4                                                             
         CLI   USTOP,C'-'                                                       
         BE    ERROR                                                            
         B     EDT8                                                             
*                                                                               
EDT4     LA    R3,CSTSHERR                                                      
         BRAS  RE,EDTSHR                                                        
         STC   R0,WORK+5                                                        
         CLI   USTOP,C'-'          SPACE SHARE ENTERED?                         
         BNE   *+12                                                             
*                                                                               
         LA    R3,SPCSHERR                                                      
         BRAS  RE,EDTSHR                                                        
         LTR   R0,R0               SPACE SHARE IS ZERO?                         
         BE    ERROR                                                            
         STC   R0,WORK+6                                                        
*                                                                               
EDT8     L     R5,SV1STELM         GET FIRST EL ADDRESS                         
         MVI   ELCODE,X'21'                                                     
EDT8A    BRAS  RE,NXTELEM                                                       
         BNE   EDT8B                                                            
         CLC   2(3,R5),WORK+2                                                   
         BNE   EDT8A                                                            
         LA    R3,DUPERR           DUPLICATE ERROR                              
         B     ERROR                                                            
EDT8B    L     R5,SV1STELM         INSERT AFTER LAST X'21' ELEM                 
         LR    R6,R5               SAVE THIS ELEM ADDRESS                       
         BRAS  RE,NXTELEM                                                       
         BE    *-6                                                              
         IC    R0,1(R6)            INSERT AFTER THIS ELEM                       
         AR    R6,R0                                                            
         L     R5,SV1STELM                                                      
         AHI   R5,-33              BACK UP TO START OF REC                      
         GOTO1 VRECUP,DMCB,(1,(R5)),WORK,(R6)                                   
*                                                                               
         XC    USCN1(6),USCN1                                                   
         LA    R1,UFRST            MORE DATA                                    
         LA    R1,PARS             SET USCAN PARMS TO EDIT ALLOCATION           
         L     RF,VUSCAN                                                        
         BASR  RE,RF                                                            
         CLI   ULNGTH+1,0                                                       
         BZ    EDT10                                                            
         MVI   UFRST,X'FF'                                                      
         B     EDT2                                                             
*                                                                               
EDT10    MVI   ELCODE,X'21'        NOW VALIDATE SUM OF WEIGHTS                  
         L     R5,SV1STELM                                                      
         SR    R7,R7                                                            
         SR    R6,R6                                                            
EDT10A   BRAS  RE,NXTELEM                                                       
         BNE   EDT10B                                                           
         IC    R0,2+3(R5)                                                       
         AR    R7,R0                                                            
         IC    R0,2+4(R5)                                                       
         AR    R6,R0                                                            
         B     EDT10A                                                           
*                                                                               
EDT10B   LA    R3,SHNEQERR         COST SHARE SUM NE SPACE SHARE SUM            
         CR    R7,R6                                                            
         BNE   ERROR                                                            
         STC   R7,BYTE2                                                         
         TM    BYTE,X'80'          DIGIT ENTERED?                               
         BZ    EDT10C                                                           
         LA    R3,SHSUMERR                                                      
         OI    BYTE2,X'80'                                                      
         CLC   BYTE,BYTE2                                                       
         BNE   ERROR                                                            
*                                                                               
EDT10C   BRAS  RE,REMPO#EL         REMOVE PURCHASE ORDER # ELEMS                
         TM    GLBVALSW,BUYPO#VQ   PURCHASE ORDER # ALREADY VALIDATED?          
         BNZ   EDT10D                                                           
         MVI   DMCB,X'18'                                                       
         BRAS  RE,LOADOVLY                                                      
         GOTOR (RF),DMCB,(RC),(RA),('VAL_PO#Q',0)                               
         ICM   R3,15,DMCB+8                                                     
         LTR   R3,R3               CAME BACK WITH ERROR?                        
         JNZ   ERROR                                                            
EDT10D   MVI   DMCB,X'03'                                                       
         BRAS  RE,LOADOVLY                                                      
         ST    RF,VT41103                                                       
         GOTOR (RF),DMCB,(RC),(RA),BYPO#ELQ                                     
*                                                                               
         L     RE,SV1STELM         GET PBDELEM ADDRESS                          
         LA    RE,PBDWTSUM-PBDELEM(RE)                                          
         MVC   0(1,RE),BYTE2                                                    
         OC    INSDA,INSDA         NEW BUY?                                     
         BE    POL10                                                            
*                                                                               
         CLI   ALLOMOD,0           ALLOCATION MODIFIED?                         
         JE    POL05                                                            
*                                                                               
         L     RE,SV1STELM                                                      
         LA    RE,PBDDTIND-PBDELEM(RE)                                          
         OI    0(RE),X'80'         SET ALLOCATION CHANGE                        
         OI    CHGIND1,X'80'                                                    
*                                                                               
POL05    GOTOR (RF),DMCB,(RC),(RA),CHGELEMQ                                     
*                                                                               
* VALIDATE THAT ALL PAID/BILLED PRDS STILL PRESENT                              
* AND PAID PRDS HAVE SAME COST SHARES.                                          
*                                                                               
POL10    LA    R6,SVALLOCA         POINT TO ALLOCATION TABLE                    
         CLI   0(R6),0             TABLE IS EMPTY?                              
         BE    POL12                                                            
POL10A   MVI   ELCODE,X'21'                                                     
         L     R5,SV1STELM                                                      
POL10B   BRAS  RE,NXTELEM                                                       
         BE    POL10C                                                           
         TM    4(R6),X'40'         PRD GONE, BILLED?                            
         BZ    POL10D                                                           
         LA    R3,PRDGONE                                                       
         B     ERROR                                                            
*                                                                               
POL10C   CLC   0(3,R6),2(R5)       PRD MATCH THAT OF ALLOCATION ELEM?           
         BNE   POL10B                                                           
         OI    4(R6),X'20'         SET IND FOR 'MATCHED'                        
         CLC   0(4,R6),2(R5)       SAME COST SHARE?                             
         BE    POL10D                                                           
         TM    4(R6),X'80'         NOT SAME COST SHARE, PAID?                   
         BZ    POL10D                                                           
         LA    R3,SHRGONE          MAY NOT CHANGE SHARE IF PAID                 
*                                                                               
         B     POL10D              ERROR IS DISABLED                            
*                                                                               
POL10D   LA    R6,5(R6)            NEXT PRD IN ALLOCATION TABLE                 
         CLI   0(R6),0             END OF ALLOCATION TABLE?                     
         BNE   POL10A                                                           
*                                                                               
* VERIFY ESTHDR ON FILE FOR ALL NEW PRRDS IN REC.                               
* FIRST SEARCH LIST OF OLD PRDS IN REC AND ADD NEW PRDS TO LIST                 
*                                                                               
POL12    MVI   ELCODE,X'21'                                                     
         L     R5,SV1STELM         GET FIRST ELEM ADDRESS                       
         B     POL12X                                                           
*                                                                               
POL12A   LA    R6,SVALLOCA         POINT TO ALLOCATION TABLE                    
         CLI   0(R6),0             TABLE IS EMPTY?                              
         BE    POL12B3                                                          
POL12B   CLC   2(3,R5),0(R6)       PRD MATCHED?                                 
         BE    POL12X                                                           
         LA    R6,5(R6)                                                         
         CLI   0(R6),0             END OF ALLOCATION TABLE?                     
         BNE   POL12B                                                           
POL12B3  MVC   0(4,R6),2(R5)       ADD TO LIST OF PRDS IN REC                   
         OI    4(R6),X'01'         SET IND FOR NEW PRD                          
*                                                                               
         LA    R1,SVPRDS           CK PRD AGAINST LIST OF PRD CODES             
         LA    R0,L'SVPRDS/3                                                    
         CLC   0(3,R1),2(R5)                                                    
         BE    POL12X                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         XC    KEY,KEY             NOT IN LIST, SO READ EST RECORD              
         MVC   KEY(7),NEWREC       A/M/CLT                                      
         MVI   KEY+3,7             REC TYPE                                     
         MVC   KEY+7(3),0(R6)      PRD                                          
         MVC   KEY+10(2),BEST      ESTIMATE                                     
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEYSAVE(25),KEY                                                  
         BE    POL12B5                                                          
         LA    R3,ESERR                                                         
         CLC   KEY(10),KEYSAVE     PRD FOUND FOR EST KEY?                       
         BE    ERROR                                                            
         XC    KEY,KEY             EST NOT FOUND, SEE PRD IS ON FILE            
         MVC   KEY(10),KEYSAVE                                                  
         MVI   KEY+3,6                                                          
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEYSAVE(25),KEY     PRD IS ON FILE?                              
         BE    ERROR                                                            
         LA    R3,PRERR                                                         
         B     ERROR                                                            
*                                                                               
POL12B5  L     RE,SV1STELM         FIRST ELEM OF REC OR NEWREC                  
         CLI   PBDBFD-PBUYREC-33(RE),C'T'                                       
         BE    POL12C                                                           
         MVC   AREC,AWRKREC        GET EST RECORD                               
         BRAS  RE,PRT_GETR                                                      
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         L     RF,AWRKREC                                                       
         TM    PESTTEST-PESTREC(RF),X'80'                                       
         BZ    POL12B7                                                          
         LA    R3,ESERR            BRAND EST STATUS DOESN'T MATCH ZZZ'S         
         B     ERROR                                                            
*                                                                               
POL12B7  OC    INSDA,INSDA         NEW BUY?                                     
         BZ    POL12C                                                           
         MVC   KEY+27(4),INSDA     NEED TO RETRIEVE BUY RECORD                  
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVC   AREC,AWRKREC                                                     
         BRAS  RE,PRT_GETR                                                      
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         NI    DMINBTS,X'F7'       RESET DMINBTS                                
*                                                                               
POL12C   LA    R1,SVPRDS           ADD PRD TO SVPRD LIST                        
         LA    R0,L'SVPRDS/3                                                    
         CLI   0(R1),0                                                          
         BE    POL12D                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,*-12                                                          
         MVC   SVPRDS(L'SVPRDS-3),SVPRDS+3                                      
         LA    R1,SVPRDS+L'SVPRDS-3                                             
POL12D   MVC   0(3,R1),2(R5)       MOVE PRD CODE                                
*                                                                               
POL12X   BRAS  RE,NXTELEM          X'21' ELEMENT FOUND?                         
         BE    POL12A                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ALL ESTIMATES ON FILE, DELETE AND ADD BILL ELEMENTS AS REQUIRED               
* DELETE AND ADD BILL ELEMENTS AS REQUIRED                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         CLI   SVCLPROF+13,C'N'    NO DETAIL BILLING?                           
         BE    POL14D                                                           
         LA    R6,SVALLOCA         POINT TO ALLOCATION TABLE                    
POL14A   TM    4(R6),X'20'         PRD STILL THERE?                             
         BO    POL14C                                                           
         TM    4(R6),X'01'         ADDED?                                       
         BZ    POL14B                                                           
*                                                                               
         XC    WORK,WORK           PREPARE TO ADD BILL ELEM                     
         MVC   WORK(2),=X'2617'                                                 
         MVC   WORK+2(3),0(R6)                                                  
         MVI   ELCODE,X'25'        FIND PAY ELEM                                
         L     R5,SV1STELM         GET FIRST ELEM ADDRESS                       
         OC    INSDA,INSDA         TEST ADD                                     
         BE    *+12                                                             
         LA    RE,PBDDTIND-PBDELEM(R5)                                          
         OI    0(RE),X'80'         SET ALLOCATION CHANGE                        
         BRAS  RE,NXTELEM                                                       
         IC    R0,1(R5)            ADD AFTER PAY ELEMENT                        
         AR    R5,R0                                                            
         L     RF,SV1STELM         GET PBDELEM ADDRESS                          
         AHI   RF,-33              BACK UP TO KEY                               
         GOTO1 VRECUP,DMCB,(1,(RF)),WORK,(R5)                                   
         B     POL14C                                                           
*                                                                               
POL14B   MVI   ELCODE,X'26'        DELETE BILL ELEMENT                          
         L     R5,SV1STELM                                                      
         LA    RE,PBDDTIND-PBDELEM(R5)                                          
         OI    0(RE),X'80'         SET ALLOCATION CHANGE                        
         BRAS  RE,NXTELEM                                                       
         BNE   POL14C                                                           
         CLC   0(3,R6),2(R5)       MATCH PRD                                    
         BNE   *-14                                                             
         L     RF,SV1STELM                                                      
         AHI   RF,-33                                                           
         GOTO1 VRECUP,DMCB,(1,(RF)),(R5)                                        
*                                                                               
POL14C   LA    R6,5(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   POL14A                                                           
*                                                                               
POL14D   CLI   DDLINKSW,C'F'       DRAFT INSERTION (ADBUYER)?                   
         BE    *+12                                                             
         CLI   LKDRFTSW,C'F'       DRAFT MODE?                                  
         BNE   *+14                                                             
         MVC   REC(25),NEWREC      CHGX RESTORES NEWREC (10,11,12,13)           
         B     POL20X              EXIT                                         
*                                                                               
         CLI   ZZZTPASS,C'Y'       PASS, DON'T WRITE ANYTHING?                  
         BE    POL20X                                                           
*                                                                               
         XC    SVINSDAT,SVINSDAT   CLEAR OLD INS DATE SAVE AREA                 
         OC    INSDA,INSDA         ADD?                                         
         BNE   POL15                                                            
         BRAS  RE,ADDLINE                                                       
         CLI   ERRAREA,0                                                        
         BZ    POL16                                                            
         DC    H'0'                                                             
*                                                                               
POL15    CLC   NEWREC+16(3),REC+16 NEW INSERTION DATE?                          
         BE    POL15A                                                           
         MVC   SVINSDAT,REC+16     SAVE OLD INSERTION DATE                      
         MVC   SVLINENO,REC+24     SAVE OLD LINE NUMBER                         
         BRAS  RE,CHGPTR                                                        
         JNE   EXIT                                                             
*                                                                               
POL15A   MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERRORS                      
         BRAS  RE,PRT_PUTR                                                      
         MVI   DMOUTBTS,X'FD'      RESTORE DMOUTBTS                             
*                                                                               
* DELETE PASSIVE POINTERS FOR DE-ALLOCATED PRDS OR FOR ALL PRDS                 
* IF INSERTION DATE CHANGED                                                     
*                                                                               
POL16    MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERROR TESTS                 
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         MVC   KEY+21(3),=C'ZZZ'                                                
         OC    SVINSDAT,SVINSDAT   HAVE NEW INSERTION DATE?                     
         BZ    *+16                                                             
         MVC   KEY+24(1),SVLINENO  RESTORE OLD LINE NUMBER                      
         MVC   KEY+16(3),SVINSDAT  MOVE OLD INSERTION DATE TO KEY               
         LA    R6,SVALLOCA         POINT TO ALLOCATION TABLE                    
         B     POL16H                                                           
*                                                                               
POL16A   MVC   KEY+7(3),0(R6)                                                   
         BRAS  RE,PRT_RDHI                                                      
         BRAS  RE,CHECK                                                         
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                POINTER NOT FOUND                            
         OI    KEY+25,X'80'                                                     
         OC    SVINSDAT,SVINSDAT   HAVE NEW INSERTION DATE?                     
         BZ    *+8                                                              
         OI    KEY+25,X'FF'        MARK FF ONLY IF DATE CHANGE                  
         BRAS  RE,PRT_WRIT                                                      
         BRAS  RE,CHECK                                                         
POL16B   LA    R6,5(R6)            NEXT ENTRY IN ALLOCATION TABLE               
POL16H   CLI   0(R6),0             END OF ALLOCATION TABLE?                     
         BE    POL18                                                            
         TM    4(R6),1             NEW PRD?                                     
         BO    POL16B                                                           
         OC    SVINSDAT,SVINSDAT   HAVE NEW INSERTION DATE?                     
         BZ    POL16M                                                           
         TM    4(R6),X'20'         STILL ALLOCATED?                             
         BZ    POL16A                                                           
         OI    4(R6),X'01'         SET TO ADD NEW                               
         B     POL16A                                                           
POL16M   TM    4(R6),X'20'         STILL ALLOCATED?                             
         BZ    POL16A                                                           
         B     POL16B                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
POL18    LA    R6,SVALLOCA         POINT TO ALLOCATION TABLE                    
         MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERROR TESTS                 
         B     POL18H                                                           
POL18A   XC    KEY,KEY             BUILD KEY TP ADD NEW POINTERS                
         MVC   KEY(25),REC                                                      
         MVC   KEY+7(3),0(R6)                                                   
         MVC   KEY+21(3),=C'ZZZ'                                                
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         BRAS  RE,PRT_RDHI                                                      
         BRAS  RE,CHECK                                                         
         LA    RF,PRT_ADD_                                                      
         CLC   KEYSAVE(25),KEY                                                  
         BNE   *+8                                                              
         LA    RF,PRT_WRIT                                                      
         MVC   KEY(25),KEYSAVE     RESTORE                                      
         MVC   KEY+25(2),REC+27    CONTROL BYTES                                
         MVC   KEY+27(4),INSDA     DISK ADDRESS                                 
         BASR  RE,RF                                                            
         BRAS  RE,CHECK                                                         
*                                                                               
POL18B   LA    R6,5(R6)                                                         
POL18H   CLI   0(R6),0                                                          
         BE    POL20                                                            
         TM    4(R6),1             ADD INDICATOR?                               
         BO    POL18A                                                           
         B     POL18B                                                           
*                                                                               
POL20    MVI   DMOUTBTS,X'FD'      RESTORE DMOUTBTS                             
*                                                                               
POL20X   B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EDTSHR   LR    R0,RE               R3 HAS ERROR NUMBER ON ENTRY                 
         MVI   UVAL,X'80'                                                       
         LA    R1,PARS             SET USCAN PARMS TO EDIT ALLOCATION           
         L     RF,VUSCAN                                                        
         BASR  RE,RF                                                            
         J     *+6                                                              
EDTSHR2  LR    R0,RE                                                            
         L     RF,UADDR-1                                                       
         LH    RE,ULNGTH                                                        
         CLI   ULNGTH+1,3                                                       
         JH    ERROR                                                            
         LTR   RE,RE                                                            
         JZ    ERROR                                                            
         TM    UVAL,X'01'                                                       
         JNZ   ERROR                                                            
         BCTR  RE,0                                                             
         BASR  R1,0                                                             
         EX    RE,8(R1)                                                         
         J     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,100                                                           
         JH    ERROR                                                            
         LR    RE,R0                                                            
         LR    R0,R1               RETURN BINARY VALUE                          
         BR    RE                                                               
*                                                                               
NXTELEM  SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
ADDLINE  LA    RF,ADDINSRQ                                                      
         J     G_T41103                                                         
CHGPTR   LA    RF,CHGBRECQ                                                      
G_T41103 LR    R0,RE                                                            
         GOTOR VT41103,DMCB,(RC),(RA),(RF)                                      
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET_ETXT LR    R0,RE               R3 HAS ERROR NUMBER                          
         XC    BUYMSG,BUYMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
LOADOVLY LR    R0,RE                                                            
         XC    DMCB+1(23),DMCB+1                                                
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PRT_READ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMREAD'                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_WRIT LR    R0,RE                                                            
         MVC   COMMAND,=C'DMWRT '                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_RSEQ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRSEQ'                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_ADD_ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMADD '                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_RDHI LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
PRT_DDIR LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR  ',KEY,KEY,   +        
               (TERMNAL,0)                                                      
         J     EXIT_VRE                                                         
*                                                                               
PRT_GETR LR    R0,RE                                                            
         MVC   COMMAND,=C'GETREC'                                               
         J     PRT_DFIL                                                         
*                                                                               
PRT_ADDR LR    R0,RE                                                            
         MVC   COMMAND,=C'ADDREC'                                               
         J     PRT_DFIL                                                         
*                                                                               
PRT_PUTR LR    R0,RE                                                            
         MVC   COMMAND,=C'PUTREC'                                               
*                                                                               
PRT_DFIL LA    RF,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    RF,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE ',           +        
               (RF),AREC,(TERMNAL,DMWORK)                                       
         J     EXIT_VRE                                                         
*                                                                               
PUB_READ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMREAD'                                               
         J     PUB_DDIR                                                         
*                                                                               
PUB_RDHI LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
PUB_DDIR LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR  ',KEY,KEY,   +        
               (TERMNAL,0)                                                      
         J     EXIT_VRE                                                         
*                                                                               
PUB_GETR LR    R0,RE                                                            
         MVC   COMMAND,=C'GETREC'                                               
*                                                                               
         LA    RF,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    RF,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE ',           +        
               (RF),APUBIO,(TERMNAL,DMWORK)                                     
*                                                                               
EXIT_VRE LR    RE,R0               EXIT VIA SAVED RE                            
         BR    RE                                                               
*                                                                               
CHECK    TM    DMCB+8,X'FD'        TEST DATAMGR ERRS                            
         BCR   8,RE                                                             
         DC    H'0'                BLOW UP ON DATAMGR ERRS                      
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         BRAS  RE,GET_ETXT                                                      
*                                                                               
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
         ST    R2,ADBERRFD         SAVE ADDRESS OF ERROR FIELD                  
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REMPO#EL NTR1  BASE=*,LABEL=*      REMOVE PURCHASE ORDER # ELEMS                
*                                                                               
         MVI   ELCODE,PBYPOELQ                                                  
         L     R5,SV1STELM         POINT TO FIRST ELEM                          
         BRAS  RE,NXTELEM          PO# ELEM FOUND?                              
         BNE   E_EXIT                                                           
         XC    WKTEMP1,WKTEMP1     INIT TEMPORARY PO# ELEM TABLE                
         LA    R2,WKTEMP1                                                       
         MVI   ELCODE,X'21'                                                     
         L     R5,SV1STELM         POINT TO FIRST ELEM                          
         BRAS  RE,NXTELEM          PRD ALLOCATION ELEM FOUND?                   
         BE    *+6                                                              
         DC    H'0'                PO# & PRD ALLOCATION NOT IN SYNC             
*                                                                               
R_PO#20  OC    0(3,R2),0(R2)       EMPTY ENTRY?                                 
         BZ    *+12                                                             
         LA    R2,3(R2)            POINT TO NEXT ENTRY                          
         B     *-14                                                             
         MVC   0(3,R2),(PPRCODE-PPRELEM)(R5)                                    
         OC    0(3,R2),0(R2)                                                    
         BNZ   *+6                                                              
         DC    H'0'                PRD CODE IS MISSING                          
         BRAS  RE,NXTELEM                                                       
         BE    R_PO#20                                                          
*                                                                               
         MVI   ELCODE,PBYPOELQ                                                  
R_PO#30  L     R5,SV1STELM         POINT TO FIRST ELEM                          
R_PO#34  BRAS  RE,NXTELEM                                                       
         BNE   E_EXIT                                                           
         USING PBYPOELD,R5                                                      
         TM    PBYPOSTA,BYPOZZZQ   PO# ELEM FOR ZZZ BUY?                        
         BNZ   *+6                                                              
         DC    H'0'                BAD ELEM                                     
         LA    R2,WKTEMP1          POINT TO PRD CODE TABLE                      
R_PO#36  CLI   0(R2),0             END OF TABLE?                                
         BE    R_PO#38                                                          
         CLC   PBYPOPRD,0(R2)      PRD CODE MATCH THAT OF TABLE ENTRY?          
         BE    R_PO#34                                                          
         LA    R2,3(R2)            NEXT PRD CODE IN TABLE                       
         B     R_PO#36                                                          
R_PO#38  L     RF,SV1STELM         POINT TO FIRST ELEM                          
         SHI   RF,33                                                            
         GOTO1 VRECUP,DMCB,(1,(RF)),(R5)                                        
         B     R_PO#30                                                          
*                                                                               
E_EXIT   XIT1                                                                   
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                                                               
         LTORG                                                                  
SPACES   DC    CL50' '                                                          
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   NEWREC              MAP BUY RECORD TO NEWREC                     
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WORK04D  DSECT                                                                  
*                                                                               
SV1STELM DS    F                   ADDRESS OF FIRST BUY ELEMENT                 
SVINSDAT DS    XL3                 BINARY INSERTION DATE                        
SVLINENO DS    XL1                 BINARY LINE NUMBER                           
SVALLOCA DS    XL(100*5)            ALLOCATION TABLE                            
WKBUYELM DS    XL256                                                            
WKTEMP1  DS    XL80                                                             
WKESTALO DS    XL94                                                             
WKESTALX DS    XL94                                                             
*                                                                               
WKAIO1   DS    XL4096                                                           
*                                                                               
WORK04X  EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056PPBUY04   02/26/20'                                      
         END                                                                    
