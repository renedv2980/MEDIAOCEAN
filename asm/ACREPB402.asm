*          DATA SET ACREPB402  AT LEVEL 032 AS OF 04/24/06                      
*PHASE ACB402B                                                                  
         TITLE 'ACREPB402-BUDGET COPY/UPDATE/DELETE UTILITY'                    
*INCLUDE SORTER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE SQUASHER                                                               
ACB402   CSECT                                                                  
         PRINT NOGEN                                                            
         USING ACWORKD,RA                                                       
         USING ACB4D,RC                                                         
         NMOD1 0,**ACB4**,R9,RR=R5                                              
         L     RA,0(,R1)                                                        
         LA    RC,SPACEND                                                       
*-------------------------------------------------------------------*           
* ACREPB402 ALLOWS BUDGET RECORDS TO BE MANIPULATED 6 WAYS          *           
*   (C) 1: COPY BUDGET AMOUNTS UNDER A BUDGET TYPE TO ANOTHER       *           
*   (D) 2: DELETION OF BUDGET RECS                                  *           
*   (G) 3: UPDATE OF SUBSIDIARY BUDGET RECS TO A GEN LDG BDG REC    *           
*   (T) 4: ADD/UPDATE BUDGET RECS WITH ACTUAL TRANSACTIONS          *           
*   (A) 5: AMEND BUDGET DATA WITHIN A SINGLE BDGT TYPE. IE TAKE     *           
*          12 INCOME BUDGT AMNTS MULTIPLY BY BDGT PCT INDICATED BY  *           
*          REQUEST AND CREATE NEW BILLING BUDGT RECORDS.            *           
*   (M) 6: ADD/UPDATE BUDGET RECS WITH ACTUAL TRANSACTIONS          *           
*          ADJUSTING THE REMAINING ANNUAL BUDGET BALANCE            *           
*                                                                   *           
* REQUEST FILTERS:                                                  *           
*  QSTART/QEND-DATE RANGE OF BDG RECS ACTED UPON(12 MOS MAX)        *           
*     QSRTAREA-POS 1/2 CONTAIN BDGT NO. FROM/TO OR FROM/FACTOR-     *           
*              BUDGET.                                              *           
*      QCUL    POS 1-2 FROM CONTRA U/L                              *           
*      QSELECT     3-4 TARGET CNTRA U/L                             *           
*              FOR AMEND REQUESTS ONLY                              *           
*        QUNIT-ACT FROM     S         REQUIRED FOR GEN UPDATE       *           
*                           ANY       REQUIRED FOR AMEND            *           
*      QLEDGER-             ANY       REQUIRED FOR AMEND            *           
*        QOPT1-ACTION OPTS  (A)AMEND (C)COPY (D)DELETE (G)GENERAL   *           
*              (M)TRANSACTIONS                                      *           
*        QOPT2-DRAFT/LIVE   BLANK/L   DEFAULT=BLANK(SOFT/HARD)      *           
*        QOPT3-OPTIONAL REVISION YEAR VALUE                         *           
*        QOPT4-REPLACE OR AMMEND TO TARGET BUDGET. Y=REPLACE        *           
*              N OR BLANK = AMEND                                   *           
*-------------------------------------------------------------------*           
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*              RUN INITIALIZATION                                               
*-------------------------------------------------------------------*           
         CLI   MODE,RUNFRST                                                     
         BNE   B1                                                               
         LA    RE,RELOTBL          ESTABLISH A TYPE ADDRESSABILITY              
         LA    R1,ATYPES                                                        
B0       L     RF,0(,RE)                                                        
         ST    RF,0(,R1)                                                        
         LA    RE,4(,RE)                                                        
         LA    R1,4(,R1)                                                        
         CLI   0(RE),X'FF'                                                      
         BNE   B0                                                               
*                                                                               
         L     RF,=A(SAVERC)                                                    
         ST    RC,0(,RF)           SAVE REG C                                   
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     RF,MCBXAREA                                                      
         L     R7,=A(ADBOX)        STORE ADDR OF BOX ROUTINE                    
         ST    RF,0(,R7)                                                        
         L     RF,=A(HOOK)                                                      
         ST    RF,HEADHOOK                                                      
         DROP  RF                                                               
*                                                                               
         ZAP   RECSADED,=P'0'      INIT REC COUNTS FOR LOGO PAGE                
         ZAP   RECSDLT,=P'0'                                                    
         ZAP   RECSROTE,=P'0'                                                   
         ZAP   RECCNT,=P'0'                                                     
         MVI   LIVESW,NO           SET LIVE FOR RUN SWITCH TO "NO"              
         B     B4EXT                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        REQUEST INITIALIZATION                                                 
*--------------------------------------------------------------------*          
B1       CLI   MODE,REQFRST                                                     
         BNE   B2                                                               
         MVI   FCRDTRNS,NO         PREVENT ACT REC PASSES IF U/L NOT            
         MVI   FCRDACC,NO          IN TABLE                                     
         GOTO1 AINITIT,DMCB,(RC)   INITIAL FIRST PASS PER REQUEST               
         CLI   RQSTERR,YES         IF ERROR-LEAVE FC VALUES AT 'N'              
         BE    B4EXT                                                            
         MVI   FCRDTRNS,YES        PREVENT ACT REC PASSES IF U/L NOT            
         MVI   FCRDACC,YES         IN TABLE                                     
         B     B4EXT                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        CHECK REMAINING MODES                                                  
*--------------------------------------------------------------------*          
B2       CLI   RQSTERR,YES         IF ERROR ENCOUNTERED IGNORE FURTHER          
         BE    B13X                PROCESS-LOOK FOR RUNLAST                     
*------------------------------------*                                          
*              FIRST FOR UNIT                                                   
*------------------------------------*                                          
         CLI   MODE,UNITFRST                                                    
         BNE   B3                                                               
         L     R1,ADUNTNAM         GET UNIT NAME                                
         LA    R3,SVUNTNM                                                       
         BAS   R7,GETLVLNM                                                      
         B     B4EXT                                                            
*------------------------------------*                                          
*              FIRST FOR LEDGER                                                 
*------------------------------------*                                          
         USING ACTRECD,R2                                                       
         USING B4ULTBLD,R6                                                      
B3       CLI   MODE,LEDGFRST                                                    
         BNE   B5                                                               
         MVI   LDGPROC,NO          INDICATE TO NONPROC THIS U/L                 
         L     R6,SUBTBL           A(U/L TABLE)                                 
         L     R2,ADLEDGER         A(LEDGER RECORD)                             
*                                                                               
B3A      CLI   B4ACTID,C'A'        IS CURRENT ENT ACCOUNT                       
         BNE   B4                                                               
         CLC   B4ULENT,ACTKUNT     CHK IF CURRENT U/L INCLUDED IN RQST          
         BNE   B4                                                               
         MVI   LDGPROC,YES                                                      
         ST    R6,AULENTRY         SAVE A(TBL ENT)                              
         L     R1,ADLDGNAM         GET LEDGER NAME                              
         LA    R3,B4LDGNAM                                                      
         BAS   R7,GETLVLNM                                                      
         MVC   B4UNNAM,SVUNTNM                                                  
*                                                                               
         USING ACLELD,RE                                                        
         L     RE,ADLDGHIR         FILL OUT UL ENT WITH LVL INFO                
         ZIC   R7,B4ULLVL          U/L BUDGET LEVEL DEFINITION                  
         LA    RF,ACLVLEN          A(ACCT LEVEL LEN)                            
         LA    R8,LVLATOT          A(COINCIDING LEVEL TOTAL)                    
B3B      BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BZ    B3C                                                              
         AHI   RF,L'ACLVALS        A(NEXT ACCT LVL)                             
         LA    R1,TOTALSLN         LEVEL TOTAL LENGTH                           
         SR    R8,R1               A(NEXT HIGHER LEVEL TOTAL)                   
         B     B3B                                                              
*                                                                               
B3C      ST    R8,B4ULALOW         A(LOW LEVEL TOTAL)                           
         MVC   B4ACTLEN,0(RF)      LOW LEVEL ACCT LEN                           
         MVC   B4ULLVLA,ACLELLVA   FILL TBL WITH LEVEL LENS                     
         MVC   B4ULLVLB,ACLELLVB                                                
         MVC   B4ULLVLC,ACLELLVC                                                
         MVC   B4ULLVLD,ACLELLVD                                                
         B     B4EXT                                                            
         DROP  RE                                                               
*                                                                               
B4       AHI   R6,B4ENTLEN         NEXT ENTRY                                   
         CLI   B4ACTID,X'FF'       END OF TBL                                   
         BNE   B3A                 CHECK NEXT TABLE ENTRY                       
         B     B4EXT                                                            
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
B5       CLI   LDGPROC,YES         IS RQST FOR CURRENT U/L                      
         BNE   B13                 NO                                           
         L     R6,AULENTRY         A(CURRENT U/L ENTRY)                         
*------------------------------------*                                          
*              FIRST FOR LEVEL A                                                
*------------------------------------*                                          
         CLI   MODE,LEVAFRST                                                    
         BNE   B6                                                               
         LA    R3,SORTANAM                                                      
         L     R1,ADLVANAM         ACCOUNT LVL A NAME                           
         BAS   R7,GETLVLNM                                                      
         CLI   ACTION,C'G'         IF SUBSIDIARY TO GENERAL UPDATE              
         BNE   B4EXT                                                            
         MVC   FILTERS,SPACES      INIT OFFICE FILTERS FIELD                    
         L     R1,ADLVASTA         A(STATUS ELEMENT)                            
         BAS   R7,MVCFILT                                                       
         B     B4EXT                                                            
*------------------------------------*                                          
*              FIRST FOR LEVEL B                                                
*------------------------------------*                                          
B6       CLI   MODE,LEVBFRST                                                    
         BNE   B7                                                               
         LA    R3,SORTBNAM                                                      
         L     R1,ADLVBNAM         ACCOUNT LVL B NAME                           
         BAS   R7,GETLVLNM                                                      
         CLI   ACTION,C'G'         IF SUBSIDIARY TO GENERAL UPDATE              
         BNE   B4EXT                                                            
         L     R1,ADLVBSTA         A(STATUS ELEMENT)                            
         BAS   R7,MVCFILT                                                       
         B     B4EXT                                                            
*------------------------------------*                                          
*              FIRST FOR LEVEL C                                                
*------------------------------------*                                          
B7       CLI   MODE,LEVCFRST                                                    
         BNE   B8                                                               
         LA    R3,SORTCNAM                                                      
         L     R1,ADLVCNAM         ACCOUNT LVL B NAME                           
         BAS   R7,GETLVLNM                                                      
         CLI   ACTION,C'G'         IF SUBSIDIARY TO GENERAL UPDATE              
         BNE   B4EXT                                                            
         L     R1,ADLVCSTA         A(STATUS ELEMENT)                            
         BAS   R7,MVCFILT                                                       
         B     B4EXT                                                            
*------------------------------------*                                          
*              PROCESS ACCOUNT                                                  
*------------------------------------*                                          
B8       CLI   MODE,PROCACC                                                     
         BNE   B9                                                               
         MVI   ACACTIV,NO                                                       
         LA    R3,SORTANAM                                                      
         ZIC   R5,B4ULLVL          BUDGET ACCOUNT LEVEL                         
         LA    R8,ADLVANAM                                                      
B8A      BCTR  R5,0                ARRIVE AT LOW LEVEL ADDRESSES                
         LTR   R5,R5                                                            
         BZ    B8B                                                              
         AHI   R8,20               A(NEXT NAME ELEMENT)                         
         AHI   R3,L'NAMEREC        A(NEXT LEVEL SORT NAME)                      
         B     B8A                                                              
                                                                                
B8B      L     R1,0(,R8)                                                        
         BAS   R7,GETLVLNM                                                      
         CLI   ACTION,C'T'         IF ACTUAL TO BUDGET WAIT ON PROCSBAC         
         BE    B4EXT               FOR OTHER ACTS-                              
         CLI   ACTION,C'G'         IF SUBSIDIARY TO GENERAL UPDATE              
         BNE   *+12                                                             
         L     R1,12(,R8)          A(LOW LEVEL STATUS ELEMENT)                  
         BAS   R7,MVCFILT                                                       
         L     R3,ADACC            A(KEY)                                       
         B     B14                 GO TO READRECS RTN WITH CURRENT REC          
         EJECT ,                                                                
*----------------------------------------------------*                          
*              PROCESS SUB-ACCOUNT                                              
*----------------------------------------------------*                          
B9       CLI   MODE,PROCSBAC                                                    
         BNE   B10                                                              
         CLI   ACTION,C'T'         IF ACTUAL TO BUDGET                          
         BNE   B4EXT               MATCH CURRENT CONTRA TO UL TABLE             
         MVI   TRNSOK,YES          OKS PROCESS OF SUCCEEDING PROCTRNS           
         MVI   FILTSB,0            INITIALLY ASSUME NOT FILT SUBACCTS           
                                                                                
         USING CACRECD,R2                                                       
         L     R2,ADSUBAC          A(KEY WITH CONTRA)                           
         AHI   R6,B4ENTLEN         ADVANCE TO 1ST POSSIBLE CONTRA ENT           
         CLI   B4ACTID,C'C'        IF (C)ONTRA DROP THRU AND EDIT               
         BNE   B4EXT               NO CONTRA REQUIRE-WAIT ON PROCTRNS           
***********************************************************************         
*        MUST GET AMTS FOR ACTUALIZING 14,15,16 CONTRAS FROM HISTORY            
*        RECS - SO FOOL PROGRAM INTO USING THESE AMTS INSTEAD OF TRNS           
***********************************************************************         
         CLC   =C'14',CACKCUNT                                                  
         BE    B9A                                                              
         CLC   =C'15',CACKCUNT                                                  
         BE    B9A                                                              
         CLC   =C'16',CACKCUNT                                                  
         BNE   B9K                                                              
*                                                                               
B9A      DS    0H                                                               
         MVI   TRNSOK,NO           IGNORE SUCCEEDING TRANSACTIONS               
         CLC   B4ULENT(3),=C'ALL'  WAS CONTRA LIMITED IN BUD TYPE               
         BE    B9E                                                              
B9C      CLC   B4ULENT,CACKCUNT    USE THIS ONE                                 
         BE    B9E                                                              
         AHI   R6,B4ENTLEN         BUMP TABLE OF POSSIBLE CONTRAS               
         CLI   B4ACTID,X'FF'       END OF TABLE                                 
         BNE   B9C                                                              
         B     B4EXT                                                            
*                                                                               
B9E      OI    FILTSB,FILTON       SIGNAL USING HISTORY, NOT TRANS              
         MVI   ELCODE,BUKELQ       X'45'                                        
         BAS   RE,B4GETEL                                                       
         BNE   B4EXT                                                            
         ST    R2,FILTADR          SAVE ADDRESS FOR NEXT PASS                   
                                                                                
         USING BUKELD,R4                                                        
B9G      L     R4,FILTADR          GET FIRST HISTORY ELEMENT                    
         MVC   FILTMOS,BUKYEAR     USE THIS MOS                                 
         ZAP   FILTAMT,BUKCR       AND THIS AMT                                 
         B     B14                                                              
         DROP  R4                                                               
*                                                                               
B9K      CLC   B4ULENT(3),=C'ALL'                                               
         BE    B4EXT                                                            
B9M      CLC   B4ULENT,C'+'        IF SPECIAL-U/L POSITIONS ARE BLANK           
         BNE   B9P                                                              
         CLI   CACKCUNT,C' '       NO UNIT                                      
         BE    B4EXT                                                            
         B     B9T                                                              
*                                                                               
B9P      CLC   B4ULENT,CACKCUNT                                                 
         BE    B4EXT                                                            
B9T      AHI   R6,B4ENTLEN         MAY BE ANOTHER                               
         CLI   B4ACTID,X'FF'       END OF TABLE                                 
         BNE   B9M                                                              
         MVI   TRNSOK,NO           IGNORE SUCCEEDING TRANSACTIONS               
         B     B4EXT                                                            
*------------------------------------*                                          
*              PROCESS TRANSACTIONS                                             
*------------------------------------*                                          
B10      CLI   MODE,PROCTRNS       FOR ACTUAL TO BUDGET ONLY                    
         BNE   B12                                                              
         CLI   ACTION,C'T'                                                      
         BNE   B4EXT                                                            
         CLI   TRNSOK,YES          WAS THE CONTRA MATCHED                       
         BNE   B4EXT               NO, IGNORE THESE TRANSACTIONS                
         L     R3,ADTRANS                                                       
         SH    R3,DATADISP         SET R2 TO A(RECORD)                          
         B     B14                 GO TO READRECS RTN                           
         EJECT ,                                                                
*------------------------------------*                                          
*              ACCLAST                                                          
*------------------------------------*                                          
B12      CLI   MODE,ACCLAST                                                     
         BNE   B13                                                              
         CLI   ACTION,C'C'                                                      
         BE    B12A                                                             
         CLI   ACTION,C'D'                                                      
         BNE   B12B                                                             
                                                                                
         USING B4ULTBLD,R6                                                      
B12A     L     R6,AULENTRY                                                      
         AHI   R6,B4ENTLEN                                                      
         CLC   B4ULENT(3),=C'ALL'                                               
         BNE   B12C                                                             
         CLI   ACTION,C'D'                                                      
         BE    B4EXT                                                            
         CLI   COPDEL,YES                                                       
         BNE   B12C                                                             
         MVI   COPDEL,NO                                                        
         B     B4EXT                                                            
         DROP  R6                                                               
*                                                                               
B12B     CLI   ACTION,C'T'                                                      
         BNE   B4EXT                                                            
         CLI   UPNILS,YES                                                       
         BNE   B4EXT                                                            
                                                                                
         USING B4ULTBLD,R6                                                      
         L     R6,AULENTRY                                                      
         AHI   R6,B4ENTLEN                                                      
         CLC   B4ULENT(3),=C'ALL'                                               
         BNE   B12C                                                             
         CLI   ACACTIV,YES                                                      
         BE    B4EXT                                                            
         DROP  R6                                                               
*                                                                               
B12C     LA    R1,SORTDT1          INIT SORT REC DATE/BUCKET FLDS               
         LA    R6,DATAB                                                         
         ZIC   R7,MNTHCNT                                                       
B12F     MVC   0(2,R1),0(R6)       YR/MNTH                                      
         XC    2(8,R1),2(R1)                                                    
         AHI   R1,10                                                            
         AHI   R6,2                                                             
         BCT   R7,B12F                                                          
*                                                                               
         MVC   SORTREC(65),SPACES                                               
         LA    R1,SORTREC                                                       
         CLI   ACTION,C'D'                                                      
         BE    B12G                                                             
         CLI   ACTION,C'C'                                                      
         BNE   B12J                                                             
B12G     MVI   SORTREC,BUDKTYPQ       X'1B'                                     
         AHI   R1,1                                                             
                                                                                
         USING B4ULTBLD,R6                                                      
B12J     L     R6,AULENTRY                                                      
         ZIC   RF,B4ACTLEN                                                      
         AHI   RF,2                   ACCOUNT FOR COMP,U/L                      
         L     R3,ADACC                                                         
         EXMVC RF,0(R1),0(R3)                                                   
         MVC   SORTREC+17(3),=XL3'FFFFFF'                                       
         BAS   RE,PUTSORT                                                       
         B     B4EXT                                                            
         DROP  R6                                                               
         EJECT ,                                                                
*------------------------------------*                                          
*        LAST FOR REQUEST                                                       
*        FROM SORT OUTPUT,UPDATE FILE                                           
*        AND PROCESS PREPORT                                                    
*------------------------------------*                                          
B13      CLI   MODE,REQLAST                                                     
         BE    B15                                                              
*------------------------------------*                                          
*              LAST FOR RUN                                                     
*------------------------------------*                                          
B13X     CLI   MODE,RUNLAST                                                     
         BNE   B4EXT               DO END LOGIC                                 
         GOTO1 AENDIT,DMCB,(RC)                                                 
         B     B4EXT                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* FILTERS FIELD INITIALIZED TO SPACES IN INITIAL. ROUTINE MOVES NON-            
* BLANK VALUES TO ASSIGNED POSITIONS WITH LOWEST LEVEL VALUES BEING             
* DOMINANT.                                                                     
*---------------------------------------------------------------------*         
         USING RSTELD,RE                                                        
MVCFILT  LR    RE,R1               A(STATUS ELEMENT)                            
         CLI   RSTFILT1,C' '       FILTER 1                                     
         BE    *+10                                                             
         MVC   FILTERS(1),RSTFILT1                                              
         CLI   RSTFILT2,C' '       FILTER 2                                     
         BE    *+10                                                             
         MVC   FILTERS+1(1),RSTFILT2                                            
         CLI   RSTFILT3,C' '       FILTER 3                                     
         BE    *+10                                                             
         MVC   FILTERS+2(1),RSTFILT3                                            
         CLI   RSTFILT4,C' '       FILTER 4                                     
         BE    *+10                                                             
         MVC   FILTERS+3(1),RSTFILT4                                            
         BR    R7                                                               
         DROP  RE                                                               
*                                                                               
         USING NAMELD,R1                                                        
GETLVLNM MVC   0(36,R3),SPACES                                                  
         ZIC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         EXMVC RF,0(R3),NAMEREC                                                 
         BR    R7                                                               
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*                                                                               
***********************************************************************         
B14      BAS   RE,READRECS         BUILD SORT RECORDS-PUT TO SORT               
         TM    FILTSB,LOOPAGN                                                   
         BO    B9G                                                              
         CLI   ACTION,C'G'         IF UPDATE TO GENERAL                         
         BNE   B4EXT                                                            
         CLI   RQSTERR,0           ANY FAILURE TO FIND POST TO ACCOUNTS         
         BE    B4EXT               YES-PRINT ERROR MSG-END IT                   
         XC    SORTCNT,SORTCNT                                                  
B14A     MVI   NOBOX,YES                                                        
         GOTO1 APRNTIT,DMCB,(RC)                                                
         MVI   RQSTERR,YES         PREVENT FURTHER PROCESS                      
         B     B4EXT                                                            
*                                                                               
B15      OC    SORTCNT,SORTCNT     ANY DATA                                     
         BNZ   B15Z                GO TO RDSORT RTN                             
         MVC   P+1(28),=C'NO DATA FOR PERIOD REQUESTED'                         
         B     B14A                                                             
*                                                                               
B15Z     BAS   RE,RDSORT           UPDATE BDGT FILE-PROCESS REPORT              
         BNZ   B14A                BAD RETURN                                   
         B     B4EXT                                                            
*                                                                               
B4EXT    XMOD1 1                   COMMON RETURN (PROGRAM EXIT)                 
*                                                                               
LVLXIT   XIT1  1                   COMMON RETURN (EXIT ONE LEVEL)               
*                                                                               
         EJECT ,                                                                
*-------------------------------------------------------------------*           
* UPON ENTRY R3 = ADACC FOR ALL ACTIONS OTHER THAN ACTUAL TO                    
* BUDGET.  BUDGET KEY IS BUILT USING 1B KEY ID AND (15,R3). A                   
* READ HI IS PERFORMED AND THE RESULT OF THAT READ IS FILTERED                  
* AGAINST THE REMAINING REQUEST REQUIREMENTS.                                   
* FOR ACTUAL TO BUDGET R3 = A(TRANSACTION) KEY                                  
*-------------------------------------------------------------------*           
         USING B4ULTBLD,R6         COVERS CURRENT U/L ENTRY                     
         USING BUDRECD,R2                                                       
READRECS NTR1                                                                   
         L     R6,AULENTRY                                                      
         L     R2,AIO1                                                          
*                                                                               
         CLI   MODE,PROCTRNS       ONLY ACTION T HAS THIS MODE PASSED           
         BE    B20B                                                             
         CLI   MODE,PROCSBAC       ONLY ACTION T HAS THIS MODE PASSED           
         BE    B20B                                                             
*                                                                               
         MVC   BUDKEY,SPACES                                                    
         MVI   BUDKTYP,BUDKTYPQ        BUDGET KEY ID X'1B'                      
         MVC   BUDKCULA(3),0(R3)       CO, U/L                                  
         ZIC   RF,B4ACTLEN                                                      
         BCTR  RF,0                                                             
         EXMVC RF,BUDKACT,3(R3)                                                 
         MVC   BUDKBUDN,OLDBDGNO       BDG #                                    
*                                                                               
         AHI   RF,4                   ID, CO, U/L                               
         EXCLC RF,BUDKTYP,KEY1SAVE    DID I READ FOR A HIGHER LVL ?             
         BE    B4EXT             BDGT INITIATED BY THIS LOW LVL ACT KEY         
*                                                                               
B20      XC    BUDKBCKT(7),BUDKBCKT      SPARE BINARY 0                         
         MVC   KEY1SAVE,BUDKEY                                                  
*                                                                               
         MVC   COMMAND,=CL8'DMRDHI'                                             
         GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
*                                                                               
         CLC   BUDKTYP(17),KEY1SAVE      COMPARE THRU ACCOUNT                   
         BNE   B80                                                              
*                                                                               
         CLC   BUDKBUDN,OLDBDGNO   CHECK TO SEE IF CORRECT BUDGET NO.           
         BE    B20B                GOT ONE                                      
B20A     SR    RF,RF                                                            
         ICM   RF,3,BUDKBUDN       READ HI FOR NEXT                             
         AHI   RF,1                                                             
         STCM  RF,3,BUDKBUDN                                                    
         B     B20                                                              
*                                                                               
B20B     LR    RE,R6               SAVE                                         
         LA    R7,SORTCLR          CLEAR FOR NEW SORT REC                       
         LA    R6,SORTREC                                                       
         LA    R1,0                                                             
         MVCL  R6,R0                                                            
         LR    R6,RE               RESTORE A(UL ENTRY)                          
*                                                                               
         CLI   ACTION,C'G'         IS IT SUB TO GEN UPDATE                      
         BNE   B23A                NO                                           
         MVC   SORTFROM(49),BUDKEY POSTING'FROM' BDG REC                        
         B     B35                                                              
*                                                                               
*              BUILD SORT KEY FROM ACTUAL ACCOUNT                               
B23A     CLI   ACTION,C'T'         IF ACTUAL TO BUDGET                          
         BNE   B23M                                                             
         MVC   SORTKEY,SPACES                                                   
         ZIC   RF,B4ACTLEN                                                      
         AHI   RF,3                C/U/L                                        
         BCTR  RF,0                                                             
         EXMVC RF,SORTKEY,0(R3)    ACTUAL ACT                                   
         L     R6,AULENTRY                                                      
         AHI   R6,B4ENTLEN                                                      
         CLC   B4ULENT(3),=C'ALL'                                               
         BE    *+10                                                             
         MVC   SORTKEY+17(15),17(R3)                                            
*                                                                               
         TM    FILTSB,FILTON                                                    
         BNO   B23F                                                             
         L     R4,ADSUBAC                                                       
         BAS   RE,TRANFILT         DO FILTER ON ACCOUNT RECORDS                 
         BNZ   B23B                                                             
         BAS   RE,PUTSORT                                                       
         MVI   COPDEL,YES                                                       
                                                                                
B23B     L     R2,FILTADR                                                       
         MVI   ELCODE,BUKELQ       X'45'                                        
         BAS   RE,NXTELL                                                        
         BNE   B23C                                                             
         OI    FILTSB,LOOPAGN                                                   
         ST    R2,FILTADR                                                       
         B     B4EXT                                                            
                                                                                
B23C     MVI   FILTSB,0                                                         
         MVI   TRNSOK,NO                                                        
         B     B4EXT                                                            
*                                                                               
         USING CACRECD,R2                                                       
B23F     DS    0H                                                               
         L     R2,ADTRANS          A(TRANSACTION RECORD)                        
         SH    R2,DATADISP                                                      
         LR    R7,R6               SEE IF CONTRA AGREES WITH TRGT DEF.          
         L     R6,AULENTRY                                                      
         AHI   R6,B4ENTLEN         ADVANCE TO NEXT ENTRY                        
         CLI   B4ACTID,C'C'        IF (C)CONTRA DROP THRU AND EDIT              
         BNE   B23Z                NOT C-NO CONTRA REQUIREMENT                  
B23H     CLC   B4ULENT(3),=C'ALL'                                               
         BE    B23Z                                                             
         CLI   B4ACTID,X'FF'                                                    
         BE    B23J                                                             
         CLC   B4ULENT,CACKCUNT    COMPARE FOR CONTRA U/L                       
         BE    B23Z                                                             
         AHI   R6,B4ENTLEN                                                      
         CLI   B4ACTID,C'A'                                                     
         BNE   B23H                                                             
B23J     MVI   TRNSOK,NO                                                        
         LR    R6,R7                                                            
         B     B4EXT                                                            
*                                                                               
         USING ACMD,RE                                                          
B23Z     L     RE,AMONACC                                                       
         MVC   FILTMOS,ACMMDTE     GET PACKED MOS OF TRANACTION                 
         OC    FILTMOS,FILTMOS                                                  
         BNZ   *+6                                                              
         DC    H'00'                                                            
         DROP  RE                                                               
                                                                                
         USING TRNELD,R4                                                        
         LR    R6,R7                                                            
         L     R4,ADTRANS          A(TRANSACTION RECORD)                        
         ZAP   FILTAMT,TRNAMNT                                                  
         BAS   RE,TRANFILT         DO FILTER ON ACCOUNT RECORDS                 
         BNZ   B4EXT               NO ACCEPTABLE DATA-READ NEXT                 
         B     B70                                                              
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*              WRITE ENTIRE KEY FOR COPY, DELETE, AMEND                         
***********************************************************************         
         USING BUDRECD,R2                                                       
B23M     LA    R1,BUDKEY           A(BUDGET KEY)                                
         MVC   SORTKEY(49),0(R1)                                                
*                                                                               
B24      LR    R7,R6             CHECK IF CONTRA AGREES WITH TRGT DEF.          
         AHI   R6,B4ENTLEN         ADVANCE TO NEXT ENTRY                        
         CLI   B4ACTID,C'C'        IF (C)CONTRA DROP THRU AND EDIT              
         BNE   B26                 NOT C-NO CONTRA REQUIREMENT                  
*                                                                               
B25      CLC   B4ULENT(3),=C'ALL'                                               
         BE    B26                                                              
         CLI   B4ULENT,C'+'                                                     
         BNE   B25A                                                             
         CLI   BUDKCUNT,C' '       NO U/L WOULD MEAN SPECIAL FOUND              
         BE    B26                 I.E. C'  SPOT'                               
         B     B20A                                                             
                                                                                
B25A     CLC   B4ULENT,BUDKCUNT    COMPARE FOR CONTRA U/L                       
         BE    B26                                                              
*                                                                               
         CLI   ACTION,C'A'         FOR 'U' ACT THE 1ST CONTRA IS THE            
         BNE   *+10                SENDING AND THE ONE WE WANT                  
         LR    R6,R7               RESTORE                                      
         B     B20A                                                             
*                                                                               
         AHI   R6,B4ENTLEN         ADVANCE TO NEXT ENTRY                        
         CLI   B4ACTID,C'C'        IS IT A CONTRA FOR PREVIOUS ACT U/L          
         BE    B25                 YES                                          
         B     B20A                BACK TO READ LOOP                            
*                                                                               
B26      LR    R6,R7               RESTORE A(CURRENT ULTBL ENT)                 
*                                  FOR ACTIONS A,C,D, AND G                     
B35      BAS   RE,BDGTOSRT         PLACE AMOUNTS IN SORT REC                    
         BZ    B20A                NO AMOUNTS PLACED                            
         EJECT ,                                                                
*                                                                               
         CLI   ACTION,C'G'                                                      
         BNE   B70                 FOR ACTION G DETERMINE POSTING ACCT          
         DROP  R2                                                               
***********************************************************************         
*              FIND ACCOUNT TO POST CURRENT SUBSIDIARY REC TO                   
***********************************************************************         
         USING GLPELD,R2                                                        
B38      MVC   SORTTO,SPACES                                                    
         MVI   ELCODE,GLPELQ       X'15'                                        
         LR    R2,R3               A(ACCOUNT)                                   
         BAS   RE,B4GETEL                                                       
         BNE   B40                 POST TO NOT AVAILABLE IN ACCT REC            
         MVI   SORTRULE,C'A'       POSTING ACCT. CAME FROM ACCT. REC            
         LA    R1,GLPACC1          A(POST TO)                                   
         B     B42                                                              
***********************************************************************         
*              FIND LEDGER DEFAULT                                              
***********************************************************************         
         USING PSTBD,R5                                                         
B40      L     R5,APSTB            A(LEDGER POST ACCTS)                         
         ZIC   R7,SVXSCNT          COUNT OF SORTED ENTRIES                      
B40A     CLC   PSTCD(2),=X'011D'                                                
         BNE   B50                 FORCE SOFT-RULE NOT AVAILABLE                
         CLC   POSTID,B4ULENT      FIND REC(S) FOR CURRENT U/L                  
         BE    B40B                                                             
         AHI   R5,POSTLN           NEXT SORT REC                                
         BCT   R7,B40A                                                          
         B     B50                 FORCE SOFT EOF                               
*                                                                               
B40B     CLC   PSTFR,SPACES       IF SPACES LDG DEFAULT-NO MORE FOR U/L         
         BE    B40E                                                             
*                                                                               
         USING ACTRECD,R2                                                       
B40C     L     R2,AIO1                                                          
         ZIC   R1,PSTFRLN          LEN TO COMPARE FOR CURRENT ENTRY             
         EX    R1,B40D                                                          
         BE    B40E                CLOSEST MATCH POST TO THIS                   
         AHI   R5,POSTLN                                                        
         CLC   POSTID,B4ULENT      STILL IN CURRENT U/L RULES                   
         BE    B40B                YES                                          
         B     B50                 NO-FORCE SOFT                                
                                                                                
B40D     CLC   PSTFR(0),ACTKEY+4   ACCOUNT TO POST FROM RULE                    
*                                                                               
B40E     LA    R1,PSTTO                                                         
         MVI   SORTRULE,C'L'       POSTING ACCT. CAME FROM LEDGER               
         DROP  R5                                                               
*                                                                               
B42      LA    RE,GUNIT1           FIND POST TO G LEDG LEVEL LENGTH             
         LA    R7,2                FOR GL ACCTS                                 
B42A     CLC   0(2,RE),0(R1)                                                    
         BE    B42B                                                             
         AHI   RE,4                ADVANCE TO NEXT G U/L                        
         BCT   R7,B42A                                                          
         B     B80       THIS ACCT HAS A RULE NOT COVERED BY TYPE REC           
*                                                                               
B42B     ZIC   R7,3(,RE)           G U/L ACCT LEN                               
         LTR   R7,R7               NO LEN FORCE SOFT                            
         BZ    B50                                                              
         AHI   R7,1                1 FOR U/L (EXEC INSTR.)                      
         EXMVC R7,SORTTO,0(R1)                                                  
*                                                                               
         LA    R5,SORTTO+13        CHECK IF *                                   
         CLI   0(R5),C' '                                                       
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         CLI   0(R5),C'*'                                                       
         BNE   B45                                                              
         CLI   B4ULOFF,C' '        IF =                                         
         BE    B43A                NO WAY                                       
         CLI   B4ULOFF,C'T'        IF =                                         
         BE    B43A                NOT SUPPORTABLE                              
         CLI   B4ULOFF,C'C'        IF CLIENT                                    
         BNE   B42C                                                             
                                                                                
         USING PPRELD,RE                                                        
         L     RE,ADPROFIL                                                      
         MVC   0(1,R5),PPRUFORA                                                 
         B     B43A                                                             
         DROP  RE                                                               
*                                                                               
B42C     TM    B4ULOFF,X'F0'                                                    
         BNZ   B43                 IF THERE ON NEED TO ANALYZE FILTERS          
         ZIC   R1,B4ULOFF          VALUE POINTS TO OFF DISP IN ACT              
         L     RF,ADACC                                                         
         LA    RF,2(R1,RF)                                                      
         MVC   0(1,R5),0(RF)                                                    
         B     B43A                                                             
*                                                                               
B43      MVC   0(1,R5),FILTERS       FILTER 1                                   
         CLI   B4ULOFF,C'1'                                                     
         BE    B43A                                                             
         MVC   0(1,R5),FILTERS+1     FILTER 2                                   
         CLI   B4ULOFF,C'2'                                                     
         BE    B43A                                                             
         MVC   0(1,R5),FILTERS+2     FILTER 3                                   
         CLI   B4ULOFF,C'3'                                                     
         BE    B43A                                                             
         MVC   0(1,R5),FILTERS+3     FILTER 4                                   
*                                                                               
B43A     CLI   0(R5),C' '                                                       
         BE    *+12                                                             
         CLI   0(R5),0                                                          
         BNE   B45                                                              
         MVC   P+1(14),=C'CANNOT SUPPORT'                                       
         MVC   P+16(15),SORTTO                                                  
         B     B50A                                                             
*********************************************                                   
*              GET G ACT NAME                                                   
*********************************************                                   
         USING ACTRECD,R2                                                       
B45      L     R2,AIO2                                                          
         MVC   ACTKEY,SPACES           BUILD G ACT KEY                          
         MVC   ACTKCPY,KEY1SAVE+1      CO.                                      
         MVC   ACTKULA,SORTTO                                                   
         MVC   COMMAND,=CL8'DMREAD'                                             
         GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
         DROP  R2                                                               
                                                                                
         TM    DMCB+8,X'FF'                                                     
         BNZ   B70                 NO ACCOUNT REC                               
*                                                                               
         LA    R1,SORTPNAM                                                      
         L     R2,AIO2                                                          
         BAS   R7,GETNAME                                                       
         B     B70                                                              
*                                                                               
B50      MVC   P+1(37),=C'CANNOT DETERMINE POST TO ACCOUNT FOR '                
         MVC   P+38(14),SORTFROM+1                                              
B50A     MVI   RQSTERR,YES          NO RULE FOUND THIS FROM ACT                 
         B     B4EXT                                                            
*                                                                               
B70      BAS   RE,PUTSORT                                                       
         MVI   COPDEL,YES                                                       
         CLI   ACTION,C'T'         FOR ACTIONS OTHER THAN ACTUAL TO BDG         
         BNE   B20A                RETURN TO BDG READ LOOP-PERSIST              
         B     B4EXT                                                            
*                                                                               
B80      L     R2,AIO1                                                          
         MVC   0(42,R2),0(R3)                                                   
         MVC   COMMAND,=CL8'DMREAD'                                             
         GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
         B     B4EXT                                                            
         DROP  R6                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* ACCOUNT RECORD VALUES TO SORT REC *                                           
*--------------------------------------------------------------------*          
TRANFILT NTR1                                                                   
*                                                                               
         TM    FILTSB,FILTON                                                    
         BNO   TF100                                                            
         L     R2,ADSUBAC                                                       
         CLI   UPNILS,YES                                                       
         BE    TF080                                                            
         CP    FILTAMT,=P'0'       0 AMNTS ARE NOT ACCEPTED IN T MODE           
         BE    NOAMNT                                                           
*                                                                               
TF080    ZIC   R7,MNTHCNT          COMPARE FOR MOS                              
         LA    R5,DATAB            COINCIDING PACKED YR/MNTH                    
TF085    CLC   FILTMOS(2),0(R5)                                                 
         BE    TF240                                                            
         AHI   R5,2                NEXT PACKED                                  
         BCT   R7,TF085                                                         
         B     NOAMNT              MOS NOT WITHIN RQST RANGE                    
*                                                                               
         USING TRNRECD,R2                                                       
         USING TRNELD,R4                                                        
TF100    L     R4,ADTRANS                                                       
         LR    R2,R4                                                            
         SH    R2,DATADISP                                                      
*                                                                               
         CLI   0(R4),TRNELQ        X'44'                                        
         BNE   NOAMNT              NO TRANS ELEMENT                             
*        OC    ACDTPEEL,ACDTPEEL   IS IT A PEELED ITEM                          
         OC    TRNKEY+ACCOPEEL(2),TRNKEY+ACCOPEEL                               
         BNZ   NOAMNT              IF YES DO NOT PROCESS                        
*                                                                               
         CLI   UPNILS,YES                                                       
         BE    TF200                                                            
         CP    TRNAMNT,=P'0'      0 AMNTS ARE NOT ACCEPTED IN T MODE            
         BE    NOAMNT                                                           
         TM    TRNSTAT,TRNSREV     IS IT A REVERSED ITEM                        
         BO    NOAMNT              IF YES DO NOT PROCESS                        
*                                                                               
TF200    ZIC   R7,MNTHCNT          COMPARE FOR MOS                              
         LA    R5,DATAB            COINCIDING PACKED YR/MNTH                    
TF220    CLC   FILTMOS(2),0(R5)                                                 
         BE    TF240                                                            
         AHI   R5,2                NEXT PACKED                                  
         BCT   R7,TF220                                                         
         B     NOAMNT              MOS NOT WITHIN RQST RANGE                    
*                                                                               
TF240    LA    R1,SORTDT1          INIT SORT REC DATE/BUCKET FLDS               
         LA    R6,DATAB                                                         
         ZIC   R7,MNTHCNT                                                       
TF250    MVC   0(2,R1),0(R6)       YR/MNTH                                      
         AHI   R1,SORTNTYQ                                                      
         AHI   R6,2                                                             
         BCT   R7,TF250                                                         
*                                                                               
         LA    R1,SORTDT1          DETERMINE SORT BKT TO ADD TO                 
         ZIC   R7,MNTHCNT                                                       
TF300    CLC   0(2,R1),0(R5)       COMPARE PACKED YR/MN                         
         BE    TF400                                                            
         AHI   R1,SORTNTYQ                                                      
         BCT   R7,TF300                                                         
         DC    H'0'                                                             
*                                                                               
TF400    DS    0H                                                               
         ZAP   TOTTOT,FILTAMT                                                   
         TM    FILTSB,FILTON                                                    
         BO    TF500                                                            
         CLC   =C'1C',TRNKUNT                                                   
         BE    TF420                                                            
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+10                ADD AMOUNT AS CREDIT                         
         MP    TOTTOT,=P'-1'       REVERSE                                      
TF420    ZAP   TRNAMNT,TOTTOT                                                   
         ZAP   FILTAMT,TOTTOT                                                   
*                                                                               
TF500    OC    2(8,R1),2(R1)                                                    
         BNZ   TF520                                                            
         ZAP   2(8,R1),=P'0'                                                    
TF520    AP    2(8,R1),FILTAMT                                                  
*                                                                               
         SR    R0,R0               SET ACCEPTED CC                              
         MVI   ACACTIV,YES                                                      
         B     TFEXT                                                            
*                                                                               
NOAMNT   LTR   RB,RB               SET NO DATA FOUND RETURN CONDITION           
TFEXT    B     B4EXT                                                            
         DROP  R2,R4                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        BUDGET REC AMOUNT VALS TO SORTREC                                      
*--------------------------------------------------------------------*          
BDGTOSRT NTR1                                                                   
         LA    R4,SORTDT1          INIT SORT REC DATE/BUCKET FIELDS             
         LA    R5,DATAB                                                         
         ZIC   R7,MNTHCNT                                                       
RB55     MVC   0(2,R4),0(R5)       YR/MNTH                                      
         AHI   R4,10                                                            
         AHI   R5,2                                                             
         BCT   R7,RB55                                                          
***********************************************************************         
* SCAN X'1D' ELEMENTS IN CURRENT BDG REC-PUT TO SORTREC THOSE AMOUNTS           
* FALLING WITHIN REQUEST DATE RANGE                                             
***********************************************************************         
         USING ACTRECD,R4                                                       
         LA    R2,SORTDT1          1ST YR/MONTH BUCKET OF SORTREC               
         L     R4,AIO1                                                          
         LR    R5,R4                                                            
         SR    R7,R7                                                            
         LH    R7,ACTRLEN                                                       
         AR    R5,R7                                                            
         BCTR  R5,0                A(END OF REC)                                
*                                                                               
         XC    FULL,FULL           RETURN CC                                    
         LA    R4,ACTKEY+ACCORFST  A(INITIAL X'1D' ELEMENT)                     
                                                                                
         USING BAMELD,R4                                                        
         CLI   0(R4),BAMELQ        X'1D'                                        
         BNE   RB63                                                             
RB60     CLC   0(2,R2),BAMMNTH     FOR THIS MONTH                               
         BE    RB65                                                             
         BH    RB63                GO ADVANCE REC LOC                           
         AHI   R2,SORTNTYQ         NEXT SORT BKT                                
         CLI   0(R2),0             END OF SORT BKTS                             
         BE    RBEXT                                                            
         B     RB60                                                             
*                                                                               
RB63     ZIC   R7,1(,R4)           EL LEN                                       
         AR    R4,R7                                                            
         CR    R4,R5               END OF REC                                   
         BNL   RBEXT                                                            
         CLI   0(R4),BAMELQ        X'1D'                                        
         BNE   RB63                                                             
         B     RB60                                                             
*                                                                               
RB65     ZAP   2(8,R2),BAMBUDG   AMOUNT TO SORT REC                             
         ST    RB,FULL                                                          
         B     RB63                                                             
*                                                                               
RBEXT    OC    FULL,FULL                                                        
         B     B4EXT                                                            
         DROP  R4                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* MAIN LINE OF LOGIC FOR RETRIEVAL OF DATA FROM SORT. ACCORDING                 
* TO THE REQUEST A REPORT IS PRODUCED AND/OR THE BUDGET FILE IS                 
* UPDATED.                                                                      
*---------------------------------------------------------------------*         
RDSORT   NTR1                                                                   
         MVI   FIRST,0             INDICATE FIRST PASS                          
         XC    KEYDEL1,KEYDEL1                                                  
         XC    KEYDEL2,KEYDEL2                                                  
         XC    KEYTEMP,KEYTEMP                                                  
*                                                                               
RD10     DS    0H                                                               
         BAS   RE,GETSORT                                                       
**T                                                                             
         USING B4ULTBLD,R6                                                      
         L     R6,SUBTBL                                                        
         LA    R1,SORTKEY          SET R1 TO A(CO-U/L-ACT OF SORT REC)          
         CLI   ACTION,C'T'         ACTUAL TRANSACTION UPDATE                    
         BE    RD10B               YES                                          
         LA    R1,SORTKEY+1        FOR-DELETE-AMEND, PAST KEY ID                
         CLI   ACTION,C'G'                                                      
         BNE   RD10B                                                            
         LA    R1,SORTKEY+17       FOR S-G UPDATE FROM ACCT U/L                 
RD10B    CLI   B4ACTID,C'A'        GET MATCHING U/L ENTRY FROM TABLE            
         BNE   RD10E                                                            
         ST    R6,AULENTRY         SAVE A(CURRENT SORTRECS U/L ENTRY)           
         CLC   1(2,R1),B4ULENT                                                  
         BE    RD17                                                             
RD10E    AHI   R6,B4ENTLEN         NEXT ENT                                     
         CLI   B4ACTID,X'FF'                                                    
         BNE   RD10B                                                            
         DC    H'0'                NOT FOUND                                    
**T                                                                             
*                                                                               
RD17     CLI   QOPT3,C' '          REQUEST TO REVISE YEAR                       
         BE    *+8                 NO                                           
         BAS   RE,REVISE           CALCULATES YEAR REVISION PLACES NEW          
*                                  YEAR IN SORTREC YY LOCATIONS                 
         CLI   ACTION,C'D'                                                      
         BE    RD80                                                             
**T      OC    REQPCT,REQPCT       WAS REVISION PERCENTAGE SPECIFIED            
**T      BZ    *+8                                                              
**T      BAS   RE,REVPCT                                                        
*                                  YEAR IN SORTREC YY LOCATIONS                 
RD80     CLI   FIRST,0                                                          
         BE    RD90                                                             
*                                                                               
         BAS   RE,CHKBRKS          CHK FOR LEVEL BREAKS-CNTRL PRINT             
         CLI   ACTION,C'C'                                                      
         BNE   RD82                                                             
                                                                                
         USING B4ULTBLD,R6                                                      
         L     R6,AULENTRY                                                      
         AHI   R6,B4ENTLEN                                                      
         CLC   B4ULENT(3),=C'ALL'                                               
         BNE   RD85                                                             
         OC    ASORT,ASORT         IS IT END OF SORT                            
         BZ    B4EXT               YES-RETURN                                   
*                                                                               
RD82     CLI   ACTION,C'T'                                                      
         BNE   RD85                                                             
         CLI   UPNILS,YES                                                       
         BNE   RD85                                                             
         CLC   KEYDEL1+18(3),=X'FFFFFF'                                         
         BE    RD92                                                             
*                                                                               
RD85     CLI   ADDKEY,YES          NEED TO UPDATE BUDGET FILE                   
         BNE   RD90                NO                                           
         CLI   ACTION,C'T'         IF ACTUAL TO BUDGET ROUND THE AMNTS          
         BNE   RD89                                                             
         CLI   SAVOPT1,C'M'                                                     
         BNE   RD88                                                             
         L     R2,AIO1                                                          
         BAS   RE,BUDUPELS                                                      
         ZAP   RADDTOT,=P'0'                                                    
RD88     BAS   R7,ROUND                                                         
*                                                                               
RD89     CLI   LIVESW,YES          IS THIS A LIVE RUN                           
         BNE   RD90                NO - **DON'T ADD THE REC**                   
         L     R2,AIO1                                                          
         MVI   KEYSW,1                                                          
         GOTO1 ADDREC,DMCB,(R2)                                                 
         CLI   ADDOK,YES                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
RD90     OC    ASORT,ASORT         IS IT END OF SORT                            
         BZ    B4EXT               YES-RETURN                                   
*                                                                               
RD92     DS    0H                                                               
**T                                                                             
         OC    ASORT,ASORT         IS IT END OF SORT                            
         BZ    B4EXT               YES-RETURN                                   
**T                                                                             
         MVC   KEY3SAVE,SPACES                                                  
         MVI   KEY3SAVE,BUDKTYPQ           X'1B'                                
         CLI   ACTION,C'T'                                                      
         BE    RD93A                                                            
         MVC   KEY3SAVE+1(32),SORTREC+1                                         
         B     RD93B                                                            
RD93A    MVC   KEY3SAVE+1(32),SORTREC                                           
RD93B    MVC   KEY3SAVE+33(2),OLDBDGNO                                          
         CLI   ACTION,C'D'                                                      
         BE    RD94                                                             
         MVC   KEY3SAVE+33(2),NEWBDGNO                                          
RD94     XC    KEY3SAVE+35(14),KEY3SAVE+35                                      
**T                                                                             
         USING B4ULTBLD,R6                                                      
         CLI   KEY3SAVE+19,X'FF'                                                
         BE    RD94M                                                            
         L     R6,AULENTRY                                                      
         AHI   R6,B4ENTLEN                                                      
         CLC   B4ULENT(3),=C'ALL'                                               
         BE    RD94M                                                            
RD94F    CLI   B4ACTID,X'FF'       END OF TBL                                   
         BNE   *+6                 CHECK NEXT TABLE ENTRY                       
         DC    H'0'                                                             
                                                                                
         CLC   B4ULENT,KEY3SAVE+19                                              
         BE    RD94J                                                            
         AHI   R6,B4ENTLEN                                                      
         B     RD94F                                                            
                                                                                
RD94J    MVC   KEYTEMP2,SPACES                                                  
         MVC   KEYTEMP2(15),KEY3SAVE+18                                         
         MVC   KEY3SAVE+18(15),SPACES                                           
         ZIC   R3,B4ACTLEN                                                      
         AHI   R3,2                                                             
         EXMVC R3,KEY3SAVE+18,KEYTEMP2                                          
*                                                                               
RD94M    BAS   RE,CHKDEL                                                        
*                                                                               
         USING B4ULTBLD,R6                                                      
         CLC   SORTREC+17(3),=X'FFFFFF'                                         
         BNE   RD95                                                             
**       L     R6,AULENTRY                                                      
**       LA    R6,B4ENTLEN(R6)                                                  
**       CLC   B4ULENT(3),=C'ALL'                                               
**       BNE   RD100                                                            
         B     RD100                                                            
*                                                                               
RD95     DS    0H                                                               
         BAS   RE,ACTBDG           BUILD A BDGT KEY FROM SORT RECORD            
         GOTO1 ATOTMNG,DMCB,(RC)   BUILDS NEW OR UPDATES LEVEL TOTS             
         BAS   RE,BUILDREC         BUILD NEW BUDGET REC FOR FILE UPDATE         
         MVI   FIRST,1                                                          
RD100    MVI   ADDKEY,NO                                                        
         B     RD10                                                             
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* ROUTINE ADDS THE VALUE CARRIED IN THE QOPT3 FIELD TO THE                      
* YEAR VALS IN THE SORTDT FIELDS OF THE CURRENT SORT RECORD                     
*--------------------------------------------------------------------*          
REVISE   NTR1                                                                   
         LA    R8,DATAB            IN CASE OF BDGT REPLACE REVISE DATAB         
         LA    R5,SORTDT1          A(1ST DATE)                                  
         ZIC   R7,MNTHCNT          NUMBER OF BUCKETS                            
REV10    MVC   WORK(2),0(R5)                                                    
         MVI   WORK+2,1            YMD-PACKED NO SIGN                           
         GOTO1 DATCON,DMCB,(1,WORK),(3,DUB) TO BINARY YMD                       
         MVC   REVVAL,QOPT3        QOPT3=EBCIDIC 1-9                            
         NI    REVVAL,X'0F'        NOW 01-09                                    
         ZIC   RE,REVVAL                                                        
         ZIC   RF,DUB                                                           
         AR    RF,RE               ADD 01-09 TO BINARY YEAR                     
         STC   RF,DUB                                                           
         GOTO1 DATCON,DMCB,(3,DUB),(1,WORK) NEW YMD PACKED-NO SIGN              
         MVC   0(2,R5),WORK        UPDATE VALUE YM                              
         CLI   FIRST,0                                                          
         BNE   *+14                ONCE IS QUITE ENOUGH                         
         MVC   0(2,R8),WORK                                                     
         AHI   R8,2                                                             
         AHI   R5,10               NEXT SORT DATE FIELD                         
         BCT   R7,REV10                                                         
         B     B4EXT                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        REVISE BUDGET AMOUNTS BY PERCENT GIVEN AT REQUEST TIME                 
*--------------------------------------------------------------------*          
REVPCT   NTR1                                                                   
         LA    R5,SORTDT1          A(1ST DATE)                                  
         ZIC   R7,MNTHCNT          NUMBER OF BUCKETS                            
PCT10    DS    0H                                                               
         OC    2(8,R5),2(R5)       DO NOT AFFECT NIL BUDGETS                    
         BZ    PCT30                                                            
         CP    2(8,R5),=P'0'       A BILLION % OF ZERO IS STILL ZERO            
         BE    PCT30                                                            
         ZAP   WRKPCT,2(8,R5)      ADJUST BUDGET AMOUNT                         
         MP    WRKPCT,REQPCT                                                    
         SRP   WRKPCT,60,5                                                      
         AP    2(8,R5),WRKPCT                                                   
PCT30    LA    R5,10(,R5)          NEXT SORT DATE FIELD                         
         BCT   R7,PCT10                                                         
         B     B4EXT                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        CHECK IF ANY BUDGETS WITH NO ACTUALS NEED TO BE UPDATED                
*--------------------------------------------------------------------*          
CHKDEL   NTR1                                                                   
         CLI   ACTION,C'C'                                                      
         BE    CHKD10                                                           
         CLI   ACTION,C'D'                                                      
         BE    CHKD10                                                           
         CLI   ACTION,C'T'                                                      
         BNE   CHKDXIT                                                          
         CLI   UPNILS,C'Y'                                                      
         BNE   CHKDXIT                                                          
CHKD10   MVC   SVDMACT,DMACT                                                    
         MVI   DMACT,C'W'                                                       
*                                                                               
         USING BUDRECD,R2                                                       
         L     R2,AIO2                                                          
         XC    0(ACCORFST,R2),0(R2)                                             
*                                                                               
         CLI   FIRST,0                                                          
         BNE   CHKD30                                                           
CHKD20   MVC   BUDKEY(BUDKWORK-BUDKEY),KEY3SAVE                                 
         MVC   COMMAND,=CL8'DMRDHI'                                             
         GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
         B     CHKD45                                                           
*                                                                               
CHKD30   DS    0H                                                               
         CLC   KEYDEL1(BUDKEND2-1),KEY3SAVE                                     
         BE    CHKD200                                                          
         CLC   KEYDEL1(BUDKWORK-BUDKEY),KEY3SAVE                                
         BNE   CHKD20                                                           
         MVC   BUDKEY,KEYDEL1                                                   
                                                                                
CHKD38   OI    DMINBTS,X'88'                                                    
         MVC   COMMAND,=CL8'DMREAD'                                             
         GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
         CLI   QOPT2,C'L'               LIVE REPORT                             
         BNE   CHKD40                                                           
         TM    DMCB+8,X'02'                                                     
         BO    CHKD40                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
CHKD40   MVC   COMMAND,=CL8'DMRSEQ'                                             
         GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
*                                                                               
CHKD45   MVC   KEYTEMP,0(R2)                                                    
         CLC   BUDKEY(BUDKWORK-BUDKEY),KEY3SAVE                                 
         BNE   CHKD200                                                          
         CLC   BUDKBUDN,TARGET                                                  
         BNE   CHKD40                                                           
         CLC   BUDKEY(BUDKEND2-1),KEY3SAVE                                      
         BNL   CHKD200                                                          
*                                                                               
CHKD100  DS    0H                                                               
         CLI   SAVOPT1,C'M'                                                     
         BNE   CHKD110                                                          
         L     R2,AIO2                                                          
         BAS   RE,RDELTTL                                                       
*                                                                               
CHKD110  ZIC   R3,MNTHCNT          NUMBER OF SORT BKTS                          
         MVC   HALF,BUDRLEN                                                     
         LA    R5,SORTDT1                                                       
CHKD120  CLC   BUDRLEN,=X'0035'                                                 
         BNH   CHKD125                                                          
         CLI   BUDKEY+ACCORFST,EOR                                              
         BE    CHKD125                                                          
         GOTO1 DELEL,DMCB,('BAMELQ',(R2)),(2,(R5)),0                            
         AHI   R5,10               NEXT DATE                                    
         BCT   R3,CHKD120                                                       
                                                                                
         CLC   HALF,BUDRLEN                                                     
         BE    CHKD40                                                           
*                                                                               
CHKD125  CLI   SAVOPT1,C'M'                                                     
         BNE   CHKD130                                                          
         ZAP   RADDTOT,=P'0'                                                    
         L     R2,AIO2                                                          
         BAS   RE,BUDUPELS                                                      
                                                                                
CHKD130  CLI   LIVESW,YES          IS THIS A LIVE RUN                           
         BNE   CHKD140             NO - **DON'T ADD THE REC**                   
         L     R2,AIO2                                                          
         NI    BUDRSTA,TURNOFF-X'80'  DELETE BIT                                
         L     R2,AIO2                                                          
         MVI   KEYSW,2                                                          
         GOTO1 ADDREC,DMCB,(R2)                                                 
         CLI   ADDOK,YES                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CHKD140  DS    0H                                                               
         L     R6,AULENTRY                                                      
         LA    R7,1                HOW MANY TOTALS-1 FOR U/L                    
         ZIC   R1,B4ULLVL                                                       
         AR    R7,R1               PLUS NUMBER OF ACCOUNT LEVELS                
         LA    RF,4                = MAX AMNT OF ACT LVLVS                      
         LA    R3,B4ULLVLD         LOWEST POSSIBLE LEVEL LEN                    
         LA    R5,CNTRTOT          A(LOWEST TOTAL)                              
         LA    RE,TOTALSLN         TOTAL LENGTH                                 
CHKD142  CR    RF,R1                                                            
         BE    CHKD145                                                          
         BCTR  R3,0                BACK TO NEXT HIGHEST LEN                     
         BCTR  RF,0                                                             
         AR    R5,RE               NEXT HIGHER LEVEL TOTAL                      
         B     CHKD142                                                          
*                                                                               
CHKD145  DS    0H                                                               
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         CLC   0(L'BUDKACT,R5),BUDKACT             REGULAR PROCESSING           
         BE    CHKD149                                                          
         MVC   0(14,R5),SPACES                                                  
         MVC   0(L'BUDKACT,R5),BUDKACT                                          
         MVC   P+1(L'BUDKACT),BUDKACT                                           
         GOTO1 AACCTPL,DMCB,(RC)                                                
                                                                                
CHKD149  MVC   P+18(14),BUDKCUNT                                                
         MVC   SVUNTNM,B4UNNAM              UNIT LEDGET FOR HEADLINES           
         MVC   SVLDGNM,B4LDGNAM             IF THIS IS FIRST ACCOUNT            
         MVC   SVUNT,BUDKUNT                FOR REPORT                          
         MVC   SVLDG,BUDKLDG                                                    
         BAS   RE,MYNUMS                                                        
         GOTO1 APRNTIT,DMCB,(RC)                                                
**T                                                                             
         MVC   SAVEDACT(14),BUDKUNT         ON REPORT WHEN RESUMING             
         XC    CNTRTOT(14),CNTRTOT                                              
         XC    0(14,R5),0(R5)                                                   
**T                                                                             
*                                                                               
CHKD150  DS    0H                                                               
         MVC   BUDKEY,KEYTEMP                                                   
         B     CHKD38                                                           
*                                                                               
CHKD200  DS    0H                                                               
         MVC   KEYDEL2,KEYDEL1                                                  
         MVC   KEYDEL1,KEY3SAVE                                                 
         NI    DMINBTS,X'FF'-X'88'                                              
         MVC   DMACT,SVDMACT                                                    
*                                                                               
CHKDXIT  DS    0H                                                               
         B     B4EXT                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        FILL IN COLS WITH NILS                                                 
*--------------------------------------------------------------------*          
MYNUMS   NTR1                                                                   
         ZIC   R5,HOOKCNT                                                       
         CHI   R5,1                                                             
         BE    *+8                                                              
         AHI   R5,1                                                             
         LA    R4,P+37                                                          
MYNUM5   MVC   7(3,R4),=C'NIL'                                                  
         AHI   R4,14                                                            
         BCT   R5,MYNUM5                                                        
         B     B4EXT                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* ROUTINE CHECKS CURRENT SORTKEY AGAINST EXISTENT LEVEL TOTS                    
* AN UNEQUAL CONDITION INDICATES A NEED TO PRINT THAT LEVEL,                    
* AND IF A LIVE RUN A NEED TO UPDATE THE BUDGET FILE.                           
*--------------------------------------------------------------------*          
         USING TOTALSD,R2          COVERS TOTALS                                
         USING B4ULTBLD,R6         COVERS U/L TABLE                             
CHKBRKS  NTR1                                                                   
         L     R6,AULENTRY         A(CURRENT ACCT U/L ENTRY)                    
         L     R5,ACNTENT          A(CURRENT CNTR U/L ENTRY)                    
         SR    R8,R8               1ST PASS INDICATOR                           
*                                                                               
         OC    ASORT,ASORT         IF END OF SORT RECORDS                       
         BNZ   *+10                                                             
         XC    SORTKEY,SORTKEY     CLEAR FORCING FINAL BREAKS                   
*                                                                               
         LA    R2,PACTTOT          HIGHEST LVL TOT FOR G ACTION                 
         LA    R7,2                NUMBER OF TOTS FOR G ACTION                  
         LA    R4,SORTTO           U/L-POSTING ACCOUNT                          
         CLI   ACTION,C'G'                                                      
         BE    CH16A                                                            
*                                                                               
         LA    R4,SORTKEY+3        SET R4 TO A(ACT OF SORT REC)                 
         CLI   ACTION,C'T'                                                      
         BE    *+8                                                              
         AHI   R4,1                ACTIONS OTHER THAN-THERE IS 1B ID            
*                                                                               
         LA    R7,1                HOW MANY TOTALS-1 FOR U/L                    
         ZIC   R1,B4ULLVL                                                       
         AR    R7,R1               PLUS NUMBER OF ACCOUNT LEVELS                
         LA    RF,4                = MAX AMNT OF ACT LVLVS                      
         LA    R3,B4ULLVLD         LOWEST POSSIBLE LEVEL LEN                    
         LA    R2,LVLDTOT          A(LOWEST TOTAL)                              
         LA    RE,TOTALSLN         TOTAL LENGTH                                 
CH15     CR    RF,R1                                                            
         BE    CH15A                                                            
         BCTR  R3,0                BACK TO NEXT HIGHEST LEN                     
         BCTR  RF,0                                                             
         AR    R2,RE               NEXT HIGHER LEVEL TOTAL                      
         B     CH15                                                             
*                                                                               
CH15A    LTR   R5,R5               IS THERE A CONTRA                            
         BZ    CH16                NO                                           
         LA    R4,SORTKEY+18       A(CURRENT SORT REC U/L-CONTRA ACCT)          
         CLI   ACTION,C'T'                                                      
         BE    *+8                                                              
         AHI   R4,1                                                             
         ZIC   RE,4(,R5)           ACT LEN                                      
         AHI   RE,2                + U/L LEN                                    
         LA    R2,CNTRTOT          A(CURRENT CONTRA TOTAL ACCOUNT)              
*                                                                               
CH16     LA    RF,ULTOT            IN CASE I HAVE 2 U/LS W/ SAME ACCNT          
         LA    R1,SORTKEY+1                                                     
         CLI   ACTION,C'T'                                                      
         BE    *+8                                                              
         AHI   R1,1                                                             
         B     CH16B               INSURE THAT ALL BRKS ARE RECOGNIZED          
                                                                                
CH16A    LA    RF,GLTOT                                                         
         LA    R1,SORTTO                                                        
         LA    RE,14               1ST PASS G ACT-COMP THRU POSTTO              
CH16B    CLC   0(2,RF),0(R1)                                                    
         BNE   CH35                                                             
*                                                                               
         CLI   ACTION,C'G'         IF ACTION G RE IS SET                        
         BE    CH25                                                             
*                                                                               
         LA    R0,CNTRTOT                                                       
         CR    R0,R2               IF CONTRA PASS RE IS SET                     
         BE    CH25                                                             
*                                                                               
CH17     ZIC   RE,0(,R3)                                                        
CH25     BCTR  RE,0                                                             
         EX    RE,CH30                                                          
         BNE   CH35                                                             
         B     CH30A                                                            
CH30     CLC   TOTACT(0),0(R4)     CURRENT TOTAL TO CURRENT SORT REC            
*                                                                               
CH30A    CLI   ACTION,C'G'         IF G ACT                                     
         BNE   CH40                                                             
         LTR   R8,R8               AND POSTTO ACCOUNT PASS                      
         BNZ   CH40                                                             
         CLC   TOTFROM,SORTFROM    COMPARE FROM ACCOUNTS                        
         BE    CH40                                                             
         B     *+8                 LET FROMS WITH=POSTTO ACCUMALATE             
*                                                                               
CH35     MVI   ADDKEY,YES          A BREAK REQUIRES UPDATE OF BDG FILE          
         OC    CNTRTOT(14),CNTRTOT ALWAYS PRINT LOWEST                          
         BZ    *+8                                                              
         LA    R2,CNTRTOT                                                       
CH36     GOTO1 APRNTTOT,DMCB,(RC),(R2),(R8)   R8=0 HIGHEST TOT PRINT            
CH37     XC    CNTRTOT(14),CNTRTOT CLEAR CNTRA EVERY BRK                        
         XC    TOTACT,TOTACT       WILL INDICATE NEED FOR NEW TOTAL             
*                                                                               
         LA    R0,ULTOT            IF U/L PASS-CLEAR LOWER BUCKETS              
         CLI   ACTION,C'G'                                                      
         BNE   *+8                                                              
         LA    R0,GLTOT                                                         
         CR    R2,R0               A U/L TOTAL                                  
         BNE   CH40                NO                                           
*                                                                               
         LA    RF,CNTRTOT          A(1ST TOTAL                                  
         LA    R1,POSTBKT                                                       
CH400    XC    0(14,RF),0(RF)                                                   
         AHI   RF,TOTALSLN         NEXT TOTAL                                   
         CR    R0,RF               IF U/L                                       
         BE    *-6                 GO TO NEXT                                   
         CR    RF,R1               PAST LAST TOTAL                              
         BH    CH40                YES                                          
         B     CH400                                                            
*                                                                               
CH40     LR    R8,RB               BEYOND CONTRA AND LOWEST LEVEL TOTAL         
         L     RF,B4ULALOW         CURRENTLY ON LOW LVL OR CONTRA PASS          
         CR    R2,RF                                                            
         BH    CH40D               NO, HI LVL TOTS                              
*                                                                               
         LTR   R5,R5               CONTRA PROCESSING                            
         BZ    CH40D               NO                                           
*                                                                               
         LA    R4,SORTKEY+3        RESTORE A(SORTKEY)                           
         CLI   ACTION,C'T'                                                      
         BE    *+8                                                              
         AHI   R4,1                                                             
*                                                                               
         LA    R0,CNTRTOT                                                       
         CLI   ADDKEY,YES          WAS THERE A BREAK                            
         BNE   *+12                NO                                           
*                                                                               
         CLI   ACCTBRK,YES         DID THE ACCT BRK                             
         BE    CH40B               YES                                          
         CR    R2,R0               NO, RETURNED FROM CONTRA BREAK PASS          
         BNE   CH40D               NO, GO ADVANCE TO NEXT HI TOT                
CH40A    LR    R2,RF               R2=A(LOW LEVEL)                              
         B     CH16                GO BACK MAKE PASS FOR LOW LEVEL              
*                                                                               
CH40B    MVI   ACCTBRK,NO                                                       
         CR    R2,R0               RETURNED FROM CONTRA BREAK PASS              
         BNE   CH40CC              NO, ACCOUNT                                  
         CLI   PSWITCH,YES         YES, PRINTED CONTRA ONLY                     
         BNE   CH40CC              NO, ACCOUNT-CONTRA                           
         MVI   PSWITCH,NO                                                       
         B     CH40A               LOOP BACK FOR LOW LEVEL TOT PASS             
*                                                                               
CH40CC   LR    R2,RF               SET R2 A(LOW LEVEL)                          
         XC    TOTACT,TOTACT       MAKE SURE LOW TOT CLEARED                    
         MVI   POSTCNT,0           REINIT COUNT OF CONTRAS UNDER ACCT           
*                                                                               
CH40D    LA    R1,TOTALSLN                                                      
         CLI   ACTION,C'G'                                                      
         BE    *+10                                                             
         AR    R2,R1               TO NEXT HIGHER LEVEL TOTAL                   
         B     *+6                                                              
         SR    R2,R1               TO U/L TOT FOR G ACT                         
         BCTR  R3,0                TO NEXT HIGHER LEVEL LENGTH                  
         CHI   R7,2                LAST PASS(U/L PASS)                          
         BNE   CH41                                                             
         LA    R4,SORTTO           A(U/L)                                       
         CLI   ACTION,C'G'                                                      
         BE    CH40E                                                            
         LA    R4,SORTKEY+1                                                     
         CLI   ACTION,C'T'                                                      
         BE    *+8                                                              
         AHI   R4,1                                                             
CH40E    LA    RE,2                                                             
         BCTR  R7,0                                                             
         B     CH25                                                             
CH41     BCT   R7,CH16                                                          
         B     B4EXT                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* ROUTINE ROUNDS BUDGET AMOUNTS IN ACTUAL TO BUDGET MODE                        
*---------------------------------------------------------------------*         
ROUND    L     R2,AIO1             A(RECORD TO BE ADDED TO FILE)                
         MVI   ELCODE,BAMELQ       X'1D'                                        
         BAS   RE,B4GETEL                                                       
         B     *+8                                                              
R10      BAS   RE,B4NXTEL                                                       
         BNER  R7                                                               
                                                                                
         USING BAMELD,R2                                                        
         SRP   BAMBUDG,64-2,5                                                   
         MP    BAMBUDG,=P'100'                                                  
         B     R10                                                              
         DROP  R2                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        THIS ROUTINE UPDATES THE BUDGET FILE.                                  
*        MUST PASS IN R2 THE ADDRESS OF THE RECORD YOU ARE UPDATING             
*--------------------------------------------------------------------*          
ADDREC   NTR1                                                                   
         USING ACTRECD,R2                                                       
         MVI   ADDOK,YES                                                        
         CLI   KEYSW,1                                                          
         BNE   ADD10                                                            
         CLC   LASTADD1,0(R2)           COULD ENCOUNTER THIS PROBLEM            
         BE    AR40                     FROM CHECKING FOR BREAKS                
         B     ADD20                                                            
ADD10    CLC   LASTADD2,0(R2)           COULD ENCOUNTER THIS PROBLEM            
         BE    AR40                     FROM CHECKING FOR BREAKS                
*                                                                               
ADD20    MVC   COMMAND,=CL8'DMWRT'                                              
         CLI   DMACT,C'A'               ADDING NEW REC                          
         BNE   AR00                     NO                                      
         MVC   COMMAND,=CL8'DMADD'                                              
         AP    RECSADED,=P'1'                                                   
         B     AR07                                                             
AR00     AP    RECSROTE,=P'1'                                                   
***********************************************************************         
*        OPT 6 IS FOR TESTING TO DITTO OUT KEYS OF ADDS AND WRITES              
*        REPORT IS NOT RELIABLE FOR TESTING SINCE IT IS OUT OF SYNCH            
*        WITH DATAMGR WRITES                                                    
***********************************************************************         
AR07     DS    0H                                                               
         CLC   ACTRLEN,=X'0032'                                                 
         BH    AR0700                                                           
         LA    R6,ELBUILD                                                       
         XC    ELBUILD,ELBUILD                                                  
         MVC   0(3,R6),=X'FE0300'                                               
         GOTO1 ADDEL,DMCB,(R2),(R6)                                             
         OI    ACTRSTA,X'80'            MARK DELETED                            
                                                                                
AR0700   CLC   COMMAND(5),=C'DMADD'                                             
         BE    AR07A                                                            
         CLC   ACTRLEN,=X'0035'         WILL BE SKIPPING ADDING THESE           
         BH    AR07A                    AT DATAMGR CALL                         
         OI    ACTRSTA,X'80'            MARK DELETED                            
                                                                                
AR07A    CLI   QOPT6,C'W'               DITTO OUT ALL WRITES (ADDS TOO)         
         BE    AR08                                                             
         CLI   QOPT6,C'A'               DITTO OUT ONLY ADDS                     
         BNE   AR10                                                             
         CLC   COMMAND(5),=C'DMADD'                                             
         BNE   AR10                                                             
                                                                                
AR08     MVC   P,SPACES                                                         
         MVC   P+5(60),0(R2)                                                    
         MVC   P+70(8),=C'COMMAND='                                             
         MVC   P+78(8),COMMAND                                                  
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
AR10     DS    0H                                                               
*                                                                               
         CLI   RCWRITE,YES                                                      
         BNE   AR40                                                             
         CLC   ACTRLEN,=H'1000'                                                 
         BNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
AR30     GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
         CLI   DMCB+8,0                                                         
         BE    *+8                                                              
         MVI   ADDOK,NO                                                         
*                                                                               
AR40     ST    R2,ADADDR                  NEED THIS TO AVOID PRINTING           
**       MVC   LASTADD1,0(R2)                                                   
         AP    RECCNT,=P'1'               NON PROCESSED ACCOUNTS                
         CLI   KEYSW,1                                                          
         BNE   AR50                                                             
         MVC   LASTADD1,0(R2)           COULD ENCOUNTER THIS PROBLEM            
         B     AR60                                                             
AR50     MVC   LASTADD2,0(R2)           COULD ENCOUNTER THIS PROBLEM            
         BE    AR40                     FROM CHECKING FOR BREAKS                
AR60     B     B4EXT                                                            
         DROP  R2                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* ROUTINE BUILDS A BUDGET AMOUNT REC KEY FROM  THE CURRENT                      
* SORT RECORD.                                                                  
*---------------------------------------------------------------------*         
ACTBDG   NTR1                                                                   
         USING B4ULTBLD,R6                                                      
         L     R6,SUBTBL                                                        
         MVC   KEY3SAVE,SPACES                                                  
         MVI   KEY3SAVE,BUDKTYPQ        X'1B'                                   
         MVC   KEY3SAVE+33(2),OLDBDGNO  USE FROM BDG TYPE NO. IF DELETE         
         CLI   ACTION,C'D'                                                      
         BE    AB05                                                             
         CLI   ACTION,C'A'         SAME FOR AMEND                               
         BE    AB05                                                             
         MVC   KEY3SAVE+33(2),NEWBDGNO NEW BDG NUMBER                           
AB05     XC    KEY3SAVE+35(14),KEY3SAVE+35   SPARE BINARY 0'S AND REST          
*                                                                               
         LA    R1,SORTKEY          SET R1 TO A(CO-U/L-ACT OF SORT REC)          
         CLI   ACTION,C'T'         ACTUAL TRANSACTION UPDATE                    
         BE    AB10                YES                                          
         LA    R1,SORTKEY+1        FOR-DELETE-AMEND, PAST KEY ID                
         CLI   ACTION,C'G'                                                      
         BNE   AB10                                                             
         LA    R1,SORTKEY+17       FOR S-G UPDATE FROM ACCT U/L                 
AB10     CLI   B4ACTID,C'A'        GET MATCHING U/L ENTRY FROM TABLE            
         BNE   AB20                                                             
         ST    R6,AULENTRY         SAVE A(CURRENT SORTRECS U/L ENTRY)           
         CLC   1(2,R1),B4ULENT                                                  
         BE    AB30                                                             
AB20     AHI   R6,B4ENTLEN         NEXT ENT                                     
         CLI   B4ACTID,X'FF'                                                    
         BNE   AB10                                                             
         DC    H'0'                NOT FOUND                                    
*                                                                               
AB30     CLI   ACTION,C'G'                                                      
         BNE   AB35                                                             
         MVC   KEY3SAVE+1(1),QCOMPANY                                           
         MVC   KEY3SAVE+2(14),SORTTO   POSTING TO U/L ACCOUNT                   
         MVC   SVUNTNM(7),=C'GENERAL' UNIT NAME                                 
         LA    R1,GBNAME                                                        
         CLC   SORTTO(2),=C'GB'                                                 
         BE    *+8                                                              
         LA    R1,GPNAME                                                        
         MVC   SVLDGNM,0(R1)       LEDGER NAME                                  
         MVC   SVRULE,SORTRULE     SOURCE                                       
         B     AB80                NO CONTRAS ON GENERAL ACCOUNTS               
*                                                                               
AB35     MVC   KEY3SAVE+1(3),0(R1) CO, U/L                                      
         ZIC   R7,B4ACTLEN         =LENGTH OF ACCOUNT TO NEW BUDGET KEY         
         LTR   R7,R7                                                            
         BZ    AB40                NO ACCOUNT                                   
         BCTR  R7,0                                                             
*                                  FILL BDG KEY WITH DIRECTED ACCT LEN          
         EXMVC R7,KEY3SAVE+4,3(R1) ACCOUNT                                      
*                                                                               
AB40     LA    R1,SORTKEY+17       SET R1 AT A(SORT RECS CONTRA)                
         CLI   ACTION,C'T'         FOR T OPT                                    
         BE    AB43                                                             
         LA    R1,SORTKEY+18       FOR C,D,A OPT                                
*                                                                               
AB43     XC    ACNTENT,ACNTENT                                                  
         CLC   0(15,R1),SPACES     IS THERE A CONTRA                            
         BE    AB75                NO                                           
*                                                                               
AB50     AHI   R6,B4ENTLEN         TO CONTRA ENTRY UNDER CURRENT ACCT           
*                                                                               
         CLI   ACTION,C'A'         IF AMEND                                     
         BNE   *+12                                                             
         AHI   R6,B4ENTLEN         ADVANCE TO TARGET CONTRA                     
         B     AB60A                                                            
*                                                                               
         CLC   0(3,R1),=XL3'FFFFFF'                                             
         BE    AB75                                                             
         CLI   B4ACTID,C'C'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   B4ULENT(3),=C'ALL'  FOR ALL                                      
         BE    AB75                NO CONTRA                                    
         LA    RE,15                                                            
         CLI   B4ULENT,C'+'        + GETS WHOLE ACCT LEN                        
         BE    AB65                                                             
*                                                                               
AB60     CLC   1(2,R1),B4ULENT     GET MATCH ON CONTRA U/L                      
         BNE   AB50                                                             
*                                                                               
AB60A    ZIC   RE,B4ACTLEN                                                      
         AHI   RE,3                +CO, U/L LENGTH                              
AB65     BCTR  RE,0                                                             
         ST    R6,ACNTENT          A(CURRENT SORTREC CONTRA TBL ENT)            
         EXMVC RE,KEY3SAVE+18,0(R1) CONTRA ACCT                                 
*                                                                               
         CLI   ACTION,C'A'         IF AMEND                                     
         BNE   *+10                                                             
         MVC   KEY3SAVE+19(2),B4ULENT    TARGET CONTRA U/L TO KEY               
*                                                                               
AB75     L     R6,AULENTRY         INIT FIELDS FOR REPORT PRINT/BANNER          
         MVC   SVUNTNM,B4UNNAM     UNIT NAME                                    
         MVC   SVLDGNM,B4LDGNAM    LEDGER NAME                                  
AB80     ZIC   RE,B4ULLVL                                                       
         LA    RF,SORTANAM         LOWEST LVL                                   
AB81     BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    AB82                                                             
         AHI   RF,36                                                            
         B     AB81                                                             
*                                                                               
AB82     MVC   SAVENAME,0(RF)      LOWEST LVL ACCOUNT NAME FOR PRINT            
         MVC   SAVETO,SORTTO       POST TO ACCOUNT                              
         MVC   SVPOSTNM,SORTPNAM   POST TO ACCOUNT NAME                         
         MVC   SAVEFROM,SORTFROM   POSTING FROM ACCOUNT                         
         MVC   SAVEANM,SORTANAM    ACCOUNT LEVEL NAMES                          
         MVC   SAVEBNM,SORTBNAM                                                 
         MVC   SAVECNM,SORTCNAM                                                 
         MVC   SAVEDNM,SORTDNAM                                                 
         LA    R1,SORTKEY+1                                                     
         CLI   ACTION,C'T'                                                      
         BE    *+8                                                              
         LA    R1,1(,R1)                                                        
         MVC   SVUNT,0(R1)         UNIT                                         
         MVC   SVLDG,1(R1)         LEDGER                                       
         B     B4EXT                                                            
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* CONSTRUCTS BUDGET RECORDS TO UPDATE OR ADD TO FILE                            
*---------------------------------------------------------------------*         
         USING BUDRECD,R2          COVERS BDG KEY                               
BUILDREC NTR1                                                                   
         L     R2,AIO1                                                          
         ZAP   RADDTOT,=P'0'                                                    
         LA    R5,SORTDT1          R5=A(DATES BKTS IN SORTREC)                  
*                                                                               
         CLI   FIRST,0             FIRST PASS                                   
         BE    BR020               BUILD FIRST                                  
         CLI   ADDKEY,YES          IF I'VE JUST ADDED A REC THEN I              
         BE    BR020               NEED TO READ FOR THE NXT BDG REC             
*                                  IN KEY3SAVE                                  
         CLI   ACTION,C'G'                                                      
         BNE   BR012                                                            
         LA    R5,POSTBKT          LOWEST LVL FOR G ACT TOTS                    
         B     BR014                                                            
*                                                                               
         USING TOTALSD,R5          AMNTS WITH LOWEST LEVEL ACCUMS               
BR012    LA    R5,CNTRTOT          IF NOT, GO REPLACE CURRENT BUDG EL           
BR013    OC    TOTACT,TOTACT       1ST NON-ZERO IS LOWEST LEVEL TOTAL           
         BZ    BR015                                                            
                                                                                
BR014    LA    R5,TOTMN1           TO 1ST MONTH                                 
         B     BR160                                                            
                                                                                
BR015    AHI   R5,TOTALSLN                                                      
         B     BR013                                                            
*                                                                               
BR020    MVC   BUDKEY,KEY3SAVE     CURRENT BUDGET KEY                           
         MVC   COMMAND,=CL8'DMREAD'                                             
         OI    DMINBTS,X'88'       READ FOR DELETE-UPDATE                       
         GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
         NI    DMINBTS,X'FF'-X'88' TURN'EM OFF                                  
         TM    DMCB+8,X'FF'                                                     
         BZ    BR150               FOUND AND OK                                 
*                                                                               
         TM    DMCB+8,X'02'        WAS THE RECORD DELETED                       
         BZ    BR100                                                            
         CLI   ACTION,C'D'         IF DELETE                                    
         BE    B4EXT               RETURN READ NEXT SORT REC                    
         MVI   BUDRSTA,0           FOR REST, TURN OFF DELETE                    
         B     BR120               SET ACT TO ADD                               
*                                                                               
BR100    TM    DMCB+8,X'10'        REC NOT FOUND                                
         BNZ   *+6                 SET ACT TO ADD                               
         DC    H'0'                SOMETHING HORRIBLE                           
         CLI   ACTION,C'D'         FOR DELETE                                   
         BE    B4EXT               RETURN FOR NEXT SORT REC                     
         MVC   BUDKEY(49),KEY3SAVE RESTORE KEY TO IOAREA                        
         MVI   DMACT,C'A'          INDICATE ADD NEW RECORD                      
         B     *+8                                                              
BR120    MVI   DMACT,C'W'          WRITE THIS RECORD                            
         MVI   BUDRLEN+1,50        LENGTH FOR 50                                
         XC    BUDKEY+ACCORFST(12),BUDKEY+ACCORFST                              
         B     BR250               GO ADDELS                                    
***********************************************************************         
*              DELETE EXISTENT BDG AMNTS WITHIN                                 
*              RQST DATE RANGE FOR ALL PROCESSES                                
***********************************************************************         
BR150    MVI   DMACT,C'W'          INDICATE WRITE TO ADDREC                     
         CLI   SAVOPT1,C'M'                                                     
         BNE   BR180                                                            
         L     R2,AIO1                                                          
         BAS   RE,RDELTTL                                                       
BR160    DS    0H                                                               
*                                                                               
BR180    ZIC   R3,MNTHCNT          NUMBER OF SORT BKTS                          
         LR    R6,R5               SAVE                                         
BR200    CLC   BUDRLEN,=X'0035'                                                 
         BNH   BR210                                                            
         CLI   BUDKEY+ACCORFST,EOR                                              
         BE    BR210                                                            
         GOTO1 DELEL,DMCB,('BAMELQ',(R2)),(2,(R5)),0                            
         AHI   R5,TOTMNTHQ         NEXT DATE                                    
         BCT   R3,BR200                                                         
                                                                                
BR210    CLI   ACTION,C'D'                                                      
         BE    B4EXT               FOR DELETE THAT'S QUITE ENOUGH               
         LR    R5,R6               RESTORE                                      
*                                                                               
BR250    ZIC   R3,MNTHCNT                NUMBER OF SORT BKTS                    
BR330    OC    2(L'TOTAMNT1,R5),2(R5)    DON'T ADD ELS WITH NIL                 
         BZ    BR350                                                            
                                                                                
         USING BAMELD,R6                                                        
BR340    LA    R6,ELBUILD                                                       
         MVI   BAMEL,BAMELQ        X'1D'                                        
         MVI   BAMLN,BAMLNQ                                                     
         MVC   BAMMNTH(TOTMNTHQ),0(R5)      SORT DATE/AMNT                      
         AP    RADDTOT,2(L'TOTAMNT1,R5)                                         
         GOTO1 ADDEL,DMCB,(R2),(R6)                                             
BR350    AHI   R5,TOTMNTHQ         TO NEXT SORT DATE                            
         BCT   R3,BR330                                                         
         B     B4EXT                                                            
         DROP  R5,R6                                                            
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        TOTAL BUDGET ONLY BETWEEN REQUEST START AND END DATES                  
*--------------------------------------------------------------------*          
RDELTTL  NTR1                                                                   
         ST    RE,SAVRE                                                         
         MVC   SVYEAR,MYEND                                                     
         ZAP   RDELTOT,=P'0'                                                    
                                                                                
         USING BAMELD,R2                                                        
         MVI   ELCODE,BAMELQ                                                    
         BAS   RE,B4GETEL                                                       
         B     *+8                                                              
RDE100   BAS   RE,NXTELL                                                        
         BNE   RDEXIT                                                           
         CLC   BAMMNTH,MYSTART       IS THIS ELEMENT WITHIN REQUEST             
         BL    RDE100                                                           
         CLC   BAMMNTH,MYEND         IS THIS ELEMENT WITHIN REQUEST             
         BH    RDE100                                                           
         AP    RDELTOT,BAMBUDG       ADD TO BUDGET TOTAL                        
         B     RDE100                                                           
*                                                                               
RDEXIT   DS    0H                                                               
         L     RE,SAVRE                                                         
         B     LVLXIT                                                           
         DROP  R2                                                               
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        REALLOCATE BUDGET TO KEEP SAME OVERALL TOTAL                           
*        IF DELETED TOTAL EQUALS ZERO MEANS NO BUDGET REC WAS EVER              
*        SET UP FOR THIS ACCOUNT, THEREFORE NO REALLOCATION NECESSARY           
*--------------------------------------------------------------------*          
         USING BAMELD,R2                                                        
BUDUPELS NTR1                                                                   
         ST    RE,SAVRE                                                         
         OC    RDELTOT,RDELTOT          IF ZERO NO BUDGET REC                   
         BZ    BUDUPXIT                 EXIT                                    
         ZAP   ELCNT,=P'0'              NUMBER OF ELEMENTS UPDATED              
         ZAP   RDIFTOT,RDELTOT          CALCULATE BUDGET DIFFERENCE             
         SP    RDIFTOT,RADDTOT                                                  
         SRP   RDIFTOT,62,5                                                     
*                                                                               
         XC    SAV1DEL,SAV1DEL                                                  
         MVI   ELCODE,BAMELQ            X'1D'                                   
         BAS   RE,B4GETEL                                                       
         BNE   BUD200                                                           
         B     BUD120                                                           
BUD100   BAS   RE,NXTELL                DISTRIBUTE BUDGET DIFFERENCE            
         BNE   BUD200                   OVER                                    
BUD120   CLC   BAMMNTH(1),SVYEAR        ONLY DISTRIBUTE TO END OF YEAR          
         BL    BUD100                                                           
         BH    BUD200                                                           
         CLC   BAMMNTH,MYEND                                                    
         BL    BUD100                                                           
         BE    BUD100                                                           
         OC    SAV1DEL,SAV1DEL                                                  
         BNZ   BUD130                                                           
         ST    R2,SAV1DEL                                                       
BUD130   AP    ELCNT,=P'1'                                                      
         CLI   BAMMNTH+1,X'12'         IS IT DECEMBER                           
         BNE   BUD100                                                           
*                                                                               
BUD200   DS    0H                                                               
         CP    ELCNT,=P'0'              NO ELEMENTS TO DISTRIBUTE OVER          
         BE    BUDUPXIT                                                         
         ZAP   RBUDWRK,RDIFTOT                                                  
         DP    RBUDWRK,ELCNT                                                    
         SRP   RBUDWRK(10),2,0                                                  
         L     R2,SAV1DEL               ADDRESS OF LAST ELEMENT CHANGED         
         B     BUD241                                                           
                                                                                
BUD240   BAS   RE,NXTELL                                                        
         BNE   BUDUPXIT                                                         
BUD241   CLC   BAMMNTH(1),SVYEAR                                                
         BNE   BUDUPXIT                                                         
         AP    BAMBUDG,RBUDWRK(10)                                              
BUD242   CP    RBUDWRK+10(2),=P'0'                                              
         BE    BUD250                   NO REMAINDER                            
         BH    BUD245                   REMAINDER IS POSITIVE                   
         SP    BAMBUDG,=P'100'          REMAINDER IS NEGATIVE                   
         AP    RBUDWRK+10(2),=P'1'                                              
         B     BUD250                                                           
BUD245   AP    BAMBUDG,=P'100'                                                  
         SP    RBUDWRK+10(2),=P'1'                                              
BUD250   DS    0H                                                               
         CLI   SAVOPT1,C'M'                                                     
         BNE   BUD253                                                           
         CLI   SAVPROF+1,C'Y'           IF NOT ON CAN HAVE NO NEGATIVE          
         BE    BUD255                   AMOUNTS IN BUDGETS                      
BUD253   CP    BAMBUDG,=P'0'                                                    
         BNL   BUD255                                                           
         ZAP   BAMBUDG,=P'0'                                                    
BUD255   CLI   BAMMNTH+1,X'12'                                                  
         BNE   BUD240                                                           
*                                                                               
BUDUPXIT L     RE,SAVRE                                                         
         B     LVLXIT                                                           
         DROP  R2                                                               
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*        PUT RECORDS TO SORT AND OPTIONALLY PRINT OUT DETAILS                   
*-------------------------------------------------------------------*           
PUTSORT  NTR1                                                                   
         OC    SORTCNT,SORTCNT                                                  
         BNZ   PUT20                                                            
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD OPEN SORT                           
PUT20    GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         L     R1,SORTCNT                                                       
         AHI   R1,1                COUNT RECS                                   
         ST    R1,SORTCNT                                                       
*                                                                               
         CLI   QOPT6,C'P'                            *                          
         BNE   PUT90                                 *                          
         MVC   P(80),SORTKEY                         *                          
         GOTO1 ACREPORT                              *                          
         LA    R4,6                                  *                          
         LA    R2,SORTDT1                            *                          
         LA    R3,P+1                                *                          
PUT30    MVC   TEMP(2),0(R2)                         *                          
         MVI   TEMP+2,X'01'                          *                          
         OC    2(8,R2),2(R2)                         *                          
         BZ    PUT50                                 *                          
         EDIT  (P8,2(R2)),(15,7(R3)),0,ZERO=NOBLANK,MINUS=YES                   
PUT50    GOTO1 DATCON,DMCB,(1,TEMP),(6,0(R3))        *                          
         LA    R2,10(,R2)                            *                          
         LA    R3,22(,R3)                            *                          
         BCT   R4,PUT30                              *                          
         GOTO1 ACREPORT                              *                          
*                                                    *                          
         LA    R4,6                                  *                          
         LA    R3,P+1                                *                          
PUT70    MVC   TEMP(2),0(R2)                         *                          
         MVI   TEMP+2,X'01'                          *                          
         OC    2(8,R2),2(R2)                         *                          
         BZ    PUT80                                 *                          
         EDIT  (P8,2(R2)),(15,7(R3)),0,ZERO=NOBLANK  *                          
PUT80    GOTO1 DATCON,DMCB,(1,TEMP),(6,0(R3))        *                          
         AHI   R2,10                                 *                          
         AHI   R3,22                                 *                          
         BCT   R4,PUT70                              *                          
         GOTO1 ACREPORT                              *                          
PUT90    B     B4EXT                                 *                          
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*        GET RECORDS FROM SORT AND OPTIONALLY PRINT OUT DETAILS                 
*-------------------------------------------------------------------*           
GETSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'GET',SORTREC                                      
         MVC   ASORT,DMCB+4                                                     
         OC    ASORT,ASORT         IS IT EOF                                    
         BNZ   GET05               YES                                          
         B     GET90                                                            
*                                                                               
GET05    L     RE,ASORT            A(RECORD FROM SORT)                          
         LA    R1,SORTRCLN                                                      
         LA    RF,SORTREC                                                       
         MOVE  ((RF),(R1)),(RE)    OUTPUT FROM SORTER TO SORTREC                
*                                                                               
         CLI   QOPT7,C'P'                             *                         
         BNE   GET90                                  *                         
         MVC   P(32),SORTKEY                          *                         
         GOTO1 ACREPORT                               *                         
         LA    R4,6                                   *                         
         LA    R2,SORTDT1                             *                         
         LA    R3,P+1                                 *                         
GET10    MVC   TEMP(2),0(R2)                          *                         
         MVI   TEMP+2,X'01'                           *                         
         OC    2(8,R2),2(R2)                          *                         
         BZ    GET20                                  *                         
         EDIT  (P8,2(R2)),(15,7(R3)),0,ZERO=NOBLANK,MINUS=YES                   
GET20    GOTO1 DATCON,DMCB,(1,TEMP),(6,0(R3))         *                         
         AHI   R2,10                                  *                         
         AHI   R3,22                                  *                         
         BCT   R4,GET10                               *                         
         GOTO1 ACREPORT                               *                         
*                                                     *                         
         LA    R4,6                                   *                         
         LA    R3,P+1                                 *                         
GET30    MVC   TEMP(2),0(R2)                          *                         
         MVI   TEMP+2,X'01'                           *                         
         OC    2(8,R2),2(R2)                          *                         
         BZ    GET40                                  *                         
         EDIT  (P8,2(R2)),(15,7(R3)),0,ZERO=NOBLANK   *                         
GET40    GOTO1 DATCON,DMCB,(1,TEMP),(6,0(R3))         *                         
         AHI   R2,10                                  *                         
         AHI   R3,22                                  *                         
         BCT   R4,GET30                               *                         
         GOTO1 ACREPORT                               *                         
GET90    B     B4EXT                                  *                         
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*              ROUTINE TO GET AN ELEMENT                                        
*                                                                               
*                  R2=A(IOAREA)                                                 
*              ELCODE=ELEMENT CODE                                              
*-------------------------------------------------------------------*           
B4GETEL  AH    R2,DATADISP                                                      
B4FRSTEL CLI   0(R2),0                                                          
         BNE   *+10                                                             
         CLI   0(R2),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R2)                                                     
         BCR   8,RE                                                             
*                                                                               
B4NXTEL  SR    RF,RF                                                            
         IC    RF,1(,R2)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,RF                                                            
         B     B4FRSTEL                                                         
*                                                                               
NXTELL   DS    0H                                                               
         ZIC   RF,1(,R2)                                                        
         AR    R2,RF                                                            
         B     B4FRSTEL                                                         
*--------------------------------------------------------------------*          
*              ROUTINE TO ADD AN ELEMENT                                        
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
*--------------------------------------------------------------------*          
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,ELIST,(C'P',=C'ACCOUNT '),(R2),(R3)                        
         CLI   DMCB+12,0                                                        
         BE    B4EXT                                                            
         DC    H'0'                CAN'T ADD THE ELEMENT                        
*--------------------------------------------------------------------*          
*              ROUTINE TO DELETE AN ELEMENT                                     
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
*--------------------------------------------------------------------*          
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(,R1)                                                        
         ZIC   R5,4(,R1)                                                        
         GOTO1 HELLO,DMCB,(C'D',=C'ACCOUNT '),((R4),(R2)),((R5),(R3))           
         B     B4EXT                                                            
         EJECT ,                                                                
*                                                                               
         USING NAMELD,R2                                                        
GETNAME  DS    0H                  COMMON RTN FOR RETRIEVAL OF NAMES            
         MVI   ELCODE,NAMELQ       R3 IS PRESET WITH RECEIVING ADDRESS          
         BAS   RE,B4GETEL                                                       
         BNER  R7                                                               
         ZIC   RE,NAMLN          CALCULATE LEN                                  
         SHI   RE,NAMLN1Q+1                                                     
         EXMVC RE,0(R1),NAMEREC                                                 
         BR    R7                                                               
         DROP  R2                                                               
*                                                                               
         EJECT ,                                                                
HELLO    DC    V(HELLO)                                                         
SQUASHER DC    V(SQUASHER)                                                      
SORTER   DC    V(SORTER)                                                        
SORTCARD DC    CL80'SORT FIELDS=(1,65,A),FORMAT=CH,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(367,,,,)'                             
*                                                                               
RELOTBL  DS    0A                                                               
         DC    A(PRNTITC)                                                       
         DC    A(PRNTTOTC)                                                      
         DC    A(INITITC)                                                       
         DC    A(MYIOCLC)                                                       
         DC    A(SUBTBLC)                                                       
         DC    A(APOSTBC)                                                       
         DC    A(ENDITC)                                                        
         DC    A(ACCTPLC)                                                       
         DC    A(TOTMNGC)                                                       
         DC    X'FF'                                                            
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* PUT ACCOUNT CODES AND NAMES TO P LINE                                         
*--------------------------------------------------------------------*          
         USING B4ULTBLD,R6                                                      
ACCTPLC  DS    0H                                                               
         NMOD1 0,*ENDI*                                                         
         L     RC,0(,R1)                                                        
         MVC   ACCTBLK,SPACES                                                   
         MVC   ACCTID(12),P+1                                                   
         L     R6,AULENTRY                                                      
         ZIC   RE,B4ULLVL                                                       
         LA    RF,SORTANAM                                                      
APL03    BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    APL05                                                            
         AHI   RF,36                                                            
         B     APL03                                                            
*                                                                               
APL05    MVC   ACCTNAME(36),0(RF)                                               
         GOTO1 SQUASHER,DMCB,ACCTBLK,49                                         
         LA    R8,P+1                                                           
         MVI   BYTE,15                                                          
APL10    GOTO1 CHOPPER,DMCB,(49,ACCTID),(BYTE,(R8)),(C'P',3)                    
         XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* ROUTINE BUILDS OR UPDATES U/L AND LEVEL TOTALS. TOTALS ARE                    
* CLEARED TO BINARY ZEROES AT PRINT TIME INDICATING NEED TO                     
* BUILD NEW TOTAL. NON-ZERO TOTALS REQUIRE UPDATE OF AMOUNT                     
* BUCKETS.                                                                      
*--------------------------------------------------------------------*          
         USING B4ULTBLD,R6         COVERS U/L TABLE                             
         USING TOTALSD,R2          COVERS TOTALS                                
TOTMNGC  DS    0H                                                               
         NMOD1 0,*TOTM*                                                         
         L     RC,0(,R1)                                                        
         L     R6,AULENTRY         A(CURRENT ACCT U/L ENTRY)                    
         L     R5,ACNTENT          A(CURRENT CNTR U/L ENTRY)                    
         LA    R2,ULTOT            A(U/L TOTAL)                                 
         MVI   BYTE,0              1ST PASS INDICATOR                           
*                                                                               
         CLI   ACTION,C'G'         IF SUBSIDIARY TO GENERAL UPDATE              
         BNE   TOT0                                                             
         LA    R7,2                                                             
         LA    R2,GLTOT            A(GENERAL LEDGER TOT)                        
         LA    R1,2                                                             
TOT      OC    0(14,R2),0(R2)      NEED TO BUILD KEY                            
         BNZ   TOT08               NO, UPDATE AMOUNTS                           
         BCTR  R1,0                                                             
         EXMVC R1,0(R2),KEY3SAVE+2                                              
         B     TOT5A                                                            
*                                                                               
TOT0     LA    R7,1                HOW MANY TOTALS ARE THERE-1 FOR U/L          
         ZIC   R1,B4ULLVL                                                       
         AR    R7,R1               + NUMBER OF ACCOUNT LEVELS                   
         LTR   R5,R5                                                            
         BZ    *+8                                                              
         AHI   R7,1                +1 FOR CONTRA                                
*                                                                               
         LA    R3,B4ULLVLA         A(LVL A LENGTH)                              
TOT01    SR    R0,R0               UPDATING OR BUILDING MODE                    
         OC    TOTACT,TOTACT       NEED NEW TOTAL                               
         BNZ   TOT07               NO                                           
         LR    R0,RB               R0 NE 0 MEANS BUILD NEW TOTAL                
         LA    R1,KEY3SAVE+2       A(CURRENT U/L)                               
         LA    R8,2                U/L LEN                                      
         CLI   BYTE,0              U/L(1ST) PASS                                
         BE    TOT03               YES                                          
         AHI   R1,2                A(CURRENT ACCOUNT)                           
         ZIC   R8,0(,R3)           CURRENT LVL LEN                              
         LTR   R5,R5               IS THERE A CONTRA                            
         BZ    TOT03               NO                                           
         CHI   R7,1                CONTRA(LAST) PASS                            
         BNE   TOT03               NO                                           
         LA    R8,14                                                            
         LA    R1,KEY3SAVE+19      A(U/L CURRENT CONTRA)                        
         LA    R2,CNTRTOT                                                       
*                                                                               
TOT03    LA    R0,ULTOT            IF I AM BUILDING A NEW U/L TOT               
         CLI   ACTION,C'G'         CLEAR KEY SAVED FROM SCAN OF FILE IN         
         BNE   *+8                 RDOTHERS ROUTINE-INDICATING NEW U/L          
         LA    R0,GLTOT                                                         
         CR    R0,R2                                                            
         BNE   *+10                                                             
         XC    KEY4SAVE,KEY4SAVE   THIS TELLS RDOTHERS RTN NEW U/L              
*                                                                               
         BCTR  R8,0                                                             
         EXMVC R8,TOTACT,0(R1)     NEW ACCOUNT TOT KEY                          
*                                                                               
TOT5A    CLI   ACTION,C'A'         IF AMEND                                     
         BNE   TOT5B               AND NOT LOWEST LVL TOT                       
         LA    RF,CNTRTOT          DON'T TOTAL HERE. TOTALS ARE UPDATED         
         LTR   R5,R5               AFTER FACTOR CALC ON LOWEST LEVEL.           
         BNZ   *+8                                                              
         L     RF,B4ULALOW                                                      
         CR    RF,R2                                                            
         BNE   TOT07                                                            
TOT5B    LA    R8,TOTDALN          LENGTH OF DATES-AMOUNTS                      
         BCTR  R8,0                                                             
         LA    R1,SORTDT1                                                       
         EXMVC R8,TOTMN1,0(R1)     DATES-AMOUNTS                                
*                                                                               
TOT07    CLI   ACTION,C'G'         IF S TO G                                    
         BE    TOT14                                                            
         CLI   BYTE,0              IF 1ST PASS                                  
         BE    *+8                                                              
         AHI   R3,1                DON'T ADVANCE A(LVL LENGTHS)                 
         MVI   BYTE,1                                                           
         LTR   R0,R0               ADDING TO EXISTENT AMOUNTS                   
         BNZ   TOT12               NO                                           
*                                                                               
TOT08    CLI   ACTION,C'A'         NO UPDATE OF HI LEVEL TOTS IF AMEND          
         BNE   TOT08A                                                           
         LA    RF,CNTRTOT                                                       
         LTR   R5,R5                                                            
         BNZ   *+8                                                              
         L     RF,B4ULALOW                                                      
         CR    RF,R2                                                            
         BNE   TOT10B                                                           
TOT08A   ZIC   R1,MNTHCNT                                                       
         LA    RE,TOTAMNT1                                                      
         LA    RF,SORTBKT1                                                      
TOT10    OC    0(8,RF),0(RF)       IF ALL BINARY 0'S IT'S NIL                   
         BZ    TOT10A              NOTHING TO ADD                               
         OC    0(8,RE),0(RE)       CURRENT BKT MAY HAVE NIL VAL                 
         BNZ   *+10                                                             
         ZAP   0(8,RE),=P'0'                                                    
         AP    0(8,RE),0(8,RF)     UPDATE TOTAL AMOUNT                          
TOT10A   AHI   RE,TOTMNTHQ                                                      
         AHI   RF,TOTMNTHQ                                                      
         BCT   R1,TOT10                                                         
                                                                                
TOT10B   CLI   ACTION,C'G'                                                      
         BE    TOT14                                                            
TOT12    CHI   R7,2                ABOUT TO LOOP FOR LAST PASS                  
         BNE   TOT12A              NO                                           
         LTR   R5,R5               YES, IS THERE CONTRA PROCESSING              
         BZ    TOT12A              NO                                           
         LA    R2,CNTRTOT          LOOP BACK TO UPDATE CONTRA TOTAL             
         B     TOT12B                                                           
                                                                                
TOT12A   LA    R1,TOTALSLN                                                      
         SR    R2,R1               NEXT LOWER TOTAL                             
TOT12B   BCT   R7,TOT01                                                         
         B     TOTXIT                                                           
*                                                                               
TOT14    CLI   BYTE,0                                                           
         BNE   TOT15                                                            
         LA    R1,TOTALSLN                                                      
         AR    R2,R1               TO PACTTOT(POST ACCOUNT TOTAL)               
         LA    R1,14               ACCOUNT LEN                                  
         MVC   TOTFROM,SORTFROM+2  POSTING FROM ACCOUNT                         
         MVC   TOTWRK,SPACES                                                    
         MVI   BYTE,1                                                           
         B     TOT                 LOOP TO FILL TOTACCT WITH POST TO            
*                                                                               
TOT15    CLC   TOTACT,POSTBKT      POSTTO/POSTFROM TO POSTTO                    
         BE    TOT16                                                            
         LA    R1,TOTALSLN                                                      
         BCTR  R1,0                                                             
         EXMVC R1,POSTBKT,TOTACT   INIT NEW POSTTO POSTBKT                      
         B     TOTXIT                                                           
*                                                                               
TOT16    LA    RE,POSTBKT+30       UPDATE POSTBKT                               
         LA    RF,TOTMN1                                                        
         ZIC   R1,MNTHCNT                                                       
TOT17    OC    2(L'TOTAMNT1,RF),2(RF) IF BINARY 0'S IT'S NIL                    
         BZ    TOT17A                                                           
         OC    2(L'TOTAMNT1,RE),2(RE)                                           
         BNZ   *+10                                                             
         ZAP   2(L'TOTAMNT1,RE),=P'0'                                           
         AP    2(L'TOTAMNT1,RE),2(L'TOTAMNT1,RF)                                
TOT17A   AHI   RF,TOTMNTHQ                                                      
         AHI   RE,TOTMNTHQ                                                      
         BCT   R1,TOT17                                                         
TOTXIT   XMOD1 1                                                                
         SPACE 2                                                                
         DROP  R6                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* CLOSE SORT-LOGO REPORTING                                                     
*--------------------------------------------------------------------*          
ENDITC   DS    0D                                                               
         NMOD1 0,*ENDI*                                                         
         L     RC,0(,R1)                                                        
         OC    SORTCNT,SORTCNT                                                  
         BZ    EN10                                                             
         GOTO1 SORTER,DMCB,=C'END'                                              
EN10     CLI   LIVESW,YES          LIVE REPORT IN RUN?                          
         BNE   CTXIT               NO - DONT DO CONTROL SHEET                   
*                                                                               
         MVI   NOBOX,YES           NO BOXES                                     
         XC    SVLDG,SVLDG         FORCES BANNER FORMAT                         
         L     R7,LOGOC                                                         
         USING LOGOD,R7                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 LOGO,DMCB,(R7)                                                   
         L     R6,VEXTRAS                                                       
         USING RUNXTRAD,R6                                                      
         L     R6,ADMASTD                                                       
         USING MASTD,R6                                                         
         L     R6,MCVREMOT                                                      
         USING REMOTED,R6                                                       
         OC    REMOTKEY,REMOTKEY                                                
         BZ    UPD820              NOT REMOTE                                   
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         XC    REMOTKEY,REMOTKEY                                                
UPD820   MVI   LOGOEND,C'X'                                                     
         MVI   LOGOTYPE,C'S'                                                    
         MVC   LOGONAME,=CL33'******** INTERNAL CONTROL *******'                
         MVC   LOGOADD,=CL33'******** DO NOT SEND OUT ******'                   
         MVC   LOGO1,=C'CONTROL'                                                
         GOTO1 LOGO,DMCB,(R7)                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         CLI   RQSTERR,C'Y'        WAS THERE AN ERROR ON THE REQUEST            
         BE    CTXIT               YES                                          
*                                                                               
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         LA    R7,3                                                             
         LA    R2,LOGLITS                                                       
         LA    R4,RECSADED                                                      
         LA    R3,P+22                                                          
MYLAB0   MVC   P+10(12),0(R2)                                                   
         CP    0(4,R4),=P'0'                                                    
         BNE   MYLAB1                                                           
         MVC   3(3,R3),=C'NIL'                                                  
         B     MYLAB2                                                           
MYLAB1   EDIT  (P4,0(R4)),(6,0(R3)),0                                           
MYLAB2   MVI   SPACING,2                                                        
         GOTO1 APRNTIT,DMCB,(RC)                                                
         LA    R2,12(,R2)                                                       
         LA    R4,4(,R4)                                                        
         BCT   R7,MYLAB0                                                        
CTXIT    XMOD1 1                                                                
LOGLITS  DC    CL12'RECORD ADDS '                                               
         DC    CL12'    DELETES '                                               
         DC    CL12'    CHANGES '                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* ROUTINE PUTS LEVEL BREAKS TO PRINT LINE                                       
* P1 = A(PRG WORK DSECT)                                                        
* P2 = A(TOT TO PRINT)                                                          
* P3 = 0 OR NON-ZERO 0=LOWEST LEVEL PASS                                        
*                   >0=HIGHER LEVEL PASSES                                      
*--------------------------------------------------------------------*          
         USING TOTALSD,R2                                                       
         USING B4ULTBLD,R6         COVERS U/L TABLE                             
PRNTTOTC DS    0D                                                               
         NMOD1 0,*PTOT*                                                         
         L     RC,0(,R1)                                                        
         L     R6,AULENTRY         A(CURRENT ACCT U/L ENTRY)                    
         L     R2,4(,R1)           A(CURRENT TOTAL)                             
         L     R7,8(,R1)           LEVEL OF TOTAL INDICATOR                     
*                                                                               
         CLI   ACTION,C'G'         IF SUBSIDIARY TO GENERAL                     
         BNE   PT10                                                             
*                                                                               
         LTR   R7,R7               U/L OR POSTTO BREAK                          
         BZ    PT00                POSTTO BREAK                                 
         LA    R4,GBNAME                                                        
         CLC   TOTACT(2),=C'GB'                                                 
         BE    *+8                                                              
         LA    R4,GPNAME                                                        
         MVC   P+17(8),=C'*TOTALS*'                                             
         SR    R7,R7               FORCE ACCT DATA TO CORRECT COLM              
         BAS   RE,ACCTIDS          PUT TO PRINT LINE U/L CODE/DESC              
         BAS   RE,ACTAMNTS                                                      
         B     PT60                                                             
*                                                                               
PT00     CLI   POSTCNT,0           IF = 1ST PASS FOR NEW POSTTO                 
         BNE   PT03                                                             
*                                                                               
         LA    R4,SVPOSTNM         POSTTO-POSTFROM TO PRINT LINE                
         BAS   RE,ACCTIDS          POSTTO ACCOUNT/DESC                          
*                                                                               
         LA    RE,P+33             PRINT SOURCE                                 
         MVI   0(RE),C'('                                                       
         MVC   1(1,RE),SVRULE                                                   
         MVI   2(RE),C')'                                                       
*                                                                               
PT03     DS    0H                                                               
         LA    R4,SAVENAME         POSTFROM TO PRINT LINE                       
         LR    R7,RB               FORCE TO CORRECT COLM OF REPORT              
         BAS   RE,ACCTIDS          SORTFROM/DESC TO PRINT                       
*                                                                               
         SR    R7,R7               FORCE NON-TOT(*) AMNT LINE                   
         BAS   RE,ACTAMNTS                                                      
         GOTO1 APRNTIT,DMCB,(RC)                                                
*                                                                               
         CLI   ADDKEY,YES          IF SET ITS A POSTTO BREAK                    
         BNE   PT05                AND I MAY HAVE TOTALS                        
*                                                                               
         BAS   RE,POSTTOT          POSTTO TOTS                                  
         B     PTEXT                                                            
*                                                                               
PT05     ZIC   R1,POSTCNT          COUNT POSTFROMS UNDER POSTTO                 
         AHI   R1,1                                                             
         STC   R1,POSTCNT                                                       
         B     PTEXT                                                            
         EJECT ,                                                                
*                                                                               
POSTTOT  NTR1                                                                   
         CLI   POSTCNT,0           DON'T REPEAT AMOUNTS                         
         BNH   PO10                                                             
         MVC   P+1(11),=C'*TOTALS FOR'                                          
         MVC   P+17(14),POSTBKT    POSTTO ACCOUNT                               
         LR    R3,R2               SAVE A(CURRENT TOT)                          
         LA    R2,POSTBKT                                                       
         LR    R7,RB               FORCE TOT(*)                                 
         BAS   RE,ACTAMNTS                                                      
         LR    R2,R3               RESTORE                                      
         SR    R7,R7                                                            
         GOTO1 APRNTIT,DMCB,(RC)   PRINT POSTTO TOTAL LINE                      
PO10     MVI   POSTCNT,0                                                        
         XC    POSTBKT,POSTBKT                                                  
         B     PTEXT                                                            
         EJECT ,                                                                
***********************************************************************         
*              PUT DATA TO PLINE FOR ACTIONS D, T, AND C.                       
***********************************************************************         
PT10     LA    R4,SAVENAME         ACCOUNT NAME                                 
         L     R5,B4ULALOW         IS IT A CONTRA BRK OR LOWEST LVL             
         CR    R2,R5                                                            
         BH    PT25                NO, IT'S A HI LVL BREAK                      
*                                                                               
PT12     L     RF,ACNTENT          IS THERE CONTRA PROCESSING                   
         LTR   RF,RF                                                            
         BZ    PT25                NO                                           
*                                                                               
         LA    R3,SORTKEY+3                                                     
         CLI   ACTION,C'T'                                                      
         BE    *+8                                                              
         AHI   R3,1                                                             
         ZIC   RE,B4ACTLEN         LOW LEVEL ACCOUNT LENGTH                     
         BCTR  RE,0                                                             
         EX    RE,PT15A            COMPARE LOW LEVEL ACCOUNT TO CURR            
         BE    PT15B                                                            
*                                                                               
         MVI   ACCTBRK,YES         INDICATE ACCOUNT BREAK CHKBRKS RTN           
         LA    RF,CNTRTOT          IS THIS A CONTRA BRK PASS                    
         CR    R2,RF                                                            
         BNE   PT15AA              NO, ACCOUNT                                  
         CLI   POSTCNT,0           MORE THAN 1 CONTRA UNDER CURRENT ACT         
         BNH   PT15C               NO PRINT ACCCT-CONTRA COL1-2                 
         MVI   PSWITCH,YES                                                      
         B     PT50                YES PRINT CONTRA COL2                        
*                                                                               
PT15AA   CLI   POSTCNT,0           MORE THAN 1 CONTRA UNDER CURRENT ACT         
         BNH   PT15C               PRINT ACCOUNT-CONTRA                         
         B     PT40                YES, PRINT TOTAL ACCOUNT LINE                
*                                                                               
PT15A    CLC   0(0,R5),0(R3)                                                    
*                                                                               
PT15B    CLI   POSTCNT,0           IF FIRST PASS THIS ACCOUNT                   
         BNE   PT50                PRINT CONTRA                                 
PT15C    LR    R2,R5               A(LOW LEVEL ACCOUNT TOTAL)                   
         SR    R7,R7               DIRECT PRINT LINE LOCATIONS                  
         B     PT45                PRINT ACCOUNT-CONTRA                         
*                                                                               
PT25     CR    R5,R2               ALWAYS PRINT LOW LEVEL                       
         BNE   PT27                                                             
         SR    R7,R7                                                            
         B     PT45                                                             
*                                                                               
PT27     LA    R1,ULTOT            ALWAYS PRINT U/L TOT                         
         CR    R1,R2                                                            
         BNE   *+12                                                             
         LA    R4,B4LDGNAM         LEDGER NAME                                  
         B     PT40                                                             
*                                                                               
         LA    R4,SAVEDNM          ACT LVL NM                                   
         LA    RE,LVLDTOT          DETERMINE LENGTH OF ACCT                     
PT39     CR    R2,RE               R2=A(CURRENT TOT)                            
         BE    PT40                                                             
         SHI   R4,36                                                            
         AHI   RE,TOTALSLN         NEXT TOT                                     
         B     PT39                                                             
*                                                                               
PT40     MVC   P+1(11),=C'*TOTALS FOR'                                          
         LR    R7,RB                                                            
*                                                                               
PT45     LTR   R7,R7               1ST PASS LOW LVL                             
         BNZ   PT45A               NO                                           
*                                                                               
PT45A    DS    0H                                                               
**T                                                                             
         CLC   TOTACT,SPACES                                                    
         BE    PT61                                                             
         OC    TOTACT,TOTACT                                                    
         BZ    PT61                                                             
**T                                                                             
         BAS   RE,ACCTIDS          ACT CODE AND DESCRIPTION TO P LINE           
         MVC   SAVEDACT,TOTACT     JUST PRINT CONTRAS WITH SAME ACTS            
         CLC   P+1(7),=C'*TOTALS'  IF LEVEL TOTAL                               
         BE    PT55                NO CONTRA-PRINT AMOUNTS                      
         L     R8,ACNTENT                                                       
         LTR   R8,R8               AND CONTRA PROCESSING                        
         BZ    PT55                                                             
PT50     LA    R2,CNTRTOT                                                       
         MVC   P+18(14),CNTRTOT    CONTRA ACCOUNT TO P LINE                     
         CLI   ACCTBRK,YES                                                      
         BE    PT55                                                             
         ZIC   RF,POSTCNT          COUNT CONTRAS UNDER A ACCOUNT                
         AHI   RF,1                                                             
         STC   RF,POSTCNT                                                       
*                                                                               
PT55     BAS   RE,ACTAMNTS                                                      
         XC    TOTACT,TOTACT                                                    
*                                                                               
PT60     GOTO1 APRNTIT,DMCB,(RC)                                                
PT61     LA    R1,ULTOT                                                         
         CR    R1,R2                                                            
         BE    PT65                                                             
         LA    R1,GLTOT                                                         
         CR    R1,R2                                                            
         BNE   *+8                                                              
PT65     MVI   FORCEHED,YES        PAGE BREAK FOR U/L                           
*                                                                               
PTEXT    XMOD1 1                                                                
         EJECT ,                                                                
***********************************************************************         
*              PUT AMOUNTS TO PRINT LINE                                        
***********************************************************************         
ACTAMNTS NTR1                                                                   
         ZIC   R5,MNTHCNT          NUMBER OF MONTHS                             
         LA    R4,TOTAMNT1         1ST ACCUM                                    
         XC    PASTOT,PASTOT       CLEAR TOTAL ACCUM                            
         LA    R3,P+37                                                          
         LR    R7,RB               R7=TOT LINE SWITCH                           
         CLC   P+1(7),=C'*TOTALS'                                               
         BE    PT66                                                             
         CLC   P+17(7),=C'*TOTALS'                                              
         BE    PT66                                                             
         SR    R7,R7                                                            
*                                                                               
         CLI   ACTION,C'A'         FOR ACTION AMEND MULTIPLY BDG VALS-          
         BNE   *+12                BY FACTOR REC PCTS.                          
         BAS   RE,FACTOR                                                        
         B     PT66                                                             
*                                                                               
         CLI   ACTION,C'T'                                                      
         BNE   *+8                                                              
         BAS   RE,RNDLVLS          ON LOW LVL-ROUND TOTS                        
*                                                                               
PT66     CLI   QPRNT,YES           QUARTER FORMAT(MORE THAN 3 MONTHS)           
         BNE   PT75A                                                            
         LA    RF,QTOT1                                                         
         XC    QTOT1(32),QTOT1     CLEAR QUARTER ACCUMS                         
PT67     LA    R8,3                3 MONTHS IS A QUARTER                        
PT70     OC    0(8,R4),0(R4)       MAY BE NIL(BINARY 0'S)                       
         BZ    PT73                                                             
         OC    0(8,RF),0(RF)       CHK TO INIT CURRENT TOT                      
         BNZ   *+10                                                             
         ZAP   0(8,RF),=P'0'                                                    
         AP    0(8,RF),0(8,R4)                                                  
PT73     BCTR  R5,0                DECREMENT TOTAL MONTHS                       
         AHI   R4,TOTMNTHQ                                                      
         BCT   R8,PT70                                                          
                                                                                
         AHI   RF,8                NEXT QTOT                                    
         LTR   R5,R5               ANOTHER QUARTER                              
         BZ    PT75                                                             
         CHI   R5,3                FIQURE COUNT FOR NEXT QUARTER                
         BNL   PT67                                                             
         LR    R8,R5                                                            
         B     PT70                                                             
*                                                                               
PT75     ZIC   R5,HOOKCNT          NUMBER OF QUARTERS                           
         LA    R4,QTOT1                                                         
PT75A    CHI   R5,1                1 MONTH REQUEST-NO TOTAL COLUMN              
         BE    *+8                                                              
         AHI   R5,1                FOR TOTAL COLUMN                             
*                                                                               
PT80     OC    0(8,R4),0(R4)       ANY VALUE TO PRINT                           
         BNZ   PT90                YES                                          
         LR    RE,R3                                                            
         AHI   RE,7                =PRINT LINE POSITION FOR 'NIL'               
         MVC   0(3,RE),=C'NIL'                                                  
         B     PT110                                                            
*                                                                               
PT90     OC    PASTOT,PASTOT       CHK TO INIT                                  
         BNZ   *+10                                                             
         ZAP   PASTOT,=P'0'                                                     
         CHI   R5,1                LAST PASS JUST PRINT TOT COLUMN              
         BE    *+10                                                             
         AP    PASTOT(8),0(8,R4)  ACCUM FOR TOT COLUMN THIS PASS                
         EDIT  (P8,0(R4)),(14,0(R3)),2,MINUS=YES                                
*                                                                               
PT110    LTR   R7,R7               IF TOTAL LINE                                
         BZ    PT110E                                                           
         LA    RF,1                IF U/L BREAK                                 
         LA    RE,ULTOT                                                         
         CR    RE,R2                                                            
         BE    PT110C                                                           
         CLI   ACTION,C'G'         OR IF SUBSIDIARY TO GENERAL                  
         BE    PT110C                                                           
         LA    RE,LVLDTOT                                                       
         LA    R1,B4ULLVLD         DETERMINE LOWEST LEVEL                       
PT110A   CLI   0(R1),0                                                          
         BNE   *+14                                                             
         BCTR  R1,0                CHK HIGHER LEN                               
         AHI   RE,TOTALSLN         NEXT HIGHER TOT                              
         B     PT110A                                                           
                                                                                
         LA    RF,1                1 ASTERISK FOR LOWEST LVL TOT LINE           
PT110B   CR    RE,R2               IF ON LVLATOT MAX ASTERISKS                  
         BE    PT110C                                                           
         AHI   RF,1                INCREASE A STAR FOR EACH LEVEL               
         AHI   RE,TOTALSLN         NEXT HIGHER TOT                              
         B     PT110B                                                           
PT110C   LR    R1,R3                                                            
PT110C2  CLI   0(R1),C' '          FIND START OF EDIT OUTPUT                    
         BNE   PT110D                                                           
         AHI   R1,1                                                             
         B     PT110C2                                                          
                                                                                
PT110D   SR    R1,RF               POSITION FOR STARS                           
         BCTR  RF,0                                                             
         LA    RE,STARS                                                         
         EXMVC RF,0(R1),0(RE)                                                   
*                                                                               
PT110E   AHI   R3,14               NEXT REPORT COLUMN                           
         AHI   R4,8                NEXT QTOT                                    
         CLI   QPRNT,YES                                                        
         BE    *+8                                                              
         AHI   R4,2                NEXT TOTAL BKT                               
         CHI   R5,2                ABOUT TO LOOP FOR TOTAL COLUMN               
         BNE   PT115               NO                                           
         LA    R4,PASTOT                                                        
         BCTR  R5,0                                                             
         B     PT80                                                             
PT115    BCT   R5,PT80                                                          
*                                                                               
         CLI   ACTION,C'A'         FOR AMEND CLEAR PRINTED TOTS                 
         BNE   PTEXT                                                            
         XC    TOTMN1(TOTDALN),TOTMN1                                           
         B     PTEXT                                                            
*                                                                               
STARS    DC    CL4'****'                                                        
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* ROUTINE CALCULATES NEW BUDGET REC AMOUNTS. EXISTENT AMNTS                     
* IN CURRENT SORT RECORD ARE MULTIPLIED BY FACTOR VALUES                        
* FOUND IN THE BUDGET RECORD INDICATED BY THE REQUEST.                          
*--------------------------------------------------------------------*          
*                                                                               
         USING BUDRECD,R5          COVERS BDG KEY                               
         USING B4ULTBLD,R6                                                      
*                                                                               
FACTOR   NTR1                                                                   
         L     R5,AIO2                                                          
         L     R4,AIO1             A(BUDGET REC ABOUT TO UPDATE FILE)           
         L     R6,SUBTBL           U/L TBL                                      
         SR    R7,R7               INDICATE SPECIFIC OR DEFAULT READ            
*                                                                               
* READ FOR SPECIFIC ACCOUNT LOCATED IN IO1 FIRST PASS.                          
* READ FOR A DEFAULT KEY ON 2ND PASS. DEFAULT HAS IO1 CONTRA UL/ACCT            
* PLACED IN IO2 ACCOUNT POSITION.                                               
*                                                                               
         AHI   R6,B4ENTLEN         ADVANCE TO FROM CONTRA U/L                   
FC05     MVC   BUDKEY,SPACES       BUILD FACTOR KEY                             
         MVC   BUDKEY(2),0(R4)     KEY ID, CO.                                  
         LA    RF,BUDKUNT                                                       
         LTR   R7,R7               SPECIFIC OR DEFAULT PASS                     
         BNZ   *+14                DEFAULT                                      
*                                                                               
         MVC   BUDKEY(19),0(R4)    KEY ID, CO., ACCOUNT, WRK CD, CO.            
         LA    RF,BUDKCUNT                                                      
         MVC   0(2,RF),B4ULENT     CONTRA U/L                                   
         AHI   RF,2                                                             
         ZIC   RE,B4ACTLEN         CONTRA LENGTH                                
         BCTR  RE,0                                                             
         EXMVC RE,0(RF),21(R4)     CONTRA ACCOUNT                               
         MVI   BUDKBUDN,0                FACTOR BUDGET NUMBER                   
         MVC   BUDKBUDN+1(1),QSRTAREA+1                                         
         XC    BUDKBUDN+2(7),BUDKBUDN+2  SPARE BINARY ZEROES                    
*                                                                               
         MVC   COMMAND,=CL8'DMREAD'                                             
         GOTO1 AMYIOCL,DMCB,(RC),(R5)                                           
         CLI   DMCB+8,0                                                         
         BE    FC15                FOUND                                        
         LTR   R7,R7               DEFAULT PASS                                 
         BNZ   *+10                YES                                          
         LR    R7,RB                                                            
         B     FC05                LOOP FOR READ OF DEFAULT                     
         XC    BUDKEY,BUDKEY                                                    
         B     PTEXT               NOT FOUND BDG AMOUNTS UNAFFECTED             
*                                                                               
FC15     LA    R0,GLTOT                                                         
         LA    R3,TOTMN1           A(1ST DATE-AMOUNT LOW LVL TOTAL)             
         ZIC   R7,MNTHCNT          NUMBER OF DATE AMOUNT PAIRS                  
         AHI   R5,49               TO FIRST FACTOR EL LOC                       
                                                                                
         USING BAMELD,R5                                                        
FC17     CLI   0(R5),EOR                                                        
         BE    FC36                                                             
         CLI   0(R5),BAMELQ        X'1D'                                        
         BNE   FC25                                                             
         CLC   BAMMNTH,0(R3)                                                    
         BE    FC27                                                             
         BL    FC25                NEED MORE RECENT EL DATE                     
         AHI   R3,TOTMNTHQ         NEXT SORT DATE                               
         B     FC30                                                             
                                                                                
FC25     ZIC   RE,1(,R5)                                                        
         AR    R5,RE                                                            
         B     FC17                                                             
*                                                                               
FC27     ZAP   WORK(16),2(8,R3)    SORT AMOUNT TO MULTIPLICAN                   
         MP    WORK(16),BAMBUDG    * FACTOR                                     
         SRP   WORK(16),64-4,5     BDGT AMNTS CARRY .00 PENNIES                 
         ZAP   2(8,R3),WORK+8(8)   QUOTIENT BACK TO SORT AMOUNT                 
         B     FC25                                                             
*                                                                               
FC30     BCT   R7,FC17                                                          
*                                                                               
*              UPDATE HIGHER LEVEL TOTS WITH FACTORED ANSWERS                   
FC36     LA    RE,TOTALSLN(,R2)    NEXT HIGHER LVL TOTS                         
FC36A    CR    RE,R0               IF = BEYOND PERTINENT TOTS                   
         BE    FC38                                                             
         CLI   TOTACT,0            IF =                                         
         BE    FC36C               LOOP FOR NEXT HIGHER LVL                     
         LA    RF,TOTAMNT1         A(1ST LOW LEVEL AMOUNT)                      
         LA    R1,32(,RE)          A(HIGHER LVL AMOUNT)                         
         ZIC   R7,MNTHCNT                                                       
FC36B    OC    0(8,RF),0(RF)       COULD BE BINARY ZEROES(NIL)                  
         BZ    FC36BB                                                           
         OC    0(8,R1),0(R1)       IF 1ST NEEDS INIT                            
         BNZ   *+10                                                             
         ZAP   0(8,R1),=P'0'                                                    
         AP    0(8,R1),0(8,RF)     UPDATE HIGHER LEVELS                         
FC36BB   AHI   RF,TOTMNTHQ                                                      
         AHI   R1,TOTMNTHQ                                                      
         BCT   R7,FC36B                                                         
                                                                                
FC36C    AHI   RE,TOTALSLN         TO NEXT HIGHER                               
         B     FC36A                                                            
***********************************************************************         
*              ZAP LOWEST LVL TOT AMOUNTS (FACTORED AMOUNTS)                    
*              INTO CORRESPONDING BUDGET RECORD AMOUNT FIELDS.                  
***********************************************************************         
FC38     LA    R5,49(,R4)          TO 1ST ELEMENT LOCATION                      
         LA    RE,TOTMN1                                                        
FC40     CLI   0(R5),EOR                                                        
         BE    PTEXT                                                            
         CLI   0(R5),BAMELQ        X'1D'                                        
         BE    FC45                                                             
FC43     ZIC   RF,1(,R5)                                                        
         AR    R5,RF                                                            
         B     FC40                                                             
                                                                                
FC45     CLC   BAMMNTH,0(RE)       TO DATE                                      
         BNE   FC47                                                             
         OC    2(L'TOTAMNT1,RE),2(RE)       DON'T ADD BINARY ZEROES             
         BZ    FC47A                                                            
         ZAP   BAMBUDG,2(L'TOTAMNT1,RE)                                         
         B     FC47A                                                            
                                                                                
FC47     BL    FC43                                                             
FC47A    AHI   RE,TOTMNTHQ                                                      
         BCT   R7,FC45                                                          
         B     PTEXT                                                            
         DROP  R5,R6                                                            
         EJECT ,                                                                
*                                                                               
RNDLVLS  NTR1                 FOR ACTUALS NEED TO ROUND REC BRK TIME            
         LA    R0,GLTOT            =A(BEYOND LAST TOT PERTINENT)                
RL05     LR    RF,R5               COUNT OF MONTHS                              
         LA    RE,TOTAMNT1                                                      
RL10     OC    0(L'TOTAMNT1,RE),0(RE)    IF NIL                                 
         BZ    RL12                                                             
         SRP   0(L'TOTAMNT1,RE),64-2,5   ROUND TOTAL $ AT LOW LVL BRKS          
         MP    0(L'TOTAMNT1,RE),=P'100'  CARRY 00 PENNIES                       
RL12     AHI   RE,TOTMNTHQ               NEXT TOT BKT                           
         BCT   RF,RL10                                                          
                                                                                
RL15     AHI   R2,TOTALSLN         NEXT HIGHER LVL TOT                          
         CR    R0,R2               IF = ROUNDING COMPLETE                       
         BE    PTEXT                                                            
         CLI   TOTACT,0            CHK IF REPORTING THIS LVL                    
         BE    RL15                CHK NEXT LVL                                 
         B     RL05                                                             
         EJECT ,                                                                
*--------------------------------------------------------------------*          
* PUT ACCOUNT CODES AND NAMES TO P LINE                                         
*--------------------------------------------------------------------*          
ACCTIDS  NTR1                                                                   
         CLI   ACTION,C'T'                                                      
         BNE   AT03                                                             
         CLC   P+1(11),=C'*TOTALS FOR'                                          
         BE    AT03                                                             
**T      CLC   SAVEDACT(12),TOTACT                                              
         CLC   SAVEDACT,TOTACT                                                  
         BE    PTEXT                                                            
AT03     MVC   ACCTBLK,SPACES                                                   
         MVC   ACCTID(12),TOTACT                                                
         CLI   ACTION,C'G'         IF ACTION G                                  
         BNE   AT05                                                             
         LTR   R7,R7               AND SORT FROM BREAK                          
         BZ    AT05                                                             
         MVC   ACCTID(14),TOTFROM  MOVE SORTFROM INFO                           
AT05     MVC   ACCTNAME(36),0(R4)                                               
         GOTO1 SQUASHER,DMCB,ACCTBLK,49                                         
         LA    R8,P+1                                                           
         MVI   BYTE,15                                                          
         LTR   R7,R7                                                            
         BZ    AT10                                                             
         CLI   ACTION,C'G'                                                      
         BE    *+8                                                              
         MVI   BYTE,20                                                          
         LA    R8,P+17                                                          
AT10     GOTO1 CHOPPER,DMCB,(49,ACCTID),(BYTE,(R8)),(C'P',3)                    
         B     PTEXT                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT ,                                                                
*---------------------------------------------------------------------*         
* ANALYZE REQUEST FOR BANNER FORMAT-PRINT A LINE                                
*---------------------------------------------------------------------*         
PRNTITC  DS    0D                                                               
         NMOD1 0,*PRNL*                                                         
         L     RC,0(,R1)                                                        
*                                                                               
         CLI   ACTION,C'C'                                                      
         BNE   PI01                                                             
         OC    P+18(12),P+18                                                    
         BZ    PIXIT                                                            
PI01     CLC   KEYDEL1+18(3),=X'FFFFFF'      SORTBREAK                          
         BNE   PI05                                                             
         CLC   KEYDEL2+18(3),=X'FFFFFF'                                         
         BNE   PI02                                                             
         CLC   KEY3SAVE+18(3),=X'FFFFFF'                                        
         BNE   PI02                                                             
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         B     PIXIT                                                            
*                                                                               
PI02     DS    0H                                                               
         CLI   ACTION,C'T'                                                      
         BNE   PI02A                                                            
         OC    P+18(12),P+18                                                    
         BZ    PIXIT                                                            
         CLI   DMACT,C'A'          OMIT PRINTING ACCOUNTS I PROCESSED           
         BNE   PI05                THAT HAD NO CORRESPONDING BUDGET             
         L     R6,ADADDR                                                        
         TM    44(R6),X'80'                                                     
         BNO   PI05                                                             
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         B     PIXIT                                                            
*                                                                               
PI02A    DS    0H                                                               
         CLI   DMACT,C'A'          OMIT PRINTING ACCOUNTS I PROCESSED           
         BNE   PI03                THAT HAD NO CORRESPONDING BUDGET             
         L     R6,ADADDR                                                        
         TM    44(R6),X'80'                                                     
         BO    PIXIT                                                            
PI03     DS    0H                                                               
         OC    P+18(12),P+18                                                    
         BZ    PIXIT                                                            
                                                                                
         USING B4ULTBLD,R6                                                      
         L     R6,AULENTRY                                                      
         L     RF,B4ULALOW         CURRENTLY ON LOW LVL OR CONTRA PASS          
         CR    R2,RF                                                            
         BL    PIXIT               NO, HI LVL TOTS                              
         DROP  R6                                                               
                                                                                
**       L     R6,AULENTRY                                                      
**       USING B4ULTBLD,R6                                                      
**       LA    R6,B4ENTLEN(R6)                                                  
**       CLC   B4ULENT(3),=C'ALL'                                               
**       BNE   PIXIT                                                            
**       DROP  R6                                                               
***                                                                             
PI05     CLI   QOPT3,C' '                                                       
         BE    *+12                                                             
         CLI   ACTION,C'A'                                                      
         BNE   *+10                                                             
         MVC   HEAD2+1(40),XTRALIT                                              
*                                                                               
PI07     MVC   HEAD7+1(6),=C'LEDGER'                                            
         MVC   HEAD7+10(1),SVLDG                                                
         MVC   HEAD7+12(36),SVLDGNM                                             
         MVC   HEAD6+1(4),=C'UNIT'                                              
         MVC   HEAD6+10(1),SVUNT                                                
         MVC   HEAD6+12(36),SVUNTNM                                             
*                                                                               
         OC    START,START                                                      
         BZ    PI20                                                             
         CLI   MNTHCNT,1                                                        
         BNE   PI10                                                             
         MVC   HEAD6+83(16),=C'FOR THE MONTH OF'                                
         MVC   HEAD6+100(6),START                                               
         B     PI20                                                             
PI10     MVC   HEAD6+82(4),=C'FROM'                                             
         MVC   HEAD6+87(6),START                                                
         MVC   HEAD6+94(2),=C'TO'                                               
         MVC   HEAD6+97(6),END                                                  
*                                                                               
PI20     CLI   ACTION,C'G'                                                      
         BNE   PI21                                                             
         MVC   HEAD10+1(15),=C'POST TO ACCOUNT'                                 
         MVC   HEAD10+17(12),=C'FROM ACCOUNT'                                   
         MVI   HEAD10+34,C'S'                                                   
         B     PI21A                                                            
*                                                                               
PI21     MVC   HEAD10+1(7),=C'ACCOUNT'                                          
         MVC   HEAD10+17(14),=C'CONTRA ACCOUNT'                                 
*                                                                               
PI21A    LA    R3,DATEBANR                                                      
         ZIC   R7,MNTHCNT                                                       
         CHI   R7,3                                                             
         BNH   PI28                                                             
*                                                                               
         LA    R6,HEAD10+38        QUARTERS AND SINGLES                         
PI23     MVC   0(6,R6),0(R3)       1ST MONTH OF QUARTER                         
         LA    R3,28(,R3)                                                       
         MVI   6(R6),C'-'                                                       
         MVC   7(6,R6),0(R3)       LAST MONTH OF QUARTER                        
         AHI   R6,14               NEXT LOC                                     
         AHI   R3,14               NEXT MONTH                                   
         SHI   R7,3                                                             
         CHI   R7,3                ANOTHER QUARTER                              
         BNL   PI23                YES                                          
         LTR   R7,R7               MORE                                         
         BNZ   PI25                YES                                          
         SHI   R6,14                                                            
         B     PI27                                                             
                                                                                
PI25     CHI   R7,1                                                             
         BE    PI26                ODD MONTH                                    
         MVC   0(6,R6),0(R3)       ODD 2 MONTHS                                 
         MVI   6(R6),C'-'                                                       
         AHI   R3,14                                                            
PI26     MVC   7(6,R6),0(R3)       LAST MONTH OF QUARTER                        
PI27     MVC   20(5,R6),=C'TOTAL'                                               
         B     PI30                                                             
*                                                                               
PI28     LA    R6,HEAD10+42        SINGLE MONTHS                                
         CLI   MNTHCNT,1                                                        
         BE    PI28B                                                            
         AHI   R7,1                                                             
PI28A    CLI   R7,1                                                             
         BNE   PI28C                                                            
PI28B    LA    R2,4                                                             
         B     *+8                                                              
PI28C    LA    R2,5                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(6,R6),0(R3)                                                    
         AHI   R6,14                                                            
         AHI   R3,14                                                            
         BCT   R7,PI28A                                                         
*                                                                               
PI30     ZIC   R6,HDLNLEN                                                       
         LA    R7,HEAD3+51                                                      
         LR    R5,R6                                                            
         SRL   R6,1                                                             
         SR    R7,R6                                                            
         EXMVC R5,0(R7),HDLINLIT                                                
         MVC   HEAD2+82(7),=C'*DRAFT*'                                          
         CLI   QOPT2,C'L'               LIVE REPORT                             
         BNE   PI35                                                             
         MVC   HEAD2+82(6),=C'*LIVE*'                                           
PI35     MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
PIXIT    XMOD1 1                                                                
         SPACE  2                                                               
         LTORG                                                                  
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        INITIAL AND EDIT FOR REQUEST                                           
*--------------------------------------------------------------------*          
INITITC  DS    0D                                                               
         NMOD1 0,*MYIN*                                                         
         L     RC,0(,R1)                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=X'0001'                                                    
*                                                                               
         MVC   SAVCOMP,QCOMPANY                                                 
         MVC   SAVQACT,QACCOUNT                                                 
         MVC   SAVPROF,PROGPROF                                                 
         MVC   SAVOPT4,QOPT4                                                    
         MVC   SAVOPT1,QOPT1                                                    
         MVC   FRMUL,QCUL                                                       
         MVC   TOUL,QSELECT+2                                                   
*                                                                               
         MVI   UPNILS,NO                                                        
         CLI   QOPT4,NO                                                         
         BE    INIT04                                                           
         CLI   QOPT4,YES                                                        
         BE    INIT03                                                           
         CLI   QOPT1,C'T'                                                       
         BE    INIT04                                                           
         CLI   QOPT1,C'M'                                                       
         BNE   INIT04                                                           
         CLI   PROGPROF,NO                                                      
         BE    INIT04                                                           
INIT03   MVI   UPNILS,YES                                                       
*                                                                               
INIT04   XC    REQPCT,REQPCT                                                    
         OC    QSRTAREA+2(4),QSRTAREA+2     DID THEY INPUT FACTOR PCT?          
         BZ    INIT05                                                           
         ICM   R0,15,QSRTAREA+2                                                 
         CVD   R0,WORK                                                          
         ZAP   REQPCT,WORK+4(4)                                                 
**       EDIT  (P4,REQPCT),(7,P+20),2,MINUS=YES    *FOR MY TESTING I            
**       MVI   SPACING,2                           PRINT OUT PERCENT            
**       GOTO1 ACREPORT                            AS I GOT IT                  
**       MVI   FORCEHED,YES                                                     
*                                                                               
INIT05   LA    R7,RECWRKLN         CLEAR WORK                                   
         LA    R6,MYWORK                                                        
         LA    R1,0                                                             
         MVCL  R6,R0                                                            
*                                                                               
         LA    RF,40                                                            
         L     RE,SUBTBL           CLEAR TABLES OF 1ST ENTRY                    
         USING B4ULTBLD,RE                                                      
INIT08   XC    B4ACTID(B4ENTLEN),B4ACTID                                        
         AHI   RE,B4ENTLEN                                                      
         BCT   RF,INIT08                                                        
         DROP  RE                                                               
*                                                                               
         LA    R7,POSTLN                                                        
         L     R6,APSTB                                                         
         LA    R1,0                                                             
         MVCL  R6,R0                                                            
*                                                                               
         LA    R1,IOAREAS          SET IOAREA ADDRESSABILITY                    
         ST    R1,AIO1                                                          
         LA    R1,1008(,R1)                                                     
         ST    R1,AIO2                                                          
*                                                                               
         MVI   LIVESW,NO           SET LIVE FOR RUN SWITCH                      
         CLI   QOPT2,C'L'          IF LIVE RUN                                  
         BNE   *+8                                                              
         MVI   LIVESW,YES          SET LIVE FOR RUN SWITCH                      
*                                                                               
* EDIT QSRT POS 1-2(CONTAINS FROM AND ACT UPON BUDGET NUMBERS). CONFIRM         
* THAT THIS INPUT CORRESPONDS WITH QOPT1(ACTION) INPUT.                         
         XC    TARGET,TARGET                                                    
         CLI   QSRTAREA,0                                                       
         BNE   INIT12                                                           
         CLI   QSRTAREA+1,0                                                     
         BNE   INIT09                                                           
*                                                                               
INITERR  LA    R3,ERROR1L                                                       
INITERRA MVI   RQSTERR,YES                                                      
         ST    R3,AERROR                                                        
         B     INIT134                                                          
*                                                                               
INIT09   MVC   TARGET+1(1),QSRTAREA+1                                           
         CLI   QOPT1,C'M'          NO INPUT QSRTAREA REQUIRES 'T' OR            
         BNE   INIT10              'R' IN QOPT1, WILL KEEP ACTION = T           
         MVI   ACTION,C'T'         SINCE THEY PROCESS SIMILARLY, (CAN           
         B     INIT20              CHECK QOPT1 = R FOR ANY SPECIFIC             
INIT10   CLI   QOPT1,C'T'          PROCESSING)                                  
         BNE   INITERR                                                          
         MVI   ACTION,C'T'         ACTION=T(ACTUAL TRANSACTION UPDATE)          
         B     INIT20                                                           
*                                                                               
INIT12   CLI   QSRTAREA+1,0        NO INPUT REQUIRE 'D'(DELETE)-QOPT1           
         BNE   INIT15                                                           
         MVC   TARGET+1(1),QSRTAREA                                             
         CLI   QOPT1,C'D'          DELETE INDICATION                            
         BNE   INITERR                                                          
         MVI   ACTION,C'D'         ACTION=D(DELETE FROM EXISTENT DATA)          
         B     INIT38              SKIP EDIT OF ACT UPON BUDGET TYPE            
*                                                                               
INIT15   MVC   TARGET+1(1),QSRTAREA+1                                           
         CLI   QOPT1,C'C'          INPUT IN BOTH QSRT1-2 REQUIRES A             
         BE    INIT18              'C'(COPY), 'G'(GEN UPDATE) OR 'A'            
         CLI   QOPT1,C'G'          (AMEND) IN QOPT1.                            
         BE    INIT18                                                           
*                                                                               
         CLI   QOPT1,C'A'                                                       
         BNE   INITERR                                                          
         MVC   FRMUL,QCUL                                                       
         MVC   TOUL,QSELECT+2                                                   
         CLI   FRMUL,C' '          FROM CONTRA                                  
         BE    INITERRA                                                         
         CLI   TOUL,C' '           TO   CONTRA                                  
         BE    INITERRA                                                         
         CLI   QUNIT,C' '          ACCOUNT UNIT                                 
         BE    INITERRA                                                         
         CLI   QLEDGER,C' '        ACCOUNT LEDGER                               
         BE    INITERRA                                                         
*                                                                               
INIT18   MVC   ACTION,QOPT1        MARK SUB TO GEN UNIT UPDATE                  
*                                                                               
         LA    RE,QACCOUNT+11      STORE LENGTH OF QACCT RQST INPUT             
         LA    RF,12                                                            
         LR    R0,RF                                                            
INIT19   CLI   0(RE),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,INIT19                                                        
         STC   RF,QACTLN                                                        
         EJECT ,                                                                
***********************************************************************         
*              READ ACT UPON BUDGET(QSRTAREA+1) INTO IO1                        
*              IF AMEND QSRTAREA+1 = FACTOR BUDGET                              
***********************************************************************         
         USING BUDRECD,R2                                                       
INIT20   L     R2,AIO1                                                          
         MVC   BYTE,QSRTAREA+1                                                  
         BAS   RE,RDBDGTS                                                       
         MVC   NEWBDGNO,BUDKNO1    SAVE ACT UPON BDG NUMBER                     
         MVC   SVBDGTCD,BUDKCOD    SAVE TYPE CODE                               
*                                                                               
         USING BIVELD,R2                                                        
         CLI   ACTION,C'G'         IF UPDATE SUBSIDIARY TO GENERAL              
         BNE   INIT38              CONFIRM THAT 1C BUD DEFINITION EL            
         MVI   ELCODE,BIVELQ       HAS A GENERAL UNIT.                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
INIT25   BAS   RE,NEXTEL                                                        
         BNE   INIT35                                                           
         CLI   BIVAUNT,C'G'                                                     
         BE    INIT36                                                           
         B     INIT25              CHECK FOR ANOTHER X'1C'                      
*                                                                               
INIT35   LA    R3,ERROR3L                                                       
         B     INITERRA                                                         
*                                                                               
*              SAVE GENERAL U/L AND LEVELS FOR LATER PROCESSING                 
INIT36   MVC   GUNIT1,BIVAUNT      U/L                                          
         MVC   G1LVL,BIVACLV       LVL                                          
INIT37   BAS   RE,NEXTEL           CHECK FOR THE OTHER G UNIT                   
         BNE   INIT38                                                           
         CLI   BIVAUNT,C'G'                                                     
         BNE   INIT37                                                           
         MVC   GUNIT2,BIVAUNT                                                   
         MVC   G2LVL,BIVACLV                                                    
         EJECT ,                                                                
***********************************************************************         
*              READ ACT FROM BUDGET INTO IO2-SELECT U/L'S                       
*              THAT FIT REQUEST AND MOVE THEM TO THE SUBTBL.                    
***********************************************************************         
INIT38   LA    R7,2                DETERMINE RQST LEN IF ANY FOR U/L            
         CLI   QLEDGER,C' '                                                     
         BNE   INIT40                                                           
         BCTR  R7,0                                                             
         CLI   QUNIT,C' '                                                       
         BNE   INIT40                                                           
         BCTR  R7,0                                                             
*                                                                               
         USING B4ULTBLD,R6                                                      
         USING BUDRECD,R2                                                       
INIT40   L     R6,SUBTBL           CONTAINS BUDGET ACT UPON INFO                
*                                                                               
         L     R2,AIO1             IN T ACT THE FROM BDGT NONEXISTENT           
         CLI   ACTION,C'T'         SO THE U/L'S THAT ARE PERTINENT ARE          
         BE    INIT43              IN THE TARGET BUDGET.                        
*                                                                               
         L     R2,AIO2                                                          
         MVC   BYTE,QSRTAREA                                                    
         BAS   RE,RDBDGTS                                                       
         MVC   SVOLDCD,BUDKCOD     SAVE TYPE CODE                               
         MVC   OLDBDGNO,BUDKNO1    SAVE BUDGET NO TO READ FOR                   
***********************************************************************         
*                                                                               
*              SCAN BUDG VALIDATION ELEMENT FOR                                 
*              REQUESTED UNIT OR UNIT/LEDGER                                    
***********************************************************************         
         USING BIVELD,R2                                                        
INIT43   SR    R3,R3               REQUEST ERROR INDICATOR                      
         MVI   ELCODE,BIVELQ       X'1C'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
INIT45   BAS   RE,NEXTEL                                                        
         BNE   INIT47                                                           
*                                                                               
         CLI   ACTION,C'G'         IF ACTION G NEED SUBSIDIARY                  
         BNE   INIT46                                                           
         CLI   BIVAUNT,C'S'                                                     
         BNE   INIT45                                                           
*                                                                               
INIT46   LTR   R7,R7               WAS ACT UPON UNIT OR U/L SPECIFIED           
         BZ    INIT50              NO                                           
         LR    R5,R7                                                            
         BCTR  R5,0                                                             
         EX    R5,INIT48                                                        
         BE    INIT50                                                           
         B     INIT45                                                           
*                                                                               
INIT47   LTR   R3,R3                                                            
         BNZ   INIT60                                                           
         LA    R3,ERROR2L                                                       
         B     INITERRA                                                         
*                                                                               
INIT48   CLC   BIVAUNT(0),QUNIT    REQUIRE MATCH                                
*                                                                               
INIT50   LR    R3,RB               FOUND AT LEAST ONE U/L FITTING RQST          
         MVI   B4ACTID,C'A'        IDENTIFY AS ACCOUNT                          
         LA    RE,2                                                             
         CLI   ACTION,C'C'         IF COPY                                      
         BE    *+8                                                              
         LA    RE,1(,RE)           FOR TRANS AND DELETE ACT INCLUDE LVL         
         BCTR  RE,0                                                             
         EXMVC RE,B4ULENT,BIVAUNT  U/L AND POSSIBLY LEVEL                       
         ZIC   RF,BIVLN                                                         
         LR    R1,R2                                                            
         AR    RF,R1               =A(END OF CURRENT ELEMENT)                   
         LA    R0,2                IF AMEND LOOP FOR 2 CONTRAS                  
         LA    R8,FRMUL            A(CONTRAS FOR AMEND ACTION)                  
*                                                                               
         LA    R1,BIVVCUNT         A(1ST POSSIBLE CONTRA UNDER CUR ACT)         
INIT51   AHI   R6,B4ENTLEN         TO NEXT ENTRY LOC                            
*                                                                               
         CLI   ACTION,C'A'         IF AMEND                                     
         BNE   INIT53              FROM BUDGET TYPE MUST INCLUDE THE            
*                                                                               
INIT52   CR    R1,RF               AND TRGT CONTRAS AT FROM U/L                 
         BNL   INITERRB            BEYOND 1C EL CONTAINING ACT U/L              
         CLC   0(2,R8),0(R1)       LOOK FOR MATCH                               
         BE    INIT53              FALL THRU PLACE CONTRA ENT IN TBL            
         AHI   R1,L'BIVVALS        NEXT POSSIBLE CONTRA                         
         B     INIT52                                                           
*                                                                               
INIT53   CR    R1,RF               IF NEXT ELEMENT ADDRESS-NO CONTRA            
         BNL   INIT45              TRY FOR ANOTHER 1C ELEMENT                   
*                                                                               
         MVI   B4ACTID,C'C'        IDENTIFY AS CONTRA OF PREVIOUS U/L           
         CLI   0(R1),0             INDICATES 'ALL'                              
         BNE   *+14                                                             
         MVC   B4ULENT(3),=C'ALL'  MEANS ALL U/L ARE EXCEPTABLE CNTRS           
         B     INIT55                                                           
         EXMVC RE,B4ULENT,0(R1)    U/L AND POSSIBLY LEVEL                       
*                                                                               
         CLI   ACTION,C'A'         IF AMEND                                     
         BNE   INIT55                                                           
         LA    R1,BIVVCUNT         RESTORE A(1ST CONTRA)                        
         AHI   R8,2                =A(ACT UPON CONTRA)                          
         BCT   R0,INIT51                                                        
         B     INIT80                                                           
*                                                                               
INIT55   AHI   R1,3                IS THERE ANOTHER CONTRA THIS ACCT            
         B     INIT51                                                           
         EJECT ,                                                                
*                                                                               
INIT60   L     R6,SUBTBL                                                        
         CLI   ACTION,C'C'         IS IT COPY                                   
         BNE   INIT80              NO                                           
**********************************************************************          
* FOR COPY ACTION THE FROM BUDGET'S ACCOUNTS/LEVELS-CONTRAS/LEVELS              
* SET IN THE SUBTBL MUST AGREE WITH THE TARGET BUDGET'S VALIDATION              
* ELEMENT.                                                                      
**********************************************************************          
         USING BIVELD,R2                                                        
         L     R2,AIO2                                                          
         MVI   ELCODE,BIVELQ       GET ELEMENTS REPRESENTED IN TBL              
         BAS   RE,GETEL                                                         
         B     *+8                                                              
INIT65   BAS   RE,NEXTEL                                                        
         BNE   INITERRB                                                         
         CLC   B4ULENT,BIVAUNT     MATCH TBL U/L WITH ELEMENT U/L               
         BNE   INIT65              NO                                           
         ST    R2,FULL             SAVE A(CURRENT FROM ELEMENT)                 
*                                                                               
         L     R2,AIO1             A(TARGET BUDGET)                             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
INIT70   BAS   RE,NEXTEL                                                        
         BNE   INITERRB                                                         
         CLC   B4ULENT,BIVAUNT     FIND TARGET BDG U/L FOR CUR TBL ENT          
         BNE   INIT70              ADVANCE TO NEXT 1C                           
         L     R3,FULL                                                          
*                                                                               
         CLC   4(L'BIVACLV,R3),BIVACLV   FROM ACCT U/L LEV TO TARGET'S          
         BL    INITERRB                                                         
         MVC   B4ULLVL,BIVACLV     TARGET LEVEL TO TABLE                        
*                                                                               
         LA    R5,BIVAUNT          SAVE A(1ST TRGT CONTRA U/L-LVL)              
         ZIC   R1,BIVLN                                                         
         LR    RF,R2                                                            
         AR    R1,RF               =A(END OF TARGET ELEMENT)                    
         AHI   R2,5                1ST POSSIBLE TARGET CONTRA                   
         AHI   R3,5                1ST POSSIBLE FROM CONTRA                     
         DROP  R2                                                               
*                                                                               
INIT73   AHI   R6,B4ENTLEN         NEXT TBL ENTRY                               
         CLI   B4ACTID,0           ANOTHER ENTRY                                
         BE    INIT80                                                           
         CLI   B4ACTID,C'C'        IS THERE A CONTRA ENTRY FOR CUR ACCT         
         BE    *+12                YES                                          
         L     R2,FULL             RESTORE A(LAST 1C EL)                        
         B     INIT65              GO GET NEXT 1C ELEMENT-FROM BDGT             
*                                                                               
INIT75   CLC   B4ULENT(3),=C'ALL'  IF ALL                                       
         BE    INIT79                                                           
         CLC   0(2,R3),0(R2)       FROM CONTRA U/L TO TARGET'S                  
         BE    INIT77                                                           
         AHI   R2,3                TO NEXT CONTRA 3 BYTE ENTRY-TRGT             
         CR    R1,R2                                                            
         BH    INIT75                                                           
         B     INITERRB            TARGET LACKS CORRECT CONTRA                  
*                                                                               
INIT77   CLI   B4ULENT,C'+'        IF SPECIAL                                   
         BNE   INIT78                                                           
         CLC   1(1,R3),1(R2)       MATCH ON DISPLACEMENT                        
         BNE   INITERRB                                                         
         MVC   B4ACTLEN,B4ULENT+1  DISPLACEMENT TO LEN                          
         B     INIT79                                                           
*                                                                               
INIT78   CLC   2(1,R3),2(R2)       COMPARE LEVELS                               
         BL    INITERRB            TARGET LEVEL TOO HI                          
         MVC   B4ULLVL,2(R2)       TARGET LEVEL TO TBL                          
*                                                                               
INIT79   AHI   R3,3                NEXT POSSIBLE FROM CONTRA                    
         LR    R2,R5               RESTORE A(1ST CONTRA TRGT BDT)               
         B     INIT73                                                           
*                                                                               
INITERRB LA    R3,ERROR4L                                                       
         B     INITERRA                                                         
         EJECT ,                                                                
*              COMPLETE UNIT/LEDGER TABLE ENTRIES                               
INIT80   L     R6,SUBTBL                                                        
         SR    R5,R5               POST ACCOUNT COUNT OR ZERO                   
*                                                                               
         USING ACTRECD,R2                                                       
INIT82   L     R2,AIO1             READ FOR CURRENT U/L ENTRY IN TBL            
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY    CO                                           
         MVC   ACTKUNT(2),B4ULENT  U/L                                          
         MVC   KEY1SAVE(42),0(R2)                                               
*                                                                               
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
         CLC   0(L'ACTKEY,R2),KEY1SAVE                                          
         BE    *+6                                                              
         DC    H'0'                MISSING LEDGER RECORD                        
*                                                                               
         CLI   B4ACTID,C'C'        IF CONTRA ENTRY-INIT LVL LENS                
         BNE   INIT85                                                           
         MVI   ELCODE,ACLELQ       NEED TO INITIAL BDGT CONTRA LEVELS           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING ACLELD,R2                                                        
         ZIC   R7,B4ULLVL          U/L BUDGET LEVEL DEFINITION                  
         LTR   R7,R7                                                            
         BZ    INIT85              BUDGET CONTRA IS ONLY U/L, NO LVLS           
         LA    RF,ACLVALS          A(ACCT LEVEL LEN)                            
         LA    R8,LVLATOT          A(COINCIDING LEVEL TOTAL)                    
INIT83   BCTR  R7,0                                                             
         LTR   R7,R7                                                            
         BZ    INIT84                                                           
         AHI   RF,L'ACLVALS        A(NEXT ACCT LVL)                             
         LA    R1,TOTALSLN         LEVEL TOTAL LENGTH                           
         SR    R8,R1               A(NEXT HIGHER LEVEL TOTAL)                   
         B     INIT83                                                           
                                                                                
INIT84   ST    R8,B4ULALOW         A(LOW LEVEL TOTAL)                           
         MVC   B4ACTLEN,0(RF)      LOW LEVEL ACCT LEN                           
         MVC   B4ULLVLA,ACLELLVA   FILL TBL WITH LEVEL LENS                     
         MVC   B4ULLVLB,ACLELLVB                                                
         MVC   B4ULLVLC,ACLELLVC                                                
         MVC   B4ULLVLD,ACLELLVD                                                
         DROP  R2                                                               
*                                                                               
         USING LDGELD,R2                                                        
         L     R2,AIO1                                                          
INIT85   CLI   ACTION,C'G'         S TO G UNIT UPDATE                           
         BNE   INIT96              NO                                           
         MVC   B4ULOFF,=C' '                                                    
         MVI   ELCODE,LDGELQ       GET POINTER TO OFFICE FOR GENERAL            
         BAS   RE,GETEL            ACCOUNTS ENDING WITH '*'                     
         BNE   INIT86                                                           
         MVC   B4ULOFF,LDGOPOS                                                  
         DROP  R2                                                               
                                                                                
         USING GLPELD,R2                                                        
         USING PSTBD,R8                                                         
         L     R2,AIO1                                                          
INIT86   L     R8,APSTB                                                         
INIT87   MVI   ELCODE,GLPELQ       GET GNRL LDG POSTING INSTR. FOR U/L          
         BAS   RE,GETEL            SET TBL OF POST TO ACCTS                     
         B     *+8                                                              
INIT88   BAS   RE,NEXTEL                                                        
         BNE   INIT96                                                           
         MVC   PSTCD(2),=X'011D'   ENT CODE/LEN                                 
         MVC   POSTID,B4ULENT                                                   
         MVC   PSTTO,SPACES                                                     
         MVC   PSTTO(10),GLPACC1   G/L ACCOUNT                                  
         CLI   GLPLN,26            CHECK FOR EL LENGTH                          
         BL    *+10                INDICATES NEW LEN                            
         MVC   PSTTO(14),GLPACC1                                                
         MVC   PSTFR,GLPSUB                                                     
         AHI   R5,1                COUNT POST ACCTS                             
         AHI   R8,POSTLN           NEXT ENTRY POSITION                          
         B     INIT88                                                           
*                                                                               
INIT96   AHI   R6,B4ENTLEN         NEXT ENTRY POSITION                          
         CLI   B4ACTID,X'FF'       END OF TABLE                                 
         BE    INIT98              NO                                           
         CLI   B4ACTID,0           END OF ENTRIES                               
         BE    INIT98              YES                                          
         CLC   B4ULENT,=C'ALL'                                                  
         BE    INIT96                                                           
         CLI   B4ULENT,C'+'        IF SPECIAL OR + FURTER INIT UNNEEDED         
         BE    INIT96                                                           
         B     INIT82              LOOP TO COMPLETE NEXT U/L ENTRY              
*                                                                               
INIT98   LTR   R5,R5               RECORDS TO SORT                              
         BZ    INIT120             NO                                           
*                                                                               
*              SORT POST ENTS ON DESCENDING SEQUENCE ON FROM ACCT-U/L           
         GOTO1 XSORT,DMCB,(1,APSTB),(R5),29,12,17                               
         STC   R5,SVXSCNT                                                       
*                                                                               
         L     R8,APSTB                                                         
INIT99   CLC   PSTBD(2),=X'011D'                                                
         BNE   INIT114             END OF RECS                                  
         CLC   PSTFR,SPACES                                                     
         BE    INIT112             PSTFRLN=0 FOR DEFAULTS                       
         LA    R1,9                DETERMINE FROM LEN                           
         LA    R2,PSTFR+9                                                       
INIT105  CLI   0(R2),X'40'                                                      
         BNE   INIT111                                                          
         BCTR  R1,0                                                             
         BCTR  R2,0                                                             
         B     INIT105                                                          
INIT111  STC   R1,PSTFRLN                                                       
INIT112  LA    R8,POSTLN(,R8)      NEXT ENTRY                                   
         BCT   R5,INIT99                                                        
         DROP  R8                                                               
*                                                                               
*              READ FOR G UNIT/LEDGER REC(S)                                    
INIT114  LA    R8,GUNIT1                                                        
         B     *+8                                                              
INIT115  LA    R8,GUNIT2                                                        
*                                                                               
         USING ACTRECD,R2                                                       
         L     R2,AIO1             BUILD U/L KEY                                
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,QCOMPANY    CO                                           
         MVC   ACTKUNT(2),0(R8)     U/L                                         
         MVC   KEY1SAVE(42),0(R2)                                               
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
         CLC   0(L'ACTKEY,R2),KEY1SAVE                                          
         BE    *+6                                                              
         DC    H'0'                NO U/L REC                                   
*                                                                               
         MVI   ELCODE,ACLELQ       HIERARCHY, X'16'                             
         BAS   RE,GETEL            GET ACCT LEVEL FOR UL                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING ACLELD,R2                                                        
         ZIC   R7,2(,R8)           G UNIT LVL                                   
         LA    RF,ACLVLEN                                                       
INIT115A BCTR  R7,0                                                             
         LTR   R7,R7               AT LOWEST                                    
         BZ    INIT115B            YES                                          
         AHI   RF,L'ACLVALS        NEXT LEVEL LENGTH                            
         B     INIT115A                                                         
INIT115B MVC   3(1,R8),0(RF)       LVL LENGTH FOR GENERAL U/L                   
*                                                                               
         LA    R3,GBNAME           NAME                                         
         CLC   0(2,R8),=C'GB'                                                   
         BE    *+8                                                              
         LA    R3,GPNAME                                                        
         BAS   R7,NAMEOUT                                                       
*                                                                               
         AHI   R8,4                TO GUNIT2                                    
         CLI   0(R8),0             IS THERE A 2ND                               
         BZ    INIT120             NO                                           
         CLI   3(R8),0             DONE                                         
         BZ    INIT115             NO                                           
         EJECT ,                                                                
*                                                                               
INIT120  MVC   QSTART+4(2),=C'28'  MAKE DATCON WORK                             
         MVC   QEND+4(2),=C'28'                                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(1,MYSTART)                               
         GOTO1 DATCON,DMCB,(0,QEND),(1,MYEND)                                   
         GOTO1 DATCON,DMCB,(1,MYSTART),(6,START)                                
         GOTO1 DATCON,DMCB,(1,MYEND),(6,END)                                    
*                                                                               
*              SET UP REQUEST DATE RANGE TABLE-BANNER DATE LINE(S)              
         LA    R7,1                COUNT OF YR/MN RANGE                         
         LA    R2,DATAB                                                         
         MVC   0(2,R2),MYSTART                                                  
         CLC   MYSTART(2),MYEND    ONE MONTH REQUEST                            
         BE    INIT124                                                          
*                                                                               
         MVC   WORK(6),QSTART      START-END TO WORK                            
         LA    R6,1                ADD UP TO NEXT YR/MN                         
*                                                                               
INIT122  GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,(R6)                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,FULL)                                  
         AHI   R2,2                NEXT TABLE LOC                               
         MVC   0(2,R2),FULL        YMD PACKED                                   
         AHI   R7,1                INCREMENT MONTH COUNT                        
         MVC   WORK(6),WORK+6                                                   
         CLC   FULL(2),MYEND                                                    
         BE    INIT124                                                          
         CHI   R7,12               MORE THAN 12 MONTHS ?                        
         BL    INIT122             NO,  CONTINUE                                
         LA    R3,ERROR5L          TOO  MANY MONTHS                             
         B     INITERRA            ERROR                                        
*                                                                               
INIT124  STC   R7,MNTHCNT                                                       
         LA    R3,DATEBANR                                                      
         LA    R2,DATAB                                                         
*                                                                               
INIT125  MVC   WORK(2),0(R2)       YM PACKED NO SIGN                            
         MVI   WORK+2,1            MAKE DATCON WORK-YMD                         
         CLI   QOPT3,C' '          ARE SORT DATE YEARS BEING REVISED            
         BE    INIT126             NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(1,WORK),(3,DUB) TO BINARY YMD                       
         MVC   REVVAL,QOPT3        QOPT3=EBCIDIC 1-9                            
         NI    REVVAL,X'0F'        NOW 01-09                                    
         ZIC   RE,REVVAL                                                        
         ZIC   RF,DUB                                                           
         AR    RF,RE               ADD 01-09 TO BINARY YEAR                     
         STC   RF,DUB                                                           
         GOTO1 DATCON,DMCB,(3,DUB),(1,WORK) NEW YMD PACKED-NO SIGN              
*                                                                               
INIT126  GOTO1 DATCON,DMCB,(1,WORK),(6,(R3))                                    
         AHI   R2,2                                                             
         AHI   R3,14                                                            
         BCT   R7,INIT125                                                       
*                                                                               
         CLI   MNTHCNT,1                                                        
         BE    *+10                                                             
         MVC   0(5,R3),=C'TOTAL'                                                
*                                                                               
         ZIC   R7,MNTHCNT          SET UP COUNT FOR HOOK                        
         CHI   R7,3                                                             
         BNH   INIT128             MONTHLY FORMAT                               
         SR    R6,R6                                                            
         LA    R1,3                                                             
         DR    R6,R1               HOW MANY QUARTERS                            
         LTR   R6,R6               REMAINDER                                    
         BZ    INIT128             NO                                           
         AHI   R7,1                ADD ONE FOR ODD MONTHS                       
INIT128  STC   R7,HOOKCNT                                                       
*                                                                               
         ZAP   BDGTOT,=P'0'                                                     
         ZAP   TOTTOT,=P'0'                                                     
         MVI   POSTCNT,0                                                        
*                                                                               
         CLI   MNTHCNT,3                                                        
         BNH   *+8                                                              
         MVI   QPRNT,YES           INDICATE QUARTERLY PRINT                     
*                                                                               
         CLI   QOPT3,C' '          DOES REQUEST INCLUDE YR REVISION             
         BE    INIT134             NO                                           
         MVC   XTRALIT(30),=C'DATES OF BUDGETS INCREASED BY '                   
         MVC   XTRALIT+30(1),QOPT3                                              
         LA    RE,4                                                             
         CLI   QOPT3,C'1'                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         EXMVC RE,XTRALIT+32,=C'YEARS'                                          
*                                                                               
INIT134  CLI   ACTION,C'G'         UPDATE SUBSIDIARY TO GENERAL                 
         BNE   INIT136                                                          
         MVC   HDLINLIT(16),=C'G/L UPDATE FROM '                                
         MVC   HDLINLIT+17(10),SVOLDCD                                          
         MVC   HDLINLIT+27(4),=C' TO '                                          
         MVC   HDLINLIT+31(10),SVBDGTCD                                         
         B     INIT142                                                          
*                                                                               
INIT136  CLI   ACTION,C'C'                                                      
         BNE   INIT138                                                          
         MVI   RCSUBPRG,3          COPY                                         
         MVC   HDLINLIT(10),=C'COPY FROM '                                      
         MVC   HDLINLIT+10(10),SVOLDCD                                          
         MVC   HDLINLIT+20(4),=C' TO '                                          
         MVC   HDLINLIT+24(10),SVBDGTCD                                         
         B     INIT142                                                          
*                                                                               
INIT138  CLI   ACTION,C'D'                                                      
         BNE   INIT140                                                          
         MVI   RCSUBPRG,4          DELETE                                       
         MVC   HDLINLIT(12),=C'DELETE FROM '                                    
         MVC   HDLINLIT+13(10),SVOLDCD                                          
         B     INIT142                                                          
*                                                                               
INIT140  CLI   ACTION,C'T'                                                      
         BNE   INIT141                                                          
         MVI   RCSUBPRG,2          ACTUAL                                       
         MVC   HDLINLIT(18),=C'UPDATE ACTUALS TO '                              
         MVC   HDLINLIT+18(10),SVBDGTCD                                         
         B     INIT142                                                          
*                                                                               
INIT141  CLI   ACTION,C'A'                                                      
         BNE   INIT145                                                          
         MVI   RCSUBPRG,3          AMEND                                        
         MVC   HDLINLIT(2),TOUL    TARGET U/L                                   
         L     R6,SUBTBL                                                        
         AHI   R6,B4ENTLEN                                                      
         MVC   HDLINLIT+3(36),B4LDGNAM                                          
         MVC   HDLINLIT+40(22),=C'BUDGET DEVELOPED FROM '                       
         MVC   HDLINLIT+62(2),FRMUL                                             
         LA    R6,B4ENTLEN(,R6)                                                 
         MVC   HDLINLIT+65(36),B4LDGNAM                                         
         MVC   HDLINLIT+102(11),=C'BUDGET DATA'                                 
*                                                                               
INIT142  GOTO1 SQUASHER,DMCB,HDLINLIT,(0,120)                                   
         L     R1,DMCB+4                                                        
         STC   R1,HDLNLEN                                                       
         CLI   RQSTERR,YES                                                      
         BE    INIT145                                                          
         SR    R0,R0                                                            
         B     INITEXIT                                                         
*                                                                               
INIT145  XC    SVLDG,SVLDG                                                      
         MVI   NOBOX,YES                                                        
         L     RE,AERROR                                                        
         ZIC   RF,0(,RE)           ERROR LENGTH                                 
         BCTR  RF,0                                                             
         EXMVC RF,P,1(RE)                                                       
         GOTO1 APRNTIT,DMCB,(RC)                                                
         LTR   RB,RB                                                            
*                                                                               
INITEXIT XMOD1 1                                                                
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*        READ BUDGET TYPE RECORD -                                              
*        GET BUDGET HEX CODE AND BUDGET CODE TRANSLATION -                      
*        GET ELEMENTS FOR LEDGER SET UP                                         
*-------------------------------------------------------------------*           
*                                                                               
RDBDGTS  NTR1                                                                   
         LR    RE,R2                                                            
         USING BUDRECD,RE                                                       
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ       X'1B', BDGT KEY ID                        
         MVC   BUDKCPY,QCOMPANY                                                 
         MVC   BUDKNO1+1(1),BYTE      BDGT BINARY NO.                           
         MVC   KEYSAVE(5),0(R2)                                                 
*                                                                               
         MVC   COMMAND,=C'DMRDHI'                                               
         GOTO1 AMYIOCL,DMCB,(RC),(R2)                                           
         CLC   KEYSAVE(5),0(R2)                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         B     INITEXIT                                                         
         DROP  RE                                                               
         SPACE 2                                                                
NAMEOUT  L     R2,AIO1             COMMON RTN FOR RETRIEVAL OF NAMES            
         MVI   ELCODE,NAMELQ       R3 IS PRESET WITH RECEIVING ADDRESS          
         BAS   RE,GETEL                                                         
         BNER  R7                                                               
                                                                                
         USING NAMELD,R2                                                        
         ZIC   RE,NAMLN            CALCULATE LEN                                
         SHI   RE,NAMLN1Q+1                                                     
         EXMVC RE,0(R3),NAMEREC                                                 
         BR    R7                                                               
         DROP  R2                                                               
         SPACE 2                                                                
*************************************                                           
* GETEL                                                                         
*************************************                                           
         GETEL R2,DATADISP,ELCODE                                               
         EJECT ,                                                                
*                                                                               
ERROR1L  DC    AL1(L'ERROR1)                                                    
ERROR1   DC    C'INVALID REQUEST, ACTION CODE DISAGREES WITH BDGT CODE X        
               INPUT.'                                                          
ERROR2L  DC    AL1(L'ERROR2)                                                    
ERROR2   DC    C'INCOMPATIBLE BUDGET STRUCTURE.'                                
ERROR3L  DC    AL1(L'ERROR3)                                                    
ERROR3   DC    C'ACT UPON BUDGET TYPE DOES NOT CONTAIN A GENERAL UNIT.'         
ERROR4L  DC    AL1(L'ERROR4)                                                    
ERROR4   DC    C'BUDGET TYPES ARE INCOMPATABLE FOR THIS ACTION.'                
ERROR5L  DC    AL1(L'ERROR5)                                                    
ERROR5   DC    C'REPORT REQUESTED FOR MORE THAN 12 MONTHS.'                     
         LTORG                                                                  
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*        DATAMANAGER CALLS                                                      
*-------------------------------------------------------------------*           
         USING ACTRECD,R2                                                       
MYIOCLC  DS   0D                                                                
         NMOD1 0,*MYIO*                                                         
         L     RC,0(,R1)                                                        
         L     R2,4(,R1)                                                        
         CLC   COMMAND(3),=C'DMR'                                               
         BE    MY20                                                             
         CLC   ACTRLEN,=X'0035'                                                 
         BH    MY20                                                             
         CLC   COMMAND(5),=C'DMADD'                                             
         BE    MY30                                                             
         OI    ACTRSTA,X'80'                                                    
MY20     GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',(R2),(R2)             
MY30     XMOD1 1                                                                
         DROP  R2                                                               
         LTORG                                                                  
         EJECT ,                                                                
*-------------------------------------------------------------------*           
*              BOX ROUTINES                                                     
*-------------------------------------------------------------------*           
HOOK     DS    0D                                                               
         NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         L     R2,ADBOX                                                         
         USING BOXD,R2                                                          
*                                                                               
         CLI   NOBOX,C'Y'          LOGO MODE                                    
         BNE   HOOK01                                                           
         MVI   BOXYORN,C'N'                                                     
         B     HOOKEXT                                                          
*                                                                               
HOOK01   MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+8,C'T'        TOP ROW                                      
         MVI   MYROW+10,C'M'                                                    
         MVI   MYROW+56,C'B'       BOTTOM ROW                                   
*                                                                               
         LA    R3,MYCOL                                                         
         USING MYCLMSD,R3                                                       
         MVI   MYCLM,C'L'                                                       
         MVI   MYCLM1,C'C'         ACCT                                         
         MVI   MYCLM2,C'C'         ACCT NAME                                    
         LA    R4,MYCLM3           FIRST FOR DATES                              
         ZIC   R7,HOOKCNT                                                       
         CLI   MNTHCNT,1                                                        
         BE    HOOK05                                                           
         LA    R7,1(,R7)           FOR TOTAL COLUMN                             
         B     HOOK10                                                           
HOOK03   CH    R7,=H'1'                                                         
         BNE   HOOK10                                                           
HOOK05   MVI   0(R4),C'R'                                                       
         B     HOOKX                                                            
HOOK10   MVI   0(R4),C'C'                                                       
         LA    R4,MYDTLN(,R4)                                                   
         BCT   R7,HOOK03                                                        
HOOKX    MVI   BOXYORN,C'Y'                                                     
         MVC   BOXROWS,MYROW                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
HOOKEXT  XMOD1 1                                                                
         DROP  R3                                                               
         DROP  R2                                                               
*                                                                               
SAVERC   DC    A(0)                                                             
ADBOX    DS    A                                                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
SUBTBLC  DS    0H           U/L TABLE DEFINED BY BUDGET TYPE RECS               
         DS    40CL(B4ENTLEN)                                                   
SUBLEN   EQU   *-SUBTBLC                                                        
         DC    X'FF'                                                            
*                                                                               
APOSTBC  DS    0H           GENERAL LEDGER POSTING TO ACCOUNTS                  
         DS    21CL29                                                           
POSLEN   EQU   *-APOSTBC                                                        
         EJECT ,                                                                
*                                                                               
ACB4D    DSECT PROGRAM WORK                                                     
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOR      EQU   0                                                                
TURNOFF  EQU   X'FF'                                                            
                                                                                
ATYPES   DS    0A                                                               
APRNTIT  DS    A                                                                
APRNTTOT DS    A                                                                
AINITIT  DS    A                                                                
AMYIOCL  DS    A                                                                
SUBTBL   DS    A                                                                
APSTB    DS    A                                                                
AENDIT   DS    A                                                                
AACCTPL  DS    A                                                                
ATOTMNG  DS    A                                                                
* LOGO COUNTS                                                                   
RECSADED DS    PL4                 COUNT OF RECS ADDED                          
RECSDLT  DS    PL4                 COUNT OF RECS DELETED                        
RECSROTE DS    PL4                 COUNT OF RECS WRITTEN                        
RECCNT   DS    PL4                 COUNT OF RECS PROCESSED NOT POSTED           
*                                                                               
MYWORK   DS    CL8                                                              
SVTOT    DS    CL(TOTDALN)         SAVE OF CUR CONTR FOR PRINT CNTL             
*                                                                               
MYROW    DS    CL100                                                            
MYCOL    DS    CL132                                                            
HOOKCNT  DS    X                   NUMBER OF MONTHS FOR HOOK                    
QACTLN   DS    C                                                                
*                                                                               
SORTREC  DS    0H                                                               
SORTKEY  DS    0CL49               FOR COPY-DELETE-TRANS                        
SORTLEDG DS    CL2                                                              
SORTTO   DS    CL14                GENERAL LEDGER ACCT. TO POST TO              
SORTFROM DS    CL49                SUBSIDIARY ACCT. TO POST FROM                
SORTPNAM DS    CL36                G UNIT ACT NAME                              
SORTRULE DS    C                   POST FROM INDICATION                         
STODTBKT EQU   *-SORTREC           DISP TO 1ST DATE-BUCKET                      
SORTDT1  DS    PL2                 YY/MM 1                                      
SORTBKT1 DS    PL8                 BUCKET 1                                     
SORTNTYQ EQU   *-SORTDT1           ENTRY LENGTH                                 
SORTDT2  DS    PL2                 YY/MM 2                                      
SORTBKT2 DS    PL8                 BUCKET 2                                     
SORTDT3  DS    PL2                 YY/MM 3                                      
SORTBKT3 DS    PL8                 BUCKET 3                                     
SORTDT4  DS    PL2                 YY/MM 4                                      
SORTBKT4 DS    PL8                 BUCKET 4                                     
SORTDT5  DS    PL2                 YY/MM 5                                      
SORTBKT5 DS    PL8                 BUCKET 5                                     
SORTDT6  DS    PL2                 YY/MM 6                                      
SORTBKT6 DS    PL8                 BUCKET 6                                     
SORTDT7  DS    PL2                 YY/MM 7                                      
SORTBKT7 DS    PL8                 BUCKET 7                                     
SORTDT8  DS    PL2                 YY/MM 8                                      
SORTBKT8 DS    PL8                 BUCKET 8                                     
SORTDT9  DS    PL2                 YY/MM 9                                      
SORTBKT9 DS    PL8                 BUCKET 9                                     
SORTDTA  DS    PL2                 YY/MM A                                      
SORTBKTA DS    PL8                 BUCKET A                                     
SORTDTB  DS    PL2                 YY/MM B                                      
SORTBKTB DS    PL8                 BUCKET B                                     
SORTDTC  DS    PL2                 YY/MM C                                      
SORTBKTC DS    PL8                 BUCKET C                                     
SORTDBLN EQU   *-SORTDT1           LENGTH OF DATES AND BUCKETS                  
SORTCLR  EQU   *-SORTREC                                                        
SORTANAM DS    CL36                ACCOUNT LEVEL NAMES                          
SORTBNAM DS    CL36                ACCOUNT LEVEL NAMES                          
SORTCNAM DS    CL36                ACCOUNT LEVEL NAMES                          
SORTDNAM DS    CL36                ACCOUNT LEVEL NAMES                          
SORTEND  DS    C                   END TAG                                      
SORTRCLN EQU   *-SORTREC           SORTREC LEN                                  
*                                                                               
MNTHCNT  DS    XL1                 COUNT OF YR/MNTHS TO COPY                    
MYSTART  DS    PL3                 START DATE                                   
MYEND    DS    PL3                 END DATE                                     
DATAB    DS    CL24                TABLE OF UP TO 12 YEAR/MONTHS PACKED         
FILTERS  DS    CL4                 OFFICE CODE HOLDS FOR GACCT*                 
*                                                                               
SORTCNT  DS    F                   SORT RECORD COUNTER                          
SAVEFROM DS    CL14                SAVED FROM ACCT.                             
SAVETO   DS    CL14                SAVED TO ACCT.                               
SVPOSTNM DS    CL36                POST ACT NAMR                                
BDGTOT   DS    PL8                 BUDGET TOTAL FOR UPDATE                      
TOTTOT   DS    PL8                 BUDGET TOTAL FOR COPY-DELETE                 
PASTOT   DS    PL8                 TOTALS FOR A PASS OF MONTHS                  
QPRNT    DS    C                   QUARTERLY PRINT INDICATOR                    
PSW      DS    C                   CONTRA BREAK SWITCH                          
TRNSOK   DS    C                                                                
LDGPROC  DS    C                                                                
*                                                                               
KEY1SAVE DS    CL42                                                             
KEY3SAVE DS    CL49                                                             
KEY4SAVE DS    CL42                                                             
KEYDEL1  DS    CL42                                                             
KEYDEL2  DS    CL42                                                             
KEYTEMP  DS    CL42                                                             
KEYTEMP2 DS    CL42                                                             
LASTADD1 DS    CL35                                                             
LASTADD2 DS    CL35                                                             
KEYSW    DS    C                                                                
HISW     DS    C                                                                
OLDBDGNO DS    CL2                 BINARY BDG NO TO ACT FROM                    
NEWBDGNO DS    CL2                 BINARY BDG NO TO ACT UPON                    
ACTION   DS    C                   CURRENT UTILITY FUNCTION                     
DMACT    DS    C                   DATAMGR ACT SWITCH                           
ELCODE   DS    C                                                                
ELIST    DS    3F                  FOR GET AND ADEL                             
ELERR    DS    CL1                                                              
         ORG   ELERR                                                            
ELADDR   DS    F                   A(ELEMENT FROM HELLO)                        
         DS    2F                                                               
ELBUILD  DS    CL12                BUILD X'1D' ELEMENT AREA                     
SVBDGTCD DS    CL10                BUDGET TYPE CODE TO                          
SVOLDCD  DS    CL10                BUDGET TYPE CODE FROM                        
SVLDG    DS    CL1                 CURRENT LEDGER                               
SVLDGNM  DS    CL36                CURRENT LEDGER NAME                          
SVUNT    DS    CL1                 CURRENT UNIT                                 
SVUNTNM  DS    CL36                CURRENT UNIT NAME                            
ADDKEY   DS    CL1                 BUILD KEY FOR NEW REC SWITCH                 
HDLINLIT DS    CL120               BANNER LIT FOR COPY                          
HDLNLEN  DS    C                   LEN OF COPY BANNER LIT                       
SAVENAME DS    CL36                HOLDS ACCOUNT REC NAMES                      
SAVEANM  DS    CL36                                                             
SAVEBNM  DS    CL36                                                             
SAVECNM  DS    CL36                                                             
SAVEDNM  DS    CL36                                                             
SAVEDNME DS    CL36                                                             
OTHERNAM DS    CL36                                                             
FIRST    DS    C                   FIRST PASS                                   
DATEBANR DS    CL174               12 MONTHS 14 A PIECE + 'TOTAL'               
AULENTRY DS    A                   SAVE A(PREVIOUS U/L TBLE ENT)                
ACNTENT  DS    A                   A(CURRENT CONTRA U/L ENTRY)                  
ASORT    DS    A                   A(CURRENT SORT REC READ FROM SORTER)         
SVRULE   DS    C                   POST RULE S-G UPDATE                         
GUNIT1   DS    CL2                 FOR ACTION G-G U/L                           
G1LVL    DS    C                   G UNIT LVL                                   
G1LN     DS    C                   G UNIT ACT LN                                
GUNIT2   DS    CL2                 FOR ACTION G-G U/L                           
G2LVL    DS    C                   G UNIT LVL                                   
G2LN     DS    C                   G UNIT ACT LN                                
NOBOX    DS    C                   TURN    OFF      BOXES FOR  LOGO             
GBNAME   DS    CL36                GB      LDG      NAME                        
GPNAME   DS    CL36                GB      LDG      NAME                        
RQSTERR  DS    C                   REQUEST ERROR    SWITCH                      
ACCTBRK  DS    C                   ACCOUNT BREAK    SWITCH                      
PSWITCH  DS    C                   PRINT   LINE     INDICATOR                   
REVVAL   DS    C                   YEAR    REVISION VALUE                       
START    DS    CL6                 EBCDIC  START    DATE MMM/YY                 
END      DS    CL6                 EBCDIC  END      DATE MMM/YY                 
FRMUL    DS    CL2                 FROM    U/L                                  
TOUL     DS    CL2                 TO      U/L                                  
*                                                                               
CNTRTOT  DS    CL(TOTALSLN)                                                     
LVLDTOT  DS    CL(TOTALSLN)                                                     
LVLCTOT  DS    CL(TOTALSLN)                                                     
LVLBTOT  DS    CL(TOTALSLN)                                                     
LVLATOT  DS    CL(TOTALSLN)                                                     
ULTOT    DS    CL(TOTALSLN)                                                     
GLTOT    DS    CL(TOTALSLN)                                                     
PACTTOT  DS    CL(TOTALSLN)                                                     
POSTBKT  DS    CL(TOTALSLN)                                                     
*                                                                               
POSTCNT  DS    C                                                                
*                                                                               
SVXSCNT  DS    C                   SAVE COUNT OF XSORT ENTRIES                  
ACCTBLK  DS    0CL49               CHOPPER PRINT BLOCK                          
ACCTID   DS    CL12                                                             
         DS    C                                                                
ACCTNAME DS    CL36                                                             
AERROR   DS    F                                                                
SAVEDACT DS    CL14                SAVED PRINTED ACCT-NO REPEATS                
QTOT1    DS    PL8                 QUARTER TOTALS                               
QTOT2    DS    PL8                                                              
QTOT3    DS    PL8                                                              
QTOT4    DS    PL8                                                              
*                                                                               
AIO1     DS    A                                                                
AIO2     DS    A                                                                
*                                                                               
XTRALIT  DS    CL60                BANNER LIT FOR DATE REVISION                 
COMMAND  DS    CL8                                                              
RECWRKLN EQU   *-MYWORK                                                         
LIVESW   DS    CL1                 LIVE FOR RUN SWITCH FOR CONTROL RPT          
***                                                                             
SAVRE    DS    F                                                                
SAV1DEL  DS    F                                                                
ADADDR   DS    F                                                                
TEMP     DS    CL3                                                              
SAVOPT1  DS    CL1                                                              
SAVOPT4  DS    CL1                                                              
REQPCT   DS    PL4                                                              
SAVPROF  DS    CL16                                                             
SVYEAR   DS    CL1                                                              
UPNILS   DS    CL1                                                              
ACACTIV  DS    CL1                                                              
ELCNT    DS    PL2                                                              
RDELTOT  DS    PL10                                                             
RADDTOT  DS    PL10                                                             
RDIFTOT  DS    PL10                                                             
RBUDWRK  DS    PL12                                                             
WRKPCT   DS    PL10                                                             
SAVQACT  DS    CL12                                                             
TARGET   DS    CL2                                                              
SAVCOMP  DS    CL1                                                              
SVDMACT  DS    CL1                                                              
ADDOK    DS    CL1                                                              
COPDEL   DS    CL1                                                              
FILTMOS  DS    CL2                 PACK MOS                                     
FILTAMT  DS    PL6                                                              
FILTADR  DS    F                                                                
FILTSB   DS    C                                                                
FILTON   EQU   X'80'                                                            
LOOPAGN  EQU   X'40'                                                            
***                                                                             
IOAREAS  DS    3024C               IO AREA 1 AND 2, FACTOR BDGT IO              
*                                                                               
         EJECT ,                                                                
MYCLMSD  DSECT                     COVERS HOOK COLUMNS                          
MYCLM    DS    CL16                                                             
MYCLM1   DS    CL21                                                             
MYCLM2   DS    CL14                                                             
MYCLM3   DS    CL14                                                             
MYCLM4   DS    CL14                                                             
MYCLM5   DS    CL14                                                             
MYCLM6   DS    CL14                                                             
MYDTLN   EQU   *-MYCLM6                                                         
*                                                                               
TONXTENT EQU   24                                                               
TONXT    EQU   8                                                                
*                                                                               
B4ULTBLD DSECT                     COVERS SUBTBL                                
B4ACTID  DS    C                   A OR C (ACCOUNT OR CONTRA)                   
B4ULENT  DS    CL2                 SUBSIDIARY UNIT LEDGER                       
B4ULLVL  DS    C                   ACCOUNT LEVEL                                
B4ACTLEN DS    C                   LOW LEVEL ACCOUNT LENGTH                     
B4ULLVLA DS    C                   LEVEL A LEN                                  
B4ULLVLB DS    C                   LEVEL B LEN                                  
B4ULLVLC DS    C                   LEVEL C LEN                                  
B4ULLVLD DS    C                   LEVEL D LEN                                  
B4DFLTAC DS    CL14                GENERAL LDG. DEFAULT POSTING ACCT            
B4ULACNM DS    CL36                POST TO ACCT NAME                            
B4LDGNAM DS    CL36                LEDGER NAME                                  
B4UNNAM  DS    CL36                UNIT NAME                                    
B4ULALOW DS    F                   A(LOW LEVEL ACCOUNT TOTAL)                   
B4ULOFF  DS    C                   OFFICE POS POINTER FOR GEN ACCTS*            
B4ENTLEN EQU   *-B4ACTID           ENTRY LENGTH                                 
*                                                                               
TOTALSD  DSECT                                                                  
TOTACT   DS    CL14                U/L-ACCT                                     
TOTWRK   DS    CL2                 WORK FLD MIMICS BDG KEY FOR LOW LVL          
TOTFROM  DS    CL14                U/L-ACCOUNT OF POSTING FROM                  
TOTMN1   DS    CL2                 MONTH                                        
TOTAMNT1 DS    PL8                 AMOUNT                                       
TOTMNTHQ EQU   *-TOTMN1                                                         
TOTMN2   DS    CL2                 MONTH                                        
TOTAMNT2 DS    PL8                 AMOUNT                                       
TOTMN3   DS    CL2                 MONTH                                        
TOTAMNT3 DS    PL8                 AMOUNT                                       
TOTMN4   DS    CL2                 MONTH                                        
TOTAMNT4 DS    PL8                 AMOUNT                                       
TOTMN5   DS    CL2                 MONTH                                        
TOTAMNT5 DS    PL8                 AMOUNT                                       
TOTMN6   DS    CL2                 MONTH                                        
TOTAMNT6 DS    PL8                 AMOUNT                                       
TOTMN7   DS    CL2                 MONTH                                        
TOTAMNT7 DS    PL8                 AMOUNT                                       
TOTMN8   DS    CL2                 MONTH                                        
TOTAMNT8 DS    PL8                 AMOUNT                                       
TOTMN9   DS    CL2                 MONTH                                        
TOTAMNT9 DS    PL8                 AMOUNT                                       
TOTMNA   DS    CL2                 MONTH                                        
TOTAMNTA DS    PL8                 AMOUNT                                       
TOTMNB   DS    CL2                 MONTH                                        
TOTAMNTB DS    PL8                 AMOUNT                                       
TOTMNC   DS    CL2                 MONTH                                        
TOTAMNTC DS    PL8                 AMOUNT                                       
TOTDALN  EQU   *-TOTMN1            LEN OF DATES-AMOUNTS                         
TOTALSLN EQU   *-TOTALSD           LEN OF TOT                                   
*                                                                               
*                                                                               
PSTBD    DSECT U/L POSTING RULE ENTRIES                                         
PSTF     DS    0CL29                                                            
PSTCD    DS    C                   ID CODE                                      
PSTLN    DS    C                   LENGTH                                       
PSTTO    DS    CL14                POST TO ACCOUNT                              
PSTFRLN  DS    C                   LEN-1 OF FROM ACCOUNT                        
POSTID   DS    CL2                 U/L                                          
PSTFR    DS    CL10                POST FROM ACCOUNT                            
POSTLN   EQU   *-PSTBD                                                          
         EJECT ,                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDREPMASTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDREMOTED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032ACREPB402 04/24/06'                                      
         END                                                                    
