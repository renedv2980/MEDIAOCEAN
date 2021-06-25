*          DATA SET PPREPAR02  AT LEVEL 138 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044155.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PPAR02A                                                                  
*INCLUDE PPRTLOOK                                                               
*INCLUDE DATVAL                                                                 
         TITLE 'PPAR02 - PRINT AOR CONTRACT LISTING CHANGE LOG'                 
*                                                                               
*      CHANGE LOG                                                               
*                                                                               
* SMYE 04/06    DISPLAY "S" BEFORE DATE FOR STEWARDSHIP BUYS                    
*                                                                               
* BPLA  08/02   EXPAND RTLKELS - NOW A CSECT                                    
*                                                                               
* KWAN 01/01    CORRECT DUPLICATED LABEL USE PROBLEMS (FOR PANAPT)              
*                                                                               
* SMYE 05/00    CHANGES FOR LARGER PPNEWFILE                                    
*                                                                               
* SMYE 10/30/98 NO-OP MEDIA N EXCEPTION TO RATES-BY-PRODUCT DISPLAY             
*               IN PROC CON58Z                                                  
*                                                                               
* BPLA  1/96    IF PRBLIND IS "N" (NET $) TREAT AS IF PRBLIND WAS "$"           
*                                                                               
* SMYE 12/7/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                        
*                                                                               
*  BPLA 3/95    HANDLE INCH RATES FOR NON-INCH LEVEL INDS                       
*                                                                               
* SMUR 7/5/94   DISPLAY NET $ FOR NET CONTRACTS WHERE QOPT1-1 =R                
*               DISPLAY A NET $ COLUMN WHERE QOPT1-1 =A                         
*                                                                               
* BPLA 6/8/93   ADD DISPLAY OF PCATMAXZ (MAX/ISSUE ACROSS Z/E)                  
*                                                                               
* BPLA 3/27/92  DISALLOW RATE CHANGE IF MATCHED BUT NOT PAID                    
*                                                                               
* BPLA 3/03/92  ADD DISPLAY OF PCATMAX -HAD TO MOVE CON60-CON70                 
*               TO A NEW CSECT (COMPRT)                                         
* BPLA 7/13/90  CHANGE CONTROL DATE MOVE FROM QPUBCLAS+1(3) TO                  
*               QCNTLDT (QPUBCLAS)                                              
*                                                                               
* BPLA 12/5/89  GROUP REQUESTS                                   L11            
*                                                                               
* BPLA 6/16/89  PASS SVCOSIN TO RATELOOK SO IT WILL IGNORE       L10            
*               'C' RATE CONTRACT RATE ELEMS FOR NON- 'C' RATE   L10            
*               INSERTIONS                                       L10            
*                                                                               
* BPLA 5/7/89   DISPLAY CD,ATTN,SUPERSEDS OVERRIDES              L09            
*               AND LAST AUTO SPACE RESERVATION INFO             L09            
*               BEFORE COMMENTS                                  L09            
*                                                                               
* BPLA 4/24/89  FOR 'C' RATE BUYS DON'T LOOK-UP AC OR CD         L08            
*               ALSO ALWAYS SAVE AND RESTORE TAX FOR ALL BUYS    L08            
*               ('C' RATE BUYS - NO TAX)                         L08            
*                                                                               
*                                                                BUG02          
* ROSA 7/8/88 WHEN REQUESTING 16 ON LINE FOR A SPECIFIC PUB      BUG02          
*       AN INDICATOR IS SET ON BY THE REQUEST PGM WITHIN THE     BUG02          
*       16 REQUEST FOR END OF DAY TO AUTOMATICALLY CREATE A 15   BUG02          
*       CONTRACT REQUEST. EOD DOES THIS BUT DOES NOT REMOVE      BUG02          
*       THIS INDICATOR WHICH CAUSES PROBLEMS HERE. THAT INDICATORBUG02          
*       IS REMOVED IN THIS PGM.                                  BUG02          
*                                                                BUG02          
* ROSA 7/6/88 ADD NEW OPTION 'B' TO PRINT BOTH HIGHER AND LOWER    L07          
*       RATES // ALSO ONLY PRINT LOWER OR HIGHER IN 14 AND 18      L07          
*                                                                  L07          
* ROSA 6/29/88 PASS NATIONALITY OF COUNTRY TO RATELOOK             L06          
*                                                                BUG01          
* ROSA 6/3/88 ** BUG01 ** PBDCOSIN AND PBDCTYP WERE NOT CLEARED  BUG01          
*             GOING TO RATELOOK.                                 BUG01          
*             THE NET INDICATOR WAS STILL BE ON FOR GROSS RATE   BUG01          
*                                                                BUG01          
* ROSA  6/2/88 **L05 **  PRINT S FOR S TYPE RATES                   L05         
*                                                                   L05         
* ROSA 5/26/88 **L04 **  PRINT S FOR S TYPE RATES                   L04         
*                                                                   L03         
* BPLA  5/23/88 **L03** CONTRACT NOT PRINTING SUMMARY OF SPACE      L03         
*              DESCRIPTIONS WHEN SORT OPTION MADE..                 L03         
*                                                                   L03         
*                                                                   L02         
*  ROSA 5/6/88  ** L02 **   PRINT LOWER AND HIGHER RATES IF         L02         
*              REQUESTED  -- QOPT3 = L (LOWER) OR H (HIGHER)        L02         
*            IF AUTO RATE CHANGE AND QOPT3 L OR H FORCE QOPT3 =Y    L02         
*               QOPT3 = DO NOT WRITE TO FILE                        L02         
*  ROSA 4/29/88  ** L01 **  PRINT A 'N' PRIOR TO RATE IF 'NET'      L01         
*                                                                               
         TITLE 'PPAR02 - PRINT AOR CONTRACT LISTING'                            
*                                                                               
* REQUEST RULES                                                                 
*                                                                               
*  PUB   DATE  CON NUM                                                          
*  ---   ----  -------                                                          
*                                                                               
*  ALL    YES   NO                                                              
*                                                                               
*  YES    YES   NO                                                              
*                                                                               
*  YES    NO    YES                                                             
*        SPACE 2                                                                
*        QOPT1-1  REPORT TYPE                                                   
*        L= AOR CONTRACT LISTING (LIKE P14)                                     
*        A= AOR CONTRACT ANALYSIS  (LIKE P18)                                   
*        R= AOR CONTRACT RATE CHANGE (LIKE P16)                                 
*                                                                               
*        QOPT1 Y=CONTRACT AND REQ END MTHS MUST MATCH  PP14,PP18                
*        QOPT2 Y=PRINT FREE FORM COMMENTS       PP14,PP16                       
*        QOPT3 Y=TEST RUN - DON'T MARK FILE     PP16 ONLY                       
*         B= BOTH    L=LOWER RATE  H= HIGHER                        L02         
*        QOPT4 Y=SHOW ONLY CHANGED CONTRACTS    PP14,PP18                       
*                                                                               
*        QOPT5 Y=SKIP CONTRACTS IF CLT HAS NO BUYS ON PUB                       
*                                                                               
*        QOPT6 Y=INCLUDE TEST BUYS FOR PP18                                     
*                                                                               
*        QOPT7  SET BUY PPG FOR 'SORTED' REQUESTS                               
*              F=FIRST REQ, C=CONTINUATION                                      
*        QCNTLDT  CHANGE CONTROL DATE                                           
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
PPAR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPAR02,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         LA    R8,SPACEND                                                       
         USING PPARWRKD,R8                                                      
*                                                                               
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         MVC   ACONIO1,ACONIO      (A)PCONREC                                   
         DROP  RF                                                               
*                                                                               
         LA    R3,PPAR02+4095                                                   
         LA    R3,1(R3)                                                         
         USING PPAR02+4096,R3          ** NOTE USE OF SECOND BASE REG *         
*                                                                               
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         AH    R9,=H'4096'                                                      
         USING PPFILED,RC,R9                                                    
         EJECT                                                                  
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   CON1                                                             
         MVI   SVOPT7,0                                                         
         L     RE,SSB                                                           
         OI    3(RE),X'08'         FOR RECOVERY COPIES - PP16                   
         XC    PUBREC(35),PUBREC                                                
         MVI   LNEED,0                                                          
         MVI   WKOPEN,0                                                         
         MVI   TCONVDTE,0                                                       
         XC    TCONVFAC,TCONVFAC                                                
         MVI   TCONVIND,0                                                       
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         MVI   DASHES1,C'-'                                                     
         MVC   DASHES1+1(L'DASHES1-1),DASHES1                                   
* SET RELOCATABLE ADDRESSES                                                     
         L     R0,=V(RATELOOK)                                                  
         A     R0,RELO                                                          
         ST    R0,ARTLOOK                                                       
         L     R0,=V(DATVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,ADATVAL                                                       
         L     R0,=A(CLPRT)                                                     
         A     R0,RELO                                                          
         ST    R0,ACLPRT                                                        
         L     R0,=A(COMPRT)                                                    
         A     R0,RELO                                                          
         ST    R0,ACOMPRT                                                       
         L     R0,=A(CONSCHD)                                                   
         A     R0,RELO                                                          
         ST    R0,ACONSCHD                                                      
         L     R0,=A(CHGEL)                                                     
         A     R0,RELO                                                          
         ST    R0,ACHGEL                                                        
         L     R0,=A(POST18)                                                    
         A     R0,RELO                                                          
         ST    R0,APOST18                                                       
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         L     R0,=A(RTLKELS)                                                   
         A     R0,RELO                                                          
         ST    R0,ARTLKELS                                                      
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         B     EXIT                                                             
         EJECT                                                                  
CON1     DS    0H                                                               
         CLI   MODE,LBUYREQ        WILL GET FOR LAST SORTED CONTRACT            
         BNE   CON2                AND GETS SET AT CON120                       
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),REALSE      MUST RETURN TO REAL FILE                     
*                                  FOR NEXT REQUEST                             
         CLI   QOPT1-1,C'R'       RATE CHANGE                                   
         BE    EXIT                                                             
*****                                                                           
CON1A    CLI   QOPT1-1,C'A'        CON ANAL SKIP TO NEW PAGE                    
         BE    CON1B                                                            
         CLI   QOPT1-1,C'L'        CONTRACT LISTING                             
         BNE   CON1F                                                            
         B     EXIT                                                             
*              IF I REMOVE PRECEEDING BRANCH TO EXIT                            
*              REPORT TOTALS WILL PRINT AT END OF LISTING AND ANAL              
*              SHOWS TOTAL CONTRACTS                                            
*              TOTAL PUBS                                                       
*              CHANGED CONTRACTS (IF CHG CONTROL DATE SPECIFIED)                
*              LOCKED CONTRACTS (REQ BY DUPONT)                                 
*                                                                               
         MVI   LNEED,6                                                          
         GOTO1 ACLPRT              SKIP                                         
         B     CON1C                                                            
*                                                                               
CON1B    DS    0H                                                               
         CP    TOTCNT,=P'1'       SEE IF ONLY ONE CONTRACT                      
         BNH   CON1C              YES - THEN NO REPORT TOTALS                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   HDSW,19                                                          
         CLI   QMEDIA,C'O'     OUTDOOR                                          
         BNE   *+8                                                              
         MVI   HDSW,20                                                          
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CON1B0                                                           
         CLI   QCLIENT,C'*'                                                     
         BE    CON1B0                                                           
         CLI   QCLIENT,C'&&'                 GROUP REQUEST      L11             
         BE    CON1B0                                                           
         ZIC   R0,HDSW                                                          
         SH    R0,=H'10'                                                        
         STC   R0,HDSW                                                          
*                                                                               
CON1B0   DS    0H                                                               
         MVC   P+0(18),=C'** REPORT TOTALS**'                                   
         CLC   QPUB+8(3),=C'ZZZ'                                                
         BNE   *+10                                                             
         MVC   P+0(28),=C'** ACROSS EDITION SUMMARY **'                         
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT                                                           
         MVC   MID1+49(60),=C'SPACE              INSERTIONS           GX        
               ROSS            NET'                                             
         MVC   MID2+49(60),=C'-----              ----------           -X        
               ----            ---'                                             
         CLI   QMEDIA,C'N'                                                      
         BNE   TONL22                                                           
         CLI   TCONVDTE,X'FF'      SEE IF I HAVE COL CONV DATE                  
         BNE   TONL21                                                           
         MVC   MID1+29(13),=C'SPACE    RATE'                                    
         MVC   MID2+29(13),=C'-----    ----'                                    
         MVC   MID1+49(9),=C'   INCHES'                                         
         MVC   MID2+49(9),=C'   ------'                                         
         MVC   MID1+61(5),=C'LINES'                                             
         MVC   MID2+61(5),DASHES1                                               
         B     TONL25                                                           
*                                                                               
TONL21   DS    0H                                                               
         MVC   MID1+49(45),SPACES                                               
         MVC   MID2+49(45),SPACES                                               
         MVC   MID1+11(13),=C'SPACE    RATE'                                    
         MVC   MID2+11(13),=C'-----    ----'                                    
         MVC   MID2+34(6),=C'INCHES'                                            
         MVC   MID2+43(5),=C'LINES'                                             
         MVC   MID2+50(7),=C'INSRTNS'                                           
         MVC   MID2+62(6),=C'INCHES'                                            
         MVC   MID2+71(5),=C'LINES'                                             
         MVC   MID2+78(7),=C'INSRTNS'                                           
         MVC   MID2+90(6),=C'INCHES'                                            
         MVC   MID2+99(5),=C'LINES'                                             
         MVC   MID1+34(23),=C'-------- O L D --------'                          
         MVC   MID1+62(23),=C'-------- N E W --------'                          
         MVC   MID1+89(16),=C'-OLD EQUIVALENT-'                                 
         CLI   TCONVIND,C'-'                                                    
         BE    *+10                                                             
         MVC   MID1+90(3),=C'NEW'                                               
         MVC   MID1+106(10),=C'INSERTIONS'                                      
         MVC   MID2+106(10),DASHES1                                             
         MVC   MID1+122(5),=C'GROSS'                                            
         MVC   MID2+122(5),DASHES1                                              
         B     TONL25                                                           
TONL22   CLI   QMEDIA,C'O'         OUTDOOR                                      
         BNE   TONL25                                                           
         MVC   MID1+68(10),=CL10'POSTINGS'                                      
         MVC   MID2+68(10),=CL10'--------'                                      
*                                                                               
TONL25   MVI   FORCEMID,C'Y'                                                    
         MVC   SAVMID1,MID1                                                     
         MVC   SAVMID2,MID2                                                     
*                                                                               
         MVI   TOTSW,0                                                          
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'10'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     CON1B2                                                           
*                                                                               
CON1B1   GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFTYP,BUFFBUFF),BUFREC,0                  
*                                                                               
CON1B2   CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    CON1B2B                                                          
         CLI   BUFTYP,X'10'                                                     
         BE    CON1B2D                                                          
CON1B2B  MVC   P+60(17),=C'** NO ACTIVITY **'                                   
         GOTO1 ACLPRT                                                           
         B     CON1BXX                                                          
*                                                                               
CON1B2D  DS    0H                                                               
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BE    CON1B8                                                           
*                                                                               
*****    TM    SPACESW,X'01'       SEE IF ANY SPACES FOUND                      
*****    BZ    CON1B1              IGNORE NON-TOTAL BUFRECS                     
*****    CP    OAGYCNT,=P'0'       SEE IF OHTER AGY DATA                        
*****    BE    CON1B2F             NO                                           
         CLI   TOTSW,1             SEE IF 'REPORT TOTAL' PRINTED                
         BE    CON1B2F                                                          
         LA    R4,P+8                                                           
CON1B2E  MVC   0(12,R4),=C'REPORT TOTAL'                                        
         GOTO1 ACLPRT                                                           
         MVI   TOTSW,1                                                          
*                                                                               
CON1B2F  CLI   QMEDIA,C'N'                                                      
         BNE   CON1B2H                                                          
         CLI   TCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CON1B2F5                                                         
         MVC   P+11(20),BUFDESC                                                 
         B     CON1B4                                                           
CON1B2F5 MVC   P+29(20),BUFDESC                                                 
         B     CON1B4                                                           
*                                                                               
CON1B2H  LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R0,BUFDESC                                                       
         ST    R0,0(R5)                                                         
         MVI   PBYOCTL,X'80'       SPACE DESC ONLY                              
         GOTO1 PPBYOUT,DMCB,PPBYOWRK                                            
         MVC   P+49(17),PBYOSPC                                                 
         MVC   PSECOND+49(17),PBYOSPC2                                          
*                                                                               
         DROP  R5                                                               
*                                                                               
CON1B4   CLI   QMEDIA,C'N'                                                      
         BNE   CON1B5                                                           
         CLI   TCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CON1B4W             NO                                           
         CP    BUFINCH,=P'0'                                                    
         BE    CON1B4C                                                          
         EDIT  (P8,BUFINCH),(10,P+30),2,COMMAS=YES                              
CON1B4C  CP    BUFLINES,=P'0'                                                   
         BE    CON1B4C5                                                         
         EDIT  (P8,BUFLINES),(7,P+41),0,COMMAS=YES                              
CON1B4C5 CP    BUFINS,=P'0'                                                     
         BE    CON1B4D                                                          
         EDIT  (P8,BUFINS),(7,P+49),0,COMMAS=YES                                
CON1B4D  CP    BUFNINC,=P'0'                                                    
         BE    CON1B4G                                                          
         EDIT  (P8,BUFNINC),(10,P+58),2,COMMAS=YES                              
CON1B4G  CP    BUFNLNS,=P'0'                                                    
         BE    CON1B4G5                                                         
         EDIT  (P8,BUFNLNS),(7,P+69),0,COMMAS=YES                               
CON1B4G5 CP    BUFNINS,=P'0'                                                    
         BE    CON1B4H                                                          
         EDIT  (P8,BUFNINS),(7,P+77),0,COMMAS=YES                               
*                                                                               
CON1B4H  BAS   RE,TBUFCONV                                                      
CON1B4H8 CP    BUFINCH,=P'0'                                                    
         BE    CON1B4I                                                          
         EDIT  (P8,BUFINCH),(10,P+86),2,COMMAS=YES                              
CON1B4I  CP    BUFLINES,=P'0'                                                   
         BE    CON1B4J                                                          
         EDIT  (P8,BUFLINES),(7,P+97),0,COMMAS=YES                              
*                                                                               
CON1B4J  EDIT  (P8,BUFINS),(7,P+105),0,COMMAS=YES                               
         EDIT  (P8,BUFGRS),(14,P+113),2,COMMAS=YES,FLOAT=-                      
         B     CON1B7                                                           
*                                                                               
*              NOT DOING COL CONV                                               
*                                                                               
CON1B4W  CP    BUFINCH,=P'0'                                                    
         BE    CON1B4X                                                          
         EDIT  (P8,BUFINCH),(10,P+48),2,COMMAS=YES                              
CON1B4X  CP    BUFLINES,=P'0'                                                   
         BE    CON1B5                                                           
         EDIT  (P8,BUFLINES),(7,P+59),0,COMMAS=YES                              
*                                                                               
CON1B5   EDIT  (P8,BUFINS),(7,P+68),0,COMMAS=YES                                
         EDIT  (P8,BUFGRS),(14,P+80),2,COMMAS=YES,FLOAT=-                       
         MVC   BUFNET,BUFGRS                                                    
         SP    BUFNET,BUFACOM                                                   
         EDIT  (P8,BUFNET),(14,P+95),2,COMMAS=YES,FLOAT=-                       
CON1B7   GOTO1 ACLPRT                                                           
         B     CON1B1                                                           
*                                                                               
CON1B8   TM    SPACESW,X'01'                                                    
         BZ    CON1B8B                                                          
         GOTO1 ACLPRT                                                           
CON1B8B  MVC   P+36(19),=C'** REPORT TOTALS **'                                 
         CLI   QMEDIA,C'N'                                                      
         BNE   CON1B9                                                           
         MVC   P+36(21),SPACES                                                  
         LA    R4,P+24                                                          
         CLI   TCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CON1B8B5                                                         
         LA    R4,P+6                                                           
CON1B8B5 MVC   0(19,R4),=C'** REPORT TOTALS **'                                 
         CLI   TCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CON1B8W             NO                                           
         EDIT  (P8,BUFINCH),(10,P+29),2,COMMAS=YES                              
         MVI   P+39,C'*'                                                        
CON1B8C  EDIT  (P8,BUFLINES),(8,P+40),0,COMMAS=YES                              
         MVI   P+48,C'*'                                                        
         EDIT  (P8,BUFINS),(7,P+49),0,COMMAS=YES                                
         MVI   P+56,C'*'                                                        
         CP    BUFNINC,=P'0'                                                    
         BE    CON1B8G                                                          
         EDIT  (P8,BUFNINC),(10,P+58),2,COMMAS=YES                              
         MVI   P+68,C'*'                                                        
CON1B8G  CP    BUFNLNS,=P'0'                                                    
         BE    CON1B8G5                                                         
         EDIT  (P8,BUFNLNS),(7,P+69),0,COMMAS=YES                               
         MVI   P+76,C'*'                                                        
CON1B8G5 CP    BUFNINS,=P'0'                                                    
         BE    CON1B8H                                                          
         EDIT  (P8,BUFNINS),(7,P+77),0,COMMAS=YES                               
         MVI   P+84,C'*'                                                        
*                                                                               
CON1B8H  BAS   RE,TBUFCONV                                                      
CON1B8H8 CP    BUFINCH,=P'0'                                                    
         BE    CON1B8I                                                          
         EDIT  (P8,BUFINCH),(10,P+86),2,COMMAS=YES                              
         MVI   P+96,C'*'                                                        
CON1B8I  CP    BUFLINES,=P'0'                                                   
         BE    CON1B8J                                                          
         EDIT  (P8,BUFLINES),(7,P+97),0,COMMAS=YES                              
         MVI   P+104,C'*'                                                       
*                                                                               
CON1B8J  EDIT  (P8,BUFINS),(7,P+105),0,COMMAS=YES                               
         EDIT  (P8,BUFGRS),(14,P+113),2,COMMAS=YES,FLOAT=-                      
         MVI   P+112,C'*'                                                       
         MVI   P+127,C'*'                                                       
         B     CON1BX                                                           
*                                                                               
*              NOT DOING COL CONV                                               
*                                                                               
CON1B8W  EDIT  (P8,BUFINCH),(10,P+47),2,COMMAS=YES                              
         MVI   P+57,C'*'                                                        
         EDIT  (P8,BUFLINES),(8,P+58),0,COMMAS=YES                              
         MVI   P+66,C'*'                                                        
CON1B9   EDIT  (P8,BUFINS),(7,P+68),0,COMMAS=YES                                
         EDIT  (P8,BUFGRS),(14,P+80),2,COMMAS=YES,FLOAT=-                       
         MVC   BUFNET,BUFGRS                                                    
         SP    BUFNET,BUFACOM                                                   
         EDIT  (P8,BUFNET),(14,P+95),2,COMMAS=YES,FLOAT=-                       
         MVI   P+75,C'*'                                                        
         MVI   P+94,C'*'                                                        
         MVI   P+109,C'*'                                                       
CON1BX   GOTO1 ACLPRT                                                           
CON1BXX  MVI   FORCEHED,C'Y'                                                    
         EJECT                                                                  
CON1C    B     CON1F                                                            
*                           SKIP REPORT TOTALS FOR NOW                          
*                                                                               
         MVC   P+10(13),=C'REPORT TOTALS'                                       
         MVC   PSECOND+10(13),DASHES1                                           
         GOTO1 ACLPRT                                                           
         MVC   P+8(15),=C'TOTAL CONTRACTS'                                      
         EDIT  (P4,TOTCNT),(7,P+24),0,COMMAS=YES,ZERO=NOBLANK                   
         GOTO1 ACLPRT                                                           
         CP    LOCKCNT,=P'0'                                                    
         BE    CON1D                                                            
         MVC   P+17(6),=C'LOCKED'                                               
         EDIT  (P4,LOCKCNT),(7,P+24),0,COMMAS=YES                               
         GOTO1 ACLPRT                                                           
CON1D    CLC   QCNTLDT(3),SPACES                                                
         BE    CON1E                                                            
         MVC   P+16(7),=C'CHANGED'                                              
         EDIT  (P4,CHACNT),(7,P+24),0,COMMAS=YES,ZERO=NOBLANK                   
*                                                                               
CON1E    MVC   P+11(12),=C'PUBLICATIONS'                                        
         EDIT  (P4,TOTPCNT),(7,P+24),0,COMMAS=YES,ZERO=NOBLANK                  
         GOTO1 ACLPRT                                                           
CON1F    B     EXIT                                                             
*                                                                               
*        COL. CONVERT BUFFALO ACCUMS                                            
TBUFCONV AP    BUFINS,BUFNINS      ADD NEW TO OLD INS                           
         L     R0,TCONVFAC         COL CONV FACTOR                              
         CVD   R0,DUB                                                           
         CLI   TCONVIND,C'-'       SEE IF DOING AFTER TO BEFORE                 
         BE    TBUFC5                                                           
         MP    BUFINCH,DUB+5(3)                                                 
         DP    BUFINCH,=P'1000'                                                 
         CP    BUFINCH+5(3),=P'500'                                             
         BL    *+10                                                             
         AP    BUFINCH(5),=P'1'    ROUND                                        
         AP    BUFNINC,BUFINCH(5)                                               
         ZAP   BUFINCH,BUFNINC     PUT RESULT IN BUFINCH                        
         MP    BUFLINES,DUB+5(3)                                                
         DP    BUFLINES,=P'1000'                                                
         CP    BUFLINES+5(3),=P'500'                                            
         BL    *+10                                                             
         AP    BUFLINES(5),=P'1'    ROUND                                       
         AP    BUFNLNS,BUFLINES(5)                                              
         ZAP   BUFLINES,BUFNLNS    PUT RESULT IN BUFLINES                       
         B     TBUFC8                                                           
*                                                                               
TBUFC5   MP    BUFNINC,DUB+5(3)    AFTER TO BEFORE                              
         DP    BUFNINC,=P'1000'                                                 
         CP    BUFNINC+5(3),=P'500'                                             
         BL    *+10                                                             
         AP    BUFNINC(5),=P'1'    ROUND                                        
         AP    BUFINCH,BUFNINC(5)                                               
         MP    BUFNLNS,DUB+5(3)                                                 
         DP    BUFNLNS,=P'1000'                                                 
         CP    BUFNLNS+5(3),=P'500'                                             
         BL    *+10                                                             
         AP    BUFNLNS(5),=P'1'    ROUND                                        
         AP    BUFLINES,BUFNLNS(5)                                              
TBUFC8   BR    RE                  RETURN                                       
         EJECT                                                                  
*                                                                               
CON2     DS    0H                                                               
         CLI   MODE,FBUYREQ                                                     
         BNE   CON2A                                                            
         CLI   QOPT1-1,C'R'       RATE CHANGE                    BUG02          
         BNE   NNO16                                             BUG02          
         MVC   QCNTLDT(3),SPACES CLEAR INDICATOR                 BUG02          
NNO16    DS    0H                                                BUG02          
*                                                                               
         CLI   QOPT7,C'C'          DON'T RESET FOR CONTINUATION REQ L03         
         BE    CON2C                                                L03         
         CLI   QOPT7,C'F'          FIRST OF SORTED REQS                         
         BE    SRCLT0P                                                          
*                                                                   L02         
         MVC   REALAGY,QAGENCY      MUST SAVE "REAL" AGENCY                     
         MVC   REALCLT,QCLIENT      AND "REAL" CLIENT                           
         MVC   REALPUB,QPUB         AND "REAL" PUB                              
*                                                                               
*        MUST READ CLIENT HERE TO GET ADV DATA                                  
*        AND SWITCH TO THE AOR TO READ THE AOR CONTRACTS                        
*        MUST RESET QAGENCY AND RCSAVAGY TO THE AOR                             
*        MUST RESET QCLIENT TO THE AOR CLT                                      
*        THE ABOVE CHANGES WILL LET PPG READ THE AOR CONTRACTS                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),REALAGY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),REALCLT     NOTE THAT AR WILL ALWAYS                    
*                                   BE FOR ONE CLIENT                           
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'            MUST FIND THE CLIENT                             
         GOTO1 GETCLI                                                           
*                              READ PROFILE                                     
         XC    PROFKEY,PROFKEY                                                  
         MVC   PROFKEY,=CL12'P0AR'                                              
         MVC   PROFKEY+4(2),REALAGY                                             
         MVC   PROFKEY+6(1),QMEDIA                                              
         MVC   PROFKEY+7(3),REALCLT                                             
         CLI   REALOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   PROFKEY+10,C'*'                                                  
         MVC   PROFKEY+11(1),PCLTOFF                                            
         GOTO1 GETPROF,DMCB,PROFKEY,PROFAR,DATAMGR                              
         XC    SADVDATA,SADVDATA                                                
*                                                                               
         LA    R4,PCLTREC+33                                                    
SRCLT0   CLI   0(R4),X'15'                                                      
         BE    SRCLT0C                                                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   SRCLT0                                                           
         DC    H'0'                    INVALID CLIENT                           
*                                                                               
SRCLT0C  MVC   SADVDATA,2(R4)          SAVE ADV DATA                            
*                                                                               
         MVC   RCSVAGY,SVAOR                                                    
         MVC   QAGENCY,SVAOR                                                    
         MVC   QCLIENT,SVADV       USE ADV AS CLIENT                            
         MVC   PAGYKAGY,SVAOR      TRICK PPG                                    
*                                                                               
*       SEE IF PUB CONVERSION IS REQUIRED                                       
*                                                                               
         CLI   QPUB,C' '        FIRST SEE IF PUB GIVEN                          
         BE    SRCLT0M                                                          
         TM    SVAORC,X'01'      SEE IF PUB LINK REQUIRED                       
         BZ    SRCLT0M                                                          
         MVC   WORK(11),QPUB                                                    
         CLC   QPUB+8(3),=C'ZZZ'   ALL ZONES/EDTS                               
         BNE   *+10                                                             
         MVC   WORK+8(3),SPACES                                                 
         GOTO1 PUBVAL,DMCB,WORK,WORK+20                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'FE'                                                        
         MVC   KEY+1(1),QMEDIA                                                  
         MVC   KEY+2(2),REALAGY    REAL AGENCY                                  
         MVC   KEY+4(3),SVADV       ADERTISER                                   
         MVC   KEY+7(2),SVAOR       AOR                                         
         MVC   KEY+9(6),WORK+20                                                 
         GOTO1 HIGHPUB                                                          
         CLC   KEY(13),KEYSAVE     FIRST CHECK FOR "BASE" PUB                   
         BNE   MISSLNK                                                          
         CLC   QPUB+8(3),=C'ZZZ'    SEE IF ACCROSS ZONES/EDTS                   
         BE    SRCLT0G                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    SRCLT0G                                                          
*                                                                               
MISSLNK  MVC   P(35),=C'*** MISSING ADVERTISER PUB LINK ***'                    
         GOTO1 ACLPRT                                                           
         MVI   MODE,LBUYREQ                                                     
         B     EXIT               NO PUB LINK FOUND                             
*                                                                               
SRCLT0G  DS    0H                                                               
         GOTO1 PUBEDIT,DMCB,(C'0',KEY+15),(C'Q',QPUB)                           
         CLC   REALPUB+8(3),=C'ZZZ'                                             
         BNE   *+10                                                             
         MVC   QPUB+8(3),=C'ZZZ'                                                
*                                                                               
SRCLT0M  L     RF,UTL                                                           
         MVC   REALSE,4(RF)       SAVE "REAL" SE NUMBER                         
         MVC   4(1,RF),SVAORSE    SWTICH TO THE AOR                             
*                                                                               
*        OPEN THE AOR'S PRINTFILE                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'PRINT',PRTFILES,PPBYOWRK                
*                                                                               
SRCLT0P  MVI   E18SW,0                                              L02         
         CLI   QOPT3,C'B'       PRINT BOTH HIGHER AND LOWER         L07         
         BNE   *+8                                                  L07         
         MVI   E18SW,C'B'                                           L07         
         CLI   QOPT3,C'L'      LOWER RATES                          L02         
         BNE   *+8                                                  L02         
         MVI   E18SW,C'L'                                           L02         
         CLI   QOPT3,C'H'      HIGHER RATES                         L02         
         BNE   *+8                                                  L02         
         MVI   E18SW,C'H'                                           L02         
NOT18R   DS    0H                                                   L02         
*******                                                             L02         
         GOTO1 BUFFALO,DMCB,=C'RESET',BUFFBUFF                                  
CON2C    CLI   QPUB,C'0'           IF ONE PUB                                   
         BNL   CON01                                                            
         CLI   QSORT,C' '          OR NO SORT                                   
         BE    CON01               PROC REQ NOW                                 
*                                                                               
*                                                                               
         ZAP   TOTCNT,=P'0'                                                     
         ZAP   TOTPCNT,=P'0'                                                    
         ZAP   LOCKCNT,=P'0'                                                    
         ZAP   CHACNT,=P'0'                                                     
         B     EXIT                ELSE WAIT                                    
*                                                                               
CON01    DS    0H                                                               
         CLI   QOPT7,C'C'          SEE IF CONTINUATION REQ                      
         BE    CON01C                                                           
         ZAP   TOTCNT,=P'0'                                                     
         ZAP   TOTPCNT,=P'0'                                                    
         ZAP   LOCKCNT,=P'0'                                                    
         ZAP   CHACNT,=P'0'                                                     
         XC    PUBKMED(7),PUBKMED  TO INSURE PROPER TOTPCNT                     
CON01C   CLI   QOPT1-1,C'L'        LISTING                                      
         BNE   CON02                                                            
         CLI   QOPT7,C'C'          AND 'CONTINUATION' REQ                       
         BNE   CON02C                                                           
         B     CON03        O.K. SINCE WILL ALWAYS BE ONE CLT                   
*****    CLC   SVPROG+5(3),QCLIENT CHK FOR CHANGE OF CLIENT                     
*****    BE    CON03                                                            
*****    B     CON02E              NEW CLIENT FORCE NEW PAGE                    
*                                                                               
CON02    DS    0H                                                               
         CLI   QOPT7,C'C'          SEE IF CONTINUATION REQ                      
         BE    *+10                YES - DON'T RESET PAGE                       
CON02C   MVC   PAGE,=H'1'                                                       
CON02E   MVI   FORCEHED,C'Y'                                                    
CON03    DS    0H                                                               
         MVC   SVPROG,QPROG                                                     
         XC    SVRCON,SVRCON                                                    
         CLC   QEST,SPACES                                                      
         BE    CON03D                                                           
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
*        STH   R0,SVRCON          SAVE REQUESTED CONTRACT NUMBER    L01         
         STH   R0,DUB                                               L01         
         MVC   SVRCON,DUB                                           L01         
CON03D   XC    PCLTKEY,PCLTKEY                                                  
         MVI   HDSW,0                                                           
         CLI   QOPT1-1,C'L'      CONTRACT LISTING                               
         BE    CONCLT                                                           
         LA    R7,REQTOTS                                                       
         LA    R4,ACCNUM                                                        
RUNF5    ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R4,RUNF5                                                         
         MVI   HDSW,6                                                           
         MVI   FCRDBUY,X'21'                                                    
         CLI   QOPT1-1,C'R'        RATE CHANGE                                  
         BNE   CONCLT                                                           
         MVI   HDSW,1                                                           
         CLI   RCWRITE,C'Y'        SEE IF MARKING FILE                          
         BNE   CONCLT                                                           
*                                                                   L01         
*   IF AUTO RATE CHANGE AND USING LOWER/HIGHER OPTION               L01         
*        FORCE PROGRAM NOT TO WRITE TO FILE                         L01         
*                                                                   L01         
         CLI   E18SW,0                                              L01         
         BE    *+8                                                  L01         
         MVI   QOPT3,C'Y'          FORCE TO TEST RUN                L01         
*                                                                   L01         
         CLI   QOPT3,C'Y'           SEE IF TEST RUN                             
         BE    CONCLT              YES - DON'T MARK FILE                        
         CLI   WKOPEN,1                                                         
         BE    CONCLT                                                           
         XC    WID,WID                                                          
         MVC   DUB+00(2),RCDATE+6                                               
         MVC   DUB+02(2),RCDATE+0                                               
         MVC   DUB+04(2),RCDATE+3                                               
         MVC   TODAY1,DUB                                                       
         LA    R4,WID                                                           
         USING UKRECD,R4                                                        
*                                                                               
         MVC   UKUSRID,RCORIGID                                                 
         MVC   UKSYSPRG,=C'P16'                                                 
         PACK  FULL(2),TODAY1+4(3)                                              
         MVC   UKDAY,FULL                                                       
         MVI   UKCLASS,C'R'                                                     
         L     RF,=A(WRKRBUFF)                                                  
         A     RF,RELO                                                          
         ST    RF,MAPFILE                                                       
*                                  SEE IF ID ALREADY ON FILE                    
RUNF8    GOTO1 WORKER,DMCB,=C'INDEX',MAPFILE,WID                                
*                                                                               
         TM    DMCB+8,X'90'        REC NOT FOUND OR EOF                         
         BNZ   RUNF15                                                           
         TM    RCUPSI,X'80'        SEE IF RERUN                                 
         BNZ   RUNF15              YES                                          
*                  IF WORKER FILE EXISTS TRY NEXT SEQUENCE                      
*                  NUMBER                                                       
         ZIC   R0,UKSUBPRG                                                      
         AH    R0,=H'1'                                                         
         STC   R0,UKSUBPRG                                                      
         B     RUNF8                                                            
*                                                                               
         MVC   P+1(26),=C'WORKER FILE ALREADY EXISTS'                           
         GOTO1 ACLPRT                                                           
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
RUNF15   MVI   WKOPEN,1                                                         
         B     CONCLT                                                           
*                                                                               
CON2A    CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         B     CLOSEWK        MUST ALWAYS GO TO CLOSE WORK                      
******   CLC   SVPROG(2),=C'16'                                                 
******   BE    CLOSEWK                                                          
CON2AX   DS    0H                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
CONCLT   DS    0H                                                               
         CLI   QOPT7,C' '          SEE IF SORTED REQ                            
         BH    *+8                                                              
         MVI   PUBSW,0                                                          
         XC    KEY,KEY                                                          
         CLI   PCLTKEY,0           TEST FIRST TIME                              
         BZ    CONCLT2             YES                                          
         CLC   =C'ALL',QCLIENT                                                  
         BE    CONCLT1                                                          
         CLI   QCLIENT,C'&&'       GROUP REQUEST              L11               
         BE    CONCLT1                                        L11               
         CLI   QCLIENT,C'*'                                                     
         BNE   CON120                                                           
CONCLT1  DS    0H                                                               
         MVC   KEY,PCLTKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 SEQ                 READ NEXT CLIENT                             
         MVI   FORCEHED,C'Y'                                                    
         B     CONCLT4                                                          
CONCLT2  MVC   KEY(3),QAGENCY      A/M                                          
         MVI   KEY+3,2                                                          
         CLC   =C'ALL',QCLIENT                                                  
         BE    CONCLT3                                                          
         CLI   QCLIENT,C'&&'         GROUP REQUEST              L11             
         BE    CONCLT3                                          L11             
         CLI   QCLIENT,C'*'                                                     
         BNE   CONCLT6                                                          
CONCLT3  DS    0H                                                               
* ALL CLIENTS                                                                   
         GOTO1 HIGH                                                             
CONCLT4  CLC   KEY(4),KEYSAVE      A/M/X                                        
         BE    CONCLTX                                                          
         B     CON120                                                           
* ONE CLIENT                                                                    
CONCLT6  MVC   KEY+4(3),QCLIENT                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(7),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CONCLTX  LA    R0,PCLTREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET              GET CLIENT HEADER                            
*                                                                               
         CLI   QCLIENT,C'&&'            GROUP REQUEST           L11             
         BNE   CONCLTX1                                         L11             
         CLC   QCLIENT+1(1),PCLTBLGP    TEST RIGHT GROUP        L11             
         BNE   CONCLT1                                          L11             
         B     CONCLTX2                                         L11             
*                                                                               
CONCLTX1 DS    0H                                               L11             
         CLI   QCLIENT,C'*'                                                     
         BNE   CONCLTX2                                                         
         CLC   QCLIENT+1(1),PCLTOFF     TEST RIGHT OFFICE                       
         BNE   CONCLT1                                                          
CONCLTX2 DS    0H                                                               
*                                                                               
CON10    XC    BSTART(6),BSTART                                                 
         MVC   BEND,=3X'FF'                                                     
         CLC   QSTART,SPACES       TEST DATE PARAM SPECIFIED                    
         BE    CON10A              NO                                           
*                                                                               
*        GOTO1 DTCNV,DMCB,QSTART,(1,BSTART)                                     
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*        GOTO1 (RF),(R1),QEND,(1,BEND)                                          
         GOTO1 DATCON,(R1),(0,QEND),(3,BEND)                                    
*                                                                               
CON10A   CLC   QPUB,SPACES                                                      
         BNE   *+10                                                             
         MVC   QPUB(3),=C'ALL'                                                  
*                                                                               
         CLI   QOPT1-1,C'R'        RATE CHANGE                                  
         BE    CON10A3                                                          
         B     CON10A4         FOR "L" LISTING AND "A" ANALYSIS                 
*                              ALLOW ZZZ AND ONE CONTRACT                       
*                                                                               
CON10A3  CLC   QPUB+8(3),=C'ZZZ'   SEE IF DOING ALL ZONES + EDTS                
         BE    CON10A5                                                          
CON10A4  CLC   QPUB(3),=C'ALL'                                                  
         BNE   CON10B                                                           
CON10A5  CLC   QEST,SPACES         MUST NOT SPECIFY CONTRACT                    
         BNE   CONERR                                                           
         CLC   QSTART,SPACES       AND MUST NAME DATE                           
         BE    CONERR                                                           
         B     CON12                                                            
CON10B   CLC   QSTART,SPACES       MUST NAME DATE OR CONTRACT                   
         BNE   CON12                                                            
         CLC   QEST,SPACES                                                      
         BNE   CON12                                                            
CONERR   MVC   P(L'ERRMSG),ERRMSG                                               
         MVC   PSECOND(80),QRECORD                                              
         GOTO1 REPORT                                                           
         B     EXIT                                                             
ERRMSG   DC    C'INVALID PUB/CONTRACT/DATE SPECIFICATION'                       
         SPACE 2                                                                
CON12    XC    KEY,KEY             BUILD CONTRACT KEY                           
         MVC   KEY(7),PCLTKEY      A/M/X/CLT                                    
         MVI   KEY+3,X'10'                                                      
         CLC   =C'ALL',QPUB                                                     
         BE    CON16                                                            
         EJECT                                                                  
* ONE PUB                                                                       
         MVC   WORK(11),QPUB                                                    
         CLC   QPUB+8(3),=C'ZZZ'   SEE IF DOING ALL ZONES +EDTS                 
         BNE   CON12B                                                           
         MVC   WORK+8(3),SPACES                                                 
CON12B   GOTO1 PUBVAL,DMCB,WORK,KEY+7                                           
         CLC   QPUB+8(3),=C'ZZZ'                                                
         BE    CON12F                                                           
         CLC   QEST,SPACES         TEST CON SPECIFIED                           
         BNE   CON14               NO                                           
* ONE PUB/ALL CONTRACTS                                                         
* OR NNN,ZZZ AND ONE CONTRACT                                                   
*                                                                               
CON12F   GOTO1 HIGH                                                             
         B     CON13                                                            
*                                                                               
CON12G   GOTO1 SEQ                                                              
*                                                                               
CON13    CLC   KEY(11),KEYSAVE     SAME A/M/X/CLT/BASE PUB                      
         BNE   CONCLT                                                           
         CLC   QPUB+8(3),=C'ZZZ'   SEE IF DOING ALL ZONES + EDTS                
         BE    CON13D              YES                                          
         CLC   KEY(13),KEYSAVE     NO CHK FULL PUB NUMBER                       
         BNE   CONCLT                                                           
CON13D   OC    SVRCON,SVRCON       SEE IF CONTRACT NUMBER SPECIFIED             
         BZ    CON20                                                            
         CLC   KEY+13(2),SVRCON                                                 
         BNE   CON12G              WRONG NUMBER-KEEP LOOKING                    
         B     CON20               GO FILTER ON DATES                           
*                                                                               
* ONE PUB/ONE CONTRACT                                                          
*                                                                               
CON14    PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   KEY+13(2),HALF                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE     SAMEA/M/X/CLT/PUB/CON                        
         BNE   CONCLT                                                           
         B     CON20                                                            
*                                                                               
*ALL PUBS/ALL CONTRACTS                                                         
*                                                                               
CON16    GOTO1 HIGH                                                             
*                                                                               
CON17    CLC   KEY(7),KEYSAVE      TEST SAME A/M/X/CLT                          
         BNE   CONCLT                                                           
         EJECT                                                                  
* GET CONTRACT REC AND FILTER ON DATES                                          
*                                                                               
CON20    DS    0H                                                               
*NOP*    LA    R0,PCONREC                                                       
*NOP*    ST    R0,IOAREA                                                        
         L     RF,ACONIO1          (A)PCONREC                                   
         ST    RF,IOAREA                                                        
         USING PCONREC,RF                                                       
*                                                                               
         BAS   RE,GET                                                           
*                                                                               
         CLC   PCONEDT,BSTART      CON END BEFORE REQ START                     
         BL    CON92                                                            
         CLC   BEND,PCONSDT        REQ END BEFORE CON START                     
         BL    CON92                                                            
         CLI   QOPT1,C'Y'                                                       
         BNE   CON25                                                            
         CLC   BEND(2),PCONEDT        MTHS MUST MATCH                           
         BNE   CON92                                                            
*                                                                               
CON25    JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',CON28,JUMP=N          
         CLI   PCONPRD,C'A'        DOING ONE PRD - SO PASS CONS FOR             
         BL    CON28               ALL PRDS OR MATCHING PRDS                    
         CLC   PCONPRD,QPRODUCT                                                 
         BNE   CON92                                                            
         B     CON28                                                            
*                                                                               
         SPACE 2                                                                
CON28    CLI   QOPT4,C'Y'          SEE IF SHOWING ONLY CHGED CONTRACTS          
         BNE   CON29               NO                                           
         CLC   PCONMOD,QCNTLDT         CHK CHG CONTROL DATE                     
         BL    CON92               SKIP THIS CON                                
*                                                                               
CON29    CLI   QOPT5,C'Y'        SEE IF SKIPPING CON IF NO BUYS                 
         BNE   CON30                                                            
         MVI   TESTPASS,1                                                       
         MVC   BQSTART(6),PCONSTRT                                              
         GOTO1 ACONSCHD                                                         
         CLI   TESTPASS,1                                                       
         BE    CON92               MEANS NO BUY WAS FOUND                       
*                                  TEST PASS SET TO 0 IF CONSCHD                
*                                  FINDS A BUY                                  
*                                  THEN SKIP THIS CONTRACT                      
         DROP  RF                                                               
         EJECT                                                                  
* GET PUB RECORDS                                                               
CON30    DS    0H                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         USING PCONREC,RF                                                       
*                                                                               
         CLC   PCONKPUB(6),PUBKPUB                                              
         BNE   CON31                                                            
         CLC   PCONKMED(1),PUBKMED                                              
         BE    CON34                    ALREADY HAVE PUBRECORD                  
CON31    DS    0H                                                               
         MVI   PUBSW,0                                                          
         AP    TOTPCNT,=P'1'       BUMP PUB COUNTER                             
*                                                                               
*        MUST SWTICH BACK TO "REAL" AGENCY TO READ "REAL" PUB                   
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),REALSE                                                   
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
*                                                                               
         MVC   SVAORPUB,PCONKPUB                                                
*                                                                               
         TM    SVAORC,X'01'          SEE IF PUB LINK                            
         BZ    CON31C                                                           
*          FIND PUB LINK                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'FD'                                                        
         MVC   KEY+1(1),QMEDIA                                                  
         MVC   KEY+2(3),QCLIENT         REALLY ADV                              
         MVC   KEY+5(2),QAGENCY         REALLY AOR                              
         MVC   KEY+7(2),REALAGY         AGENCY                                  
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         MVC   KEY+9(6),PCONKPUB                                                
         GOTO1 HIGHPUB                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    CON31B                                                           
*                              IF NOT FOUND SWITCH BACK                         
*                              TO AOR AND SKIP THIS CONTRACT                    
         L     RF,UTL                                                           
         MVC   4(1,RF),SVAORSE                                                  
         B     CON92                                                            
*                                                                               
CON31B   DS    0H                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
*                                                                               
         MVC   PCONKPUB(6),KEY+15    PUT "REAL" PUB IN CONTRACT                 
*                                                                               
CON31C   XC    KEY,KEY                                                          
         MVC   KEY(1),PCONKEY+2    MEDIA                                        
         MVC   KEY+1(6),PCONKPUB                                                
         MVC   KEY+7(2),REALAGY                                                 
         MVI   KEY+9,X'81'                                                      
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE     CHECK FOUND X'81' REC                        
         BE    CON32               YES                                          
         CLC   KEY+7(2),=C'ZZ'     CHECK FOUND ZZ X'81' REC                     
         BE    CON32               YES                                          
*                                                                               
         MVC   KEYSAVE+7(2),=C'ZZ' TRY FOR DEFAULT                              
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE     MUST FIND DEFAULT                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CON32    LA    R0,PUBREC                                                        
         ST    R0,IOAREA                                                        
         BAS   RE,GETPUB                                                        
         MVC   PUBKED,KEY+6  SET ED CODE IN PUBREC                              
* NOW TRY FOR LTLREC                                                            
* NO-OP LTLREC READ AND REP READ ***                                            
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),SVAORSE      MUST SWITCH BACK TO AOR                     
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         MVC   PCONKPUB(6),SVAORPUB    RESET AOR PUB IN CONTRACT                
         B     CON34                                                            
*                                                                               
         DROP  RF                                                               
*                                                                               
CON34    DS    0H                                                               
*                                                                               
         CLI   PUBSW,C'P'          TEST PUB PRINTED YET                         
         BE    CON40                                                            
         MVI   PUBSW,C'P'                                                       
         MVI   LNEED,8                                                          
         OC    PUBZNAME,SPACES                                                  
         LA    R4,P                                                             
         CLI   QMEDIA,C'N'                                                      
         BE    CON35                                                            
*                                  MAG FORMAT                                   
         MVC   0(20,R4),PUBNAME                                                 
         MVC   132(20,R4),PUBZNAME                                              
         B     CON37                                                            
*                                                                               
CON35    DS    0H                                                               
         LA    R5,132(R4)                                                       
         CLI   PUBCITY,C' '                                                     
         BNH   CON36                                                            
         CLI   PUBSTATE,C' '                                                    
         BNH   CON36                                                            
         LA    R5,132+4(R4)                                                     
         MVC   0(2,R4),PUBSTATE                                                 
         MVI   2(R4),C','                                                       
         MVC   4(16,R4),PUBCITY                                                 
         LA    R4,20(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,3(R4)                                                         
*                                                                               
CON36    DS    0H                                                               
         MVC   0(20,R4),PUBNAME                                                 
         MVC   0(20,R5),PUBZNAME                                                
*                                                                               
CON37    DS    0H                                                               
         LA    R4,20(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PUBKPUB),4(R4)                                
*                                                                               
*                                                                               
         MVI   3(R4),C'('                                                       
         LA    R4,21(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
*                                                                               
CON38    DS    0H                                                               
         LA    R4,20(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVC   3(9,R4),=C'ADV LINK='                                            
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),SVAORPUB),12(R4)                              
*                                                                               
*                                                                               
         MVC   SVPNAME,P          SAVE PUB NAME AND NUMBER                      
         MVC   SVPZNAME,PSECOND   AND ZONE                                      
*                                  FOR PRINTING INSERTION CHGS FOR P16          
*                                                                   L02         
         CLI   E18SW,0    RATE COMPARISONS FOR 18                   L02         
         BE    *+14        NO                                       L02         
         LA    R4,P+58                                                          
         MVC   10(L'CURRATE,R4),CURRATE                             L02         
***                                                                 L02         
         GOTO1 ACLPRT                                                           
*                                                                               
         EJECT                                                                  
CON40    DS    0H                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         USING PCONREC,RF                                                       
*                                                                               
         CLI   PCONPRD,C'A'        SEE IF PRD CONTRACT                          
         BL    *+10                NO                                           
         MVC   P+26(3),PCONPRD                                                  
         MVI   LNEED,5                                                          
*                                                                               
         MVC   HALF,PCONNUM                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+33(3),DUB                                                      
         MVC   SVPCON,P+33         SAVE CON - USED FOR INS CHGS                 
*                                  FOR RATE CHANGE (QOPT1-1 = R)                
*                                                                               
         AP    TOTCNT,=P'1'                                                     
         CLC   QCNTLDT(3),SPACES                                                
         BE    CON40B                                                           
         CLC   PCONMOD,QCNTLDT                                                  
         BL    CON40B                                                           
         MVI   P+36,C'*'                                                        
         AP    CHACNT,=P'1'                                                     
CON40B   DS    0H                                                               
         TM    PCONLIND,X'80'      SEE IF LOCKED                                
         BZ    CON40B5                                                          
         MVI   P+37,C'L'                                                        
         AP    LOCKCNT,=P'1'                                                    
CON40B5  DS    0H                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         GOTO1 DATCON,DMCB,(3,PCONSDT),(5,P+40)                                 
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         GOTO1 DATCON,(R1),(3,PCONEDT),(5,P+49)                                 
*                                                                               
         MVI   P+48,C'-'                                                        
*                                  CHECK IF RATELOOK REQUIRED                   
         L     RF,ACONIO1          (A)PCONREC                                   
         LA    R2,PCONREC+33                                                    
E18LCODE MVI   ELCODE1,X'20'       ELCODE WILL BE MODIFIED                      
         BAS   RE,NEXTEL                                                        
         BE    CON40C                                                           
         GOTO1 ACLPRT                                                           
         B     CON60                                                            
*                                                                               
CON40C   DS    0H                                                               
         CLI   QOPT1-1,C'R'        (P16)                                        
         BNE   CON41                                                            
         MVI   NETSW,C'N'                                                       
         TM    PRBIND-PRBELEM(R2),X'10'                                         
         BNO   CON41                                                            
         CLI   PROFAR,C'Y'                                                      
         BNE   CON41                                                            
         MVI   NETSW,C'Y'                                                       
*                                                                               
CON41    DS    0H                                                               
         MVC   P+58(50),SPACES                                      L02         
         CLI   QMEDIA,C'N'                                                      
         BNE   CONRTX                                                           
         LA    R7,PRBLIND-PRBELEM(R2)                                           
         CLI   0(R7),C'L'                                                       
         BNE   CONRTX                                                           
         LA    R7,PRBIND-PRBELEM(R2)                                            
         TM    0(R7),X'80'                                                      
         BO    CONRTX                                                           
* AWAY WE GO                                                                    
         L     RF,ACONIO1          (A)PCONREC                                   
         GOTO1 ARTLOOK,DMCB,0,PUBREC,PCONREC,ARTLKELS                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLI   0(R1),0             CHECK FOR ERRORS                             
         BNE   CONRTX                                                           
*                                                                               
         L     R2,ARTLKELS         POINT TO NEW ELEMENTS                        
*                                                                               
CONRTX   B     *+12                                                             
CON51    BAS   RE,NEXTEL                                                        
         BNE   CON60                                                            
         XC    W,W       CLEAR WORK                                             
*                                                                               
         LA    R4,PRBLEVEL-PRBELEM(R2)                                          
         CP    0(L'PRBLEVEL,R4),=P'0'                                           
         BNZ   CON51A                                                           
         SR    R0,R0                                                            
         CLI   PRBLIND-PRBELEM(R2),C'S'        IF NO LEVEL JUST                 
         BE    CON51BC                         DISPLAY 'SPECIAL'                
         MVC   W+06(4),=C'OPEN'                                                 
         LA    R4,PRBIND-PRBELEM(R2)                                            
         TM    0(R4),X'01'                                                      
         BZ    CON54                                                            
         MVC   W+06(4),=C'FLAT'                                                 
         B     CON54                                                            
*                                                                               
CON51A   DS    0H                                                               
         LA    R7,PRBLEVEL-PRBELEM(R2)                                          
         CLI   PRBLIND-PRBELEM(R2),C'N'                                         
         BE    CON51A1                                                          
         CLI   PRBLIND-PRBELEM(R2),C'$'                                         
         BNE   CON51B                                                           
         B     CON51A2                                                          
*                                                                               
* SHOULD I ALSO PRECEED WITH 'N" IF PRBIND HAS X'10' (NET RATE) ON ?            
*                                                                               
CON51A1  MVI   W+05,C'N'                                                        
*                                                                               
CON51A2  EDIT  (P5,(R7)),(11,W+06),ALIGN=LEFT,COMMAS=YES,FLOAT=$                
*                                                                               
         B     CON54                                                            
*                                                                               
CON51B   DS    0H                                                               
         EDIT  (P5,(R7)),(9,W+06),ALIGN=LEFT,COMMAS=YES                         
*                                                                               
CON51BC  LA    R6,W+06                                                          
         AR    R6,R0                                                            
         LA    R7,PRBLIND-PRBELEM(R2)                                           
CON52    CLI   0(R7),C'X'                                                       
         BNE   *+10                                                             
         MVC   1(05,R6),=C'TIMES'                                               
         CLI   0(R7),C'P'                                                       
         BNE   *+10                                                             
         MVC   1(05,R6),=C'PAGES'                                               
         CLI   0(R7),C'L'                                                       
         BNE   *+10                                                             
         MVC   1(05,R6),=C'LINES'                                               
         CLI   0(R7),C'I'                                                       
         BNE   *+10                                                             
         MVC   1(06,R6),=C'INCHES'                                              
         CLI   0(R7),C'U'                                                       
         BNE   *+10                                                             
         MVC   1(6,R6),=C'ISSUES'                                               
         CLI   0(R7),C'S'                                                       
         BNE   *+10                                                             
         MVC   1(7,R6),=C'SPECIAL'                                              
*                                                                               
CON54    LA    R4,PRBPCT-PRBELEM(R2)                                            
         CP    0(L'PRBPCT,R4),=P'0'                                             
         BZ    CON55                                                            
         EDIT  (P3,(R4)),(5,W+20),2                                             
         CLC   W+22(3),=C'.00'                                                  
         BNE   *+10                                                             
         MVC   W+22(3),SPACES                                                   
*                                                                               
CON55    LA    R6,PRBRATE-PRBELEM(R2)                                           
         CP    0(5,R6),=P'0'                                                    
         BE    CON58                                                            
         LA    R7,PRBLIND-PRBELEM(R2)                                           
         TM    PRBIND-PRBELEM(R2),X'40'                                         
         BNZ   CON55B                                                           
         TM    PRBIND-PRBELEM(R2),X'20'      UNIT RATE                          
         BNZ   CON56                                                            
         CLI   QMEDIA,C'N'                                                      
         BE    CON56                                                            
         CLI   PRBLIND-PRBELEM(R2),C'L'                                         
         BE    CON56                                                            
         CLI   PRBLIND-PRBELEM(R2),C'I'                                         
         BE    CON56                                                            
CON55B   DS    0H                                                               
         TM    PRBIND-PRBELEM(R2),X'10'       NET INDICATOR       L01           
         BNO   *+8                                                L01           
         MVI   W+26,C'N'                                          L01           
         TM    PRBIND-PRBELEM(R2),X'02'         S INDICATOR       L04           
         BNO   *+8                                                L04           
         MVI   W+26,C'S'                                          L04           
         TM    PRBIND-PRBELEM(R2),X'04'         C INDICATOR       L05           
         BNO   *+8                                                L05           
         MVI   W+26,C'C'                                          L05           
         EDIT  (P5,(R6)),(10,W+27),2,COMMAS=YES                                 
         B     CON58                                                            
CON56    TM    PRBIND-PRBELEM(R2),X'10'       NET INDICATOR       L01           
         BNO   *+8                                                L01           
         MVI   W+26,C'N'                                          L01           
         TM    PRBIND-PRBELEM(R2),X'02'         S INDICATOR       L04           
         BNO   *+8                                                L04           
         MVI   W+26,C'S'                                          L04           
         TM    PRBIND-PRBELEM(R2),X'04'         C INDICATOR       L05           
         BNO   *+8                                                L05           
         MVI   W+26,C'C'                                          L05           
         EDIT  (P5,(R6)),(10,WORK),5                                            
*                                                                               
         LA    R1,W+27                                                          
         CLC   WORK+7(3),=C'000'                                                
         BNE   *+10                                                             
         MVC   WORK+7(3),SPACES                                                 
         MVC   0(10,R1),WORK                                                    
*                                                                               
         LA    RF,=C'/I'                                                        
         CLI   PRBLIND-PRBELEM(R2),C'I'                                         
         BE    CON57                                                            
         TM    PRBIND-PRBELEM(R2),X'08'                                         
         BO    CON57                                                            
         LA    RF,=C'/L'             MUST BE LINE RATE                          
*****    NOW ALWAYS SHOW /I OR /L                                               
*****    CLI   PRBLIND-PRBELEM(R2),C'L'                                         
*****    BNE   CON58                                                            
*                                                                               
CON57    DS    0H                                                               
         LA    R1,10(R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   1(2,R1),0(RF)                                                    
*                                                                               
CON58    CLI   W+26,C'N'      NET INDICATOR                        L01          
         BE    BING00                                              L01          
         CLI   W+26,C'C'      COMMISSION ONLY                      L05          
         BE    BING00                                              L05          
         CLI   W+26,C'S'      NO COMMISSION                        L04          
         BNE   CON58A                                              L04          
BING00   LA    R7,W+26                                             L01          
         LA    R5,7                                                L01          
CON58AA  CLI   1(R7),C' '                                          L01          
         BH    MOVEIT                                              L01          
         LA    R7,1(R7)                                            L01          
         BCT   R5,CON58AA                                          L01          
MOVEIT   MVC   0(1,R7),W+26                                        L04          
         CH    R5,=H'7'   NO SPACES                                L04          
         BE    *+8                                                 L41          
*                                                                  L01          
         MVI   W+26,C' '                                           L04          
*                                                                  L01          
CON58A   LA    R7,PRBDESC-PRBELEM(R2)                                           
*                                                                               
*                                                                               
*                                                                               
         LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
*                                                                               
         ST    R7,PBYOINPT                                                      
         MVC   PBYOINPT(1),QMEDIA                                               
         MVI   PBYOCTL,X'80'       ONLY SPACE INPUT                             
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*****                                                                           
         CLC   PBYOSPC(2),=C'R='                                                
         BNE   CON58C                                                           
         MVC   W+40(5),PBYOSPC                                                  
         MVI   W+45,C','                                                        
         MVC   W+46(12),PBYOSPC+5                                               
         B     *+10                                                             
*****                                                                           
CON58C   MVC   W+40(17),PBYOSPC                                                 
         CLI   PBYOSPC2,C' '                                                    
         BNH   CON5804                                                          
*****                                                                           
         CLC   PBYOSPC(2),=C'R='                                                
         BNE   CON58D                                                           
         MVC   PSECOND+95(5),PBYOSPC2                                           
         MVI   PSECOND+100,C','                                                 
         MVC   PSECOND+101(12),PBYOSPC2+5                                       
         B     CON5804                                                          
*****                                                                           
CON58D   MVC   PSECOND+95(17),PBYOSPC2                                          
         DROP  R5                                                               
CON5804  DS    0H                                                               
*                                                                               
*                                                                               
         LA    R7,PRBDATE-PRBELEM(R2)                                           
         OC    0(3,R7),0(R7)                                                    
         BZ    CON58Z                                                           
*        GOTO1 DTCNV,DMCB,(1,(R7)),(3,W+58)                                     
         GOTO1 DATCON,DMCB,(3,(R7)),(5,W+58)                                    
*                                                                               
*NOP*CON58Z   CLI   QMEDIA,C'N'                                                 
*NOP*    BE    CON59                                                            
CON58Z   LA    R7,PRBOPEN-PRBELEM(R2)   MAY HAVE PRODUCT HERE                   
         CLI   0(R7),C'A'              CHECK FOR PRODUCT                        
         BL    CON59                                                            
         MVI   W+66,C'-'                                                        
         MVC   W+67(3),0(R7)           DISPLAY PRODUCT                          
CON59    DS    0H                                                               
         MVC   P+58(68),W+3                                                     
         GOTO1 ACLPRT                                                           
*                                                                               
         B     CON51                                                            
         TITLE 'DETERMINE IF ANALYSIS WITH LOWER/HIGH RATES'                    
*                                                                  L02          
CON60    CLI   E18SW,0       RATE COMPARISION FOR 18               L02          
         BE    CON60A                                              L02          
*                                                                  L07          
         CLI   E18SW,C'B'    BOTH RATES                            L07          
         BE    *+12                                                L07          
         CLI   E18SW,C'L'    LOWER ONLY                            L07          
         BNE   E18HIGH                                             L07          
         CLI   E18LCODE+1,X'21'                                    L02          
         BE    E18HIGH                                             L02          
         BH    E18INIT       INITIALIZE E18LCODE                   L02          
*   FIRST TIME THRU                                                L02          
         GOTO1 ACLPRT   PRINT BLANK LINE                           L02          
         LA    RF,P+58                                             L02          
         MVC   10(L'LOWRATE,RF),LOWRATE                            L02          
         MVI   E18LCODE+1,X'21'  FORCE TO LOOK FOR LOWER RATES     L02          
         GOTO1 ACLPRT   PRINT MESSAGE                              L02          
         LA    RF,P+58                                             L02          
         MVC   10(31,RF),=C'    NO LOWER LEVEL RATES FOUND  '      L02          
         B     CON40B                                              L02          
*                                                                  L02          
E18HIGH  DS    0H                                                  L02          
*                                                                  L07          
         CLI   E18SW,C'B'    BOTH RATES                            L07          
         BE    *+12                                                L07          
         CLI   E18SW,C'H'    HIGHER ONLY                           L07          
         BNE   E18INIT                                             L07          
         CLI   E18LCODE+1,X'22'  IF EQ THEN OUT                    L07          
         BE    E18INIT                                             L07          
         GOTO1 ACLPRT   PRINT BLANK LINE                           L02          
         LA    RF,P+58                                             L02          
         MVC   10(L'HIGRATE,RF),HIGRATE                            L02          
         MVI   E18LCODE+1,X'22'  FORCE TO LOOK FOR HIGHER RATES    L02          
         GOTO1 ACLPRT   PRINT MESSAGE                              L02          
         LA    RF,P+58                                             L02          
         MVC   10(31,RF),=C'    NO HIGHER LEVEL RATES FOUND  '     L02          
         B     CON40B                                              L02          
*                                                                  L02          
E18INIT  MVI   E18LCODE+1,X'20'  NORMALIZE                         L02          
*                                                                  L02          
*                                                                  L02          
*                                                                  L02          
         TITLE 'PPAR02 - PRINT CONTRACT LISTING'                                
* COMMENTS                                                                      
*                                                                               
CON60A   DS    0H                                                               
*                                  PRINT COMMENTS                               
         GOTO1 ACOMPRT                                                          
*                                                                               
CON70    DS    0H                                                               
         GOTO1 ACLPRT              SKIP A LINE                                  
         CLI   QOPT1-1,C'L'       LISTING                                       
         BE    CON92                                                            
         CLI   QOPT1-1,C'A'        CON ANAL                                     
         BNE   CON71                                                            
         MVI   SPACESW,0                                                        
         MVI   HDSW,7              FOR CONTINUATION                             
         CLI   QMEDIA,C'O'         FOR OUTDOOR                                  
         BNE   *+8                                                              
         MVI   HDSW,8                                                           
*                                                                               
CON703   DS    0H                                                               
         MVC   SCONVDTE,=3X'FF'                                                 
         XC    SCONVFAC,SCONVFAC                                                
         MVI   SCONVIND,0                                                       
*        SEARCH FOR SPECIAL COMMENT WITH COLUMN CONVERSION                      
*        FIND AND SAVE CONVERSION FACTOR AND EFFECTIVE DATE                     
*                                                                               
*        FORMAT IS COL.CONV=N-N,MMMDD/YY  EQUIV AFTER TO BEFORE                 
*        FORMAT IS COL.CONV=N*N,MMMDD/YY  EQUIV BEFORE TO AFTER                 
*                                                                               
*        MVI   ELCODE1,X'40'                                                    
*        LA    R2,PCONREC+33                                                    
*ON703C  BAS   RE,NEXTEL                                                        
*        BNE   CON703N                                                          
*        CLC   2(9,R2),=C'COL.CONV='    LOOK FOR KEY WORD                       
*        BNE   CON703C                                                          
*        ZIC   R7,1(R2)            ELEM LENGHT                                  
*        SH    R7,=H'11'           ELEM CODE + KEY WORD                         
*        BNP   CON703C                                                          
*        LA    RE,11(R2)                                                        
*        LR    RF,RE                                                            
*        SR    R5,R5                                                            
*ON703E  CLI   0(RE),C'-'          SCAN FOR -                                   
*        BE    CON703G                                                          
*        CLI   0(RE),C'*'          SCAN FOR -                                   
*        BE    CON703G                                                          
*        CLI   0(RE),C'0'                                                       
*        BL    CON703C                                                          
*        CLI   0(RE),C'9'                                                       
*        BH    CON703C                                                          
*        LA    RE,1(RE)                                                         
*        LA    R5,1(R5)                                                         
*        BCT   R7,CON703E                                                       
*        B     CON703C             INVALID SKIP THIS ELEM                       
*                                                                               
*ON703G  LTR   R5,R5                                                            
*        BZ    CON703C             INVALID SKIP THIS ELEM                       
*        CH    R5,=H'2'                                                         
*        BH    CON703C             MAX 2 CHARS                                  
*        MVC   SCONVIND,0(RE)      SAVE CONV IND                                
*        BCTR  R5,0                                                             
*        EX    R5,*+8                                                           
*        B     *+10                                                             
*        PACK  DUB,0(0,RF)         FROM COLUMNS                                 
*                                                                               
*        LA    RE,1(RE)            BUMP PAST -                                  
*        BCTR  R7,0                                                             
*        LTR   R7,R7                                                            
*        BNP   CON703C             INVALID SKIP THIS ELEM                       
*        LR    RF,RE                                                            
*        SR    R5,R5                                                            
*ON703H  CLI   0(RE),C','          SCAN FOR ,                                   
*        BE    CON703J                                                          
*        CLI   0(RE),C'0'                                                       
*        BL    CON703C                                                          
*        CLI   0(RE),C'9'                                                       
*        BH    CON703C                                                          
*        LA    RE,1(RE)                                                         
*        LA    R5,1(R5)                                                         
*        BCT   R7,CON703H                                                       
*        B     CON703C             INVALID SKIP THIS ELEM                       
*                                                                               
*ON703J  LTR   R5,R5                                                            
*        BZ    CON703C             INVALID SKIP THIS ELEM                       
*        CH    R5,=H'2'                                                         
*        BH    CON703C             MAX 2 CHARS                                  
*        BCTR  R5,0                                                             
*        EX    R5,*+8                                                           
*        B     *+10                                                             
*        PACK  DOUBLE,0(0,RF)      TO COLUMNS                                   
*        CP    DOUBLE,=P'0'        IF EITHER TO OR FROM COL IS O                
*        BE    CON703C             SKIP THIS ELEM                               
*                                  TO AVOID DIVIDING BY ZERO                    
*        CP    DUB,=P'0'                                                        
*        BE    CON703C                                                          
*                                                                               
*        LA    RE,1(RE)             BUMP PAST ,                                 
*        BCTR  R7,0                DECREMENT R7                                 
*        LTR   R7,R7                                                            
*        BNP   CON703C             MUST HAVE DATE NEXT                          
*        LR    R5,RE                                                            
*        GOTO1 ADATVAL,DMCB,(0,0(R5)),WORK                                      
*        OC    DMCB(4),DMCB                                                     
*        BZ    CON703C             INVALID DATE SKIP THIS ELEM                  
*        GOTO1 DATCON,DMCB,(0,WORK),(3,SCONVDTE)                                
*        CLI   SCONVIND,C'-'                                                    
*        BE    CON703K                                                          
*        CVB   R0,DUB                                                           
*        CVB   R1,DOUBLE                                                        
*        CVD   R1,DUB                                                           
*        CVD   R0,DOUBLE           SWITCH DUB AND DOUBLE                        
*                                                                               
*ON703K  MP    DUB,=P'1000'             AFTER TO BEFORE                         
*        DP    DUB,DOUBLE+6(2)          I.E. 9-6 = 1500                         
*        CP    DUB+6(2),=P'50'  CHK REMAINDER                                   
*        BL    *+10                                                             
*        AP    DUB(6),=P'1'                                                     
*        ZAP   DUB,DUB(6)                                                       
*ON703L  CVB   R0,DUB                                                           
*        ST    R0,SCONVFAC         SAVE CONVERSION FACTOR                       
*              3 DECIMALS IMPLIED , EXAMPLE 9-6 IS 1.500                        
*                                           9*6 IS 667                          
*                                  CON ANAL - BUFFALO INS BY SPACE              
CON703N  GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'01',BUFFBUFF),(X'80',1)                
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'03',BUFFBUFF),(X'80',1)                
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'05',BUFFBUFF),(X'80',1)                
*                              LEAVE X'10' - REPORT TOTALS ALONE                
         B     CON74                                                            
*                                                                               
CON71    DS    0H                                                               
         LA    R2,PROTOTS                                                       
         LA    R7,ACCNUM*3                                                      
*                                                                               
CON72    ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R7,CON72                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   HDSW,2                                                           
         CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         MVI   HDSW,3                                                           
CON74    DS    0H                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVC   BQSTART(6),PCONSTRT                                              
         MVI   TESTPASS,0                                                       
         GOTO1 ACONSCHD       GOT PROCEES BUYS                                  
*                                                                               
CON80    MVC   QPROG(80),SVPROG        RESTORE REQUEST                          
         CLC   QPUB,SPACES                                                      
         BNE   *+10                                                             
         MVC   QPUB(3),=C'ALL'     SET TO ALL                                   
         MVC   ESTACT(4),=4C'N'                                                 
         MVI   PUBSW,0             SO PUB NAME WILL PRINT                       
         MVI   HDSW,1              RESET FOR NEXT CONTRACT                      
         CLI   QOPT1-1,C'R'        RATE CHG                                     
         BE    *+8                                                              
         MVI   HDSW,6                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     CON92               GO PROCESS NEXT CONTRACT                     
*                                                                               
CON92    DS    0H                                                               
******** CLC   QEST,SPACES         IF ONE CON SPECIFIED, DONE                   
******** BNE   CONCLT                                                           
* ELSE READ NEXT CON KEY                                                        
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         MVC   KEY,PCONKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLC   =C'ALL',QPUB                                                     
         BE    CON17                                                            
         B     CON13                                                            
*                                                                               
CON120   DS    0H                                                               
         CLI   QOPT7,C' '          IF REQUEST IS FROM SORT                      
         BNE   CON120C             LEAVE CONTROLLER IN CHARGE                   
         MVI   MODE,LBUYREQ        ELSE - NEXT REQ (SKIP ANY BUY RD)            
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),REALSE      MUST RETURN TO REAL FILE                     
*                                  FOR NEXT REQUEST                             
CON120C  CLI   QOPT1-1,C'R'                                                     
         BE    RUNL                RATE CHG - ADD RECS TO WORKER FILE           
         CLI   QOPT7,C' '          SEE IF SORTED REQ                            
         BNE   EXIT                YES - TOTALS AT IN CON1 AT LAST REQ          
         B     CON1                NO - DO TOTALS NOW                           
         EJECT                                                                  
*                                                                               
*                                  AFTER CONTRACT RATES AND COMMENTS            
*                                  MODE WILL BE SET BACK TO FBUYCLI             
*                                  SO BUYS WILL BE READ FOR NEW CON             
*                                                                               
         SPACE 2                                                                
RUNL     EQU   *                   LAST BUY FOR REQ - WRITE TO WORKER           
         CLI   RCWRITE,C'Y'          IF WRITE=NO OR TEST RUN = Y                
         BNE   EXIT                  CAN JUST SKIP THIS PART                    
         CLI   QOPT3,C'Y'                                                       
         BE    EXIT                                                             
*                                                                               
         B     RUNL5                                                            
*****    CODE TO UPDATE ASR ELEM NO-OPED                                        
***      BRAND AGENCY BUY CHANGES SHOULD NOT CAUSE                              
***      ASR ELEM UPDATE ON THE AOR                                             
*                                                                               
**       XC    KEY,KEY                                                          
**       MVC   KEY(15),PCONREC                                                  
**       GOTO1 HIGH                                                             
**       CLC   KEY(15),KEYSAVE                                                  
**       BNE   RUNL5                IF I DON'T FIND CONTRACT                    
**       LA    R0,PCONREC                                                       
**       ST    R0,IOAREA                                                        
**       BAS   RE,GET                                                           
**                                                                              
**       LA    R2,PCONREC+33                                                    
**       MVI   ELCODE1,X'85'       AUTO SPACE RESV ELEMS                        
**NL3    BAS   RE,NEXTEL                                                        
**       BNE   RUNL5                                                            
**       USING PASRELEM,R2                                                      
**       CLC   PASRCLT,SPACES               ONLY UPDATE ELEM WITH               
**       BH    RUNL3                        NO SLAVE CLIENT CODE                
**       GOTO1 DATCON,DMCB,(5,0),(3,THREE)                                      
**       CLC   PASRCDAT,THREE                                                   
**       BE    RUNL5                                                            
**       MVC   PASRLDAT,PASRCDAT                                                
**       MVC   PASRCDAT,THREE                                                   
**       BAS   RE,PUT                                                           
*****                                                                           
RUNL5    DS    0H                                                               
RL10     XC    WRKREC,WRKREC                                                    
         MVC   WRKLEN,=H'72'                                                    
         MVC   WRKID,WID                                                        
         MVC   WRKPFLE,FILENUM                                                  
         MVC   WRKAGY,REALAGY                                                   
         MVC   WRKMED,QMEDIA                                                    
         MVC   WRKCLT,REALCLT                                                   
         MVC   WRKPUB,REALPUB                                                   
         MVC   WRKCON,QEST         CONTRACT                                     
         MVC   WRKOGRS(32),REQTOTS                                              
         GOTO1 WORKER,DMCB,=C'ADD',MAPFILE,WID,WRKREC                           
         B     EXIT                                                             
*                                                                               
*                                  CLOSE WORKER FILE - AUTO RATE CHG            
CLOSEWK  DS    0H                                                               
         CLI   WKOPEN,0            WRKER FILE NOT OPEN                          
         BE    EXIT                                                             
         GOTO1 WORKER,DMCB,=C'CLOSE',MAPFILE,WID                                
         B     EXIT                                                             
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTELX             RETURN WITH CC =                             
         CLC   ELCODE1,0(R2)                                                    
         BCR   8,RE                                                             
         B     NEXTEL+2                                                         
NEXTELX  LTR   R2,R2               RETURN WITH CC NOT =                         
         BR    RE                                                               
         EJECT                                                                  
GET      NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
         B     DMX                                                              
*                                                                               
PUT      NTR1                                                                   
         GOTO1 DATAMGR,DMCB,PUTREC,PRTFILE,KEY+27,IOAREA,DMWORK                 
         B     DMX                                                              
*                                                                               
GETPUB   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
DMX      TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
*                                                                  L02          
CURRATE  DC    C'*** CURRENT LEVEL RATES ***'                      L02          
LOWRATE  DC    C'*** LOWER LEVEL RATES ***'                        L02          
HIGRATE  DC    C'*** HIGHER LEVEL RATES ***'                       L02          
E18R4SAV DS    F                                                   L02          
*                                                                  L02          
*                                                                  L02          
PRTFILES DS    0D                                                               
         DC    CL8' PRTDIR'                                                     
         DC    CL8' PRTFILE'                                                    
         DC    CL8' PUBDIR'                                                     
         DC    CL8' PUBFILE'                                                    
         DC    C'X'                                                             
*                                                                               
PATCH    DC    30X'00'                                                          
         EJECT                                                                  
COMPRT   CSECT                                                                  
         NMOD1 0,COMPRT                                                         
         USING PPWORKD,RA                                                       
         LA    R8,SPACEND                                                       
         USING PPARWRKD,R8                                                      
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         AH    R9,=H'4096'                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVI   BYTE,0                                                           
         OC    PCONREV,PCONREV                                                  
         BZ    COMP60B                                                          
         GOTO1 ACLPRT                 SKIP A LINE                               
         MVC   P+49(25),=C'SUPERSEDES CONTRACT DATED'                           
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         GOTO1 DATCON,DMCB,(3,PCONREV),(5,P+75)                                 
         GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                                                               
COMP60B  DS    0H                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         LA    R2,PCONREC+33       ATTN OVERRIDE                                
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   ELCODE1,X'50'                                                    
         BAS   RE,NEXTEL                                                        
         BNE   COMP60D                                                          
         USING PCATELD,R2                                                       
         OC    PCATNAM,SPACES                                                   
         CLC   PCATNAM,SPACES                                                   
         BE    COMP60C                                                          
         CLI   BYTE,1              ONLY SKIP IF I HAVEN'T ALREADY               
         BE    COMP60B5                                                         
         GOTO1 ACLPRT              SKIP A LINE                                  
*                                                                               
COMP60B5 MVC   P+49(5),=C'ATTN='                                                
         MVC   P+55(24),PCATNAM                                                 
         GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                                                               
COMP60C  CLC   PCATPCT,=2X'00'      SEE IF CD OVERRIDEN                         
         BE    COMP60C7                                                         
         CLI   BYTE,1                                                           
         BE    COMP60C2                                                         
         GOTO1 ACLPRT                                                           
COMP60C2 MVC   P+49(11),=C'CASH DISC.='                                         
         CLC   PCATPCT,=2X'FF'                                                  
         BNE   COMP60C5                                                         
         MVC   P+61(3),=C'0.0'                                                  
         B     COMP60C6                                                         
*                                                                               
COMP60C5 EDIT  PCATPCT,(4,P+61),1                                               
COMP60C6 GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                                                               
COMP60C7 OC    PCATMAX,PCATMAX                                                  
         BZ    COMP60C8                                                         
         MVC   P+49(23),=C'MAXIMUM BUYS PER ISSUE='                             
         EDIT  (B2,PCATMAX),(4,P+73),0,ALIGN=LEFT                               
         GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                                                               
COMP60C8 OC    PCATMAXZ,PCATMAXZ                                                
         BZ    COMP60D                                                          
         MVC   P+23(49),=C'MAXIMUM BUYS PER ISSUE ACROSS ZONES AND EDITX        
               IONS='                                                           
*                                                                               
         EDIT  (B2,PCATMAXZ),(4,P+73),0,ALIGN=LEFT                              
         GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                                                               
COMP60D  DS    0H                                                               
         CLI   QOPT2,C'Y'          SEE IF PRINTING FREE FORM COMMENTS           
         BNE   COMPX                                                            
         MVI   BYTE,0                                                           
         MVI   ELCODE1,X'40'                                                    
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         USING PCONREC,RF                                                       
         LA    R2,PCONREC+33                                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
COMP62   BAS   RE,NEXTEL                                                        
         BNE   COMPX                                                            
         CLI   BYTE,0                                                           
         BNE   COMP64                                                           
         GOTO1 ACLPRT              SKIP A LINE BEFORE 1ST COMMENT               
         MVI   BYTE,1                                                           
COMP64   BAS   RE,PRTCOM                                                        
         B     COMP62                                                           
*                                                                               
COMPX    XMOD1 1                                                                
         EJECT                                                                  
PRTCOM   NTR1                                                                   
         LA    R4,2(R2)                                                         
         SR    R5,R5                                                            
*                                                                               
         CLC   0(2,R4),=C'E-'      E- COMMENTS DONT PRINT ON CONTRACTS          
         BE    PRTCOMX                                                          
         CLC   0(3,R4),=C'RC='                                                  
         BE    PRTCOMX                                                          
*                                                                               
PRTCOM2  IC    R5,1(R2)                                                         
         SH    R5,=H'2'                                                         
         CLI   0(R4),C'+'                                                       
         BNE   PRTCOM3                                                          
         MVC   SPACING,1(R4)                                                    
         NI    SPACING,X'0F'                                                    
         CLI   SPACING,3                                                        
         BNH   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPACING,0                                                        
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
         LA    R4,2(R4)                                                         
         SH    R5,=H'2'                                                         
         GOTO1 ACLPRT                                                           
         B     PRTCOM6                                                          
*                                                                               
PRTCOM3  CLC   0(2,R4),=C'E+'                                                   
         BNE   PRTCOM6                                                          
         LA    R4,2(R4)            DONT PRINT E+                                
         SH    R5,=H'2'                                                         
*                                                                               
PRTCOM6  LTR   R5,R5                                                            
         BNP   PRTCOMX                                                          
         BCTR  R5,0                                                             
         EX    R5,MVCOM                                                         
         GOTO1 ACLPRT                                                           
PRTCOMX  XIT1                                                                   
*                                                                               
MVCOM    MVC   P+49(0),0(R4)                                                    
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
CONSCHD  CSECT                                                                  
         NMOD1 0,CONSCHD                                                        
         USING PPWORKD,RA                                                       
         LA    R8,SPACEND                                                       
         USING PPARWRKD,R8                                                      
         LA    R3,CONSCHD+4095                                                  
         LA    R3,1(R3)                                                         
         USING CONSCHD+4096,R3      ** NOTE USE OF SECOND BASE REG **           
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         AH    R9,=H'4096'                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
*       MUST SWITCH BACK TO "REAL" AGENCY                                       
*       AND USE "REAL" AGENCY'S PUB                                             
         L     RF,UTL                                                           
         MVC   4(1,RF),REALSE                                                   
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVC   SVAORPUB,PCONKPUB   SAVE AOR PUB                                 
*                                                                               
*                                                                               
         TM    SVAORC,X'01'          SEE IF PUB LINK                            
         BZ    CONS5                                                            
*          FIND PUB LINK                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'FD'                                                        
         MVC   KEY+1(1),QMEDIA                                                  
         MVC   KEY+2(3),QCLIENT         REALLY ADV                              
         MVC   KEY+5(2),QAGENCY         REALLY AOR                              
         MVC   KEY+7(2),REALAGY         AGENCY                                  
         MVC   KEY+9(6),PCONKPUB                                                
         GOTO1 HIGHPUB                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CONLX                 EXIT                                       
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         MVC   PCONKPUB(6),KEY+15    PUT "REAL" PUB IN CONTRACT                 
*                                                                               
         DROP  RF                                                               
*                                                                               
CONS5    DS    0H                    I MUST READ "REAL" CLIENT                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),REALAGY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),REALCLT     NOTE THAT AR WILL ALWAYS                    
*                                   BE FOR ONE CLIENT                           
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'            MUST FIND THE CLIENT                             
         GOTO1 GETCLI                                                           
*                                                                               
         ZAP   OAGYCNT,=P'0'       INITIALIZE OTHER AGY COUNTER                 
         XC    LASTOAGY,LASTOAGY                                                
         MVC   WORKCLT,PCLTKEY                                                  
         CLI   PCLTPROF+5,C'1'     TEST THIS A MASTER CLIENT                    
         BNE   CS4                                                              
* READ THRU CLTHDRS FOR SLAVE CLIENTS                                           
         XC    WORKCLT+4(4),WORKCLT+4        FOR FIRST TIME                     
*                                                                               
CSCLT2   XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT                                                   
         MVI   KEY+7,X'FF'                                                      
         GOTO1 HIGH                                                             
         B     CSCLT4A                                                          
CSCLT4   GOTO1 SEQ                                                              
CSCLT4A  CLC   KEY(4),KEYSAVE                                                   
         BE    CSCLT6                                                           
         CLI   QOPT1-1,C'A'       CONTRACT ANALYSIS                             
         BNE   CONL                                                             
         CLI   WORKCLT+4,X'FF'     SEE IF I'VE SEARCHED FOR                     
         BE    CONL                OTHER AGY DATA UNDER THE MASTER              
*                                  IF YES THEN I'M DONE                         
         XC    KEY,KEY             ELSE DO MASTER                               
         MVC   KEY(7),PCLTKEY                                                   
         MVI   WORKCLT+4,X'FF'                                                  
         B     CS4A                                                             
CSCLT6   LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
*                                                                               
CSCLT8   BAS   RE,SGET                                                          
*                                                                               
         CLC   PCLTKCLT,PCLTPROF+6-PCLTKEY+PBUYREC                              
         BNE   CSCLT4                                                           
*                                                                               
         MVC   WORKCLT,PBUYREC     SAVE THIS CLIENT KEY                         
*                                                                               
*                                                                               
CS4      XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT      A/M/X/CLT                                    
CS4A     DS    0H                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+7(6),PCONKPUB                                                
*                                  NOTE - IF DOING ALL ZONES,EDTS               
*                                  THEN PCONPRD IS FROM FIRST CON READ          
         CLI   PCONPRD,C'A'        SEE IF DOING A PRD-CONTRACT                  
         BL    *+10                                                             
         MVC   KEY+13(3),PCONPRD                                                
         JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',CS4B,JUMP=N           
         MVC   KEY+13(3),QPRODUCT                                               
*                                                                               
         DROP  RF                                                               
*                                                                               
CS4B     DS    0H                                                               
**NEW 2/17/88                                                                   
         OC    KEY+13(3),KEY+13      SEE IF DOING ONE PRD                       
         BZ    CS4C                                                             
         MVC   KEY+16(3),BQSTART     SET DATE IN KEY                            
**NEW 2/17/88                                                                   
CS4C     GOTO1 HIGH                                                             
         B     CS6A                                                             
CS6      MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
CS6A     DS    0H                                                               
         CLC   KEY(16),KEYSAVE     SAME A/M/X/C/PUB/PRD                         
         BNE   CSENDPRD                                                         
         CLI   QOPT1-1,C'A'        CONTRACT ANALYSIS                            
         BE    CS7                                                              
         CLI   KEY+13,C'*'         OTHER AGY BUYS ONLY FOR PP18                 
         BE    CS6                                                              
*                                                                               
CS7      CLC   KEY+16(3),BQSTART                                                
**NEW 2/17/88                                                                   
         BL    CS7LOW                                                           
**NEW 2/17/88                                                                   
         CLC   KEY+16(3),BQEND                                                  
**NEW 2/17/88                                                                   
         BH    CS7HIGH                                                          
**NEW 2/17/88                                                                   
*                                                                               
         B     CS7H                 SKIP                                        
**NEW 2/17/88                                                                   
**NEW 2/17/88       LOGIC TO CHK PRD OMITTED SINCE IF ONE PRODUCT               
**NEW 2/17/88       IT IS SET IN KEY IN CS4A                                    
**NEW 2/17/88       ALSO CHG OF PRODUCT IS CHECKED IN CS6A                      
**NEW 2/17/88                                                                   
*                                                                               
CS7LOW   MVC   KEY+16(3),BQSTART                                                
         XC    KEY+19(6),KEY+19       MUST CLEAR EST                            
         B     CS4C                                                             
*                                                                               
CS7HIGH  MVC   KEY+16(3),=3X'FF'     TO GET NEXT PRD                            
         XC    KEY+19(6),KEY+19       CLEAR EST                                 
         B     CS4C                                                             
**NEW 2/17/88                                                                   
*                                                                               
CS7H     DS    0H                                                               
**NEW 2/17/88                                                                   
**NEW 2/17/88          DON'T READ EST FOR TEST STATUS ANYMORE                   
**NEW 2/17/88                                                                   
*                                                                               
CS7M     LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,SGET                                                          
         CLI   QOPT1-1,C'A'        SEE IF CONTRACT ANALYSIS                     
         BNE   CS7M2                                                            
***TESTBUY***                                                                   
         CLI   QOPT6,C'Y'          SEE IF INCLUDING TEST BUYS ON PP18           
         BE    CS7M1                                                            
*                                                                               
         CLI   PBDBFD,C'T'         SKIP TEST BUYS FOR ANALYSIS                  
         BE    CS6                                                              
***TESTBUY***                      NEED TO SET FOR OTHER AGY DATA               
CS7M1    GOTO1 GETINS,DMCB,PBUYREC,GROSS,(C'Y',PBUYKPRD)                        
         B     PBUY                                                             
*                                                                               
CS7M2    GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
         EJECT                                                                  
PBUY     DS    0H                  PROCESS BUY                                  
         CLI   TESTPASS,1                                                       
         BNE   PBUY3                                                            
         MVI   TESTPASS,0           SET BUY FOUND                               
         B     CONLX                EXIT                                        
*                                                                               
PBUY3    MVC   ESTACT(4),=4C'Y'                                                 
         CLI   QOPT1-1,C'A'        CON ANAL                                     
         BNE   PBUY4                                                            
         GOTO1 APOST18                                                          
         B     CS6                                                              
*                                                                               
*                                  PBUY4 TO PBUY70 FOR AUTO RATE CHG            
*                                  CHK FOR RATE CHG ELEM FOR TODAY              
*                                  MEANS THIS SHOULD BE A RE-RUN                
*                                                                               
PBUY4    DS    0H                                                               
         MVI   RCHGSW,0                                                         
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'76'                                                    
PBUY4A   BAS   RE,SNEXTEL                                                       
         BNE   PBUY4D                                                           
         USING RCHGEL,R2                                                        
         XC    WORK(20),WORK                                                    
         MVC   WORK(2),RCDATE+6    YR                                           
         MVC   WORK+2(2),RCDATE    MTH                                          
         MVC   WORK+4(2),RCDATE+3  DAY                                          
*                                  SET CHANGE DATE IN PBUYREC                   
*        GOTO1 DTCNV,DMCB,WORK,(1,WORK+10)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         CLC   PBRCDAT,WORK+10                                                  
         BNE   PBUY4A                                                           
         MVI   RCHGSW,1                                                         
         MVC   GROSS,PBRCGRS       RESTORE OLD GROSS + COSTS                    
         MVC   PBDCOS,PBRCCOS                                                   
         MVC   PBDPRCOS,PBRCPRC                                                 
         DROP  R2                                                               
*                                                                               
PBUY4D   DS    0H                                                               
*                                                                               
         LA    R4,P+20                                                          
         USING BUYLND,R4                                                        
         CLI   PCLTPROF+5,C'1'     SEE IF MASTER CLT                            
         BNE   *+10                                                             
         MVC   BCLT,PBUYKCLT                                                    
         MVC   BPRD,PBUYKPRD                                                    
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BEST,DUB                                                         
         LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R0,PBUYREC                                                       
         L     R1,DATCON                                                        
         LA    R2,GROSS                                                         
         STM   R0,R2,0(R5)                                                      
         MVI   PBYOCTL,X'20'                                                    
         GOTO1 PPBYOUT,DMCB,PPBYOWRK                                            
         MVC   BDATE,PBYOINS                                                    
***TESTBUY***                                                                   
         CLI   PBDBFD,C'T'                                                      
         BNE   PBUY4E                                                           
         MVI   BDATE-1,C'T'                                                     
         TM    PBDSTAT2,X'40'      STEWARD BUY ?                                
         BNO   PBUY4E              NO                                           
         MVI   BDATE-1,C'S'                                                     
***TESTBUY***                                                                   
PBUY4E   DS    0H                                                               
*                                                                               
         CLI   PBYOINS2,C' '                                                    
         BE    PPBY00                                                           
         MVI   BDATE+132,C'+'                                                   
         MVC   BDATE+133(8),PBYOINS2                                            
         B     PPBYX                                                            
PPBY00   DS    0H                                                               
*                                                                               
         CLC   PBYOISNM,SPACES                                                  
         BE    PPBYX                                                            
         MVC   BDATE+133(11),PBYOISNM                                           
         B     PPBYX                                                            
PPBYX    DS    0H                                                               
         MVC   BORATE,PBYOUR                                                    
         CLI   QMEDIA,C'N'                                                      
         BNE   PBUY30                                                           
         CLC   PBYOPRM,SPACES                                                   
         BNH   PBUY4G                                                           
         LA    R7,BORATE                                                        
         CLC   BORATE,SPACES       SEE IF P USED                                
         BNH   *+8                                                              
         LA    R7,BORATE+132       YES - USE PSECOND                            
         MVC   0(11,R7),PBYOPRM                                                 
PBUY4G   DS    0H                                                               
         CLI   PBYOSPC,C' '                                                     
         BE    PBUY5                                                            
         MVC   BDESC(10),PBYOSPC                                                
*                                                                               
PBUY5    DS    0H                                                               
*                                                                               
PBUY10   MVC   BLINES,PBYOUNTS                                                  
         LA    R2,5                                                             
         LA    R7,BLINES                                                        
PBUY15   CLI   BLINES+6,C' '                                                    
         BNE   PBUY35                                                           
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R7),PBYOUNTS                                                 
         MVI   0(R7),C' '                                                       
         LA    R7,1(R7)                                                         
         BCT   R2,PBUY15                                                        
         B     PBUY35                                                           
*                                                                               
PBUY30   MVC   BDESC(20),PBYOSPC     NON-NEWSPAPERS                             
         MVC   BDESC+132(20),PBYOSPC2                                           
*                                                                               
PBUY35   EQU   *                                                                
         CLI   NETSW,C'Y'                                                       
         BNE   PBUY35C                                                          
         L     R1,GROSS                                                         
         S     R1,AGYCOM                                                        
         ST    R1,NETOLD                                                        
         EDIT  (B4,NETOLD),(14,BOGROSS),2,COMMAS=YES,FLOAT=-                    
         B     PBUY36                                                           
*                                                                               
PBUY35C  EDIT  (B4,GROSS),(14,BOGROSS),2,COMMAS=YES,FLOAT=-                     
******** PAID DISPLAY NO-OPED                                                   
********                                                                        
*        CLC   GROSS,PGROSS        SEE IF FULLY PAID                            
*        BNE   PBUY36              NO                                           
*        MVI   BOGROSS+L'BOGROSS,C'P'                                           
*                                  TRY TO LOOK UP NEW RATE                      
*                                  SAVE OLD RATE AND PREMIUM                    
PBUY36   MVC   SVGROSS(12),GROSS   SAVE OLD GROSS,AC,CD                         
*                                  FOR CHGELEM IN ACHGEL                        
*                                                                               
         MVC   SVTAX,PBDTAX                                      L08            
         MVC   SVCOST,PBDCOS                                                    
         MVC   SVPRCOST,PBDPRCOS                                                
         MVC   SVAC,PBDACP         SAVE AC                                      
         MVC   SVCD,PBDCD          SAVE CD                                      
*                                                                BUG01          
         MVC   SVCOSIN,PBDCOSIN    COST INDICATOR S,C,' '        BUG01          
         MVC   SVCTYP,PBDCTYP      COST TYPE     N               BUG01          
*                                                                BUG01          
         TM    PBDRLIND,X'08'      RATE FROZEN - SKIP LOOK-UP                   
         BNZ   PBUY37B                                                          
         CP    PBDCOS,=P'0'        FREE - SKIP LOOK-UP                          
         BE    PBUY37D                                                          
*                                                                L08            
         TM    PBDSTAT,X'40'       SEE IF MATCHED                               
         BNO   PBUY36J                                                          
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'25'                                                    
PBUY36H  BAS   RE,SNEXTEL                                                       
         BNE   PBUY37A             MATCHED - NOT PAID                           
*                                  NO RATE LOOK-UP                              
         OC    2(3,R2),2(R2)                                                    
         BZ    PBUY36H             IF PAID ALLOW RATE LOOK-UP                   
*                                                                               
PBUY36J  CLI   PBDCOSIN,C'C'       SEE IF 'C' RATE BUY           L08            
         BNE   PBUY36L                                           L08            
*                                                                L08            
         ZAP   PBDACP,=P'1'     SHOULD PREVENT AC AND CD LOOK-UP L08            
         ZAP   PBDCD,=P'1'                                       L08            
*                                                                               
*                                                                BUG01          
PBUY36L  MVI   PBDCOSIN,C' ' INITIALIZE TO GROSS                 BUG01          
         MVI   PBDCTYP,0     ELIMATE NET INDICATOR               BUG01          
*                                                                BUG01          
         ZAP   PBDCOS,=P'0'                                                     
         ZAP   PBDPRCOS,=P'0'                                                   
         PRINT GEN                                                              
*                                                                L10            
         XC    DMCB+12(4),DMCB+12                                L10            
         CLI   SVCOSIN,C'C'     SEE IF 'C' RATE BUY              L10            
         BNE   *+8                                               L10            
         MVI   DMCB+12,C'C'                                      L10            
*                              SO RATELOOK WON'T FIND C RATES    L10            
*                              FOR NON C RATE BUYS               L10            
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         USING PCONREC,RF                                                       
*                                                                               
         GOTO1 ARTLOOK,DMCB,(PAGYNAT,PBUYREC),PUBREC,(E18SW,PCONREC),, X        
               ADDAY,DATCON                                   L01/L06           
         PRINT NOGEN                                                            
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLI   0(R1),0                                                          
         BE    PBUY40              NO ERROR                                     
         CLI   0(R1),133           PREM NOT FOUND                               
         BNE   PBUY37                                                           
         MVC   PBDPRCOS,SVPRCOST     RESTORE PREM COST                          
         B     PBUY40              NO ERROR                                     
*                                                                               
PBUY37   MVC   BNRATE(9),=C'NOT FOUND'                                          
         B     PBUY38                                                           
PBUY37A  MVC   BNRATE(8),=C'MATCHED+'                                           
         MVC   BNRATE+133(6),=C'UNPAID'                                         
         B     PBUY38                                                           
PBUY37B  MVC   BNRATE(6),=C'FROZEN'                                             
         B     PBUY38                                                           
PBUY37C  MVC   BNRATE(8),=C'WSJ LIST'                                           
         B     PBUY38                                                           
*                                                                               
PBUY37D  MVC   BNRATE(4),=C'FREE'                                               
PBUY38   MVC   BNGROSS,BOGROSS                                                  
         MVC   BCHG+9(3),=C'.00'                                                
*                                                                BUG01          
         MVC   PBDCOSIN,SVCOSIN   RESTORE COST INDICATOR         BUG01          
         MVC   PBDCTYP,SVCTYP     RESTORE COST TYPE              BUG01          
*                                                                BUG01          
         MVC   PBDCOS,SVCOST                                                    
         MVC   PBDPRCOS,SVPRCOST     RRESTORE COSTS                             
         B     PBUY70              ROLL TO EST TOTALS                           
*                                                                               
PBUY40   EQU   *                                                                
         MVC   PBDTAX,SVTAX       ALWAYS RESTORE TAX             L08            
         CLI   SVCOSIN,C'C'       SEE IF WAS 'C' RATE BUY        L08            
         BNE   PBUY40L                                           L08            
*                                                                L08            
         MVC   PBDACP,SVAC      RESTORE AC AND CD                L08            
         MVC   PBDCD,SVCD                                        L08            
         CLI   PBDCOSIN,C'C'       'C' RATE LOOKED-UP            L10            
         BE    PBUY40H                                           L10            
         CLI   PBDCOSIN,C' '                                     L08            
         BNE   PBUY37              LOOK-UP RATE CAN'T BE S OR N  L08            
PBUY40H  MVI   PBDCOSIN,C'C'       RESET TO 'C' RATE             L08            
*                           SO IT WILL STILL BE A 'C' RATE  BUY  L08            
         CLI   PBDCL,0             CHK FOR PREMIUM               L08            
         BE    PBUY40L                                           L08            
         MVI   PBDPRIN,C'C'                                      L08            
*                                                                               
*                                                                               
PBUY40L  CP    PBDCOS,=P'1'        FREE RATE                                    
         BNE   *+14                                                L02          
         ZAP   PBDCOS,=P'0'        RESET TO 0                                   
         B     RTSUB4D                                             L02          
*          DATA SET PPBUY03    AT LEVEL 129 AS OF 04/08/88         L02          
*                                                                  L02          
         CLI   PBDCTYP,C'N'        TEST NET INPUT                  L02          
         BNE   RTSUB4D                                             L02          
*                                  GROSS UP COST                   L02          
         CP    PBDACP,=P'0'        UNLESS THERE IS NO AC           L02          
         BE    RTSUB4D             THEN LEAVE IT ALONE             L02          
*                                                                  L02          
         ZAP   DUB,PBDCOS          NEW NET COST                    L02          
         CVB   R1,DUB                                              L02          
         M     R0,=F'100000'                                       L02          
         ZAP   DUB,PBDACP          AGENCY COMMISSION               L02          
         CVB   RF,DUB                                              L02          
         S     RF,=F'100000'       =NET PCT                        L02          
         LCR   RF,RF                                               L02          
         BNP   RTSUB4D                                             L02          
         SLDA  R0,1                                                L02          
         DR    R0,RF                                               L02          
         LTR   R1,R1                                               L02          
         BNP   *+8                                                 L02          
         A     R1,=F'1'                                            L02          
         SRA   R1,1                                                L02          
         CVD   R1,DUB                                              L02          
         ZAP   PBDCOS,DUB                                          L02          
*                                                                  L02          
RTSUB4D  DS    0H                                                  L02          
         CP    PBDPRCOS,=P'1'      TEST FREE PREM                               
         BNE   *+10                                                             
         ZAP   PBDPRCOS,=P'0'      RESET TO ZERO                                
         CP    PBDCD,=P'1'         TEST CD OVERRIDE OF 0.0                      
         BNE   *+10                                                             
         ZAP   PBDCD,=P'0'         RESET TO ZERO                                
         CP    PBDACP,=P'1'                                                     
         BNE   *+10                                                             
         ZAP   PBDACP,=P'0'                                                     
         CP    PBDUNITS,=P'-1'                                                  
         BNE   *+16                                                             
         ZAP   PBDUNITS,=P'0'                                                   
         ZAP   PBDCLMS,=P'0'                                                    
         CLC   SVPRCOST,PBDPRCOS                                                
         BE    PBUY43                                                           
         CLI   PBDCL,0             CHK FOR COLORS                               
         BNE   PBUY50              YES                                          
         MVC   PBDPRCOS,SVPRCOST       RESTORE PREMIUM COST                     
*                                                                               
PBUY43   CLC   SVCOST,PBDCOS       SEE IF RATE CHANGED                          
         BNE   PBUY50                                                           
         MVC   BNRATE(9),=C'NO CHANGE'                                          
         MVC   BNGROSS,BOGROSS                                                  
         MVC   BCHG+9(3),=C'.00'                                                
         B     PBUY70                                                           
*                                                                               
PBUY50   GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
         GOTO1 PPBYOUT,DMCB,PPBYOWRK                                            
         MVC   BNRATE,PBYOUR                                                    
         LA    R7,BNRATE                                                        
         CLC   BNRATE,SPACES       SEE IF P USED                                
         BNH   *+8                                                              
         LA    R7,132(R7)          YES - USE PSECOND                            
         MVC   0(11,R7),PBYOPRM    NEW PREM CHARGE                              
PBUY55   CLI   NETSW,C'Y'                                                       
         BNE   PBUY55C                                                          
         L     R1,GROSS                                                         
         S     R1,AGYCOM                                                        
         ST    R1,NETNEW                                                        
         EDIT  (B4,NETNEW),(14,BNGROSS),2,COMMAS=YES,FLOAT=-                    
         B     PBUY56                                                           
*                                                                               
PBUY55C  EDIT  (B4,GROSS),(14,BNGROSS),2,COMMAS=YES,FLOAT=-                     
         L     R0,GROSS                                                         
         S     R0,SVGROSS                                                       
         B     PBUY56C                                                          
*                                                                               
PBUY56   L     R0,NETNEW                                                        
         S     R0,NETOLD                                                        
*                                                                               
PBUY56C  EDIT  (R0),(12,BCHG),2,COMMAS=YES,FLOAT=-                              
*                                                                               
PBUY60   EQU   *                                                                
         XC    WORK(20),WORK                                                    
         MVC   WORK(2),RCDATE+6    YR                                           
         MVC   WORK+2(2),RCDATE    MTH                                          
         MVC   WORK+4(2),RCDATE+3  DAY                                          
*                                  SET CHANGE DATE IN PBUYREC                   
*        GOTO1 DTCNV,DMCB,WORK,(1,WORK+10)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         CLC   PBDDATE,WORK+10     SEE IF SAME DATE                             
         BNE   PBUY62                                                           
         OI    PBDDTIND,X'40'      SET RATE CHANGE                              
         CP    SVCD,PBDCD           SEE IF CD LOOKED-UP                         
         BE    *+8                                                              
         OI    PBDDTIN2,X'02'       CD CHANGED                                  
         CP    SVAC,PBDACP          SEE IF AC LOOKED-UP                         
         BE    *+8                                                              
         OI    PBDDTIN2,X'04'       AC CHANGED                                  
         B     PBUY65                                                           
*                                                                               
PBUY62   MVI   PBDDTIND,X'40'      SET ONLY RATE CHANGE                         
         MVI   PBDDTIN2,0                                                       
         CP    SVCD,PBDCD           SEE IF CD LOOKED-UP                         
         BE    *+8                                                              
         OI    PBDDTIN2,X'02'       CD CHANGED                                  
         CP    SVAC,PBDACP          SEE IF AC LOOKED-UP                         
         BE    *+8                                                              
         OI    PBDDTIN2,X'04'       AC CHANGED                                  
         MVI   PBDDTIN3,0                                                       
         MVC   PBDDATE,WORK+10                                                  
*                                                                               
PBUY65   DS    0H                                                               
         MVC   PBDCHGDT,WORK+10        SET EST CHG CONTROL DATE                 
*                                                                               
*        UPDATE/ADD PCHGELEM                                                    
         GOTO1 ACHGEL                                                           
*                                                                               
*                                  TEST ESTIMATE PRINT CHANGE                   
PBUY67X  CLI   RCHGSW,1                                                         
         BE    PBUY68                                                           
         XC    WORK+30(30),WORK+30                                              
         MVC   WORK+30(2),=X'761E'     SET ELCODE + LENGHT                      
         MVC   WORK+32(3),WORK+10  SET DATE                                     
         MVC   WORK+35(4),SVGROSS                                               
         MVC   WORK+39(5),SVCOST                                                
         MVC   WORK+44(5),SVPRCOST                                              
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'99'                                                    
         BAS   RE,SNEXTEL          GETS ME TO END OF REC                        
         GOTO1 RECUP,DMCB,(1,PBUYREC),WORK+30,(R2)                              
*                                                                               
PBUY68   EQU   *                                                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   PBUY70              NOT MARKING FILE                             
         CLI   QOPT3,C'Y'          SEE IF TEST RUN                              
         BE    PBUY70              YES - DON'T MARK FILE                        
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,SPUT                                                          
*                                                                               
PBUY70   EQU   *                   FIND AND PRINT COMMENTS                      
         CLI   BDATE+132,C' '      SEE IF PSECOND USED                          
         BE    PBUY73                                                           
         GOTO1 ACLPRT                                                           
         LA    R7,BDATE                                                         
         B     PBUY75                                                           
PBUY73   LA    R7,BDATE+132        USE PSECOND FOR FIRST COMMENT                
PBUY75   LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'66'       BUY COMMENTS                                 
PBUY76   BAS   RE,SNEXTEL                                                       
         BNE   PBUY80                                                           
         ZIC   R5,1(R2)            ELEM LENGHT                                  
         SH    R5,=H'3'            ADJUST FOR CODE + LENGHT                     
         BM    PBUY76              AND EXEC                                     
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),2(R2)       EXECUTED                                     
         GOTO1 ACLPRT                                                           
         LA    R7,BDATE            RESET TO P                                   
         B     PBUY76                                                           
*                                                                               
PBUY80   CLC   P,SPACES            SEE IF ANYTHING LEFT TO PRINT                
         BE    PBUY85              NO                                           
         GOTO1 ACLPRT                                                           
***TESTBUY***                                                                   
PBUY85   CLI   PBDBFD,C'T'         SEE IF TEST BUY                              
         BE    CS6                 DON'T POST TO TOTALS FOR PP17                
***TESTBUY***                                                                   
*                                  FILE BALANCING                               
         CLI   NETSW,C'Y'                                                       
         BNE   PBUY85C                                                          
         L     R0,SVGROSS                                                       
         S     R0,SVGROSS+4                                                     
         ST    R0,SVNET                                                         
         CVD   R0,DUB                                                           
         AP    PROOGRS,DUB                                                      
         L     R0,GROSS                                                         
         S     R0,AGYCOM                                                        
         ST    R0,NET                                                           
         CVD   R0,DUB                                                           
         AP    PRONGRS,DUB                                                      
         L     R0,NET                                                           
         S     R0,SVNET                                                         
         CVD   R0,DUB                                                           
         AP    PROCGRS,DUB                                                      
         B     PBUY85F                                                          
*                                                                               
PBUY85C  L     R0,SVGROSS                                                       
         CVD   R0,DUB                                                           
         AP    PROOGRS,DUB                                                      
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         AP    PRONGRS,DUB                                                      
         L     R0,GROSS                                                         
         S     R0,SVGROSS                                                       
         CVD   R0,DUB                                                           
         AP    PROCGRS,DUB                                                      
*                                                                               
PBUY85F  CLI   PBDSPACE,C'*'       SEE IF REAL INS                              
         BE    *+10                                                             
         AP    PROINS,=P'1'                                                     
*                                                                               
         B     CS6           GO DO NEXT BUY                                     
         EJECT                                                                  
CSENDPRD DS    0H                                                               
         CLI   PRDACT,C'Y'                                                      
         BNE   CSENDP5                                                          
         CLI   QOPT1-1,C'R'        CHK FOR AUTO RATE CHANGE                     
         BNE   CSENDP5                                                          
         MVC   P+30(20),=C'** PRODUCT TOTALS **'                                
         LA    R5,PROTOTS                                                       
         BAS   RE,PRTTOTS                                                       
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT                                                           
         LA    R4,CLTTOTS                                                       
         BAS   RE,ADDCLR                                                        
CSENDP5  MVC   ESTACT(2),=2C'N'                                                 
**NEW 2/17/88                                                                   
         L     RF,ACONIO1          (A)PCONREC                                   
         USING PCONREC,RF                                                       
*                                                                               
         CLI   PCONPRD,C'A'        SEE IF DOING A PRD CONTRACT                  
         BNL   CSENDCLT            YES - DONE                                   
*                                                                               
         DROP  RF                                                               
*                                                                               
         JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',CSENDP8,     +        
               JUMP=N                                                           
         B     CSENDCLT                                                         
**NEW 2/17/88                                                                   
CSENDP8  CLC   KEY(13),KEYSAVE     TEST SAME A/M/X/CLT/PUB                      
         BE    CS7                 YES - PROCESS THIS KEY                       
*                                                                               
CSENDCLT DS    0H                  CLIENT END                                   
         CLI   CLTACT,C'Y'                                                      
         BNE   CSENDC5                                                          
         CLI   QOPT1-1,C'R'                                                     
         BNE   CSENDC5             AUTO RATE CHANGE                             
         CLI   PCLTPROF+5,C'1'     SEE IF DOING MASTER CLIENT                   
         BNE   CSENDC3             NO - NO CLT TOTALS                           
         MVC   P+30(19),=C'** CLIENT TOTALS **'                                 
         LA    R5,CLTTOTS                                                       
         BAS   RE,PRTTOTS                                                       
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT                                                           
CSENDC3  LA    R4,CONTOTS          ROLL TO CONTRACT TOTALS                      
         LA    R5,CLTTOTS                                                       
         BAS   RE,ADDCLR                                                        
CSENDC5  MVC   ESTACT(3),=3C'N'                                                 
*                                                                               
CSENDCX  DS    0H                                                               
         CLI   PCLTPROF+5,C'1'         TEST MASTER CLIENT                       
         BNE   CONL                                                             
         B     CSCLT2              READ NEXT CLIENT                             
*                                                                               
         SPACE 2                                                                
CONL     DS    0H                  END OF CONTRACT                              
         CLI   TESTPASS,1                                                       
         BE    CONLX               DO NOTHING FOR TEST PASS                     
*                                                                               
         CLI   QOPT1-1,C'R'        CHK FOR AUTO RATE CHANGE                     
         BNE   CONL20              NO                                           
         CLI   CONACT,C'Y'                                                      
         BNE   CONLX                                                            
         MVC   P+30(21),=C'** CONTRACT TOTALS **'                               
         LA    R5,CONTOTS                                                       
         BAS   RE,PRTTOT2                                                       
         GOTO1 ACLPRT                                                           
         LA    R4,REQTOTS                                                       
         BAS   RE,ADDCLR                                                        
         B     CONLX                                                            
*                                                                               
CONL20   EQU   *                                                                
         MVC   P+60(23),=C'** INSERTION SUMMARY **'                             
         CLI   QMEDIA,C'O'         OUTDOOR                                      
         BNE   *+10                                                             
         MVC   P+60(23),=C' ** POSTING SUMMARY ** '                             
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT                                                           
         MVC   MID1+49(60),=C'SPACE              INSERTIONS           GX        
               ROSS            NET'                                             
         MVC   MID2+49(60),=C'-----              ----------           -X        
               ----            ---'                                             
         CLI   QMEDIA,C'N'                                                      
         BNE   CONL22                                                           
         CLI   SCONVDTE,X'FF'      SEE IF I HAVE COL CONV DATE                  
         BNE   CONL21                                                           
         MVC   MID1+29(13),=C'SPACE    RATE'                                    
         MVC   MID2+29(13),=C'-----    ----'                                    
         MVC   MID1+49(9),=C'   INCHES'                                         
         MVC   MID2+49(9),=C'   ------'                                         
         MVC   MID1+61(5),=C'LINES'                                             
         MVC   MID2+61(5),DASHES1                                               
         B     CONL25                                                           
*                                                                               
CONL21   DS    0H                                                               
         MVC   MID1+49(45),SPACES                                               
         MVC   MID2+49(45),SPACES                                               
         MVC   MID1+11(13),=C'SPACE    RATE'                                    
         MVC   MID2+11(13),=C'-----    ----'                                    
         MVC   MID2+34(6),=C'INCHES'                                            
         MVC   MID2+43(5),=C'LINES'                                             
         MVC   MID2+50(7),=C'INSRTNS'                                           
         MVC   MID2+62(6),=C'INCHES'                                            
         MVC   MID2+71(5),=C'LINES'                                             
         MVC   MID2+78(7),=C'INSRTNS'                                           
         MVC   MID2+90(6),=C'INCHES'                                            
         MVC   MID2+99(5),=C'LINES'                                             
         MVC   MID1+34(23),=C'-------- O L D --------'                          
         MVC   MID1+62(23),=C'-------- N E W --------'                          
         MVC   MID1+89(16),=C'-OLD EQUIVALENT-'                                 
         CLI   SCONVIND,C'-'                                                    
         BE    *+10                                                             
         MVC   MID1+90(3),=C'NEW'                                               
         MVC   MID1+106(10),=C'INSERTIONS'                                      
         MVC   MID2+106(10),DASHES1                                             
         MVC   MID1+122(5),=C'GROSS'                                            
         MVC   MID2+122(5),DASHES1                                              
         B     CONL25                                                           
CONL22   CLI   QMEDIA,C'O'         OUTDOOR                                      
         BNE   CONL25                                                           
         MVC   MID1+68(10),=CL10'POSTINGS'                                      
         MVC   MID2+68(10),=CL10'--------'                                      
*                                                                               
CONL25   MVI   FORCEMID,C'Y'                                                    
         MVC   SAVMID1,MID1                                                     
         MVC   SAVMID2,MID2                                                     
*NO-OPED                                                                        
         TM    SPACESW,X'01'       SEE IF SPACES FOUND                          
         BNZ   CONL26              YES                                          
         B     CONL40              SKIP TO CONTRACT TOTALS                      
*                                                                               
CONL26   XC    BUFREC,BUFREC                                                    
         CP    OAGYCNT,=P'0'       SEE IF OTHER AGY DATA FOUND                  
         BE    CONL60              NO SKIP TO CONTRACT TOTALS                   
         MVI   BUFTYP,X'01'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     CONL32                                                           
*                                                                               
CONL30   GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFTYP,BUFFBUFF),BUFREC,0                  
CONL32   CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    CONL40                                                           
         CLI   BUFTYP,X'01'                                                     
         BNE   CONL40                                                           
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL33                                                           
         GOTO1 ACLPRT              SKIP A LINE                                  
         MVI   SPACING,2           AND SPACE AFTER                              
         MVC   P+49(6),=C'TOTAL*'                                               
         B     CONL34                                                           
*                                                                               
CONL33   CLI   QMEDIA,C'N'                                                      
         BNE   CONL33C                                                          
         CLI   SCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CONL33A                                                          
         MVC   P+11(20),BUFDESC                                                 
         B     CONL34                                                           
CONL33A  MVC   P+29(20),BUFDESC                                                 
         B     CONL34                                                           
*                                                                               
CONL33C  LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R0,BUFDESC                                                       
         ST    R0,0(R5)                                                         
         MVI   PBYOCTL,X'80'       SPACE DESC ONLY                              
         GOTO1 PPBYOUT,DMCB,PPBYOWRK                                            
         MVC   P+49(17),PBYOSPC                                                 
         MVC   PSECOND+49(17),PBYOSPC2                                          
*                                                                               
         DROP  R5                                                               
*                                                                               
CONL34   CLI   QMEDIA,C'N'                                                      
         BNE   CONL35                                                           
         CLI   SCONVDTE,X'FF'     SEE IF DOING COL CONV                         
         BE    CONL34W             NO                                           
         CP    BUFINCH,=P'0'                                                    
         BE    CONL34C                                                          
         EDIT  (P8,BUFINCH),(10,P+30),2,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL34C                                                          
         MVI   P+40,C'*'                                                        
CONL34C  CP    BUFLINES,=P'0'                                                   
         BE    CONL34D                                                          
         EDIT  (P8,BUFLINES),(7,P+41),0,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL34D                                                          
         MVI   P+48,C'*'                                                        
*                                                                               
CONL34D  CP    BUFINS,=P'0'                                                     
         BE    CONL34F                                                          
         EDIT  (P8,BUFINS),(7,P+49),0,COMMAS=YES                                
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL34F                                                          
         MVI   P+56,C'*'                                                        
CONL34F  DS    0H                                                               
         CP    BUFNINC,=P'0'                                                    
         BE    CONL34G                                                          
         EDIT  (P8,BUFNINC),(10,P+58),2,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL34G                                                          
         MVI   P+68,C'*'                                                        
CONL34G  CP    BUFNLNS,=P'0'                                                    
         BE    CONL34H                                                          
         EDIT  (P8,BUFNLNS),(7,P+69),0,COMMAS=YES                               
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL34H                                                          
         MVI   P+76,C'*'                                                        
*                                                                               
CONL34H  CP    BUFNINS,=P'0'                                                    
         BE    CONL34H3                                                         
         EDIT  (P8,BUFNINS),(7,P+77),0,COMMAS=YES                               
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL34H3                                                         
         MVI   P+84,C'*'                                                        
*                                                                               
CONL34H3 BAS   RE,BUFCONV                                                       
CONL34H8 CP    BUFINCH,=P'0'                                                    
         BE    CONL34I                                                          
         EDIT  (P8,BUFINCH),(10,P+86),2,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL34I                                                          
         MVI   P+96,C'*'                                                        
CONL34I  CP    BUFLINES,=P'0'                                                   
         BE    CONL34J                                                          
         EDIT  (P8,BUFLINES),(7,P+97),0,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL34J                                                          
         MVI   P+104,C'*'                                                       
*                                                                               
CONL34J  EDIT  (P8,BUFINS),(7,P+105),0,COMMAS=YES                               
         EDIT  (P8,BUFGRS),(14,P+113),2,COMMAS=YES,FLOAT=-                      
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL37                                                           
         MVI   P+112,C'*'                                                       
         MVI   P+127,C'*'                                                       
         B     CONL37                                                           
*                                                                               
*        NO COL CONV FORMAT                                                     
*                                                                               
CONL34W  CP    BUFINCH,=P'0'                                                    
         BE    CONL34X                                                          
         EDIT  (P8,BUFINCH),(10,P+48),2,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL34X                                                          
         MVI   P+58,C'*'                                                        
CONL34X  CP    BUFLINES,=P'0'                                                   
         BE    CONL35                                                           
         EDIT  (P8,BUFLINES),(7,P+59),0,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL35                                                           
         MVI   P+66,C'*'                                                        
*                                                                               
*                                                                               
*                                                                               
CONL35   EDIT  (P8,BUFINS),(7,P+68),0,COMMAS=YES                                
         EDIT  (P8,BUFGRS),(14,P+80),2,COMMAS=YES,FLOAT=-                       
         MVC   BUFNET,BUFGRS                                                    
         SP    BUFNET,BUFACOM                                                   
         EDIT  (P8,BUFNET),(14,P+95),2,COMMAS=YES,FLOAT=-                       
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL37                                                           
         MVI   P+75,C'*'                                                        
         MVI   P+94,C'*'                                                        
         MVI   P+109,C'*'                                                       
CONL37   GOTO1 ACLPRT                                                           
         B     CONL30                                                           
*                                                                               
CONL40   XC    BUFREC,BUFREC                                                    
         XC    LASTOAGY,LASTOAGY                                                
         MVI   BUFTYP,X'03'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     CONL42                                                           
*                                                                               
CONL41   GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFTYP,BUFFBUFF),BUFREC,0                  
*                                                                               
CONL42   CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    CONL60                                                           
         CLI   BUFTYP,X'03'                                                     
         BNE   CONL60                                                           
         CLC   BUFOAGY,LASTOAGY                                                 
         BE    CONL42D                                                          
         MVC   LASTOAGY,BUFOAGY                                                 
         CLI   BUFOAGY,X'FF'       SEE IF COMBINED OTHER AGYS                   
         BNE   CONL42B                                                          
         CP    OAGYCNT,=P'1'                                                    
         BNH   CONL60              ONLY ONE OTHER AGY                           
*                                  SKIP OTHER AGYS COMBINED                     
         MVC   P+8(21),=C'OTHER AGENCY SUBTOTAL'                                
         B     CONL42C                                                          
CONL42B  MVC   P+8(12),=C'OTHER AGENCY'                                         
         MVC   P+21(2),BUFOAGY+1   OMIT *                                       
         MVC   P+25(20),BUFCOM     OAGY NAME                                    
*                                                                               
CONL42C  GOTO1 ACLPRT                                                           
*                                                                               
CONL42D  CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL43                                                           
*                                  TOTAL WILL PRINT LATER                       
         GOTO1 ACLPRT              SKIP A LINE                                  
         MVI   SPACING,2           AND SPACE AFTER                              
         LA    R4,P+49                                                          
         CLI   QMEDIA,C'N'                                                      
         BNE   CONL42F                                                          
         LA    R4,P+39                                                          
         CLI   SCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CONL42F             NO                                           
         LA    R4,P+21                                                          
CONL42F  MVC   0(6,R4),=C'TOTAL*'                                               
         B     CONL44                                                           
*                                                                               
CONL43   CLI   QMEDIA,C'N'                                                      
         BNE   CONL43B                                                          
         CLI   SCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CONL43A             NO                                           
         MVC   P+11(20),BUFDESC                                                 
         B     CONL44                                                           
CONL43A  MVC   P+29(20),BUFDESC                                                 
         B     CONL44                                                           
*                                                                               
CONL43B  LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R0,BUFDESC                                                       
         ST    R0,0(R5)                                                         
         MVI   PBYOCTL,X'80'       SPACE DESC ONLY                              
         GOTO1 PPBYOUT,DMCB,PPBYOWRK                                            
         MVC   P+49(17),PBYOSPC                                                 
         MVC   PSECOND+49(17),PBYOSPC2                                          
*                                                                               
         DROP  R5                                                               
*                                                                               
CONL44   CLI   QMEDIA,C'N'                                                      
         BNE   CONL45                                                           
         CLI   SCONVDTE,X'FF'                                                   
         BE    CONL44W                                                          
         CP    BUFINCH,=P'0'                                                    
         BE    CONL44C                                                          
         EDIT  (P8,BUFINCH),(10,P+30),2,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL44C                                                          
         MVI   P+40,C'*'                                                        
CONL44C  CP    BUFLINES,=P'0'                                                   
         BE    CONL44D                                                          
         EDIT  (P8,BUFLINES),(7,P+41),0,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL44D                                                          
         MVI   P+48,C'*'                                                        
*                                                                               
CONL44D  CP    BUFINS,=P'0'                                                     
         BE    CONL44F                                                          
         EDIT  (P8,BUFINS),(7,P+49),0,COMMAS=YES                                
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL44F                                                          
         MVI   P+56,C'*'                                                        
CONL44F  DS    0H                                                               
         CP    BUFNINC,=P'0'                                                    
         BE    CONL44G                                                          
         EDIT  (P8,BUFNINC),(10,P+58),2,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL44G                                                          
         MVI   P+68,C'*'                                                        
CONL44G  CP    BUFNLNS,=P'0'                                                    
         BE    CONL44H                                                          
         EDIT  (P8,BUFNLNS),(7,P+69),0,COMMAS=YES                               
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL44H                                                          
         MVI   P+76,C'*'                                                        
*                                                                               
CONL44H  CP    BUFNINS,=P'0'                                                    
         BE    CONL44H3                                                         
         EDIT  (P8,BUFNINS),(7,P+77),0,COMMAS=YES                               
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL44H3                                                         
         MVI   P+84,C'*'                                                        
*                                                                               
CONL44H3 BAS   RE,BUFCONV                                                       
CONL44H8 CP    BUFINCH,=P'0'                                                    
         BE    CONL44I                                                          
         EDIT  (P8,BUFINCH),(10,P+86),2,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL44I                                                          
         MVI   P+96,C'*'                                                        
CONL44I  CP    BUFLINES,=P'0'                                                   
         BE    CONL44J                                                          
         EDIT  (P8,BUFLINES),(7,P+97),0,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL44J                                                          
         MVI   P+104,C'*'                                                       
*                                                                               
CONL44J  EDIT  (P8,BUFINS),(7,P+105),0,COMMAS=YES                               
         EDIT  (P8,BUFGRS),(14,P+113),2,COMMAS=YES,FLOAT=-                      
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BNE   CONL47                                                           
         MVI   P+112,C'*'                                                       
         MVI   P+127,C'*'                                                       
         B     CONL47                                                           
*                                                                               
*              NOT DOING COL CONV                                               
*                                                                               
CONL44W  CP    BUFINCH,=P'0'                                                    
         BE    CONL44X                                                          
         EDIT  (P8,BUFINCH),(10,P+48),2,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  SEE IF TOTAL LINE                            
         BNE   CONL44X                                                          
         MVI   P+58,C'*'                                                        
*                                                                               
CONL44X  CP    BUFLINES,=P'0'                                                   
         BE    CONL45                                                           
         EDIT  (P8,BUFLINES),(7,P+59),0,COMMAS=YES                              
         CLC   BUFDESC(3),=3X'FF'  SEE IF TOTAL LINE                            
         BNE   CONL45                                                           
         MVI   P+66,C'*'                                                        
*                                                                               
CONL45   EDIT  (P8,BUFINS),(7,P+68),0,COMMAS=YES                                
         EDIT  (P8,BUFGRS),(14,P+80),2,COMMAS=YES,FLOAT=-                       
         MVC   BUFNET,BUFGRS                                                    
         SP    BUFNET,BUFACOM                                                   
         EDIT  (P8,BUFNET),(14,P+95),2,COMMAS=YES,FLOAT=-                       
         CLC   BUFDESC(3),=3X'FF'  SEE IF TOTAL LINE                            
         BNE   CONL47                                                           
         MVI   P+75,C'*'                                                        
         MVI   P+94,C'*'                                                        
         MVI   P+109,C'*'                                                       
CONL47   GOTO1 ACLPRT                                                           
         B     CONL41                                                           
*                                                                               
CONL60   DS    0H                                                               
         MVI   TOTSW,0                                                          
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'05'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     CONL62                                                           
*                                                                               
CONL61   GOTO1 BUFFALO,DMCB,=C'SEQ',(BUFTYP,BUFFBUFF),BUFREC,0                  
*                                                                               
CONL62   CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    CONL62B                                                          
         CLI   BUFTYP,X'05'                                                     
         BE    CONL62D                                                          
CONL62B  MVC   P+60(17),=C'** NO ACTIVITY **'                                   
         GOTO1 ACLPRT                                                           
         B     CONLX                                                            
*                                                                               
CONL62D  DS    0H                                                               
         CLC   BUFDESC(3),=3X'FF'  CHK FOR TOTAL LINE                           
         BE    CONL68                                                           
*                                                                               
         TM    SPACESW,X'01'       SEE IF ANY SPACES FOUND                      
         BZ    CONL61              IGNORE NON-TOTAL BUFRECS                     
         CP    OAGYCNT,=P'0'       SEE IF OHTER AGY DATA                        
         BE    CONL62F             NO                                           
         CLI   TOTSW,1             SEE IF 'CONTRACT TOTAL' PRINTED              
         BE    CONL62F                                                          
         LA    R4,P+8                                                           
CONL62E  MVC   0(14,R4),=C'CONTRACT TOTAL'                                      
         GOTO1 ACLPRT                                                           
         MVI   TOTSW,1                                                          
*                                                                               
CONL62F  CLI   QMEDIA,C'N'                                                      
         BNE   CONL62H                                                          
         CLI   SCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CONL62F5                                                         
         MVC   P+11(20),BUFDESC                                                 
         B     CONL64                                                           
CONL62F5 MVC   P+29(20),BUFDESC                                                 
         B     CONL64                                                           
*                                                                               
CONL62H  LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R0,BUFDESC                                                       
         ST    R0,0(R5)                                                         
         MVI   PBYOCTL,X'80'       SPACE DESC ONLY                              
         GOTO1 PPBYOUT,DMCB,PPBYOWRK                                            
         MVC   P+49(17),PBYOSPC                                                 
         MVC   PSECOND+49(17),PBYOSPC2                                          
*                                                                               
         DROP  R5                                                               
*                                                                               
CONL64   CLI   QMEDIA,C'N'                                                      
         BNE   CONL65                                                           
         CLI   SCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CONL64W             NO                                           
         CP    BUFINCH,=P'0'                                                    
         BE    CONL64C                                                          
         EDIT  (P8,BUFINCH),(10,P+30),2,COMMAS=YES                              
CONL64C  CP    BUFLINES,=P'0'                                                   
         BE    CONL64C5                                                         
         EDIT  (P8,BUFLINES),(7,P+41),0,COMMAS=YES                              
CONL64C5 CP    BUFINS,=P'0'                                                     
         BE    CONL64D                                                          
         EDIT  (P8,BUFINS),(7,P+49),0,COMMAS=YES                                
CONL64D  CP    BUFNINC,=P'0'                                                    
         BE    CONL64G                                                          
         EDIT  (P8,BUFNINC),(10,P+58),2,COMMAS=YES                              
CONL64G  CP    BUFNLNS,=P'0'                                                    
         BE    CONL64G5                                                         
         EDIT  (P8,BUFNLNS),(7,P+69),0,COMMAS=YES                               
CONL64G5 CP    BUFNINS,=P'0'                                                    
         BE    CONL64H                                                          
         EDIT  (P8,BUFNINS),(7,P+77),0,COMMAS=YES                               
*                                                                               
CONL64H  BAS   RE,BUFCONV                                                       
CONL64H8 CP    BUFINCH,=P'0'                                                    
         BE    CONL64I                                                          
         EDIT  (P8,BUFINCH),(10,P+86),2,COMMAS=YES                              
CONL64I  CP    BUFLINES,=P'0'                                                   
         BE    CONL64J                                                          
         EDIT  (P8,BUFLINES),(7,P+97),0,COMMAS=YES                              
*                                                                               
CONL64J  EDIT  (P8,BUFINS),(7,P+105),0,COMMAS=YES                               
         EDIT  (P8,BUFGRS),(14,P+113),2,COMMAS=YES,FLOAT=-                      
         B     CONL67                                                           
*                                                                               
*              NOT DOING COL CONV                                               
*                                                                               
CONL64W  CP    BUFINCH,=P'0'                                                    
         BE    CONL64X                                                          
         EDIT  (P8,BUFINCH),(10,P+48),2,COMMAS=YES                              
CONL64X  CP    BUFLINES,=P'0'                                                   
         BE    CONL65                                                           
         EDIT  (P8,BUFLINES),(7,P+59),0,COMMAS=YES                              
*                                                                               
CONL65   EDIT  (P8,BUFINS),(7,P+68),0,COMMAS=YES                                
         EDIT  (P8,BUFGRS),(14,P+80),2,COMMAS=YES,FLOAT=-                       
         MVC   BUFNET,BUFGRS                                                    
         SP    BUFNET,BUFACOM                                                   
         EDIT  (P8,BUFNET),(14,P+95),2,COMMAS=YES,FLOAT=-                       
CONL67   GOTO1 ACLPRT                                                           
         B     CONL61                                                           
*                                                                               
CONL68   TM    SPACESW,X'01'                                                    
         BZ    CONL68B                                                          
         GOTO1 ACLPRT                                                           
CONL68B  MVC   P+36(21),=C'** CONTRACT TOTALS **'                               
         CLI   QMEDIA,C'N'                                                      
         BNE   CONL69                                                           
         MVC   P+36(21),SPACES                                                  
         LA    R4,P+24                                                          
         CLI   SCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CONL68B5                                                         
         LA    R4,P+6                                                           
CONL68B5 MVC   0(21,R4),=C'** CONTRACT TOTALS **'                               
         CLI   SCONVDTE,X'FF'      SEE IF DOING COL CONV                        
         BE    CONL68W             NO                                           
         EDIT  (P8,BUFINCH),(10,P+29),2,COMMAS=YES                              
         MVI   P+39,C'*'                                                        
CONL68C  EDIT  (P8,BUFLINES),(8,P+40),0,COMMAS=YES                              
         MVI   P+48,C'*'                                                        
         EDIT  (P8,BUFINS),(7,P+49),0,COMMAS=YES                                
         MVI   P+56,C'*'                                                        
         CP    BUFNINC,=P'0'                                                    
         BE    CONL68G                                                          
         EDIT  (P8,BUFNINC),(10,P+58),2,COMMAS=YES                              
         MVI   P+68,C'*'                                                        
CONL68G  CP    BUFNLNS,=P'0'                                                    
         BE    CONL68G5                                                         
         EDIT  (P8,BUFNLNS),(7,P+69),0,COMMAS=YES                               
         MVI   P+76,C'*'                                                        
CONL68G5 CP    BUFNINS,=P'0'                                                    
         BE    CONL68H                                                          
         EDIT  (P8,BUFNINS),(7,P+77),0,COMMAS=YES                               
         MVI   P+84,C'*'                                                        
*                                                                               
CONL68H  BAS   RE,BUFCONV                                                       
CONL68H8 CP    BUFINCH,=P'0'                                                    
         BE    CONL68I                                                          
         EDIT  (P8,BUFINCH),(10,P+86),2,COMMAS=YES                              
         MVI   P+96,C'*'                                                        
CONL68I  CP    BUFLINES,=P'0'                                                   
         BE    CONL68J                                                          
         EDIT  (P8,BUFLINES),(7,P+97),0,COMMAS=YES                              
         MVI   P+104,C'*'                                                       
*                                                                               
CONL68J  EDIT  (P8,BUFINS),(7,P+105),0,COMMAS=YES                               
         EDIT  (P8,BUFGRS),(14,P+113),2,COMMAS=YES,FLOAT=-                      
         MVI   P+112,C'*'                                                       
         MVI   P+127,C'*'                                                       
         B     CONL70                                                           
*                                                                               
*              NOT DOING COL CONV                                               
*                                                                               
CONL68W  EDIT  (P8,BUFINCH),(10,P+47),2,COMMAS=YES                              
         MVI   P+57,C'*'                                                        
         EDIT  (P8,BUFLINES),(8,P+58),0,COMMAS=YES                              
         MVI   P+66,C'*'                                                        
CONL69   EDIT  (P8,BUFINS),(7,P+68),0,COMMAS=YES                                
         EDIT  (P8,BUFGRS),(14,P+80),2,COMMAS=YES,FLOAT=-                       
         MVC   BUFNET,BUFGRS                                                    
         SP    BUFNET,BUFACOM                                                   
         EDIT  (P8,BUFNET),(14,P+95),2,COMMAS=YES,FLOAT=-                       
         MVI   P+75,C'*'                                                        
         MVI   P+94,C'*'                                                        
         MVI   P+109,C'*'                                                       
CONL70   GOTO1 ACLPRT                                                           
         CLI   TCONVDTE,0        FIRST TIME                                     
         BNE   CONL71                                                           
         MVC   TCONVDTE,SCONVDTE                                                
         MVC   TCONVFAC,SCONVFAC                                                
         MVC   TCONVIND,SCONVIND                                                
         B     CONLX                                                            
*                                                                               
CONL71   CLC   TCONVDTE,SCONVDTE                                                
         BE    CONL73                                                           
         B     CONL75                                                           
CONL73   CLC   TCONVFAC,SCONVFAC                                                
         BE    CONL74                                                           
         B     CONL75                                                           
CONL74   CLC   TCONVIND,SCONVIND                                                
         BE    CONLX                                                            
CONL75   MVI   TCONVDTE,X'FF'                                                   
CONLX    DS    0H                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),SVAORSE       MUST RETURN TO AOR                         
*                                                                               
         L     RF,ACONIO1          (A)PCONREC                                   
         USING PCONREC,RF                                                       
         MVC   PCONKPUB(6),SVAORPUB                                             
*                                                                               
         DROP  RF                                                               
*                                                                               
*       HERE I MUST REREAD THE AOR CLIENT                                       
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY   REALLY AOR                                      
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),QCLIENT    REALLY ADV                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'            MUST FIND THE CLIENT                             
         GOTO1 GETCLI                                                           
         XMOD1 1                                                                
         EJECT                                                                  
*        COL. CONVERT BUFFALO ACCUMS                                            
BUFCONV  AP    BUFINS,BUFNINS      ADD NEW TO OLD INS                           
         L     R0,SCONVFAC         COL CONV FACTOR                              
         CVD   R0,DUB                                                           
         CLI   SCONVIND,C'-'       SEE IF DOING AFTER TO BEFORE                 
         BE    BUFC5                                                            
         MP    BUFINCH,DUB+5(3)                                                 
         DP    BUFINCH,=P'1000'                                                 
         CP    BUFINCH+5(3),=P'500'                                             
         BL    *+10                                                             
         AP    BUFINCH(5),=P'1'    ROUND                                        
         AP    BUFNINC,BUFINCH(5)                                               
         ZAP   BUFINCH,BUFNINC     PUT RESULT IN BUFINCH                        
         MP    BUFLINES,DUB+5(3)                                                
         DP    BUFLINES,=P'1000'                                                
         CP    BUFLINES+5(3),=P'500'                                            
         BL    *+10                                                             
         AP    BUFLINES(5),=P'1'    ROUND                                       
         AP    BUFNLNS,BUFLINES(5)                                              
         ZAP   BUFLINES,BUFNLNS    PUT RESULT IN BUFLINES                       
         B     BUFC8                                                            
*                                                                               
BUFC5    MP    BUFNINC,DUB+5(3)    AFTER TO BEFORE                              
         DP    BUFNINC,=P'1000'                                                 
         CP    BUFNINC+5(3),=P'500'                                             
         BL    *+10                                                             
         AP    BUFNINC(5),=P'1'    ROUND                                        
         AP    BUFINCH,BUFNINC(5)                                               
         MP    BUFNLNS,DUB+5(3)                                                 
         DP    BUFNLNS,=P'1000'                                                 
         CP    BUFNLNS+5(3),=P'500'                                             
         BL    *+10                                                             
         AP    BUFNLNS(5),=P'1'    ROUND                                        
         AP    BUFLINES,BUFNLNS(5)                                              
BUFC8    BR    RE                  RETURN                                       
         EJECT                                                                  
ADDCLR   EQU   *                                                                
         LA    R6,ACCNUM                                                        
ADDCL5   AP    0(8,R4),0(8,R5)                                                  
         LA    R4,8(R4)                                                         
         ZAP   0(8,R5),=P'0'                                                    
         LA    R5,8(R5)                                                         
         BCT   R6,ADDCL5                                                        
         BR    RE                                                               
         SPACE 2                                                                
PRTTOT2  MVI   P+91,C'*'                                                        
         MVI   P+117,C'*'                                                       
         MVI   P+131,C'*'                                                       
PRTTOTS  DS    0H                                                               
         MVI   P+90,C'*'                                                        
         MVI   P+116,C'*'                                                       
         MVI   P+130,C'*'                                                       
         EDIT  (P8,0(R5)),(12,P+78),2,COMMAS=YES,FLOAT=-                        
         EDIT  (P8,8(R5)),(12,P+104),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,16(R5)),(12,P+118),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,24(R5)),(9,P+51),0,COMMAS=YES                                
         MVC   P+61(10),=C'INSERTIONS'                                          
         CLI   QMEDIA,C'O'         OUTDOOR                                      
         BNE   *+10                                                             
         MVC   P+61(10),=C'  POSTINGS'                                          
         CP    24(8,R5),=P'1'                                                   
         BH    *+8                                                              
         MVI   P+70,C' '           NO S                                         
         BR    RE                                                               
         SPACE 2                                                                
SNEXTEL  SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SNEXTELX            RETURN WITH CC =                             
         CLC   ELCODE1,0(R2)                                                    
         BCR   8,RE                                                             
         B     SNEXTEL+2                                                        
SNEXTELX LTR   R2,R2               RETURN WITH CC NOT =                         
         BR    RE                                                               
*                                                                               
SGET     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
         B     SDMX                                                             
*                                                                               
SPUT     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,PUTREC,PRTFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
SDMX     TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CHGEL    CSECT                                                                  
         NMOD1 0,CHGEL                                                          
         USING PPWORKD,RA                                                       
         LA    R8,SPACEND                                                       
         USING PPARWRKD,R8                                                      
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         AH    R9,=H'4096'                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
*****                                                                           
         B     CHG20               X'24' CHGELEMS FOR EVERYONE NOW              
*****                                                                           
         CLC   PBUYREC(2),=C'SJ'       TESTING WITH SJR                         
         BE    CHG20                                                            
         CLC   PBUYREC(2),=C'BS'       OR BACKER                                
         BNE   CHGELX                                                           
         CLC   PBUYREC+4(2),=C'PM'     AND PHILIP MORRIS                        
         BNE   CHGELX                                                           
*       TRY AND FIND ACTIVITY ELEM FOR TODAY                                    
CHG20    DS    0H                                                               
*        GOTO1 DTCNV,DMCB,(1,WORK+10),(2,DUB)                                   
         GOTO1 DATCON,DMCB,(3,WORK+10),(2,DUB)                                  
         XC    DUMEL(25),DUMEL                                                  
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE1,X'24'    CHANGE ELEMENT                                  
         XC    FULL,FULL                                                        
CHG22    BAS   RE,GNEXTEL                                                       
         BNE   CHG24                                                            
         B     CHG22                                                            
*                                                                               
*                                                                               
CHG24    LA    R7,DUMEL                                                         
         USING PCHGELED,R7                                                      
*                                                                               
         MVI   PCHGELEM,X'24'                                                   
         MVI   PCHGLEN,PCHGNEWL      NEW LENGTH 23                              
         MVC   PCHGDAT,DUB           TODAY'S DATE - PACKED                      
         OC    PCHGIND1,PBDDTIND  OR ON CHG BITS                                
         OC    PCHGIND2,PBDDTIN2  OR ON CHG BITS                                
         OC    PCHGIND3,PBDDTIN3  OR ON CHG BITS                                
******   CLC   SVGROSS(12),GROSS                                                
******   BE    CHG30        WILL ALWAYS HAVE CHG IN GROSS,AC,CD                 
******   CLI   PCHGLEN,20                                                       
******   BE    CHG30                                                            
******   MVI   PCHGLEN,20                                                       
         MVC   PCHGGRS(12),SVGROSS                                              
*                                                                               
         DROP  R7                                                               
*                                                                               
CHG30    DS    0H                                                               
*                                                                               
*     R2 SHOULD STILL BE IN PBUYREC                                             
         GOTO1 RECUP,DMCB,(1,PBUYREC),DUMEL,(R2)                                
*                                                                               
CHGELX   XMOD1 1                                                                
         SPACE 2                                                                
GNEXTEL  SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    GNEXTELX            RETURN WITH CC =                             
         CLC   ELCODE1,0(R2)                                                    
         BCR   8,RE                                                             
         B     GNEXTEL+2                                                        
GNEXTELX LTR   R2,R2              RETURN WITH CC NOT =                          
         BR    RE                                                               
         LTORG                                                                  
*                                                                               
DUMEL    DS    CL30                                                             
*                                                                               
         EJECT                                                                  
*                              POSTING ROUTINE FOR PP18                         
*                              CONTRACT ANALYSIS                                
*                                                                               
POST18   CSECT                                                                  
         NMOD1 0,POST18                                                         
         USING PPWORKD,RA                                                       
         LA    R8,SPACEND                                                       
         USING PPARWRKD,R8                                                      
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         AH    R9,=H'4096'                                                      
         USING PPFILED,RC,R9                                                    
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'01'                                                     
         CLI   PBUYKPRD,C'*'       SEE IF OTHER AGY DATA                        
         BNE   PB180A                                                           
         MVC   SVBKEY,KEY          SAVE BUY KEY                                 
*                                  READ PRODUCT FOR OTHER AGY NAME              
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+7(3),KEY+13     PRODUCT CODE                                 
         XC    KEY+10(23),KEY+10                                                
         CLC   PPRDREC(10),KEY     ALREADY THERE                                
         BE    PB180                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,PPRDREC                                                       
         ST    R0,IOAREA                                                        
         GOTO1 SGET                                                             
PB180    MVC   BUFCOM,PPRDNAME                                                  
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         MVC   KEY,SVBKEY          RESTORE BUYREC KEY                           
         GOTO1 HIGH                TO RESTORE SEQ READ                          
         MVI   BUFTYP,X'03'                                                     
         MVC   BUFOAGY,PBUYKPRD                                                 
         CLC   BUFOAGY,LASTOAGY                                                 
         BE    PB180A                                                           
         AP    OAGYCNT,=P'1'       BUMP OTHER AGY COUNTER                       
         MVC   LASTOAGY,BUFOAGY                                                 
PB180A   MVC   BUFDESC(17),PBDSPACE                                             
         ZAP   BUFLINES,=P'0'                                                   
         ZAP   BUFINCH,=P'0'                                                    
         ZAP   BUFNLNS,=P'0'                                                    
         ZAP   BUFNINC,=P'0'                                                    
         ZAP   BUFNINS,=P'0'                                                    
         CLI   QMEDIA,C'N'         NEWS                                         
         BNE   PB181                                                            
*                                  FOR NEWS SHOW SPACE AND RATE                 
         CLI   PBDCOSTY,C'U'       IF UNIT COST INPUT                           
         BNE   PB180X                                                           
         LA    R4,BUFDESC+9                                                     
PB180A5  EDIT  PBDCOS,(9,0(R4)),5,DROP=3,ALIGN=LEFT                             
         LA    R4,3(R4)                                                         
PB180B   CLI   0(R4),C' '                                                       
         BNH   PB180D                                                           
         LA    R4,1(R4)                                                         
         B     PB180B                                                           
*                                                                               
PB180D   MVI   0(R4),C'/'                                                       
         MVC   1(1,R4),PBDUIND                                                  
*                                                                               
PB180X   EQU   *                                                                
         ZAP   BUFLINES,PBDUNITS                                                
         CLI   PBDUIND,C'I'                                                     
         BE    PB180X5                                                          
         CLI   PBDUIND,X'89'       LOWER CASE I                                 
         BNE   PB180X8             HAS UNITS TO 2 DECIMALS                      
PB180X5  MP    BUFLINES,=P'14'     LINES= 14 X INCHES                           
         CLI   PBDUIND,X'89'       LOWER CASE I                                 
         BNE   PB180X8             HAS UNITS TO 2 DECIMALS                      
         ZAP   DUB,BUFLINES                                                     
         AP    DUB,=P'50'          MUST ROUND TO NEAREST LINE                   
         DP    DUB,=P'100'                                                      
         ZAP   BUFLINES,DUB(6)                                                  
*                                                                               
PB180X8  ZAP   BUFINCH,PBDUNITS                                                 
         CLI   PBDUIND,X'89'  LOWER CASE I - INCHES ALREADY 2 DEC               
         BE    PB181                                                            
         MP    BUFINCH,=P'100'                                                  
         CLI   PBDUIND,C'I'        REGULAR INCHES                               
         BE    PB181                                                            
         ZAP   DUB,BUFINCH         MUST BE LINES                                
         DP    DUB,=P'14'                                                       
         ZAP   BUFINCH,DUB(6)                                                   
*                                                                               
PB181    EQU   *                                                                
PB181A   ZAP   BUFINS,=P'0'                                                     
         CLI   PBDSPACE,C'*'                                                    
         BE    PB181R                                                           
         ZAP   BUFINS,=P'1'                                                     
         CLI   BUFTYP,X'03'        SEE IF DOING AN OTHER AGY BUY                
         BNE   PB181R              NO                                           
         LA    R2,PBUYREC+33       FIND NUMBER OF INS IN COMMENT                
         MVI   ELCODE1,X'66'       BUY COMMENTS                                 
PB181B   BAS   RE,SNEXTEL                                                       
         BNE   PB181R              NOT FOUND                                    
         LA    R4,13               11+2                                         
         CLC   2(11,R2),=C'INSERTIONS='                                         
         BE    PB181F                                                           
         LA    R4,6                4+2                                          
         CLC   2(4,R2),=C'INS='    ALSO ACCEPT INS=                             
         BE    PB181F                                                           
         LA    R4,10               8+2                                          
         CLC   2(8,R2),=C'INSERTS='   OR INSERTS=                               
         BE    PB181F                                                           
         LA    R4,11               9+2                                          
         CLC   2(9,R2),=C'POSTINGS='    OR POSTINGS=                            
         BE    PB181F                                                           
         B     PB181B                                                           
*                                                                               
PB181F   ZIC   R1,1(R2)            CALC NUMBER OF DIGITS                        
         SR    R1,R4                                                            
         BNP   PB181B              MUST HAVE AT LEAST ONE                       
         AR    R2,R4               POINT TO FIRST DIGIT                         
         SR    R5,R5               USED TO COUNT DIGITS                         
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
PB181G   CLI   0(R2),C'0'                                                       
         BL    PB181H              STOP ON NON-DIGIT                            
         CLI   0(R2),C'9'                                                       
         BH    PB181H                                                           
         LA    R5,1(R5)                                                         
         MVC   0(1,R6),0(R2)                                                    
         LA    R6,1(R6)                                                         
         LA    R2,1(R2)                                                         
         BCT   R1,PB181G                                                        
*                                                                               
PB181H   LTR   R5,R5               SEE IF I HAVE ANY DIGITS                     
         BZ    PB181B              NO TRY FOR ANOTHER ELEM                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         ZAP   BUFINS,DUB                                                       
PB181R   CLC   PBUYKDAT,SCONVDTE   COMPARE INS DATE TO COL CONV DATE            
         BL    PB181R5                                                          
         ZAP   BUFNLNS,BUFLINES    MOVE TO POST CONV ACCUMS                     
         ZAP   BUFNINC,BUFINCH                                                  
         ZAP   BUFNINS,BUFINS                                                   
         ZAP   BUFLINES,=P'0'                                                   
         ZAP   BUFINCH,=P'0'                                                    
         ZAP   BUFINS,=P'0'                                                     
*                                                                               
PB181R5  LA    R4,GROSS                                                         
         LA    R5,BUFGRS                                                        
         LA    R6,3                                                             
*                                                                               
PB182    L     R0,0(R4)                                                         
         CVD   R0,DUB                                                           
         ZAP   0(8,R5),DUB                                                      
         LA    R4,4(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R6,PB182                                                         
         OC    BUFDESC,BUFDESC                                                  
         BZ    *+8                                                              
         OI    SPACESW,X'01'       SET SPACE DESC FOUND                         
         CLI   BUFDESC,X'FF'       SPECIAL OUTDOOR SRI                          
         BE    *+10                                                             
         OC    BUFDESC,SPACES                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         MVC   WORK(20),BUFDESC    SAVE SPACE DESC                              
         XC    BUFDESC,BUFDESC                                                  
         MVC   BUFDESC(3),=3X'FF'  FOR TOTAL LINE                               
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFDESC,WORK        RESTORE SPACE                                
         CLI   BUFTYP,X'03'        SEE IF OTHER AGY DATA                        
         BNE   PB183               NO SKIP TO CONTRACT TOTALS                   
         MVC   BUFOAGY,=3X'FF'       FOR ALL OTHER AGY                          
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   WORK(20),BUFDESC    SAVE SPACE DESC                              
         XC    BUFDESC,BUFDESC                                                  
         MVC   BUFDESC(3),=3X'FF'  FOR TOTAL LINE                               
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFDESC,WORK        RESTORE SPACE                                
*                                                                               
PB183    MVI   BUFTYP,X'05'                                                     
         XC    BUFOAGY,BUFOAGY                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         XC    BUFDESC,BUFDESC                                                  
         MVC   BUFDESC(3),=3X'FF'  FOR TOTAL LINE                               
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         MVI   BUFTYP,X'10'              FOR REPORT TOTALS                      
         MVC   BUFDESC,WORK        RESTORE SPACE                                
         XC    BUFOAGY,BUFOAGY           FOR PUB=XXX,ALL REQS                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         XC    BUFDESC,BUFDESC                                                  
         MVC   BUFDESC(3),=3X'FF'  FOR TOTAL LINE                               
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
CLPRT    CSECT                                                                  
         NMOD1 0,CLPRT                                                          
         USING PPWORKD,RA                                                       
         LA    R8,SPACEND                                                       
         USING PPARWRKD,R8                                                      
         L     RC,PPFILEC                                                       
         LR    R9,RC                                                            
         AH    R9,=H'4096'                                                      
         USING PPFILED,RC,R9                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,LNEED                                                         
         MVI   LNEED,0                                                          
         AR    RE,RF                                                            
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    CLPRT4                                                           
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   MODE,LBUYREQ           SEE IF DOING REPORT TOTALS                
         BNE   CLPRT4                                                           
         CLI   FORCEMID,C'Y'                                                    
         BE    CLPRT4                                                           
         MVC   HEAD8,SAVMID1                                                    
         MVC   HEAD9,SAVMID2                                                    
*                                                                               
CLPRT4   DS    0H                                                               
         MVC   RCSUBPRG,HDSW                                                    
         CLI   HDSW,2                                                           
         BL    CLPRT8                                                           
         CLI   HDSW,3                                                           
         BH    CLPRT8                                                           
         CLI   NETSW,C'Y'                                                       
         BNE   CLPRT4C                                                          
         CLI   HDSW,2                                                           
         BH    *+12                                                             
         MVI   RCSUBPRG,21                                                      
         B     CLPRT4C                                                          
         MVI   RCSUBPRG,31                                                      
CLPRT4C  CLI   MODE,LBUYREQ                                                     
         BNE   CLPRT5                                                           
         MVC   HEAD7(13),=C'REPORT TOTALS'                                      
         B     CLPRT6                                                           
*                                                                               
CLPRT5   MVC   HEAD5(L'SVPNAME),SVPNAME                                         
         MVC   HEAD6(L'SVPZNAME),SVPZNAME                                       
         MVC   HEAD7(8),=C'CONTRACT'                                            
         MVC   HEAD7+9(3),SVPCON                                                
         CLI   PCLTPROF+5,C'1'     SEE IF MASTER CLT                            
         BNE   CLPRT6                                                           
         MVC   HEAD8+20(3),=C'CLT'                                              
         MVC   HEAD9+20(3),=3C'-'                                               
*                                                                               
CLPRT6   DS    0H                                                               
*                                                                   L01         
         CLI   E18SW,0                                              L01         
         BE    CLPRT6A                                              L01         
         MVC   HEAD6+53(30),=C'** USING HIGHER LEVEL RATES **'      L01         
         CLI   E18SW,C'H'                                           L01         
         BE    CLPRT6A                                              L01         
         MVC   HEAD6+62(6),=C'LOWER '                               L01         
*                                                                   L01         
CLPRT6A  CLI   RCWRITE,C'N'        SEE IF NOT WRITING TO FILE                   
         BE    CLPRT7                                                           
         CLI   QOPT3,C'Y'          SEE IF TEST RUN                              
         BE    CLPRT7                                                           
         B     CLPRTX                                                           
CLPRT7   MVC   HEAD4+25(22),=C'** FILE NOT CHANGED **'                          
         B     CLPRTX                                                           
*                                                                               
CLPRT8   DS    0H                                                               
         CLI   HDSW,7              CONTRACT ANALYSIS CONTINUATION               
         BNE   CLPRT8B                                                          
         MVC   HEAD8,SAVMID1                                                    
         MVC   HEAD9,SAVMID2                                                    
CLPRT8B  CLI   QOPT6,C'Y'             FOR PP18 ONLY                             
         BNE   CLPRT9                                                           
         MVI   HEAD4+115,C'T'                                                   
         MVC   HEAD6+97(34),=C'*INCLUDES ANY PROPOSED INSERTIONS*'              
         CLI   QMEDIA,C'O'                                                      
         BNE   CLPRT9                                                           
         MVC   HEAD6+120(11),=C'POSTINGS  '                                     
*                                                                               
CLPRT9   CLC   QCNTLDT(3),SPACES                                                
         BE    CLPRTX              NO CONTROL DATE                              
         LA    R4,HEAD4+68                                                      
         MVC   HEAD4+54(13),=C'CHANGED SINCE'                                   
*                                  SEE IF DOING CHANGES ONLY                    
         CLI   QOPT4,C'Y'                                                       
         BE    CLPRT10                                                          
         LA    R4,HEAD4+72                                                      
         MVC   HEAD4+52(19),=C'CHANGE CONTROL DATE'                             
*CLPRT10  GOTO1 DTCNV,DMCB,(1,QCNTLDT),(3,0(R4))                                
CLPRT10  GOTO1 DATCON,DMCB,(3,QCNTLDT),(5,0(R4))                                
*                                                                               
*                                                                               
CLPRTX   EQU   *                                                                
         GOTO1 REPORT                                                           
*                                                                               
         XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
PPARWRKD DSECT                                                                  
TODAY1   DS    CL6                                                              
IOAREA   DS    A                                                                
APRDTABT DS    A                                                                
ARTLOOK  DS    A                                                                
ADATVAL  DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ACLPRT   DS    A                                                                
ACOMPRT  DS    A                                                                
ACONSCHD DS    A                                                                
ACHGEL   DS    A                                                                
APOST18  DS    A                                                                
ARTLKELS DS    A                                                                
*                                                                               
WID      DS    CL16                                                             
MAPFILE  DS    A                                                                
*                                                                               
ACONIO1  DS    A                   (A)PCONREC                                   
*                                                                               
LASTCKEY DS    CL8                                                              
WORKCLT  DS    CL8                                                              
WORKCNM  DS    CL20                                                             
SVBKEY   DS    CL32                                                             
ESAVKEY  DS    CL32                                                             
ELCODE1  DS    C                                                                
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
ESTACT   DS    CL1                                                              
PRDACT   DS    CL1                                                              
CLTACT   DS    CL1                                                              
CONACT   DS    CL1                                                              
*                                                                               
PROFAR   DS    CL16                CLT PROFILE                                  
PROFKEY  DS    CL20                                                             
*                                                                               
NETSW    DS    CL1                 NET RATE SWITCH                              
HDSW     DS    CL1                                                              
TESTPASS DS    CL1                 USED BY QOPT5 TO CHK FOR                     
*                                  A BUY FOR THIS CONTRACT                      
SCONVDTE DS    CL3                 COL CONV DATE                                
SCONVFAC DS    F                   COL. CONV FACTOR                             
SCONVIND DS    CL1                 C'-' MEANS AFTER TO BEFORE                   
*                                  C'*' MEANS BEFORE TO AFTER                   
TCONVDTE DS    CL3                 COL CONV DATE                                
TCONVFAC DS    F                   COL. CONV FACTOR                             
TCONVIND DS    CL1                 C'-' MEANS AFTER TO BEFORE                   
*                                  C'*' MEANS BEFORE TO AFTER                   
SPACESW  DS    CL1                 SET TO X'01' IF SPACE DESC FOUND             
TOTSW    DS    CL1                                                              
OAGYCNT  DS    PL2                 COUNT OF OTHER AGYS                          
LASTOAGY DS    CL3                                                              
*                                  FOUND - PRD STARTS WITH *                    
WKOPEN   DS    CL1                                                              
*                                                                               
REALOFF  DS    CL1                SAVED FROM QPROG AT FBUYREQ                   
REALAGY  DS    CL2                SAVED FROM QPROG AT FBUYREQ                   
REALCLT  DS    CL3                                                              
REALPUB  DS    CL11                                                             
REALSE   DS    XL1                                                              
*                                                                               
SVAORPUB DS    XL6                                                              
*                                                                               
SADVDATA DS    0CL18                                                            
SVAOR    DS    CL2                                                              
SVADV    DS    CL3                                                              
SVADVC   DS    CL3                                                              
SVADVST  DS    XL3                                                              
SVADVED  DS    XL3                                                              
SVAORSE  DS    XL1                                                              
SVAORC   DS    XL3                CONTROL BYTES                                 
*                                                                               
DASHES1  DS    CL25                                                             
*                                                                               
*                                                                               
W        DS    CL132               WORK SPACE TO BUILD PRINT LINE               
*                                                                               
SAVEP    DS    CL132                                                            
SAVEP2   DS    CL132                                                            
SAVESPAC DS    X                                                                
SAVMID1  DS    CL132                                                            
SAVMID2  DS    CL132                                                            
*                                                                               
*                                                                               
SVPROG   DS    CL80                                                             
SVOPT7   DS    CL1                                                              
SVPNAME  DS    CL100               PUB NAME AND NUMBER                          
SVPZNAME DS    CL40                PUB ZONE NAME                                
SVPCON   DS    CL3                 CON NUMBER FOR INS CHG PAGES                 
SVRCON   DS    CL3                 SAVED REQUESTED CONTRACT NUMBER              
*                                                                               
TOTCNT   DS    PL4                 CONTRACT COUNTS FOR P14 + P18                
LOCKCNT  DS    PL4                                                              
CHACNT   DS    PL4                                                              
TOTPCNT  DS    PL4                                                              
*                                                                               
LNEED    DS    X                                                                
PUBSW    DS    X                                                                
RCHGSW   DS    X                                                                
*                                                                               
NETOLD   DS    F                                                                
NETNEW   DS    F                                                                
NET      DS    F                                                                
SVNET    DS    F                                                                
SVGROSS  DS    3F                GROSS,AC,CD                                    
SVCOST   DS    PL5                                                              
SVPRCOST DS    PL5                                                              
SVCD     DS    PL2                                                              
SVAC     DS    PL3                                                              
SVCTYP   DS    CL1                                                BUG01         
SVCOSIN  DS    CL1                                                BUG01         
SVTAX    DS    XL3                                                L08           
*                                                                               
ACCNUM   EQU   4                                                                
*                                                                               
PROTOTS  DS    0D                                                               
PROOGRS  DS    D                   OLD GROSS                                    
PRONGRS  DS    D                   NEW GROSS                                    
PROCGRS  DS    D                   CHANGE IN GROSS                              
PROINS   DS    D                   INSERTIONS                                   
*                                                                               
*                                                                               
CLTTOTS  DS    0D                                                               
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
CONTOTS  DS    0D                                                               
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
REQTOTS  DS    0D                                                               
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
*                                                                               
         DS    0D                                                               
BUFREC   DS    0CL120                                                           
BUFKEY   DS    0CL28                                                            
BUFTYP   DS    CL1                                                              
*                                  X'01' = NON-OAGY DETAILS                     
*                                          BUFOAGY = X'00'                      
*                                  X'03' = OTHER AGY                            
*                                          ALL OTHER AGYS                       
*                                          BUFOAGY = X'FF'                      
*                                                                               
*                                  X'05' = CONTRACT TOTALS                      
*                                                                               
*                                  X'10' = REPORT TOTALS                        
*                                          USED FOR PUB=XXX,ALL REQS            
*                                  FOR ABOVE BREAKS BUFDESC =                   
*                                  X'FFFFFF' FOR TOTAL LINE                     
*                                                                               
*                                  IF OTHER AGY DATA FOUND                      
*                                  CONTRACT TOTALS WILL HAVE                    
*                                  SPACE BREAKOUT                               
*                                                                               
BUFOAGY  DS    CL3                 USED FOR OTHER AGY CODE                      
*                                  I.E. PRDS STARTING WITH *                    
BUFDESC  DS    CL20                                                             
*              FOR NEWS SPACE(8) THEN RATE IN BUFDESC+9                         
         DS    CL4                 SPARE                                        
BUFCOM   DS    CL20                USED FOR OTHER AGY NAME                      
*                                                                               
BUFLINES DS    D                   LINES - NEWS                                 
BUFINCH  DS    D                   INCHES- NEWS 2 DECIMALS                      
BUFINS   DS    D                   INSERTIONS                                   
BUFGRS   DS    D                   GROSS                                        
BUFACOM  DS    D                   AGY COM                                      
BUFCD    DS    D                   CASH DISC                                    
BUFNLNS  DS    D                   POST COL.CONV LINES - NEWS                   
BUFNINC  DS    D                   POST COL.CONV INCHES NEWS 2 DEC              
BUFNINS  DS    D                   POST COL CONV. INSERTIONS                    
BUFNET   DS    D                   NET                                          
*                                                                               
         DS    0H                                                               
WRKREC   DS    0CL72                                                            
WRKLEN   DS    H                                                                
         DS    H                                                                
WRKPFLE  DS    CL1                                                              
WRKID    DS    CL8                                                              
WRKAGY   DS    CL2                                                              
WRKMED   DS    CL1                                                              
WRKCLT   DS    CL3                                                              
WRKPUB   DS    CL11                                                             
WRKCON   DS    CL3                                                              
         DS    CL7                 SPARE                                        
*                                                                               
WRKOGRS  DS    D                                                                
WRKNGRS  DS    D                                                                
WRKCGRS  DS    D                                                                
WRKINS   DS    D                                                                
E18SW    DS    C                                                    L02         
*                                                                               
PPBYOWRK DS    600C                                                             
*                                                                               
RTLKELS  CSECT                                                                  
         DS    4000C                                                            
*                                                                               
WRKRBUFF CSECT                                                                  
         DS    4096X                                                            
*                                                                               
RCHGEL   DSECT                                                                  
*                                                                               
PBRCELEM DS    0C                  ** PRINT PAK - AUTO RATE CHG ELEM **         
         DS    X'76'                                                            
         DS    AL1(30)                                                          
*                                                                               
PBRCDAT  DS    CL3 .     B         AUTO RATE CHG DATE                           
PBRCGRS  DS    CL4       B         PRE CHANGE GROSS                             
PBRCCOS  DS    PL5 .     P         PRE-CHANGE COST                              
PBRCPRC  DS    PL5 .     P         PRE-CHANGE PREMIUM COST                      
         DS    CL11                SPARE                                        
*                                                                               
BUYLND   DSECT                                                                  
BCLT     DS    CL3                                                              
         DS    CL2                                                              
BPRD     DS    CL3                                                              
         DS    CL2                                                              
BDATE    DS    CL8                                                              
         DS    CL2                                                              
BEST     DS    CL3                                                              
         DS    CL2                                                              
BDESC    DS    0CL20                                                            
         DS    CL13                                                             
BLINES   DS    CL7                                                              
         DS    CL2                                                              
BORATE   DS    CL8                                                              
         DS    CL1                                                              
BOGROSS  DS    CL14                                                             
         DS    CL3                                                              
BNRATE   DS    CL8                                                              
         DS    CL1                                                              
BNGROSS  DS    CL14                                                             
         DS    CL2                                                              
BCHG     DS    CL12                                                             
*                                                                               
       ++INCLUDE DMWRKRK                                                        
         SPACE 2                                                                
       ++INCLUDE DDLOGOD                                                        
*                                                                               
         BUFF  LINES=300,ROWS=1,COLUMNS=9,FLAVOR=PACKED,COMMENT=20,    X        
               KEYLIST=(28,A)                                                   
*                                                                               
       ++INCLUDE DDBUFFALOD                                                     
*                                                                               
PCHGELED DSECT                                                                  
       ++INCLUDE PCHGELEM                                                       
**                                                                              
PASRELED DSECT                                                                  
       ++INCLUDE PASRELEM                                                       
PCATELD  DSECT                                                                  
       ++INCLUDE PCATELEM                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE PPMODEQU                                                       
         EJECT                                                                  
       ++INCLUDE PPREPWORK                                                      
         EJECT                                                                  
       ++INCLUDE PPREPWORK2                                                     
         EJECT                                                                  
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'138PPREPAR02 07/09/14'                                      
         END                                                                    
