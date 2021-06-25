*          DATA SET SPOTSTLK   AT LEVEL 017 AS OF 11/08/19                      
*PHASE T00A56B,*                                                                
         SPACE 1                                                                
***********************************************************************         
* SPOTSTLK - T00A56 - STATION LOCKIN EXTRACT                          *         
*                                                                     *         
* INPUT  : P1=A(SPOT BLOCK)                                           *         
*          SBAIO1 = A(STATION LOCKIN RECORD)                          *         
*                                                                     *         
* OUTPUT : TABLE OF CHUNKS POINTED TO BY SBACHUNK AND COVERED BY      *         
*          SSLCHNKD.                                                  *         
*          CALLING PROGRAM CAN USE CHUNKS TO BUILD SORT RECORDS       *         
*          FOR REPORTING.                                             *         
*                                                                     *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-33500  11/08/19 SUPPORT FOR 2 DECIMAL IMPRESSIONS         *         
* AKAT SPEC-20805  03/12/18 SUPPORT COMSCORE DEMOS                    *         
* AKAT DSSUP-7383  04/29/16 FIX 2 DECIMAL DEMOS BUG                   *         
* 13JUN06 11 AKT -- SUPPORT FOR NEW SPOT LENGTHS                      *         
* 04APR05 12 AKT -- 2 DECIMAL DEMOS SUPPORT                           *         
* 05MAR03 10 EFJ -- USE SPSLNTAB                                      *         
* 06AUG01 08 EFJ -- HISTORY LOST                                      *         
*                -- CHANGE SLNTAB                                     *         
***********************************************************************         
         TITLE 'T00A56 - SPOTPAK STATION LOCKIN EXTRACT MODULE'                 
SPOTSTLK CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**SPSTLK,RA,RR=RE,CLEAR=YES                          
         USING WORKD,RC            RC = A(LOCAL WORKING STORAGE)                
         ST    R1,APARM            SAVE A(PARM LIST)                            
         L     R8,0(R1)                                                         
         USING SBLOCKD,R8          R8 = A(SPOTBLOCK)                            
         L     R9,SBCOMFAC         R9 = A(COMFACS)                              
         USING COMFACSD,R9                                                      
         L     R6,SBAIO1           R6 = A(LOCKIN RECORD)                        
         USING SLKRECD,R6                                                       
         L     R7,SBACHUNK         R7 = A(CHUNK)                                
         USING SSLCHNKD,R7                                                      
         XC    SLNEXT,SLNEXT       CLEAR START OF CHUNK                         
         ST    RE,RELO                                                          
         MVI   0(R1),0             SET NORMAL COMPLETION                        
*                                                                               
         MVI   XFF,X'FF'                                                        
         MVC   XFF+1(L'XFF-1),XFF                                               
*                                                                               
         MVC   PRD1,SLKKPRD        SET PRODUCT(S)                               
         MVC   PRD2,SLKKPRD2                                                    
         ZIC   RE,SLKKLEN          SPOT LENGTH(S)                               
         STC   RE,SLN1                                                          
         ZIC   RF,SLKKLEN2                                                      
         STC   RF,SLN2                                                          
         AR    RE,RF                                                            
         STC   RE,SLNT                                                          
         MVC   DPT,SLKKDPT         DAYPART                                      
*                                                                               
         BAS   RE,EQTAB            FIND EQUIVALENCE TABLE                       
         BAS   RE,DEML             SET DEMO LIST                                
         MVC   NDEMOS,SBENDEM      SET NUMBER OF DEMOS                          
         CLI   NDEMOS,0                                                         
         BNE   *+8                                                              
         MVI   NDEMOS,4                                                         
         CLI   NDEMOS,MAXDEMS                                                   
         BNH   *+8                                                              
         MVI   NDEMOS,MAXDEMS                                                   
         ZIC   RE,NDEMOS           COMPUTE LENGTH OF CHUNK                      
         SLL   RE,3                                                             
         LA    RE,SLDEMOS-SSLCHNKD(RE)                                          
         ST    RE,LCHUNK                                                        
*                                                                               
         MVI   SPLIT,0                                                          
         CLC   SBQPRD,=C'POL'      TEST POOL REQUEST                            
         BNE   SL4                                                              
         CLI   PRD2,0              YES-TEST 2ND PROD                            
         BE    SL2                                                              
         CLI   SBESPLIT,C'N'       YES-TEST SPLIT PIGGYBACKS                    
         BE    SL2                                                              
         MVI   SPLIT,1             YES-PROCESS FIRST PRODUCT                    
*                                                                               
SL2      XC    KEY,KEY             GENERATE POOL CHUNK                          
         MVI   KPRD1,X'FF'                                                      
         MVC   KSLN1,SLNT                                                       
         MVC   KSLNT,SLNT                                                       
         CLI   SPLIT,0                                                          
         BE    SL12                                                             
         MVC   KSLN1,SLN1                                                       
         MVC   KSLNT,SLN1                                                       
         CLI   SPLIT,1                                                          
         BE    SL12                                                             
         MVC   KSLN1,SLN2                                                       
         MVC   KSLNT,SLN2                                                       
         B     SL12                                                             
*                                                                               
SL4      MVI   SPLIT,0                                                          
         CLI   PRD2,0              TEST PIGGYBACKS                              
         BE    SL6                                                              
         CLI   SBESPLIT,C'N'       YES-TEST SPLIT PIGGYBACKS                    
         BE    SL6                                                              
         MVI   SPLIT,1             YES - PROCESS PRODUCT 1                      
*                                                                               
SL6      XC    KEY,KEY             SET CHUNK KEY                                
         CLI   SPLIT,2             TEST 2ND PRD OF SPLIT                        
         BE    SL8                 YES                                          
         MVC   KPRD1,PRD1          PRD1                                         
         MVC   KSLN1,SLN1          SLN1                                         
         MVC   KSLNT,SLN1          SLN TOTAL                                    
         CLI   SPLIT,1             TEST 1ST PRD OF SPLIT                        
         BE    SL10                YES                                          
         MVC   KPRD2,PRD2          NO - SET PRD2                                
         MVC   KSLN2,SLN2                   SLN2                                
         MVC   KSLNT,SLNT                   SLN TOTAL                           
         B     SL10                                                             
*                                                                               
SL8      MVC   KPRD1,PRD2          2ND PRD OF SPLIT - PRD1                      
         MVC   KSLN1,SLN2                             SLN1                      
         MVC   KSLNT,SLN2                             SLN TOTAL                 
*                                                                               
SL10     CLI   SBEPRD,0            TEST PRODUCT FILTER                          
         BE    SL12                                                             
         CLC   KPRD1,SBEPRD        YES                                          
         BNE   SL28                                                             
*                                                                               
SL12     MVC   BYTE,KSLNT          SET SPOT LENGTH                              
         BAS   RE,GETEQU           GET EQUIVALENCY FACTOR                       
         MVC   EQFACT,FULL                                                      
*                                                                               
         XC    DEMDSPLS,DEMDSPLS   FIND DISPLACEMENTS OF DEMOS INTO             
         LA    R1,DEMDSPLS         LOCKIN DEMOS                                 
         LA    RE,DEMOS            DEMO LIST FROM EST/OVERRIDE                  
*                                                                               
SL14     CLI   0(RE),X'FF'         END OF DEMO LIST?                            
         BE    SL20                YES                                          
*                                                                               
         XR    R3,R3               CLEAR R3                                     
         CLI   2(RE),0             COMSCORE DEMO?                               
         BNE   SL15                NO                                           
         ICM   R3,15,SBCOMDEM      HAVE A(COMSCORE DEMO LIST)?                  
         BZ    SL15                NO - CAN'T MATCH ON THIS                     
         LLC   R0,1(RE)            INDEX INTO SBCOMDEM                          
         BCTR  R0,0                -1 FOR INDEX                                 
         MHI   R0,8                COMSCORE DEMO IN SBCOMDEM                    
         AR    R3,R0               R3 = COMSCORE NAME FROM EST/OVERRIDE         
*                                                                               
SL15     LA    R0,MAXDEMS          MAX DEMOS = 6 (CAN ONLY LOCK IN 4)           
         LA    RF,SLKDEM1          DEMO 1 ON LOCK-IN RECORD                     
         LA    R2,1                DEMO 1 DISPLACEMENT                          
*                                                                               
SL16     LTR   R3,R3               LOOKING FOR COMSCORE DEMO?                   
         BZ    SL17                NO                                           
         CLI   2(RF),0             COMSCORE DEMO IN THIS SL REC SLOT?           
         BNE   SL17A               NO - CAN'T POSSIBLY MATCH                    
         ICM   R4,15,A50ELEM       HAVE A(X'50' ELEMENT)?                       
         BZ    SL17                NO - SL PRE-DATES 2 DEC IMPS                 
         LLC   R5,1(RF)            INDEX INTO X'50' ELEMENT                     
         BCTR  R5,0                -1 FOR INDEX                                 
         MHI   R5,8                COMSCORE DEMO IN SBCOMDEM                    
         LA    R4,2(R4,R5)         R4 = COMSCORE NAME FROM X'50' ELEM           
         CLC   0(8,R3),0(R4)       COMSCORE DEMOS MATCH?                        
         B     *+10                GO TEST CC                                   
SL17     CLC   1(2,RF),1(RE)                                                    
         BNE   *+12                                                             
         STC   R2,0(R1)                                                         
         B     SL18                                                             
SL17A    LA    R2,1(R2)                                                         
         LA    RF,L'SLKDEM1(RF)                                                 
         BCT   R0,SL16                                                          
*                                                                               
SL18     LA    R1,1(R1)                                                         
         LA    RE,3(RE)                                                         
         B     SL14                                                             
*                                                                               
SL20     LA    R2,SLKEL            SCAN THE WEEKLY LOCKIN ELEMENTS              
         USING LOKEL,R2                                                         
*                                                                               
SL22     CLI   0(R2),0                                                          
         BE    SL28                                                             
         CLI   0(R2),LOKELCDQ                                                   
         BE    SL26                                                             
*                                                                               
SL24     ZIC   R0,1(R2)            NEXT ELEMENT                                 
         AR    R2,R0                                                            
         B     SL22                                                             
*                                                                               
SL26     MVC   WEEK,LOKWEEK                                                     
         BAS   RE,FINDDATE         FIND DATE TABLE DATE                         
         BNE   SL24                                                             
         BAS   RE,GENCHUNK         GENERATE CHUNK DATA                          
         B     SL24                                                             
*                                                                               
SL28     CLI   KPRD1,X'FF'         TEST POL CHUNK                               
         BNE   SL30                                                             
*                                                                               
* THE FOLLOWING IS ONLY FOR LCI'S "GOAL" LOCKIN RECS                            
* (WHICH ARE ADDED BY THE BUY PROGRAM!!!)                                       
         CLI   PRD1,X'FF'                                                       
         BNE   *+12                                                             
         CLI   SPLIT,0                                                          
         BE    SL30                                                             
*                                                                               
         CLI   SPLIT,1             NO-TEST NEED CHUNK FOR 2ND SLN               
         BNE   SL4                 NO-GENERATE PRODUCT CHUNK                    
         MVI   SPLIT,2                                                          
         B     SL2                                                              
*                                                                               
SL30     CLI   SPLIT,1                                                          
         BNE   SL32                                                             
         MVI   SPLIT,2                                                          
         B     SL6                                                              
*                                                                               
SL32     B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* FIND DATE TABLE DATE FROM LOCKIN DATE                                         
* INPUT  : WEEK                                                                 
* OUTPUT : CC EQ DATE FOUND AND KDATE = DATE                                    
*             NE DATE NOT FOUND                                                 
*                                                                               
FINDDATE L     R3,SBADATE                                                       
         L     R4,SBNDATES                                                      
         CLI   SBEBEF,C'Y'         TEST NEED BEFORE REQUEST DATA                
         BNE   FD2                                                              
         CLC   WEEK,0(R3)                                                       
         BNL   FD2                                                              
         XC    KDATE,KDATE                                                      
         B     FDEQX                                                            
*                                                                               
FD2      MVC   KDATE,0(R3)         SCAN DATES TABLE                             
         CLC   WEEK,0(R3)                                                       
         BL    *+14                                                             
         CLC   WEEK,2(R3)                                                       
         BNH   FDEQX                                                            
         LA    R3,4(R3)                                                         
         BCT   R4,FD2                                                           
*                                                                               
         CLI   SBEAFTER,C'Y'       TEST NEED AFTER REQUEST DATA                 
         BNE   FDNEX                                                            
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         CLC   WEEK,0(R3)                                                       
         BNH   FDNEX                                                            
         MVC   KDATE,XFF                                                        
         B     FDEQX                                                            
*                                                                               
FDNEX    LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
FDEQX    CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GENERATE CHUNK DATA                                            
* INPUT  : R2 = A(WEEKLY LOCKIN ELEMENT)                                        
*          KEY = CHUNK KEY                                                      
*                                                                               
GENCHUNK NTR1                                                                   
         USING LOKEL,R2                                                         
         XC    SPOTS,SPOTS                                                      
         MVC   SPOTS+2(2),LOKSPOTS SPOTS                                        
         MVC   DOLS,LOKDOLS        DOLLARS                                      
         MVC   NET,LOKNET                                                       
         MVC   DOL2,LOKDOL2                                                     
         MVC   NET2,LOKNET2                                                     
         XC    DEMVALS(MAXDEMS*4),DEMVALS                                       
         LLC   R4,LOKELLN          EXTRACT DEMOS                                
         SH    R4,=Y(LOKDEM-LOKEL)                                              
         BNP   GC6                                                              
         SRL   R4,2                R4=N'DEMOS IN ELEMENT                        
         LA    RF,DEMVALS                                                       
         LA    R1,DEMDSPLS                                                      
         ZIC   R0,NDEMOS                                                        
         LA    R5,DEMOS                                                         
*                                                                               
GC2      SR    R3,R3                                                            
         ICM   R3,1,0(R1)                                                       
         BZ    GC4                                                              
         CR    R3,R4               TEST DEMO IS IN THIS ELEMENT                 
         BH    GC4                 NO                                           
         BCTR  R3,0                YES                                          
         SLL   R3,2                                                             
         LA    R3,LOKDEM(R3)                                                    
         MVC   FULL,0(R3)                                                       
GC3      BAS   RE,ADJPREC          SUPPORT FOR 2 DECIMAL DEMOS                  
         MVC   0(4,RF),FULL                                                     
*                                                                               
GC4      LA    R1,1(R1)                                                         
         LA    RF,4(RF)                                                         
         LA    R5,3(R5)                                                         
         BCT   R0,GC2                                                           
*                                                                               
GC6      L     R7,SBACHUNK                                                      
*                                                                               
GC8      OC    SLNEXT,SLNEXT       FIND RIGHT CHUNK                             
         BNZ   GC10                                                             
         L     RE,LCHUNK                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    SSLCHNKD(0),SSLCHNKD    CREATE NEW CHUNK                         
         MVC   SLKEY,KEY                                                        
         LA    RE,1(RE,R7)                                                      
         ST    RE,SLNEXT                                                        
         XC    0(4,RE),0(RE)                                                    
         B     GC12                                                             
*                                                                               
GC10     CLC   SLKEY,KEY                                                        
         BE    GC12                                                             
         ICM   R7,15,SLNEXT                                                     
         B     GC8                                                              
*                                                                               
GC12     LA    R3,DEMVALS          EQUIVALENCE THE DEMOS                        
         LA    R4,EDEMVALS                                                      
         ZIC   R0,NDEMOS                                                        
*                                                                               
GC14     ICM   RF,15,0(R3)                                                      
         BZ    *+8                                                              
         BAS   RE,DEMEQUIV                                                      
         ST    RF,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,GC14                                                          
*                                                                               
GC16     CLI   SPLIT,0             TEST PIGGYBACK SPLIT                         
         BE    GC18                                                             
         ZIC   R1,KSLN1            YES - APPORTION DOLLARS BETWEEN PRDS         
         AR    R1,R1                                                            
         M     R0,DOLS                                                          
         ZIC   RE,SLNT                                                          
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,DOLS                                                          
*                                                                               
GC18     L     RF,DOLS             EQUIVALENCE THE DOLLARS                      
         BAS   RE,DOLEQUIV                                                      
         ST    RF,EDOLS                                                         
*                                                                               
         L     R1,SLSPOTS          ACCUMULATE SPOTS                             
         A     R1,SPOTS                                                         
         ST    R1,SLSPOTS                                                       
*                                                                               
         L     R1,SLDOL            ACCUMULATE DOLLARS                           
         A     R1,DOLS                                                          
         ST    R1,SLDOL                                                         
*                                                                               
         L     R1,SLEDOL           ACCUMULATE EQUIV DOLLARS                     
         A     R1,EDOLS                                                         
         ST    R1,SLEDOL                                                        
*                                                                               
         L     R1,SLNET            ACCUMULATE NET DOLLARS                       
         A     R1,NET                                                           
         ST    R1,SLNET                                                         
*                                                                               
         L     R1,SLDOL2           ACCUMULATE COS2 DOLLARS                      
         A     R1,DOL2                                                          
         ST    R1,SLDOL2                                                        
*                                                                               
         L     R1,SLNET2           ACCUMULATE NET COS2 DOLLARS                  
         A     R1,NET2                                                          
         ST    R1,SLNET2                                                        
*                                                                               
         LA    R3,DEMVALS          ACCUMULATE DEMOS AND EQUIV DEMOS             
         LA    R4,EDEMVALS                                                      
         LA    R5,SLDEMOS                                                       
         ZIC   R0,NDEMOS                                                        
*                                                                               
GC20     L     R1,0(R5)                                                         
         A     R1,0(R3)                                                         
         ST    R1,0(R5)                                                         
         L     R1,4(R5)                                                         
         A     R1,0(R4)                                                         
         ST    R1,4(R5)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,8(R5)                                                         
         BCT   R0,GC20                                                          
*                                                                               
GCX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
ADJPREC  NTR1                                                                   
*                                                                               
         CLI   2(R5),0             COMSCORE DEMO?                               
         BNE   ADJP05              NO                                           
         ICM   RE,15,SBCOMDEM      HAVE A(COMSCORE DEMO LIST)?                  
         BZ    ADJPX               NO - SOMETHING IS WRONG                      
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,1,1(R5)          GET INDEX                                    
         BZ    ADJPX               IF 0, SOMETHING IS WRONG                     
         BCTR  RF,0                -1                                           
         MHI   RF,8                INDEX INTO DEMO LIST                         
         AR    RE,RF               INDEX COMSCORE DEMO NAMES                    
         LR    R5,RE               RE-POINT R5 TO COMSCORE DEMO NAMES           
         BCTR  R5,0                SO INSTRUCTIONS ARE SEAMLESS                 
*                                                                               
ADJP05   MVC   BYTE,FULL                                                        
         NI    FULL,X'3F'          DROP FLAGS FROM VALUE                        
*                                                                               
         TM    BYTE,X'40'          DEMO HAVE 2 DEC                              
         BO    ADJP10              YES                                          
*                                                                               
         CLI   1(R5),C'R'          RATING?                                      
         BE    *+8                 YES                                          
         CLI   1(R5),C'E'          EXTENDED RATING?                             
         BE    *+12                YES                                          
         TM    SBEFLAG9,SBE92DEC   REPORT SUPPORTS 2 DEC IMPRESSIONS?           
         B     *+8                 GO TEST CC                                   
         TM    SBEFLAG4,SBE42DEC   REPORT SUPPORT 2 DEC                         
         BZ    ADJPX               NO - LEAVE IT ALONE                          
*                                                                               
         L     R1,FULL             ADJUST 1 DECIMAL PRECISION TO 2              
         MHI   R1,10                                                            
         ST    R1,FULL                                                          
         B     ADJPX                                                            
***                                                                             
* DEMO HAS 2 DEC                                                                
***                                                                             
ADJP10   CLI   1(R5),C'R'          RATING?                                      
         BE    *+8                 YES                                          
         CLI   1(R5),C'E'          EXTENDED RATING?                             
         BE    *+12                YES                                          
         TM    SBEFLAG9,SBE92DEC   REPORT SUPPORTS 2 DEC IMPRESSIONS?           
         B     *+8                 GO TEST CC                                   
         TM    SBEFLAG4,SBE42DEC   USER WANT 2 DEC                              
         BNZ   ADJPX               YES - LEAVE IT ALONE                         
*                                                                               
         L     R0,FULL             ADJUST 2 DECIMAL PRECISION TO 1              
         SRDA  R0,31               R1 = 2*FULL                                  
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,FULL                                                          
*                                                                               
ADJPX    B     EXIT                                                             
*                                                                               
* EQUIVALENCE DEMO VALUE                                                        
* INPUT  : RF = DEMO VALUE                                                      
*          EQFACT = EQUIV FACTOR                                                
* OUTPUT : RF = EQUIVALENCED DEMO VALUE                                         
*                                                                               
DEMEQUIV ST    RE,SAVERE                                                        
         SR    RE,RE                                                            
         L     R1,EQFACT                                                        
         MR    RE,R1                                                            
         SLDA  RE,1                                                             
         D     RE,=F'1000'                                                      
         A     RF,=F'1'                                                         
         SRL   RF,1                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
* EQUIVALENCE DOLLAR                                                            
* INPUT  : RF = DOLLARS                                                         
*          EQFACT = EQUIV FACTOR                                                
* OUTPUT : RF = EQUIVALENCED DOLLARS                                            
*                                                                               
DOLEQUIV ST    RE,SAVERE                                                        
         SR    RE,RE                                                            
         M     RE,=F'1000'                                                      
         SLDA  RE,1                                                             
         SR    R1,R1                                                            
         ICM   R1,15,EQFACT                                                     
         BNZ   *+8                                                              
         L     R1,=F'1000'                                                      
         DR    RE,R1                                                            
         LTR   RF,RF                                                            
         BM    *+8                                                              
         A     RF,=F'1'                                                         
         SRA   RF,1                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO FIND THE EQUIVALENCE TABLE                                         
*                                                                               
EQTAB    ST    RE,SAVERE                                                        
         LA    RF,L'SBDPTTAB/5     RF=COUNTER                                   
         LA    RE,SBDPTTAB         RE=A(DPT TABLE)                              
         CLC   DPT,0(RE)           MATCH ON DAYPART CODE                        
         BE    EQTAB2                                                           
         LA    RE,5(RE)                                                         
         BCT   RF,*-14                                                          
         B     EQTABX                                                           
*                                                                               
EQTAB2   ZIC   RF,1(RE)            DAYPART INTERNAL CODE                        
         N     RF,=F'15'           ZERO HIGH ORDER NIBBLE                       
         IC    RF,SBDPEQTB(RF)     TABLE IND                                    
         LTR   RF,RF               TEST FOR ZERO                                
         BZ    *+6                 YES-ACTUALLY SHOULD BE 1 !!                  
         BCTR  RF,0                                                             
         MH    RF,=H'60'                                                        
*                                                                               
         LA    RF,SBEQTAB(RF)       EQUIV TABLE                                 
         ST    RF,AEQTAB                                                        
*                                                                               
EQTABX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* ROUTINE TO GET THE EQUIVALENCE FACTOR FOR A SECONDS LENGTH                    
* INPUT  : BYTE = SECONDS LENGTH                                                
* OUTPUT : FULL = EQUIVALENCY FACTOR                                            
*                                                                               
GETEQU   NTR1                                                                   
         MVC   FULL,=F'1000'       DEFAULT TO BASE                              
         ICM   R2,15,AEQTAB        RF=A(EQUIV TABLE FOR DPT)                    
         BZ    GETEQUX                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A57'                                           
         GOTO1 CCALLOV,DMCB,0         GET SPSLENTAB                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         MVI   BYTE2,C'T'                                                       
         CLI   SBMED,C'T'                                                       
         BE    GETEQU1                                                          
         CLI   SBMED,C'N'                                                       
         BE    GETEQU1                                                          
         CLI   SBMED,C'C'                                                       
         BE    GETEQU1                                                          
*                                                                               
         MVI   BYTE2,C'R'                                                       
         CLI   SBMED,C'R'                                                       
         BE    GETEQU1                                                          
         CLI   SBMED,C'X'                                                       
         BE    GETEQU1                                                          
         DC    H'0'                                                             
*                                                                               
GETEQU1  CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         BE    GETEQU2                                                          
         CLC   0(2,R1),SBAGY       ELSE MATCH AGY                               
         BNE   *+14                                                             
GETEQU2  CLC   BYTE2,2(R1)         AND MEDIA                                    
         BE    GETEQU3                                                          
*                                                                               
         BXLE  R1,RE,GETEQU1                                                    
         DC    H'0'                                                             
*                                                                               
GETEQU3  AHI   R1,4                POINT BEYOND HEADER                          
         ZIC   R3,BYTE             GET SLN                                      
         AR    R3,R3               X 2                                          
         AR    R1,R3               POINT TO ENTRY                               
         CLI   1(R1),0             SLN VALID?                                   
         BE    GETEQUX             NO                                           
         ZIC   R3,0(R1)            GET INDEX INTO EQU TABLE                     
         AR    R2,R3               INDEX INTO TABLE                             
         MVC   FULL+2(2),0(R2)                                                  
*                                                                               
GETEQUX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* SUB-ROUTINE TO ESTABLISH DEMO LIST                                            
*                                                                               
DEML     NTR1                                                                   
*                                                                               
         XC    A50ELEM,A50ELEM     CLEAR A(X'50' ELEMENT')                      
*                                                                               
         L     R6,SBAIO1           R6 = A(LOCKIN RECORD)                        
         USING SLKRECD,R6          STATION LOCKIN RECORD DSECT                  
         AHI   R6,42               A(FIRST ELEMENT)                             
         XR    RE,RE               CLEAR RE                                     
*                                                                               
DEML00   CLI   0(R6),0             END OF RECORD?                               
         BE    DEMEL1B             YES                                          
         CLI   0(R6),X'50'         HAVE A(COMSCORE DEMO NAME ELEMENT)?          
         BE    DEMEL1A             YES - SAVE ADDRESS                           
         IC    RE,1(R6)            ELEMENT LENGTH                               
         AR    R6,RE               BUMP TO NEXT ELEMENT                         
         B     DEML00              TEST NEXT ELEMENT                            
*                                                                               
DEMEL1A  STCM  R6,15,A50ELEM       SAVE A(X'50') ELEMENT                        
*                                                                               
DEMEL1B  L     R6,SBAIO1           R6 = A(LOCKIN RECORD)                        
         XC    DEMOS,DEMOS                                                      
         OC    SBEDEMOS,SBEDEMOS   TEST OVERRIDE DEMO LIST                      
         BZ    *+14                                                             
         MVC   DEMOS,SBEDEMOS                                                   
         B     DEML4                                                            
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU                               
         BZ    *+14                                                             
         MVC   DEMOS,SBPDEMOS      YES-SET DEMOS FROM THERE                     
         B     DEML4                                                            
         OC    SBAESTTB,SBAESTTB                                                
         BZ    *+14                                                             
         OC    SBAESTBF,SBAESTBF                                                
         BNZ   DEML2                                                            
         MVC   DEMOS,SBPDEMOS                                                   
         B     DEML4                                                            
*                                                                               
DEML2    LA    RE,255                                                           
         CLC   SBQPRD,=C'POL'      TEST REQUESTED PRODUCT IS POL                
         BE    *+8                 YES-USE POL DEMOS                            
         IC    RE,SLKKPRD                                                       
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         ZIC   RF,SLKKEST                                                       
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R1,SBAESTTB                                                      
         LA    R1,0(R1,RE)                                                      
         SR    RE,RE                                                            
         ICM   RE,1,0(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  RE,0                                                             
         MH    RE,=Y(ESTBUFFL)                                                  
         L     R1,SBAESTBF                                                      
         LA    RE,0(R1,RE)                                                      
         USING ESTBUFFD,RE                                                      
         MVC   DEMOS,EBDEMOS                                                    
*                                                                               
DEML4    LA    R0,MAXDEMS          INSERT EOT AFTER DEMO LIST                   
         LA    RE,DEMOS                                                         
         CLI   1(RE),0             TEST FOR EOL                                 
         BE    *+12                NO                                           
         LA    RE,3(RE)                                                         
         BCT   R0,*-12                                                          
         MVI   0(RE),X'FF'                                                      
*                                                                               
DEMLX    B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
WORKD    DSECT                                                                  
APARM    DS    A                                                                
RELO     DS    A                                                                
SAVERE   DS    A                                                                
*                                                                               
FULL     DS    F                                                                
*                                                                               
SPOTS    DS    F                                                                
DOLS     DS    F                                                                
EDOLS    DS    F                                                                
NET      DS    F                                                                
DOL2     DS    F                                                                
NET2     DS    F                                                                
DEMVALS  DS    (MAXDEMS)F                                                       
EDEMVALS DS    (MAXDEMS)F                                                       
AEQTAB   DS    A                   A(EQUIVALENCE TABLE)                         
EQFACT   DS    F                   SLN EQUIVALENCY FACTOR                       
LCHUNK   DS    F                   L'CHUNK ENTRY                                
DMCB     DS    6F                                                               
*                                                                               
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
XFF      DS    XL16                                                             
*                                                                               
DPT      DS    C                                                                
PRD1     DS    X                                                                
PRD2     DS    X                                                                
SLN1     DS    X                                                                
SLN2     DS    X                                                                
SLNT     DS    X                                                                
SPLIT    DS    X                                                                
WEEK     DS    XL2                                                              
*                                                                               
KEY      DS    0CL7                                                             
KPRD1    DS    X                                                                
KPRD2    DS    X                                                                
KSLNT    DS    X                                                                
KSLN1    DS    X                                                                
KSLN2    DS    X                                                                
KDATE    DS    XL2                                                              
*                                                                               
NDEMOS   DS    X                                                                
DEMDSPLS DS    XL(MAXDEMS)                                                      
DEMOS    DS    CL((MAXDEMS*3)+1)                                                
MAXDEMS  EQU   6                                                                
*                                                                               
A50ELEM  DS    F                   A(X'50' ELEM) FOR ADJPREC                    
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
SBLOCKD  DSECT                                                                  
       ++INCLUDE SPOTBLOCK                                                      
         EJECT                                                                  
* SPGENXLK                                                                      
*        PRINT OFF                                                              
       ++INCLUDE SPGENXLK                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPOTSTLK  11/08/19'                                      
         END                                                                    
