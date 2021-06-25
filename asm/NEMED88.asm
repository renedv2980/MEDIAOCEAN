*          DATA SET NEMED88    AT LEVEL 177 AS OF 05/01/02                      
*PHASE T31E88A,*                                                                
*INCLUDE WGTLIST                                                                
         TITLE 'T31E88 - BRAND RE-ALLOCATION'                                   
T31E88   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BBAL**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING T31E88+4096,R6                                                   
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2                                                       
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
* - NEED TO HAVE COPIES WRITTEN TO RECOVERY FILE                                
         ICM   R1,15,TWAMASTC                                                   
         BZ    MASTEND                                                          
         USING MCBLOCK,R1                                                       
         L     R1,MCSSB                                                         
         USING SSBD,R1                                                          
         OI    SSBSTAT2,SSBSROLC   RECOVER OFFLINE COPIES                       
MASTEND  DS    0H                                                               
         DROP  R1                                                               
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMED87 (T31E88)                                            *         
*                                                                     *         
*  COMMENTS: READ UNIT RECORDS - PRINTS GROSS/NET DOLLARS FOR THREE   *         
*            MONTHS FOR EACH PRODUCT                                  *         
*                                                                     *         
*  CALLS TO: NETIO                                                    *         
*                                                                     *         
*                                                                     *         
*  LOCALS: ANETWS4 USED FOR PRODUCT DOLLARS 220 X 12                  *         
*          ANETWS3 USED TO SAVE CLIENT RECORD                         *         
*          ANETWS2 USED FOR MYWORKD                                   *         
*          R7-MYWORKD                                                 *         
*                                                                     *         
***********************************************************************         
*  LOGIC:  FIRST, GET 3 MONTHS DATES- INPUT PRD/DOLLARS LIST          *         
*                                                                     *         
*          SECOND, READS UNIT RECS THROUGH NETIO AND ADDS UNIT        *         
*          DATA TO MONTH AND FINAL BUCKETS                            *         
*                                                                     *         
*          THIRD, CALCULATES % EACH PRODUCT REPRESENTS OF THE TOTAL   *         
*          DOLLLARS AND PRINTS THEM                                   *         
*                                                                     *         
*          FOURTH, PRINTS A NETWORK RECAP                             *         
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY *                                                                
         SPACE                                                                  
VK       DS    0H                                                               
         MVI   NBSEQ,C'N'          NETWORK ORDER                                
         SPACE                                                                  
*                                                                               
* VALIDATE SCREEN INPUT FIELDS                                                  
*                                                                               
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         MVC   NBACLI,ANETWS3      SAVE CLIENT IN 3RD IO AREA                   
         LA    R2,SPLCLIH                                                       
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
         MVI   UNASW,X'00'                                                      
         CLC   SPLPRD(3),=CL3'UNA'                                              
         BNE   *+14                                                             
         MVI   UNASW,X'FF'                                                      
         MVC   SPLPRD(3),=CL3'POL'                                              
         LA    R2,SPLPRDH                                                       
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         NETGO NVPRDALL,DMCB,0,0                                                
         MVI   ERROR,NBINVPRD                                                   
         CLI   NBSELLEN,0                                                       
         BNE   TRAPERR                                                          
         MVC   REQPROD(1),NBEFFPNM                                              
         OC    NBSELPIG,NBSELPIG                                                
         BZ    *+10                                                             
         MVC   REQPROD,NBSELPIG                                                 
*                                                                               
         LA    R2,SPLLENH                                                       
         BAS   RE,VALLEN                                                        
         MVC   REQLEN(1),NBSELLEN                                               
         MVC   REQLEN+1(1),HOLDLEN                                              
*                                                                               
         BAS   RE,REALOC                                                        
*                                                                               
*--RESET THE PRODUCT AND LENGTH VALUES IN NETBLOCK                              
*--THEY GOT CREAMED IN THE "REALOC" ROUTINE.                                    
         LA    R2,SPLPRDH                                                       
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         XC    NBSELPNM,NBSELPNM                                                
         XC    NBSELPIG,NBSELPIG                                                
         XC    NBSELLEN,NBSELLEN                                                
         MVI   NBRETURN,X'03'      REVALIDATING PRODUCT                         
         NETGO NVPRDALL,DMCB,0,0                                                
         MVI   ERROR,NBINVPRD                                                   
         CLI   NBSELLEN,0                                                       
         BNE   TRAPERR                                                          
         CLI   UNASW,X'FF'                                                      
         BNE   *+10                                                             
         MVC   SPLPRD(3),=CL3'UNA'                                              
*                                                                               
*-INCLUDE PB'S                                                                  
         MVI   PIGSW,0                                                          
         MVI   ERROR,NBINVAL                                                    
         LA    R2,SPLENDH                                                       
         CLI   5(R2),0                                                          
         BE    VK300                                                            
         CLI   8(R2),C'N'                                                       
         BE    VK300                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   TRAPERR                                                          
*-IF INCLUDE PB'S=Y THE FOLLOWING CONDITIONS MUST BE SET                        
         LA    RE,REQTAB           NO REQUESTED PB'S ALLOWED                    
VK200    CLI   0(RE),X'FF'                                                      
         BE    VK220                                                            
         CLI   1(RE),0                                                          
         BNE   TRAPERR                                                          
         LA    RE,11(RE)                                                        
         B     VK200                                                            
*                                                                               
VK220    CLI   NBSELPIG,0          ONLY SINGLE PROD REQUEST                     
         BNE   TRAPERR                                                          
*-MOVE PRODUCT CODE INTO INCLUDE PB'S SWITCH.                                   
*-REMOVE THE PRODUCT FILTER TO NETIO.                                           
         MVI   PIGSW,C'Y'                                                       
         XC    NBSELPIG,NBSELPIG                                                
         XC    NBSELPNM,NBSELPNM                                                
         MVC   PIGPROD,NBEFFPNM                                                 
         MVC   SPLPPRD(3),=C'POL'                                               
         LA    R2,SPLPPRDH                                                      
         MVI   5(R2),3                                                          
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         MVI   NBRETURN,X'03'      REVALIDATING PRODUCT                         
         NETGO NVPRDALL,DMCB,0,0                                                
*                                                                               
VK300    LA    R2,SPLLENH                                                       
         BAS   RE,VALLEN                                                        
*                                                                               
         LA    R2,SPLESTH                                                       
         MVI   FTERMFLG,1          SET OPTIONAL FLAG                            
         NETGO NVESTRNG,DMCB,SPLESTN,0                                          
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,1          SET OPTIONAL FLAG                            
         LA    R2,SPLPAKH                                                       
         NETGO NVPAK,DMCB,SPLPAKN                                               
         OI    SPLPAKNH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,1          SET OPTIONAL FLAG                            
         LA    R2,SPLSTAH                                                       
         NETGO NVNETALL,DMCB,0,0                                                
*                                                                               
         LA    R2,SPLSDATH         START DATE                                   
         NETGO NVSTRDAT,DMCB                                                    
         OI    SPLSDATH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLEDATH         END DATE                                     
         NETGO NVENDDAT,DMCB                                                    
         OI    SPLEDATH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLRSNH          REASON CODE                                  
         NETGO NVCHKRSN,DMCB,SPLRSN                                             
         OI    SPLRSNH+6,X'80'                                                  
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VR500                                                            
         NETGO NVUSERID,DMCB,0,0   USERID                                       
         MVC   SPLUID(2),NBUSERID                                               
         MVC   SPLUID+2(2),NBSECAGY                                             
         MVI   SPLUIDH+5,4                                                      
*        GOTO1 =V(PRNTBL),DMCB,=C'BPRD',SPLUID,C'DUMP',4,=C'1D'                 
*                                                                               
VR500    LA    R2,SPLTSTH          IS THIS A TEST                               
         CLI   5(R2),0                                                          
         BE    EXIT                                                             
         CLI   8(R2),C'N'                                                       
         BE    EXIT                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   TRAPERR                                                          
         MVI   TESTSW,C'Y'                                                      
         B     EXIT                                                             
*                                                                               
REALOC   NTR1                                                                   
         MVI   NBSELLEN,0                                                       
         LA    R2,SPLRPR1H                                                      
         LA    R3,11                                                            
         SR    R4,R4                                                            
         LA    R5,REQTAB                                                        
*                                                                               
         MVI   ERROR,NBINVPRD                                                   
         CLI   5(R2),0             MUST HAVE AT LEAST 1 PRODUCT                 
         BE    TRAPERR                                                          
*                                                                               
RELOC20  CLI   5(R2),0             PRODUCT                                      
         BE    RELOCEX                                                          
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         XC    NBSELPIG,NBSELPIG                                                
         XC    NBSELPNM,NBSELPNM                                                
         MVI   NBSELLEN,0                                                       
         MVI   NBRETURN,X'03'      REVALIDATING PRODUCT                         
         NETGO NVPRDALL,DMCB,0,0                                                
         MVI   ERROR,NBINVPRD                                                   
         CLI   NBSELLEN,0                                                       
         BNE   TRAPERR                                                          
         MVC   0(1,R5),NBEFFPNM                                                 
         OC    NBSELPIG,NBSELPIG                                                
         BZ    *+10                                                             
         MVC   0(2,R5),NBSELPIG                                                 
*-SAVE PRODUCT CODE                                                             
         XC    WORK,WORK                                                        
         GOTO1 SCANNER,DMCB,(R2),(2,WORK)                                       
         MVC   5(3,R5),WORK+12                                                  
         MVC   8(3,R5),WORK+32+12                                               
*                                                                               
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
*                                                                               
         BAS   RE,VALLEN                                                        
         BAS   RE,RELOLEN                                                       
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
*                                                                               
         TM    4(R2),X'10'         CHECK NON NUMERIC INPUT                      
         BNZ   TRAPERR                                                          
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    TRAPERR                                                          
         BCTR  R1,0                                                             
         EX    R1,RELOPK                                                        
         CVB   RE,DUB                                                           
         STC   RE,4(R5)            STORE PCT IN TABLE                           
         AR    R4,RE                                                            
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         ZIC   RF,0(R2)            NEXT FIELD                                   
         AR    R2,RF                                                            
         LA    R5,11(R5)                                                        
         BCT   R3,RELOC20                                                       
RELOCEX  LA    R2,SPLRPT1H                                                      
         MVI   ERROR,NBINVAL                                                    
         CH    R4,=H'100'                                                       
         BNE   PCTERR                                                           
         MVI   0(R5),X'FF'                                                      
         B     EXIT                                                             
RELOPK   PACK  DUB(8),8(0,R2)                                                   
PCTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(45),PCTMSG                                               
         B     TRAPERR2                                                         
PCTMSG   DC    CL45'REALLOCATED PRODUCTS NOT EQUAL TO 100 PERCENT'              
*                                                                               
VALLEN   NTR1                                                                   
         CLI   5(R2),0             CHECK FOR INPUT                              
         BE    VALENEX                                                          
         XC    HOLDLEN,HOLDLEN                                                  
         XC    NBSELLEN,NBSELLEN                                                
         XC    WORK,WORK                                                        
         GOTO1 SCANNER,DMCB,(R2),(2,WORK),0                                     
*                                                                               
         MVI   ERROR,NBINVAL                                                    
         CLI   4(R1),2             CHECK 2 LENGTHS                              
         BNE   VALEN20                                                          
         MVC   HOLDLEN,WORK+7       FIRST PRODUCTS LENGTH                       
         OC    NBSELPIG,NBSELPIG    MUST BE PIGGYBACK PRODS                     
         BZ    TRAPERR                                                          
*                                                                               
VALEN20  MVI   ERROR,NBNOTNUM       CHECK NUMERIC                               
         TM    WORK+2,X'80'                                                     
         BZ    TRAPERR                                                          
         CLI   WORK+32,0           CHECK FOR 2 LENGTH'S                         
         BE    VALEN30                                                          
         TM    WORK+32+2,X'80'                                                  
         BZ    TRAPERR                                                          
*                                                                               
VALEN30  ZIC   RE,WORK+7                                                        
         ZIC   RF,WORK+32+7                                                     
         AR    RE,RF                                                            
         STC   RE,NBSELLEN                                                      
VALENEX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
RELOLEN  NTR1                                                                   
         MVI   ERROR,NBINVAL                                                    
         CLI   REQPROD+1,0         CHECK FOR PIGGYBACK                          
         BE    RELL100             NO                                           
         CLI   NBSELPIG,0          CHECK IF REALOCATE PROD A PIGGYBACK          
         BE    RELL60              NO                                           
*-VALIDATE LENGTH FOR A PIGGY TO PIGGY REALOCATION                              
*-BOTH LENGTHS MUST EITHER BE ZERO OR DEFINED                                   
*                                                                               
*-IF LENGTH'S ARE ENTERED THE SUM OF THE LENGTHS MUST                           
*-EQUAL THE SUM OF THE REQUESTED PRODUCT LENGTH'S.                              
         OC    NBSELLEN,NBSELLEN                                                
         BZ    RELL20                                                           
         OC    HOLDLEN,HOLDLEN                                                  
         BZ    TRAPERR                                                          
         CLC   NBSELLEN,REQLEN                                                  
         BNE   TRAPERR                                                          
*                                                                               
*-MOVE OUT TO THE TABLE                                                         
         MVC   2(1,R5),NBSELLEN                                                 
         MVC   3(1,R5),HOLDLEN                                                  
         B     EXIT                                                             
*                                                                               
*-IF LENGTH'S ARE NOT ENTERED USE THE REQUESTED PRODUCTS                        
*-LENGTH'S. IF BOTH REQUESTED PRODUCTS LENGTH'S WERE NOT                        
*-ENTERED THEN USE THE DEFAULT LENGTH'S.                                        
*-X'FF' IN THE TABLE ENTRIES.                                                   
RELL20   CLI   REQLEN+1,0                                                       
         BZ    RELL30                                                           
         MVC   2(2,R5),REQLEN                                                   
         B     EXIT                                                             
*                                                                               
RELL30   MVC   2(2,R5),=XL2'FFFF'                                               
         B     EXIT                                                             
*                                                                               
*-FOR PIGGY TO SINGLE ALLOCATION THE TOTAL SPOT LENGTH                          
*-REMAINS THE SAME.                                                             
*                                                                               
*-IF LENGTHS ARE ENTERED THE TOTAL LENGTHS MUST BE EQUAL.                       
RELL60   MVC   2(2,R5),=X'FF00'                                                 
         CLI   REQLEN,0                                                         
         BE    RELL80                                                           
         CLI   NBSELLEN,0                                                       
         BE    EXIT                                                             
         CLC   REQLEN(1),NBSELLEN                                               
         BNE   TRAPERR                                                          
         B     EXIT                                                             
*                                                                               
*-IN A PIGGY TO SINGLE ALLOCATION THE RECIEVING PRODUCT                         
*-CANNOT HAVE A LENGTH IF THE GIVING PRODUCT DOES NOT.                          
RELL80   CLI   NBSELLEN,0                                                       
         BNE   TRAPERR                                                          
         B     EXIT                                                             
*                                                                               
RELL100  CLI   NBSELPIG,0          CHECK IF REALOCATE PROD A PIGGYBACK          
         BE    RELL160             NO                                           
*-FOR SINGLE TO PIGGYBACK ALLOCATION THE SINGLR PRODUCT                         
*-MUST HAVE A LENGTH DEFINED.                                                   
         CLI   REQLEN,0                                                         
         BE    TRAPERR                                                          
         CLI   NBSELLEN,0                                                       
         BE    RELL120                                                          
*                                                                               
*-FOR SINGLE TO PIGGYBACK ALLOCATION IF LENGTHS ARE GIVEN THE                   
*-TOTAL SPOT LENGTHS MUST BE EQUAL.                                             
*                                                                               
*-THE ALLOCATED PIGGYBACK LENGTHS ARE MOVED INTO THE TABLE                      
         CLC   REQLEN(1),NBSELLEN                                               
         BNE   TRAPERR                                                          
         MVC   2(1,R5),NBSELLEN                                                 
         MVC   3(1,R5),HOLDLEN                                                  
         B     EXIT                                                             
*                                                                               
*-FOR SINGLE TO PIGGYBACK ALLOCATION IF A LENGTH IS GIVEN ONLY                  
*-FOR THE SINGLE PRODUCT. THE LENGTH WILL BE DIVIDED EQUALLY                    
*-BETWEEN EACH OF THE PIGGYBACK PAIRS. AN ODD LENGTH WILL                       
*-CAUSE AN ERROR CONDITION.                                                     
RELL120  ZIC   RF,REQLEN                                                        
         SR    RE,RE                                                            
         D     RE,=F'2'                                                         
         LTR   RE,RE                                                            
         BNZ   TRAPERR                                                          
         STC   RF,3(R5)                                                         
         AR    RF,RF                                                            
         STC   RF,2(R5)                                                         
         B     EXIT                                                             
*                                                                               
*-FOR SINGLE TO SINGLE ALLOCATION THE TOTAL SPOT LENGTH                         
*-REMAINS THE SAME.                                                             
*                                                                               
*-IF LENGTHS ARE ENTERED THE TOTAL LENGTHS MUST BE EQUAL.                       
RELL160  MVC   2(2,R5),=X'FF00'                                                 
         CLI   REQLEN,0                                                         
         BE    RELL180                                                          
         CLI   NBSELLEN,0                                                       
         BE    EXIT                                                             
         CLC   REQLEN(1),NBSELLEN                                               
         BNE   TRAPERR                                                          
         B     EXIT                                                             
*                                                                               
*-IN A SINGLE TO SINGLE ALLOCATION THE RECIEVING PRODUCT                        
*-CANNOT HAVE A LENGTH IF THE GIVING PRODUCT DOES NOT.                          
RELL180  CLI   NBSELLEN,0                                                       
         BNE   TRAPERR                                                          
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
********************************************************************            
*                                                                  *            
* READS UNIT RECORDS : POSTS TO PRODUCT BUCKETS - WHEN DONE GETS   *            
*                      PERCENT OF EACH PRODUCTS DOLLARS            *            
*                      THAN ALLOCATES NEW DOLLARS BY MONTH         *            
*                                                                  *            
*  RTN USED : PRNTPRD - PRINTS PRDS FOR NETWORK                    *            
*             FINLTOT - PRINTS PRD RECAP & FINAL TOTALS            *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
LR       DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         LA    R4,REQTAB                                                        
         LA    R5,1                                                             
LR20     CLI   0(R4),X'FF'                                                      
         BE    LR30                                                             
         STC   R5,0(R3)            PRODUCT NUMBER                               
         MVC   1(1,R3),4(R4)       PERCENT                                      
         LA    R3,2(R3)                                                         
         LA    R4,11(R4)                                                        
         LA    R5,1(R5)                                                         
         B     LR20                                                             
*                                                                               
LR30     GOTO1 =V(WGTLIST),DMCB,WORK,BRDTAB,XSORT,RR=RELO                       
         LA    R3,BRDTAB                                                        
         ST    R3,SAVER3                                                        
*--INITIALIZE SORT                                                              
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         B     LR100                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=15'                                    
*                                                                               
* NOW READ UNIT RECS                                                            
LR100    LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         MVI   NBDATA,C'U'         JUST UNITS                                   
         MVI   NBSELUOP,C'A'       JUST ACT DOLLARS                             
         LA    R1,LR203                                                         
         ST    R1,NBHOOK                                                        
         SPACE                                                                  
LR200    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBMODE,NBREQLST                                                  
         BNE   LR200                                                            
         B     LR240                                                            
*                                                                               
LR203    NTR1                                                                   
         L     R3,SAVER3           LOAD BRDTAB POINTER                          
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   NBNOWRIT,C'N'                                                    
         MVI   NBUPUNIT,C'N'                                                    
*                                                                               
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LR235                                                            
         SPACE                                                                  
         GOTO1 DATCON,(R1),(2,NBACTDAT),CURDTE                                  
         CLC   CURDTE,NBSELEND     AIR DATE NOT WITHIN MONTH                    
         BH    LR235                                                            
         CLC   CURDTE,NBSELSTR                                                  
         BL    LR235                                                            
         CLI   NBSELLEN,0                                                       
         BE    *+14                                                             
         CLC   NBSELLEN,NBLEN      CHECK FIRST LENGTH                           
         BNE   LR235                                                            
         CLI   HOLDLEN,0                                                        
         BE    LR205                                                            
         CLI   NBLEN1,0                                                         
         BE    LR205                                                            
         CLC   HOLDLEN,NBLEN1      CHECK PIGGY LENGTH                           
         BNE   LR235                                                            
LR205    CLI   UNASW,X'FF'         ONLY UNALOCATED UNITS                        
         BNE   *+12                                                             
         CLI   NBPRD,0                                                          
         BNE   LR235                                                            
         CLC   NBSELPRD,=CL3'POL'                                               
         BE    LR210                                                            
         CLI   NBPRD2,0            CHECK PIGGY PROD                             
         BE    LR210                                                            
         CLC   NBSELPIG,NBPRD      MUST BE A MATCH                              
         BNE   LR235                                                            
LR210    OC    NBBILTGR(36),NBBILTGR    CHECK PAID                              
         BNZ   LR235                                                            
         CLI   PIGSW,C'Y'          WAS INCLUDE PB'S REQUESTED                   
         BNE   LR220               NO CONTINUE                                  
         CLC   PIGPROD,NBPRD                                                    
         BE    LR220                                                            
         CLC   PIGPROD,NBPRD2                                                   
         BNE   LR235                                                            
*                                                                               
LR220    BAS   RE,ESTPRDCK         CHECK IF ESTIMATES OPEN                      
         CLI   HKDMCB+8,X'FF'      CHECK RETURN CODE                            
         BNE   LR230                                                            
         LA    R3,1(R3)            GET NEXT BRAND TABLE ENTRY                   
         CLI   0(R3),0             CHECK END OF TABLE                           
         BNE   *+8                                                              
         LA    R3,BRDTAB           START AT THE BEGINNING                       
         B     LR235                                                            
*                                                                               
LR230    L     RE,NUNITS                                                        
         LA    RE,1(RE)                                                         
         ST    RE,NUNITS                                                        
*                                                                               
         BAS   RE,CHGUNIT          UPDATE THE UNITS                             
         BAS   RE,PUTSORT          PUT IT OUT TO SORT                           
         LA    R3,1(R3)            GET NEXT BRAND TABLE ENTRY                   
         CLI   0(R3),0             CHECK END OF TABLE                           
         BNE   *+8                                                              
         LA    R3,BRDTAB           START AT THE BEGINNING                       
*                                                                               
         CLI   TESTSW,C'Y'         TEST RUN DONT UPDATE UNITS                   
         BE    *+12                                                             
         MVI   NBNOWRIT,C'Y'                                                    
         MVI   NBUPUNIT,C'Y'                                                    
*                                                                               
LR235    ST    R3,SAVER3           SAVE BRDTAB POINTER                          
         B     EXIT                READ NEXT UNIT                               
*                                                                               
*-READ FROM SORT                                                                
LR240    MVI   NBMODE,NBPROCUN                                                  
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,4(R1)                                                         
         MVC   RECWORK(15),0(R3)                                                
         LTR   R3,R3                                                            
         BZ    LR300                                                            
*                                                                               
         CLI   HOLDNET,X'40'                                                    
         BH    LR320                                                            
         MVC   HOLDNET,SORTSTA                                                  
         MVC   HOLDPRG,SORTPRG                                                  
         B     LR320                                                            
*                                                                               
         SPACE                                                                  
* END OF READ - TEST FOR ANYTHING TO PRINT                                      
LR300    MVI   NBMODE,NBREQLST                                                  
         L     RE,NUNITS                                                        
         LTR   RE,RE                                                            
         BNZ   LR320                                                            
         MVC   P(16),=C'NO DATA TO PRINT'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*--CHECK BREAKS AND PRINT OUTPUT                                                
LR320    CLI   NBMODE,NBREQLST                                                  
         BE    *+14                                                             
         CLC   HOLDPRG,SORTPRG                                                  
         BE    LR340                                                            
         MVI   PRINTLEV,C'P'                                                    
         BAS   RE,PRNTPRD                                                       
         MVC   HOLDPRG,SORTPRG                                                  
*                                                                               
LR340    CLI   NBMODE,NBREQLST                                                  
         BE    *+14                                                             
         CLC   HOLDNET,SORTSTA                                                  
         BE    LR360                                                            
         MVI   PRINTLEV,C'S'                                                    
         BAS   RE,PRNTPRD                                                       
         MVC   HOLDNET,SORTSTA                                                  
*                                                                               
LR360    CLI   NBMODE,NBREQLST                                                  
         BNE   LR380                                                            
         MVI   PRINTLEV,C'R'                                                    
         BAS   RE,PRNTPRD                                                       
         B     EXIT                                                             
*                                                                               
LR380    BAS   RE,ACCUM                                                         
         B     LR240                                                            
         SPACE                                                                  
         EJECT                                                                  
*                                                                               
*-PUTS RECORD TO SORTER                                                         
PUTSORT  NTR1                                                                   
         XC    RECWORK(15),RECWORK                                              
         MVC   SORTSTA,NBACTNET                                                 
         MVC   SORTPRG,NBACTPRG                                                 
         MVC   SORTPRD,0(R3)                                                    
         MVC   SORTACT,NBACTUAL                                                 
         GOTO1 SORTER,DMCB,=C'PUT',RECWORK                                      
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
*-CHECK IF ESTIMATES OPEN FOR A GIVEN PRODUCT                                   
ESTPRDCK NTR1                                                                   
         XC    HKDMCB(24),HKDMCB                                                
         LA    R4,REQTAB           THE PRODUCT LENGTH TABLE                     
         ZIC   R5,0(R3)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'11'                                                        
         AR    R4,R5               POSITION TO PROPER TABLE ENTRY               
         CLI   0(R4),0             CHECK FOR POL                                
         BE    EXIT                NO NEED TO VALIDATE                          
*                                                                               
         LA    R0,2                                                             
ESPR050  IC    R5,0(R4)                                                         
         GOTO1 =V(SPESTCHK),HKDMCB,((R5),NBAIO),ESTTAB,(RC)                     
         CLI   8(R1),0                                                          
         BNE   ESPR100             ESTIMATE NO OPEN ERROR                       
         LA    R4,1(R4)            CHECK PIGGYBACK                              
         CLI   0(R4),0                                                          
         BE    *+8                                                              
         BCT   R0,ESPR050                                                       
         B     EXIT                                                             
*                                                                               
ESPR100  LA    R2,P                                                             
         USING LISTD,R2                                                         
*                                                                               
         MVC   LSTA(38),=CL38'**ESTIMATE NOT OPEN FOR THIS PRODUCT**'           
         MVC   LSTA+40(4),NBACTNET                                              
         MVC   LSTA+45(6),NBACTPRG                                              
         EDIT  (1,NBACTEST),(3,LSTA+52)                                         
         LA    R4,5(R4)            GET FIRST PRODUCT LITERAL                    
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R4,3(R4)            GET SECOND PRODUCT LITERAL                   
         MVC   LSTA+56(3),0(R4)                                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   NBNOWRIT,C'N'       DONT WRITE RECORD BACK                       
         MVI   NBUPUNIT,C'N'                                                    
         B     EXIT                                                             
         DROP  R2                                                               
*-CHANGE THE PRODUCT AND LENGTH FIELDS ON THE UNIT RECORDS                      
*-R3 POINTS TO THE BRAND TABLE                                                  
CHGUNIT  NTR1                                                                   
         L     R2,NBAIO                                                         
         USING NURECD,R2                                                        
*                                                                               
         LA    R4,REQTAB           THE PRODUCT LENGTH TABLE                     
         ZIC   R5,0(R3)                                                         
         BCTR  R5,0                                                             
         MH    R5,=H'11'                                                        
         AR    R4,R5               POSITION TO PROPER TABLE ENTRY               
*                                                                               
*  GET TRAFFIC ELEMENT TELL TRAFFIC OF THE PRODUCT LENGTH CHANGES               
*  IF ELEMENT DOESNT EXIST ADD IT                                               
*                                                                               
         MVI   SRCHEL,X'21'                                                     
         LA    R5,NUMAINEL                                                      
         BAS   RE,NEXTEL                                                        
         BE    CHGU050                                                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,X'21'                                                    
         MVI   ELEMENT+1,80                                                     
         L     R5,NBAIO                                                         
         GOTO1 HELLO,DMCB,(C'P',UNTFILE),(0,(R5)),ELEMENT,0                     
*                                                                               
         MVI   SRCHEL,X'21'                                                     
         LA    R5,NUMAINEL                                                      
         BAS   RE,NEXTEL                                                        
         BE    CHGU050                                                          
         DC    H'0'                                                             
         USING NUCMLEL,R5                                                       
*                                                                               
CHGU050  CLI   PIGSW,C'Y'                                                       
         BNE   CHGU100                                                          
*-INCLUDE PIGGYBACK LOGIC                                                       
*        GOTO1 =V(PRNTBL),DMCB,=C'BPRD',NUPRD,C'DUMP',2,=C'1D'                  
         CLC   PIGPROD,NUPRD                                                    
         BNE   *+14                                                             
         MVC   NUPRD,0(R4)                                                      
         OI    NUCMLFLG,X'40'                                                   
*                                                                               
         CLC   PIGPROD,NUPRD2                                                   
         BNE   *+14                                                             
         MVC   NUPRD2,0(R4)                                                     
         OI    NUCMLFLG,X'40'                                                   
*        GOTO1 =V(PRNTBL),DMCB,=C'APRD',NUPRD,C'DUMP',2,=C'1D'                  
         B     EXIT                                                             
*-STANDARD REALLOCATION                                                         
*        GOTO1 =V(PRNTBL),DMCB,=C'BRAN',(R3),C'DUMP',1,=C'1D'                   
*        GOTO1 =V(PRNTBL),DMCB,=C'BPRD',NUPRD,C'DUMP',2,=C'1D'                  
*        GOTO1 =V(PRNTBL),DMCB,=C'BLEN',NULEN,C'DUMP',1,=C'1D'                  
*        GOTO1 =V(PRNTBL),DMCB,=C'BLN1',NULEN1,C'DUMP',1,=C'1D'                 
CHGU100  MVC   NUPRD(2),0(R4)                                                   
         OI    NUCMLFLG,X'40'                                                   
         CLI   2(R4),X'FF'                                                      
         BE    *+14                                                             
         MVC   NULEN,2(R4)                                                      
         OI    NUCMLFLG,X'80'                                                   
         CLI   3(R4),X'FF'                                                      
         BE    *+14                                                             
         MVC   NULEN1,3(R4)                                                     
         OI    NUCMLFLG,X'80'                                                   
*        GOTO1 =V(PRNTBL),DMCB,=C'APRD',NUPRD,C'DUMP',2,=C'1D'                  
*        GOTO1 =V(PRNTBL),DMCB,=C'ALEN',NULEN,C'DUMP',1,=C'1D'                  
*        GOTO1 =V(PRNTBL),DMCB,=C'ALN1',NULEN1,C'DUMP',1,=C'1D'                 
*-CHANGE ACTIVITY ELEMENT                                                       
         MVC   NBUSERID,SPLUID                                                  
         MVC   NBSECAGY,SPLUID+2                                                
         CLI   SPLRSN,X'40'         CHECK IF REASON GIVEN                       
         BH    *+8                                                              
         MVI   SPLRSN,C'X'          NO, SET TO NO REASON                        
         NETGO NVBDACTY,DMCB,SPLRSN                                             
         GOTO1 HELLO,DMCB,(C'G',UNTFILE),(X'99',(R2)),ELEMENT,0                 
         CLI   12(R1),0            TEST IF PKG GUARANTEE FOUND                  
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
*        GOTO1 =V(PRNTBL),DMCB,=C'BPRD',(R3),C'DUMP',30,=C'1D'                  
*                                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         SPACE 2                                                                
*-ADD TO ACCUMULATORS                                                           
ACCUM    NTR1                                                                   
         LA    R4,PROGTOTS                                                      
         BAS   RE,ADDTOT                                                        
*                                                                               
         ZIC   R5,SORTPRD                                                       
         MH    R5,=H'4'                                                         
         AR    R4,R5               POSITION TO PROPER TABLE ENTRY               
         BAS   RE,ADDTOT                                                        
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
ADDTOT   NTR1                                                                   
         LA    R1,3                                                             
ADDT100  L     RE,0(R4)                                                         
         LA    RE,1(RE)                                                         
         ST    RE,0(R4)                                                         
*                                                                               
         LA    R4,52(R4)                                                        
         BCT   R1,ADDT100                                                       
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* SET UP PRINT LINE                                                             
PRNTPRD  NTR1                                                                   
         LA    R2,P                                                             
         USING LISTD,R2                                                         
*-CHECK THE PRINT LEVEL                                                         
         CLI   PRINTLEV,C'P'       PROGRAM LEVEL                                
         BE    PP030                                                            
         CLI   PRINTLEV,C'S'       STATION LEVEL                                
         BE    PP020                                                            
*-REPORT LEVEL                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   LSTA(12),=CL12'REPORT TOTAL'                                     
         LA    R4,REPTTOTS                                                      
         B     PP050                                                            
*                                                                               
*-STATION LEVEL                                                                 
PP020    MVC   LSTA(13),=CL13'STATION TOTAL'                                    
         LA    R4,STATTOTS                                                      
         B     PP050                                                            
*                                                                               
*-PROGRAM LEVEL                                                                 
PP030    MVC   LSTA,HOLDNET                                                     
         MVC   LPRG,HOLDPRG                                                     
         LA    R4,PROGTOTS                                                      
*                                                                               
PP050    ST    R4,SAVER4                                                        
         EDIT  (4,0(R4)),(7,LSPTSU)                                             
*                                                                               
         LA    R3,REQTAB           PRODUCT TABLE                                
         LA    R4,4(R4)            ACCUMULATORS                                 
         LA    R5,LSPTS1           PRINT FIELD                                  
PP100    CLI   0(R3),X'FF'                                                      
         BE    PPEX                                                             
         EDIT  (4,0(R4)),(7,0(R5))                                              
         LA    R3,11(R3)                                                        
         LA    R4,4(R4)                                                         
         LA    R5,9(R5)                                                         
         B     PP100                                                            
*                                                                               
PPEX     L     R4,SAVER4                                                        
         XC    0(52,R4),0(R4)      CLEAR ACCUMULATOR                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
*                                                                               
TRAPERR2 GOTO1 ERREX2                                                           
         SPACE                                                                  
*                                                                               
         EJECT                                                                  
HDRTN    NTR1                                                                   
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H1+14(4),HOLDNET                                                 
         MVC   H4(6),=C'CLIENT'                                                 
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H4+15(20),SPLCLIN                                                
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(7),SPLEST                                                  
         MVC   H5+15(20),SPLESTN                                                
         MVC   H6(7),=C'PACKAGE'                                                
         MVC   H6+10(3),SPLPAK                                                  
         MVC   H6+15(34),SPLPAKN                                                
         SPACE                                                                  
         OC    NBSELSTR,NBSELSTR                                                
         BZ    HD050                                                            
         MVC   H3+49(6),=C'PERIOD'                                              
         GOTO1 DATCON,DMCB,(0,NBSELSTR),(5,H3+57)                               
         MVI   H3+66,C'-'                                                       
         GOTO1 DATCON,DMCB,(0,NBSELEND),(5,H3+68)                               
         SPACE                                                                  
HD050    LA    R2,H10                                                           
         USING LISTD,R2                                                         
         MVC   LSPTSU(7),=C'--ALL--'                                            
*                                                                               
         LA    R3,REQTAB           PRODUCT LITERALS                             
         LA    RE,LSPTS1                                                        
         LA    RF,12                                                            
HD100    CLI   0(R3),X'FF'                                                      
         BE    HD120                                                            
         MVC   0(7,RE),=C'-------'                                              
         MVC   0(3,RE),5(R3)                                                    
         CLI   8(R3),X'40'         CHECK FOR PIGGYBACK CODE                     
         BNH   HD110                                                            
         MVI   3(RE),C'/'                                                       
         MVC   4(3,RE),8(R3)                                                    
HD110    LA    R3,11(R3)                                                        
         LA    RE,9(RE)                                                         
         BCT   RF,HD100                                                         
*                                                                               
HD120    LA    R2,H11                                                           
         MVC   LSTA(3),=C'STA'                                                  
         MVC   LPRG(4),=C'PROG'                                                 
*                                                                               
         LA    R3,REQTAB           PRODUCT LITERALS                             
         LA    RE,LSPTSU                                                        
         LA    RF,12                                                            
         B     HD160                                                            
HD150    CLI   0(R3),X'FF'                                                      
         BE    HD170                                                            
         LA    R3,11(R3)                                                        
         LA    RE,9(RE)                                                         
HD160    MVC   0(7,RE),=C'-UNITS--'                                             
         BCT   RF,HD150                                                         
*                                                                               
HD170    LA    R2,H12                                                           
         MVC   LSTA(4),=C'----'                                                 
         MVC   LPRG(6),=C'------'                                               
*                                                                               
         LA    R3,REQTAB           PRODUCT LITERALS                             
         LA    RE,LSPTSU                                                        
         LA    RF,13                                                            
         B     HD200                                                            
HD190    CLI   0(R3),X'FF'                                                      
         BE    HDX                                                              
         LA    R3,11(R3)                                                        
         LA    RE,9(RE)                                                         
HD200    MVC   0(7,RE),=C'-------'                                              
         BCT   RF,HD190                                                         
*                                                                               
HDX      B     EXIT                 (XIT1)                                      
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,50,C'NETPAK RE-ALLOCATION REPORT'                             
         SSPEC H2,50,C'---------------------------'                             
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
         GETEL R5,NBDTADSP,SRCHEL                                               
         LTORG                                                                  
         DS    0D                                                               
         DC    CL8'*ESTTAB*'                                                    
UNTFILE  DC    CL8'UNTFIL'                                                      
ESTTAB   DS    1024C                                                            
         EJECT                                                                  
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2                         
         DS    0F                                                               
*                                                                               
HOLDLEN  DS    CL1                 LENGTH OF FIRST PRODUCT                      
HOLDNET  DS    CL4                 STATION HOLD AREA                            
HOLDPRG  DS    CL6                 PROGRAM CODE HOLD AREA                       
PRINTLEV DS    CL1                 PRINTING LEVEL                               
REQPROD  DS    CL2                 REQUESTED PRODUCT                            
REQLEN   DS    CL2                 REQUESTED LENGTH                             
UNASW    DS    CL1                 READ ONLY UNALOCATED UNITS                   
TESTSW   DS    CL1                 CHECK TO UPDATE UNITS                        
PIGSW    DS    CL1                 INCLUDE PB'S SWITCH                          
PIGPROD  DS    CL1                 REQUESTED PRODUCT CODE                       
CURDTE   DS    XL6                 UNITS 6 BYTE DATE                            
REQTAB   DS    CL133               BYTE 1-2  PRODUCTS                           
*                                  BYTE 3-4  LENGTH'S                           
*                                  BYTE 5    PERCENT                            
*                                  BYTE 6-11 PRODUCT CODES                      
BRDTAB   DS    CL128               BRAND ALLOCATION TABLE                       
*                                                                               
*--F1=REQUESTED PRODUCT UNIT COUNT                                              
*--F2=PRODUCT 1 ALLOCATION UNIT COUNT                                           
*--F3=PRODUCT 2 ALLOCATION UNIT COUNT                                           
*--F4=PRODUCT 3 ALLOCATION UNIT COUNT                                           
*--F5=PRODUCT 4 ALLOCATION UNIT COUNT                                           
*--F6=PRODUCT 5 ALLOCATION UNIT COUNT                                           
*--F7=PRODUCT 6 ALLOCATION UNIT COUNT                                           
*--F8=PRODUCT 7 ALLOCATION UNIT COUNT                                           
*--F9=PRODUCT 8 ALLOCATION UNIT COUNT                                           
*--F10=PRODUCT 9 ALLOCATION UNIT COUNT                                          
*--F11=PRODUCT 10 ALLOCATION UNIT COUNT                                         
*--F12=PRODUCT 11 ALLOCATION UNIT COUNT                                         
*--F13=PRODUCT 12 ALLOCATION UNIT COUNT                                         
PROGTOTS DS    13F                 PROGRAM LEVEL ACCUMULATORS                   
STATTOTS DS    13F                 STATION LEVEL ACCUMULATORS                   
REPTTOTS DS    13F                 REPORT LEVEL ACCUMULATORS                    
*                                                                               
RELO     DS    F                                                                
NUNITS   DS    F                   NUMBER OF UNITS PROCESSED                    
SAVER4   DS    F                   R4 STORAGE AREA                              
SAVER3   DS    F                   R3 STORAGE AREA                              
HKDMCB   DS    6F                  ALTERNATE DMCB AREA                          
*                                                                               
RECWORK  DS    0CL15                                                            
SORTSTA  DS    CL4                                                              
SORTPRG  DS    CL6                                                              
SORTPRD  DS    CL1                                                              
SORTACT  DS    CL4                                                              
SRCHEL   DS    CL1                 ELEMENT LOOK UP                              
         SPACE 2                                                                
*                                                                               
LISTD    DSECT                                                                  
LSTA     DS    CL4                                                              
         DS    CL1                                                              
LPRG     DS    CL6                                                              
         DS    CL5                                                              
LSPTSU   DS    CL7                                                              
         DS    CL2                                                              
LSPTS1   DS    CL7                                                              
         DS    CL2                                                              
LSPTS2   DS    CL7                                                              
         DS    CL2                                                              
LSPTS3   DS    CL7                                                              
         DS    CL2                                                              
LSPTS4   DS    CL7                                                              
         DS    CL2                                                              
LSPTS5   DS    CL7                                                              
         DS    CL2                                                              
LSPTS6   DS    CL7                                                              
         DS    CL2                                                              
LSPTS7   DS    CL7                                                              
         DS    CL2                                                              
LSPTS8   DS    CL7                                                              
         DS    CL2                                                              
LSPTS9   DS    CL7                                                              
         DS    CL2                                                              
LSPTS10  DS    CL7                                                              
         DS    CL2                                                              
LSPTS11  DS    CL7                                                              
         DS    CL2                                                              
LSPTS12  DS    CL7                                                              
         SPACE 2                                                                
*                                                                               
******************** S P E S T C H K ****************************               
*                                                               *               
* SUBROUTINE MAINTAINS TABLE OF ESTIMATE NUMBERS OPEN BY BRAND  *               
* FIRST 32 BYTES ARE LIST OF ESTIMATE NUMBERS IN TABLE          *               
* FOLLOWED BY 4  BYTE ENTRY FOR EACH BRAND (MAX 220)            *               
* IF MORE THAN 32 ESTIMATES OPEN, ABEND OCCURS                  *               
*                                                               *               
* PARAM 1     BYTE  0    PRD CODE                               *               
*                  1-3   BUYREC ADDRESS                         *               
*                                                               *               
* PARAM 2           4                                           *               
*                  5-7   TABLE ADDRESS (1024X'00' FIRST TIME)   *               
*                                                               *               
* PARAM 3           8    ON RETURN X'00' = EST FOUND            *               
*                                  X'FF' = NOT FOUND            *               
*                  9-11  GEND ADDRESS                           *               
*                                                               *               
*****************************************************************               
         SPACE 2                                                                
SPESTCHK CSECT                                                                  
         NMOD1 0,SPESTCHK                                                       
         L     RC,8(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         LR    R2,R1               SAVE PARAM POINTER                           
         MVI   8(R2),0             CLEAR ERROR IND                              
         L     R3,0(R1)            GET A(REC)                                   
* TEST EST NUM IN TABLE                                                         
         LA    R5,X'80'            SET MASK                                     
         SLL   R5,24               GET IT IN LEFTMOST BIT                       
         L     R1,4(R2)            POINT TO TABLE                               
ESTCHK2  CLI   0(R1),0             TEST E-O-L                                   
         BE    ESTCHK4                                                          
         CLC   0(1,R1),17(R3)      TEST MATCH                                   
         BE    ESTCHK10                                                         
         LA    R1,1(R1)                                                         
         SRL   R5,1                SHIFT MASK                                   
         LTR   R5,R5                                                            
         BNZ   ESTCHK2                                                          
         DC    H'0'                TABLE FULL                                   
*                                                                               
ESTCHK4  MVC   0(1,R1),17(R3)      SET EST NUM IN TABLE                         
*                                                                               
ESTCHK10 DS    0H                                                               
         ZIC   R4,0(R2)            GET PRD CODE                                 
         CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),220                                                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         SLL   R4,2                X 4                                          
         A     R4,4(R2)            ADD TABLE ADDRESS                            
         LA    R4,32(R4)           AND ADD DSPL TO FIRST ENTRY                  
         ST    R5,ESTMASK          SAVE MASK BIT                                
         NC    ESTMASK,0(R4)       TEST EST FOUND PREVIOUSLY                    
         BNZ   ESTCHKX             YES - EXIT                                   
* MUST READ FOR EST KEY                                                         
         ST    R5,ESTMASK          SAVE MASK BIT AGAIN                          
         MVC   ESTCHKSV,KEY        SAVE CONTENTS OF KEY                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),1(R3)      A-M/CLT                                      
         MVC   KEY+7(1),17(R3)     EST                                          
         L     R6,ANETWS3                                                       
         USING CLTHDR,R6                                                        
         LA    R6,CLIST                                                         
         DROP  R6                                                               
*                                                                               
ESTCHK12 CLC   3(1,R6),0(R2)                                                    
         BE    ESTCHK14                                                         
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BH    ESTCHK12                                                         
         DC    H'0'                                                             
*                                                                               
ESTCHK14 MVC   KEY+4(3),0(R6)      PRD CODE                                     
         MVI   8(R2),X'FF'         PRESET EST NOT FOUND                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR  ',KEY,KEY,0               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   *+14                                                             
         OC    0(4,R4),ESTMASK     'OR' IN MASK                                 
         MVI   8(R2),0             AND RESET ERROR FLAG                         
* RESTORE KEY FOR SEQ READING                                                   
         MVC   KEY,NBKEY                                                        
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'UNTDIR  ',KEY,KEY,0               
*                                                                               
ESTCHKX  XMOD1 1                                                                
*                                                                               
ESTCHKSV DS    XL20                                                             
ESTMASK  DS    F                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
ESTD     DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF8D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FASSB                                                          
       ++INCLUDE DDMASTC                                                        
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'177NEMED88   05/01/02'                                      
         END                                                                    
