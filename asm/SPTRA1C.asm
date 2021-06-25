*          DATA SET SPTRA1C    AT LEVEL 023 AS OF 11/09/20                      
*PHASE T2161CB                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T2161C - TRAFFIC NETWORK ASSIGNMENT PROGRAM                 *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM WILL ASSIGN COMMERCIALS TO NETWORK UNITS    *         
*            TO ALLOW NETWORK INSTRUCTIONS TO BE RUN.                 *         
*                                                                     *         
*  OUTPUTS: UPDATED NETWORK UNIT RECORDS, OPTIONALLY REVISION         *         
*           COMMENTS RECORDS                                          *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - POINTER TO NETBLOCKD                                  *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO UNIT TABLE                      *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 - NETBLOCK  1ST 1024                               *         
*                    PRD/EST/COPY CODE NEXT 765                       *         
*                    READ STATION RECORD INTO AIO2+2004               *         
*             AIO3 - NETIO                                            *         
*                    PATTERN TABLE                                    *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2161C NETWORK TRAFFIC - ASSIGN COMMERCIALS'                    
***********************************************************************         
*                                                                     *         
* LEV 192 BGRI NOV01/05 MORE PRODUCTS                                 *         
* LEV 193 SMUR JUN21/06 FIX REASSIGN IN CUT-IN                        *         
* LEV 194 SMUR JUL20/06 BYPASS BAD X'21' ELEM                         *         
* LEV 194 MNAS JUL28/06 FIX PROBLEMS IN BPLAT (BUILD PATTERN TABLE)   *         
* LEV 194 MNAS AUG02/06 FIX MORE PROBLEMS IN BPLAT (BUILD PTRN TABLE) *         
* LEV 194 MNAS NOV14/06 FIX NETIO PROBLEM IN SRV ROUTINE - WHEN UNIT  *         
*                       RECORD IS > 2000 IS CREAMING NETBLOCK         *         
* LEV 194 MNAS DEC11/06 FIX COPY SPLIT PROBLEM - PROCESSING COPY      *         
*                       SPLIT AS PIGGYBACKS                           *         
* LEV 195 SMUR FEB09/07 FIX NETWORK REVISION NUMBER                   *         
*     196 MNAS FEB28/07 MAKE UNIT TABLE SMALLER FOR 6K CHANGES (MNAS) *         
*         SMUR MAR06/07 DELETE GETCML ROUTINE (NOT USED)              *         
*         SMUR MAR20/07 STOP REV FROM INCREMENTING WHEN NO INST RUN   *         
*         SMUR APR13/07 FIX REASSIGN ON UNITS WITH BILLBOARD          *         
* LEV 201 SMUR NOV09/07 NOP BAD INSTRUCTION IN C/S LOGIC              *         
* LEV 202 SMUR DEC27/07 ALLOW SWITCHING BETWEEN CAL/BROAD/WEE PERIODS *         
*                       INCREASE PGROUPTABLE TO 150                             
* LEV 212 MNAS DEC24/09 BUG WHEN PROCESSING OTHERS FIELD AND P/B IS   *         
*                       LAST UNIT ON SCREEN - SEE VC125               *         
* LEV 213 SMUR JAN20/10 VALIDATE PRD= OPTION AGAINST TN2PR1 PROFILE   *         
* LEV 007 SMUR JUL06/10 FIX ADID ON FEED ELEMENT                      *         
* LEV 008 SMUR DEC20/10 SUPPORT LEN=22/23 FOR 45 SEC P/B UNIT         *         
* LEV 016 SMUR MAR31/10 FIX VIGNETTE LOGIC WHEN CALLED FROM BLUNT ROUT*         
* LEV 021 MNAS JUN11/13 ADD PROFILFE LOGIC TO INCLUDE/EXCLUDE DIGITAL *         
* SPEC-42501  SMUR JAN24/20 SUPPORT CONVERTED DATES ON REVISION RECS  *         
*                     **** TERMINUS ****                              *         
***********************************************************************         
         TITLE 'T2161C NETWORK TRAFFIC - ASSIGN COMMERCIALS'                    
T2161C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2161C**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR1CRR                                                      
         L     RE,=A(OPTPGRL)                                                   
         AR    RE,R3                                                            
         ST    RE,AOPTPGRL                                                      
*                                                                               
         L     R3,AIO2                                                          
         USING NETBLOCKD,R3                                                     
*                                                                               
         L     R0,LSYSD                                                         
         AR    R0,R9                                                            
         LR    R1,R9                                                            
         AHI   R1,ENDSYSD-SYSD                                                  
         CR    R0,R1                                                            
         BNL   *+6                                                              
         DC    H'0'                USED MORE THAN SYSD                          
*                                                                               
         LHI   R1,SVSTART-SYSD+11046  START OF SAVED STOR                       
         AR    R1,R9                                                            
         ST    R1,ASVSTOR                                                       
*                                                                               
         LA    R1,UNITABLE                                                      
         AHI   R1,L'UNITABLE                                                    
         ST    R1,MAXUNT                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A1D'  GETBROAD                              
         MVC   VGTBROAD,0(R1)                                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 CALLOV,DMCB,0,X'D9000AFE' TRPACK                                 
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A50'  QSORT                                 
         MVC   QSORT,DMCB                                                       
*                                                                               
         CLI   TRLSTPGM,X'1C'      WAS NET ASSIGN LAST PROG USED                
         BE    MAIN10               YES, DONE                                   
*                                                                               
         NI    TRACLTH+4,X'FF'-X'20'                                            
         OC    CONWHEN,CONWHEN     PRINT OPTION                                 
         BZ    VK                                                               
*                                                                               
         TM    CONWHENH+4,X'80'    INPUT THIS TIME                              
         BO    VK                                                               
*                                                                               
         XC    CONWHEN,CONWHEN     NO, CLEAR PRINT FIELD                        
         NI    CONWHENH+4,X'FF'-X'40'  TURN OFF INPUT PREVIOUSLY                
         OI    CONWHENH+6,X'80'    TRANSMIT                                     
         B     VK                                                               
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
* IF ALL FIELDS VALIDATED, DO CML ASSIGNMENT *                                  
*                                                                               
VK       DS    0H                                                               
         CLI   TMPSTSW,C'Y'                                                     
         BNE   VK01                                                             
         L     RE,ATWA                                                          
         MVI   DMCB+8,4                                                         
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),2(RE)                                                 
         ICM   R0,12,=C'L='                                                     
         ICM   R0,3,=Y(TABSAVL)                                                 
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,AOPTPGRL,,(R0)              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VK01     DS    0H                                                               
         TM    TRACLTH+4,X'20'                                                  
         BZ    VK02                                                             
         TM    TRANETH+4,X'20'                                                  
         BZ    VK02                                                             
         TM    TRAPRGH+4,X'20'                                                  
         BZ    VK02                                                             
         TM    TRAPERH+4,X'20'                                                  
         BZ    VK02                                                             
         B     VC                                                               
         EJECT                                                                  
* VALIDATE KEYS                                                                 
*                                                                               
* READ ANY REVISION RECORD TO SEE IF THE FILE HAS BEEN CONVERTED.               
VK02     NI    SVFLAG,X'FF'-CONVSW INIT CONVERTED RECORDS                       
         BRAS  RE,INITXSP          SET TO XSPOT                                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A1D'                                                  
         GOTO1 HIGH                                                             
         CLC   =X'0A1D',KEY        ANY REV REC?                                 
         BE    VK03                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         MVC   0(L'NOTEMS1,R1),NOTEMS1                                          
         LA    R1,L'NOTEMS1(,R1)                                                
         MVC   0(2,R1),AGENCY                                                   
         MVC   3(3,R1),QCLT                                                     
         MVC   7(4,R1),TRANET                                                   
         MVC   12(6,R1),TRAPRG                                                  
         MVC   20(8,R1),TRAPER                                                  
                                                                                
         OC    ELEM,SPACES                                                      
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'NOTEMS1+72,ELEM)                       
         B     VK03C               NO REC FOUND, TREATE AS CONVERTED            
*                                                                               
VK03     TM    KEY+32,X'03'        CONVERTED RECORDS?                           
         BZ    *+8                                                              
VK03C    OI    SVFLAG,CONVSW                                                    
*                                                                               
         BRAS  RE,INITSPT         CHANGE TO SPOT                                
*                                                                               
         MVC   TRAOPT1,=C'OPTIONS'                                              
         XC    TRAOPT1+1(6),SPACES                                              
         OI    TRAOPT1H+6,X'80'    TRANS                                        
         MVI   CMLFLAG1,0                                                       
*                                                                               
         LA    RE,NXTUNIT                                                       
         LA    RF,INITALL                                                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    EQVPTBL,EQVPTBL                                                  
         XC    UNITABLE(256),UNITABLE                                           
*                                                                               
         LA    RE,TIMETBL          CLEAR TIMETBL                                
         LA    RF,L'TIMETBL                                                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,MCODETBL         CLEAR MAP CODE TABLE                         
         LA    RF,L'MCODETBL                                                    
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
* FAKE VALIDATE MEDIA *                                                         
*                                                                               
         LA    R2,ELEM             FAKE VALIDATE MEDIA                          
         MVC   ELEM,=X'0A01000184010001'                                        
         MVI   ELEM+8,C'N'                                                      
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH                                                       
         GOTO1 VALICLT                                                          
*                                                                               
         XC    WORK,WORK           * READ TN PROFILE *                          
         MVC   WORK(4),=C'S0TN'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VK04                                                             
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    VK04                                                             
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
*                                                                               
VK04     GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVC   MYTN2PR6,SVPROF+1                                                
*                                                                               
         MVC   WORK(4),=C'STN1'    READ TN1 PROFIILE                            
         NI    WORK,X'FF'-X'40'    MAKE S LOWERCASE                             
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         MVC   WORK(4),=C'S0N0'    * READ N0 PROFILE *                          
*                                                                               
         GOTO1 (RF),(R1),,NBUSER                                                
*                                                                               
         LA    R2,TRANETH          NETWORK                                      
         XC    NETWORK,NETWORK                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             MUST BE ENTRY                                
         BRAS  RE,VNET                                                          
         BE    *+14                                                             
         MVC   GERROR,=Y(NONET)                                                 
         B     TRAPERR2                                                         
*                                                                               
         NI    TRAPERH+4,X'FF'-X'20'                                            
*                                                                               
         MVC   WORK(4),=C'STN2'    READ TN2 PROFILE AFTER VNET                  
         NI    WORK,X'FF'-X'40'    MAKE S LOWERCASE                             
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),SVMEDIA   BY SPECIFIC MEDIA!!!                         
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         MVC   WORK+16(16),WORK    SAVE PROFILE KEY                             
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    VK06                                                             
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    VK06                                                             
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
*                                                                               
VK06     GOTO1 GETPROF,DMCB,WORK,SVTN2PRO,DATAMGR                               
*                                                                               
         CLI   SVTN2PRO+5,0                                                     
         BE    *+18                                                             
         CLI   SVTN2PRO+5,C'0'                                                  
         BE    *+10                                                             
         MVC   MYTN2PR6,SVTN2PRO+5 OVERRIDE TN PROFILE                          
*                                                                               
         MVC   WORK(16),WORK+16    RESTORE PROFILE KEY                          
         MVC   WORK+1(3),=C'TN3'                                                
         GOTO1 GETPROF,DMCB,WORK,SVTN3PRO,DATAMGR                               
                                                                                
*                                  VALIDATE OPTIONS                             
         BRAS  RE,VOPT                                                          
*MNV                                                                            
         CLI   SVTN2PRO+14,C'Y'                                                 
         BNE   VK08                                                             
         TM    FLTODOSW,OVRDIGI                                                 
         BO    VK08                                                             
         CLI   SVMEDIA,C'V'                                                     
         BNE   VK08                                                             
         MVC   GERROR,=Y(BDNETMED)                                              
         B     TRAPERR2                                                         
                                                                                
VK08     DS    0H                                                               
*MNV                                                                            
*                                  VALIDATE PERIOD                              
         BRAS  RE,VPER                                                          
*                                  GET ANY DATES                                
         BRAS  RE,VOPT                                                          
*                                                                               
         LA    R2,TRAPRGH          PROGRAM                                      
         XC    PROGRAM,PROGRAM                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         BRAS  RE,VPROG                                                         
         EJECT                                                                  
         OI    TRACLTH+4,X'20'     SET ON VALIDATED                             
         OI    TRANETH+4,X'20'                                                  
*                                                                               
         BAS   RE,SVTWA                                                         
*                                                                               
         BAS   RE,CLRSCRT          CLEAR SCREEN                                 
*                                                                               
* CLEAR UNIT TABLE                                                              
*                                                                               
         LA    RE,UNITABLE                                                      
         LHI   RF,L'UNITABLE+1                                                  
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR TABLE                                  
*                                                                               
* GO TO SEE IF UNITS ARE LOCKED OUT - CONTROLLER WILL GIVE ERR IF SO *          
*                                                                               
         XC    DUB,DUB                                                          
         MVI   BYTE,0                                                           
         MVC   DUB(4),=C'TNET'                                                  
         GOTO1 VALILOC,0                                                        
*                                                                               
* IF RUNNING FOR ALL PRODUCTS, SEE THAT NONE OF THE PRODS ARE LOCKED            
*                                                                               
         CLI   SVTN2PRO+00,C'0'                                                 
         BE    *+12                                                             
         CLI   SVTN2PRO+00,0                                                    
         BNE   TLOCK30             NOT AN ALL PRODUCT RUN                       
*                                                                               
* GET PRODUCTS FOR THIS CLIENT                                                  
*                                                                               
         BAS   RE,INITSPT                                                       
*                                                                               
         XC    SVCPROD,SVCPROD     INIT PROD                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DF1'      PROD REC                                    
         MVC   KEY+2(3),BAGYMD    & BCLT                                        
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
TLOCK15  CLC   KEY(5),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   KEY+5,X'FF'         LAST REC FOR THIS CLT                        
         BNE   *+14                                                             
         XC    SVCPROD,SVCPROD     CLEAR PROD                                   
         B     TLOCKX               YES, DONE                                   
*                                                                               
         MVC   SVCPROD,KEY+6       SAVE 3 CHAR PROD                             
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
*                                                                               
         GOTO1 VALILOC,SVCPROD                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVI   DUB+2,X'FF'         NETWORK=ALL                                  
         MVC   DUB+3(1),SVMEDIA    MEDIA                                        
*                                                                               
         GOTO1 VALILOC,SVCPROD                                                  
*                                                                               
         GOTO1 SEQ                 GET NEXT PROD REC                            
         B     TLOCK15                                                          
*                                                                               
* IF RUNNING BY PRODUCT, SEE THAT THIS PRD IS NOT LOCKED                        
*                                                                               
TLOCK30  CLI   SVTN2PRO+00,C'*'                                                 
         BNE   TLOCK50                                                          
         MVC   BYTE,SVTN2PRO+00                                                 
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
*                                                                               
         MVC   SVCPROD,VOPTPROD    THIS PROD LOCKED?                            
         GOTO1 VALILOC,SVCPROD                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVI   DUB+2,X'FF'         NETWORK=ALL                                  
         MVC   DUB+3(1),SVMEDIA    MEDIA                                        
         GOTO1 VALILOC,SVCPROD                                                  
         B     TLOCKX                                                           
*                                                                               
* IF RUNNING BY PGROUP, SEE THAT THIS PGROUP IS NOT LOCKED                      
*                                                                               
TLOCK50  OC    VOPTPRGR,VOPTPRGR   PRODUCT GROUP                                
         BNZ   *+6                                                              
         DC    H'0'                BUG CATCHER                                  
*                                                                               
         MVC   BYTE,SVTN2PRO+00                                                 
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
         MVC   DUB+6(2),VOPTPRGR   THIS PGROUP LOCKED?                          
         GOTO1 VALILOC,0                                                        
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVI   DUB+2,X'FF'         NETWORK=ALL                                  
         MVC   DUB+3(1),SVMEDIA    MEDIA                                        
         MVC   DUB+6(2),VOPTPRGR   THIS PGROUP LOCKED?                          
         GOTO1 VALILOC,0                                                        
*                                                                               
TLOCKX   DS    0H                                                               
*                                                                               
* GO BUILD LIST OF NETWORK UNITS AND FEEDS *                                    
*                                                                               
         MVI   SKIPVGN,C'Y'                                                     
         BRAS  RE,BLU                                                           
         MVI   SKIPVGN,C'N'                                                     
*                                                                               
         BRAS  RE,FREV             FIND ORIG/CURRENT REVISION NUMBER            
*                                                                               
         TM    UNITSW1,X'10'       USE PATTERNS TO SEED COMMLS                  
         BZ    VK10                 NO                                          
         BRAS  RE,BLPAT                                                         
*                                                                               
VK10     BAS   RE,INITSPT          SET FROM NET TO SPOT                         
*                                                                               
         BRAS  RE,GETDTM                                                        
*                                                                               
         OI    UNITSW,X'02'        SET ON TABLE BUILT                           
         BAS   RE,SVTWA                                                         
         MVC   TRAOPT1,=C'FILTERS'                                              
         XC    TRAOPT1+1(6),SPACES                                              
         OI    TRAOPT1H+6,X'80'    TRANS                                        
         XC    FILTERS,FILTERS     CLEAR FILTERS                                
*                                                                               
         MVI   TRLSTPGM,X'1C'      DOING NET ASSIGN                             
*                                                                               
* DISPLAY UNITS                                                                 
*                                                                               
DU       BAS   RE,CLRSCR           CLEAR SCREEN FROM HEADLINES DOWN             
*                                                                               
* DISPLAY UNIT ALWAYS EXITS VIA ERREX2 *                                        
*                                                                               
         BRAS  RE,DISUNT                                                        
         DC    H'0'                                                             
         EJECT                                                                  
* VALIDATE COMMERCIALS FOR UNITS                                                
*                                                                               
* RESTORE UNITABLE AND PROCESS ANY OPERATOR ENTRIES                             
*                                                                               
VC       BAS   RE,RDTWA                                                         
*                                                                               
         XC    NEXTFEED,NEXTFEED   ZERO OPTIONAL FEED DISPLAY                   
*                                                                               
         TM    UNITSW,X'02'        IS TABLE BUILT                               
         BZ    VK02                                                             
*                                                                               
         XC    CMLASNCT,CMLASNCT                                                
         NI    UNITSW,X'FF'-X'20'  SET OFF SVTWA NEEDED SW                      
         TM    TRAOPTH+4,X'20'                                                  
         BO    VC06                                                             
         LA    R2,TRAOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VC02                                                             
         CLC   =C'RESET',8(R2)                                                  
         BE    VC02                                                             
         CLC   =C'SAVE',8(R2)                                                   
         BE    VC06                                                             
         BRAS  RE,FLTR              GET ANY ASSIGN FILTERS                      
*                                                                               
         TM    FLTRSW,X'80'        CLEAR PREV ASSIGNED CMLS                     
         BO    *+12                                                             
         TM    UNITSW1,X'C0'      RESTORE(S) FOUND OR ACML DONE                 
         BZ    VC06                NO                                           
*                                                                               
         NI    UNITSW1,X'FF'-X'80'-X'40'                                        
         XC    NXTUNIT,NXTUNIT     START AT TOP OF LIST                         
         BAS   RE,SVTWA                                                         
         B     DU                 GO REDISPLAY UNITS                            
*                                                                               
VC02     DS    0H                                                               
         NI    FLTRSW,X'FF'-X'40' RESET PATTERN SWITCH                          
         XC    FILTERS,FILTERS                                                  
VC04     XC    TRAOPT,TRAOPT                                                    
         OI    TRAOPTH+6,X'80'                                                  
*                                                                               
VC06     DS    0H                                                               
         LA    R5,UNITABLE                                                      
         CLI   0(R5),0            ANY UNITS AT ALL                              
         BNE   VC08                                                             
         OC    UNASGND,UNASGND    SHOULD BE ZERO                                
         BNZ   VC07                                                             
         OC    TOTUNITS,TOTUNITS  SHOULD BE ZERO                                
         BNZ   VC07                                                             
         OC    DLUNTS,DLUNTS      MUST BE SOME                                  
         BNZ   VC190                                                            
VC07     DC    H'0'                                                             
*                                                                               
VC08     A     R5,NXTUNIT          GET 1ST ENTRY AT TOP OF CURR                 
         USING UNTABLED,R5                                                      
         LA    R2,TRACML1H                                                      
*                                                                               
VC08B    XC    SVCML,SVCML         NO PREV CMLS                                 
         EJECT                                                                  
* FIND ANY ENTRIES FOR COMMERCIAL, OTHER                                        
*                                                                               
VC10     TM    UNTFLAG1,X'60'      FEED OR UNIT DELETED                         
         BNZ   VC11                                                             
         BRAS  RE,TFTR             FILTER UNIT TABLE                            
         BE    VC12                                                             
*                                                                               
VC11     LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             END OF TABLE                                 
         BNE   VC10                                                             
         B     VC140                                                            
*                                                                               
* VALIDATE COMMERCIAL *                                                         
*                                                                               
VC12     DS    0H                                                               
         XC    SVTYPE,SVTYPE                                                    
         MVC   QPRD,UNTPROD        QPRD                                         
         MVC   QPRD2,UNTPROD2      QPRD2                                        
         MVC   BSLN,UNTSLN         SPOT LEN                                     
         MVC   BSLN2,UNTSLN2       SPOT LEN2                                    
         XC    CML1(3),CML1        CLEAR CML1, CML2, CML3 SLN'S                 
*                                                                               
         OC    UNTPROD2,UNTPROD2   TEST P/B CMML                                
         BZ    VC12A                                                            
         LR    RF,R2                                                            
         AHI   RF,TRACML2H-TRACML1H POINT TO FIELD FOR NEXT CMML                
         LA    R0,TRATAGH                                                       
         SR    RF,R0               TEST ROOM ON SCREEN FOR BOTH                 
         BH    VC135               NO                                           
*                                                                               
VC12A    LA    R4,UNTCML1                                                       
         MVI   CMLFLAG,1           CML1                                         
*                                                                               
         CLI   5(R2),13                                                         
         BNE   VC13                                                             
         CLI   20(R2),C'*'         WAS COMML ENTERED THIS SESSION               
         BNE   VC13                 NO                                          
*                                                                               
         SR    R0,R0                                                            
         LA    R1,8(R2)                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         LPR   R0,R0                                                            
         STC   R0,5(R2)            SET ACTUAL INPUT LENGTH                      
         MVI   20(R2),0            CLEAR *                                      
*                                                                               
VC13     NI    PRDMATSW,X'FF'-X'02' TURN OFF LOOK FOR PTR                       
         BRAS  RE,VCML             VALIDATE COMMERCIAL                          
         BNE   VC18                 NO CHANGE                                   
*                                                                               
         MVC   UNTCML1,SVCMLP      SAVE ISCI OR PACKED ADID                     
         NI    UNTCMLF,X'FF'-NUCMADF1                                           
         CLI   SVCMLFLG,C'Y'                                                    
         BNE   *+8                                                              
         OI    UNTCMLF,NUCMADF1                                                 
*                                                                               
         OC    QPRD2,QPRD2         IS THIS CML FOR BOTH PRDS                    
         BNZ   *+16                   YES                                       
         CLI   SVCMLSOL,C'P'       MUST NOT BE P/B                              
         BE    SOLOER                                                           
         B     VC14                                                             
*                                                                               
         CLI   SVCMLSOL,0          ANY ENTRY                                    
         BE    *+12                                                             
         CLI   SVCMLSOL,C'S'       MUST NOT BE SOLO                             
         BE    SOLOER                                                           
*                                                                               
         MVC   UNTCML2,UNTCML1                                                  
         NI    UNTCMLF,X'FF'-NUCMADF2                                           
         TM    UNTCMLF,NUCMADF1                                                 
         BZ    *+8                                                              
         OI    UNTCMLF,NUCMADF2    ADID CML                                     
*                                                                               
         OI    UNTFLAG1,X'04'                                                   
         OI    UNTFLAG1,UNTFL1PB   SET ON P/B CML FLAG                          
         B     VC16                                                             
*                                                                               
VC14     TM    UNTFLAG1,UNTFL1PB    WAS IT A P/B CML                            
         BZ    VC15                                                             
         XC    UNTCML2,UNTCML2                                                  
         NI    UNTFLAG1,X'FF'-UNTFL1PB SET OFF P/B CML FLAG                     
*                                                                               
VC15     CLI   SVCMLSOL,0          ANY ENTRY                                    
         BE    VC16                                                             
         OC    UNTPROD2,UNTPROD2   PRD2?                                        
         BZ    VC15F                                                            
VC15C    DS    0H                                                               
         CLI   SVCMLSOL,C'P'       MUST BE PIGGYBACK                            
         BNE   SOLOER                                                           
         B     VC16                                                             
*                                                                               
VC15F    DS    0H                                                               
         CLI   SVCMLSOL,C'S'       MUST BE SOLO                                 
         BNE   SOLOER                                                           
*                                                                               
VC16     NI    UNTFLAG1,X'FF'-X'80' SET OFF REASSIGN FLAG                       
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+10                YES, UNTREF=MAP YEAR/MAP CODE                
         XC    UNTREF,UNTREF                                                    
         MVI   UNTKEY,0                                                         
         OI    UNITSW,X'20'        SVTWA NEEDED                                 
         TM    REVALID,NONEW       SAME CML AS BEFORE                           
         BO    VC20                 YES, NO CHANGE                              
         TM    UNTFLAG1,X'08'      CML1 ASGND THIS SESSION                      
         BO    VC20                                                             
         OI    UNTFLAG1,X'08'                                                   
         TM    UNTFLAG4,UNTFLDC1                                                
         BZ    *+8                                                              
         NI    UNTFLAG4,X'FF'-UNTFLDC1 TURN OFF CML1 DLTD THIS SESSION          
*                                                                               
         LH    R1,CMLASNCT                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,CMLASNCT                                                      
*                                                                               
VC18     OC    UNTCML1,UNTCML1     IS THERE A CML ASSIGNED                      
         BNZ   VC20                                                             
         TM    UNTFLAG1,UNTFL1PB   WAS THERE A P/B CML ASSIGNED                 
         BZ    VC20                NO                                           
         XC    UNTCML2,UNTCML2     CLEAR P/B ALSO                               
         NI    UNTFLAG1,X'FF'-UNTFL1PB                                          
*                                                                               
VC20     XC    QPRD2,QPRD2                                                      
         MVI   BSLN2,0                                                          
*                                                                               
* MOVE ON TO OTHER FIELD NOW                                                    
*                                                                               
         LLC   R0,0(R2)            OTHER                                        
         AR    R2,R0                                                            
*                                                                               
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VC30                YES                                          
*                                                                               
* VALIDATE OTHER FIELD HERE                                                     
*                                                                               
         BAS   RE,VOTH                                                          
         TM    REVALID,X'01'       CHECK IF NEED TO REVALIDATE COML             
         BNO   VC30                                                             
         NI    REVALID,X'FF'-X'01'                                              
         OC    UNTPROD2,UNTPROD2                                                
         BZ    VC08B                                                            
         OI    REVALID,X'02'       SET REVALID P/B                              
         B     VC08B                                                            
*                                                                               
VC30     LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO DAY/TIME AT EOL                     
         LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO DATE/PRD                            
         LR    RF,R2   <====       SAVE ADDR OF TRAINF                          
         LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT CMML                           
*                                                                               
* VALIDATE PIGGYBACK PRODUCT IF ANY *                                           
*                                                                               
         OC    UNTPROD2,UNTPROD2   PARTNER                                      
         BZ    VC90                 NO                                          
*                                                                               
* VALIDATE P/B COMMERCIAL *                                                     
*                                                                               
         MVC   QPRD,UNTPROD2       QPRD AND SPOT LEN                            
         MVC   BSLN,UNTSLN2                                                     
*                                                                               
         LA    R4,UNTCML2                                                       
VC35     MVI   CMLFLAG,2           CML2                                         
*                                                                               
         CLI   5(R2),9                                                          
         BNE   VC40                                                             
         CLI   16(R2),C'*'                                                      
         BNE   VC40                                                             
         MVI   5(R2),8                                                          
         MVI   16(R2),0                                                         
*                                                                               
VC40     NI    PRDMATSW,X'FF'-X'01'                                             
         OI    PRDMATSW,X'02'      LOOK FOR PRD/PTR                             
         BRAS  RE,VCML               VALIDATE COMMERCIAL                        
         BNE   VC70                 NO CHANGE                                   
*                                                                               
         MVC   UNTCML2,SVCMLP      SAVE ISCI OR PACKED ADID                     
         NI    UNTCMLF,X'FF'-NUCMADF2                                           
         CLI   SVCMLFLG,C'Y'                                                    
         BNE   *+8                                                              
         OI    UNTCMLF,NUCMADF2                                                 
*                                                                               
         TM    UNTFLAG1,UNTFL1PB   IS CML1 A P/B CML                            
         BZ    VC55                                                             
*                                                                               
VC50     CLI   SVCMLSOL,C'S'       MUST NOT BE SOLO                             
         BE    SOLOER                                                           
         B     VC60                                                             
*                                                                               
VC55     CLI   SVCMLSOL,C'P'       MUST NOT BE P/B                              
         BE    SOLOER                                                           
*                                                                               
VC60     TM    UNTFLAG1,UNTFL1PB   IS CML1 A P/B CML                            
         BZ    VC65                NO                                           
         CLC   UNTCML1,UNTCML2     THEY THE SAME                                
         BE    VC65                                                             
         NI    4(R2),X'FF'-X'20'   SET OFF VALIDATED BIT                        
         MVC   GERROR,=Y(PBCML1ST)                                              
         B     CKSVTWA                                                          
*                                                                               
VC65     TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+10                YES, UNTREF=MAP YEAR/MAP CODE                
         XC    UNTREF,UNTREF                                                    
         MVI   UNTKEY,0                                                         
         NI    UNTFLAG1,X'FF'-X'80' SET OFF REASSIGN FLAG                       
*                                                                               
         TM    REVALID,NONEW       SAME CML AS BEFORE                           
         BO    VC70                 YES, NO CHANGE                              
         TM    UNTFLAG1,X'04'      CML2 ASSIGNED THIS SESSION                   
         BO    VC70                                                             
         OI    UNTFLAG1,X'04'                                                   
         TM    UNTFLAG4,UNTFLDC2                                                
         BZ    *+8                                                              
         NI    UNTFLAG4,X'FF'-UNTFLDC2 TURN OFF CML2 DLTD THIS SESSION          
*                                                                               
         LH    R1,CMLASNCT                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,CMLASNCT                                                      
*                                                                               
VC70     DS    0H                                                               
         NI    PRDMATSW,X'FF'-X'02'  TURN OF LOOK FOR PRD/PTR                   
*                                                                               
VC88     TM    REVALID,NOREVALD                                                 
         BZ    *+8                                                              
         NI    REVALID,X'FF'-NOREVALD                                           
*                                                                               
         NI    REVALID,X'FF'-X'02'   TURN OF REVALID P/B                        
*                                                                               
* VIGNETTE CALCULATIONS                                                         
*                                                                               
VC90     TM    VIGNFLG,X'01'              IS IT VIGNETTE                        
         BNO   VC120                                                            
         CLC   =CL8'VIGNETTE',UNTBBSN                                           
         BNE   VC120                      NO                                    
         NI    VIGNFLG,X'FF'-X'01'                                              
*                                  CALCULATE VIGNETTE LENGTH                    
         CLI   CML1,0              DO I HAVE COML1 LENGTH                       
         BNE   VC100                                                            
         OC    UNTCML1,UNTCML1     IS THERE A COML1                             
         BNZ   VC95                NO/CLEAR VIGNETTE                            
         XC    UNTBBSN,UNTBBSN                                                  
         MVI   UNTBBSL,0                                                        
*                                                                               
         OI    UNTFLAG4,UNTFL4BC   SET ON BILLBOARD CHANGE                      
*                                                                               
         B     VC120                                                            
*                                                                               
VC95     MVC   WORK(8),UNTCML1     YES/MUST GET LENGTH                          
         MVI   JUSLEN,C'Y'                                                      
*                                                                               
         NI    REVALID,X'FF'-NOEQCCXT                                           
         BRAS  RE,FCML             SEE IF COMML EXISTS                          
*                                                                               
         MVC   CML1,WORK                                                        
VC100    CLI   CML2,0                                                           
         BNE   VC110                                                            
         OC    UNTCML2,UNTCML2     IS THERE A COML2                             
         BZ    VC110                                                            
*                                                                               
         MVC   WORK(8),UNTCML2                                                  
         NI    REVALID,X'FF'-NOEQCCXT                                           
         MVI   JUSLEN,C'Y'                                                      
         BRAS  RE,FCML                                                          
         MVC   CML2,WORK                                                        
*                                                                               
VC110    DS    0H                                                               
         LLC   RE,CML1             RE=TOTAL COMMERCIAL LENGTHS                  
         LLC   R0,CML2                                                          
         CLC   UNTCML1,UNTCML2     IF COMMERCIALS ARE THE SAME                  
         BNE   *+6                                                              
         SR    R0,R0               CONSIDER ONLY ONE LENGTH                     
*                                                                               
         CLI   CML2,X'FF'          COMML GOOD FOR ALL LENGTHS                   
         BE    VC114                                                            
         CLI   CML2,0              OR NONE                                      
         BNE   VC118                                                            
VC114    DS    0H                                                               
         CLI   CML1,X'FF'          COMML GOOD FOR ALL LENGTHS                   
         BE    VC120                                                            
VC118    DS    0H                                                               
         AR    RE,R0                                                            
         LLC   R0,UNTSLN           R1=TOTAL UNIT LENGTH                         
         LLC   R1,UNTSLN2                                                       
         AR    R1,R0                                                            
         SR    R1,RE                                                            
         BM    INVSLNER                                                         
*                                                                               
         MVC   BYTE,UNTBBSL        SAVE OLD UNTBBSL                             
*                                                                               
         STC   R1,UNTBBSL                                                       
         CLI   UNTBBSL,0           IF VIGNETTE LENGTH=0                         
         BNE   *+8                                                              
         MVI   UNTBBSL,X'FF'       SET TO FF                                    
         LR    RF,R2               SAVE ADDR OF TRAINF                          
*                                                                               
         CLC   BYTE,UNTBBSL        IS THIS A DIFFERENT LENGTH                   
         BE    VC120                                                            
*                                                                               
         OI    UNTFLAG4,UNTFL4BC   SET ON BILLBOARD CHANGE                      
*                                                                               
VC120    TM    UNTFLAG3,UNTFL3TB   WAS IT T/B                                   
         BO    VC125               YES                                          
         OC    UNTPROD2,UNTPROD2   WAS IT P/B                                   
         BZ    VC130               NO                                           
*                                                                               
VC125    LLC   R0,0(R2)            PAST CMML FIELD                              
         AR    R2,R0                                                            
         LLC   R0,0(R2)            PAST OTHER FIELD                             
         AR    R2,R0                                                            
         LLC   R0,0(R2)            PAST DAY/TIME FIELD                          
         AR    R2,R0                                                            
         LR    RF,R2               SAVE ADDR OF TRAINF                          
         LLC   R0,0(R2)            PAST INF (AIR DATE, FEED, PRD-SLN)           
         AR    R2,R0                                                            
**       CLI   0(R2),21                                                         
**       BE    *+6                                                              
**       DC    H'0'                                                             
*                                                                               
VC130    LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             TEST END OF LIST                             
         BE    VC140               YES                                          
         TM    UNTFLAG1,X'60'      FEED OR UNIT DELETED                         
         BNZ   VC130                                                            
         OC    8(L'TRAINF1,RF),8(RF) BLANK                                      
         BZ    VC135                 MUST BE END FORCED BY PIGGYBACK            
         LA    R0,TRATAGH          END OF SCREEN                                
         CR    R2,R0               PAST END                                     
         BL    VC10                NO                                           
VC135    LA    R1,UNITABLE                                                      
         SR    R5,R1                                                            
         ST    R5,NXTUNIT                                                       
         B     VC145               PROCESS NEXT SCREEN,  IF ANY                 
VC140    XC    NXTUNIT,NXTUNIT                                                  
*                                                                               
* COUNT UNASSIGNED UNITS *                                                      
*                                                                               
VC145    NI    UNITSW2,X'FF'-RCMLTBA   TURN OFF REPLACE CML W/TBA               
         LA    R5,UNITABLE                                                      
         SR    R1,R1                                                            
VC150    TM    UNTFLAG1,X'60'      FEED OR UNIT DELETED                         
         BNZ   VC160                                                            
         OC    UNTCML1,UNTCML1                                                  
         BZ    VC155                                                            
         CLC   UNTCML1,REASSIGN                                                 
         BE    VC155                                                            
         OC    UNTPROD2,UNTPROD2                                                
         BZ    VC160                                                            
         OC    UNTCML2,UNTCML2                                                  
         BNZ   VC160                                                            
VC155    LA    R1,1(,R1)                                                        
VC160    LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             TEST END OF LIST                             
         BNE   VC150               NO                                           
         STH   R1,UNASGND                                                       
         LH    RF,TOTUNITS                                                      
         SR    RF,R1               TOTUNIT-UNASGND =                            
         STH   RF,PREVASS             PREV ASSIGNED                             
*                                                                               
         CLC   =C'SAVE',TRAOPT     WAS SAVE REQUESTED                           
         BE    *+8                                                              
         NI    FLTRSW,X'FF'-X'80' RESET CLEAR SWITCH                            
         TM    UNITSW,X'40'        WERE FEEDS ADDED                             
         BZ    VC190               NO                                           
*                                                                               
* SORT ON AIRDATE, SQH, EST, SUB, DISK ADDR, FEED *                             
*                                                                               
         SR    R4,R4                                                            
         LA    R1,UNITABLE                                                      
VC170    CLI   0(R1),0                                                          
         BE    VC180                                                            
         BCTR  R4,0                                                             
         LA    R1,L'UNTENT(,R1)                                                 
         B     VC170                                                            
VC180    LPR   R4,R4                                                            
         GOTO1 XSORT,DMCB,UNITABLE,(R4),L'UNTENT,L'UNTSORT,0                    
         NI    UNITSW,X'FF'-X'40'  SET OFF FEEDS ADDED                          
*                                                                               
         MVC   NXTUNIT,NEXTFEED                                                 
*                                                                               
VC190    CLC   =C'SAVE',TRAOPT     WAS SAVE REQUESTED                           
         BE    SV                                                               
*                                                                               
* SAVE UNIT TABLE, MISC CONTENTS, AND NETBLOCK *                                
*                                                                               
         BAS   RE,SVTWA                                                         
*                                                                               
* DISPLAY NEXT SCREEN OF UNITS OR REDISPLAY THESE *                             
*                                                                               
         B     DU                                                               
*                                                                               
* SAVE (UPDATE) UNIT RECORDS FROM UNIT TABLE & SAVE/ADD REVISION REC *          
*                                                                               
SV       BAS   RE,INITNET          SET FROM SPOT TO NET                         
*                                                                               
         BRAS  RE,SRV                                                           
         L     R1,DUB              MSG FOR OPERATOR                             
*                                                                               
         BAS   RE,INITSPT          SET FROM NET TO SPOT                         
*                                                                               
         LA    R2,TRACLTH                                                       
         BAS   RE,SVTWA                                                         
         B     TRAPERR2                                                         
*                                                                               
NOTEMS1  DC    C'AUTONOTE*SMUR:NO 0A1D REC FOUND IN 1C '                        
*                                                                               
         EJECT                                                                  
* VALIDATE OTHER FIELD - POS, BILLBOARD, FEED(S) *                              
*                                                                               
         USING UNTABLED,R5                                                      
*                                                                               
VOTH     NTR1                                                                   
         MVC   SAVEOTHR,UNTOTHR                                                 
         MVI   SAVEFLAG,0                                                       
         TM    UNTFLAG2,X'10'                                                   
         BZ    *+8                                                              
         OI    SAVEFLAG,X'10'                                                   
         NI    UNTFLAG2,X'FF'-X'10'                                             
         XC    UNTOTHR,UNTOTHR                                                  
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOTHX10             NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VOTH06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VOTH02                                                           
         LA    R1,4                                                             
         B     VOTH04                                                           
VOTH02   LLC   R1,5(R2)                                                         
VOTH04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VOTH08                                                           
VOTH06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OTHHELP),OTHHELP                                       
         CLI   SVTNPR5,C'Y'       TRAFFIC DELETES ALLOWED                       
         BNE   VOTH94                                                           
         MVC   CONHEAD+L'OTHHELP-3(8),=C'/UNIT= *'                              
         B     VOTH94                                                           
VOTH08   CLC   DELETE,8(R2)                                                     
         BNE   *+18                                                             
         XC    8(L'TRAOTH1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         B     VOTHX10                                                          
*                                                                               
         GOTO1 SCANNER,DMCB,(42,(R2)),(4,BLOCK+64)                              
         LLC   R1,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R1,R1               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK+64            ADDRESS OF FIRST BLOCK                    
*                                                                               
VOTH10   ST    R1,SCANCT           SAVE COUNT OF ENTRIES                        
         LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
* GET ADDRESS OF 'OTHER' VALIDATION RTN                                         
         LA    RF,OTHTABLE                                                      
         EX    R1,VOTHCLC                                                       
         BE    VOTHGO                                                           
         LA    RF,L'OTHTABLE(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-16                                                             
         B     VOTH90                                                           
*                                                                               
VOTHCLC  CLC   12(0,R4),0(RF)                                                   
VOTHGO   L     RE,10(RF)                                                        
         A     RE,SPTR1CRR                                                      
         BR    RE                                                               
         EJECT                                                                  
* VIGNETTE                                                                      
*                                                                               
VOTHVIG  DS    0H                                                               
*                                                                               
         CLC   UNTBBSN,=CL8'VIGNETTE'                                           
         BE    *+8                                                              
         OI    UNTFLAG4,UNTFL4BC   SET ON BILLBOARD CHANGE                      
*                                                                               
         OI    VIGNFLG,X'01'                                                    
         MVC   UNTBBSN,=CL8'VIGNETTE'                                           
         OI    UNITSW,X'20'                                                     
         B     VOTH70                                                           
*                                                                               
* POSITION                                                                      
*                                                                               
VOTHPOS  CLC   DELETE,22(R4)                                                    
         BE    VOTH16                                                           
         CLI   1(R4),1             POSITION MIN IS 1                            
         BNL   *+14                                                             
         MVC   GERROR,=Y(BDPOSLN)                                               
         B     CKSVTWA                                                          
*                                                                               
         CLI   1(R4),4             POSITION MAX IS 4                            
         BH    *-14                 NO                                          
         MVC   UNTPOS,22(R4)                                                    
         B     VOTH70                                                           
VOTH16   XC    UNTPOS,UNTPOS                                                    
         B     VOTH70                                                           
*                                                                               
* FEED                                                                          
*                                                                               
VOTHFEED DS   0H                                                                
         TM    UNTFLAG4,UNTFLCUT   THIS A CUTIN                                 
         BO    VOTH70               JUST INFO, IGNORE                           
*                                                                               
         LLC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,VOTHCLCD         IS THIS DELETE                               
         BE    VOTH22                                                           
         TM    UNTFLAG1,UNTFL1FD   IS THIS A FEED                               
         BO    VOTH18                                                           
         OC    UNTFEED,UNTFEED     MUST BE NATIONAL                             
         BZ    VOTH20                                                           
         CLC   =X'00FFE3',UNTFEED  CHK FOR TAG                                  
         BE    VOTH20                                                           
*                                                                               
VOTH18   MVC   GERROR,=Y(NOFD2FD)                                               
         B     CKSVTWA                                                          
*                                                                               
VOTH20   BRAS  RE,VFED             GO VALIDATE FEED                             
         B     VOTH70                                                           
VOTH22   TM    UNTFLAG1,UNTFL1FD   THIS MUST BE A FEED                          
         BNZ   *+14                                                             
         MVC   GERROR,=Y(NODELUNT)                                              
         B     CKSVTWA                                                          
*                                                                               
         TM    UNTFLAG2,01         THIS SET UP BY MEDIA                         
         BZ    *+14                                                             
         MVC   GERROR,=Y(NODELFD)                                               
         B     CKSVTWA                                                          
*                                                                               
         OI    UNTFLAG1,X'40'      SET ON DELETE FEED                           
*                                                                               
         BAS   RE,ADJCTR           GO SUB FROM TOTUNITS/UNASGND                 
         B     VOTH70                                                           
         EJECT                                                                  
* IP - INVERT PRODUCTS                                                          
*                                                                               
VOTHIP   DS    0H                                                               
         OC    UNTPROD2,UNTPROD2   PARTNER PROD IS REQUIRED                     
         BNZ   *+14                                                             
         MVC   GERROR,=Y(IPREQPB)                                               
         B     CKSVTWA                                                          
*                                                                               
         TM    UNTFLAG2,UNTFL2CS   NOT ALLOWED FOR COPYSPLITS                   
         BZ    *+14                                                             
         MVC   GERROR,=Y(NOIPCS)                                                
         B     CKSVTWA                                                          
*                                                                               
         OI    UNTFLAG2,X'10'      SET ON INVERT PRODS ON INSTR                 
         B     VOTH70                                                           
*                                                                               
* BB - BILLBOARD                                                                
*                                                                               
VOTHBB   CLC   DELETE,22(R4)                                                    
         BE    VOTHCLR                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM,TRAOTH1H                                                    
         MVI   ELEM+4,08          FORCE VALID NUMERIC                           
         LA    RF,ELEM+8                                                        
         LA    R3,22(R4)                                                        
         LR    RE,R3                                                            
*                                                                               
VOTH32   CLI   0(R3),C'0'          CK NUMERIC                                   
         BL    NUMERR                                                           
         CLI   0(R3),C'9'                                                       
         BH    NUMERR                                                           
         MVC   0(1,RF),0(R3)                                                    
         LA    R3,1(,R3)                                                        
         LA    RF,1(,RF)                                                        
         CLI   0(R3),C'/'          END OF BBSL                                  
         BNE   VOTH32                                                           
         LR    R0,R3                                                            
         SR    R0,RE                                                            
         STC   R0,ELEM+5          SAVE BILLBOARD LENGTH                         
         LA    R3,1(,R3)                                                        
         MVI   ERROPT,C'Y'                                                      
         LR    R1,R2                                                            
         LA    R2,ELEM                                                          
         GOTO1 VALISLN                                                          
         LR    R2,R1                                                            
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    VOTH33                                                           
*                                                                               
         CLI   ERROR,BADSLN        PRINT ERROR UNLESS BAD LENGTH                
         BNE   TRAPERR                                                          
         CLI   WORK,7              7 SECOND BILLBOARD OK                        
         BE    *+12                                                             
         CLI   WORK,3              3 SECOND BILLBOARD OK TOO                    
         BNE   TRAPERR                                                          
*                                                                               
VOTH33   MVC   UNTBBSL,WORK                                                     
         MVC   BSLN,WORK                                                        
*                                                                               
         XC    WORK,WORK                                                        
         CLI   SVTNPR4,C'Y'        PAD WITH DASHES                              
         BNE   *+10                NO                                           
         MVC   WORK(8),=8C'-'                                                   
*                                                                               
         LA    RF,WORK                                                          
         LR    RE,R3                                                            
*                                                                               
VOTH34   CLI   0(R3),C' '          CK END OF FIELD                              
         BNH   VOTH35                                                           
         CLI   0(R3),C'/'          CK END OF FIELD                              
         BE    VOTH35                                                           
         LA    R3,1(,R3)                                                        
         B     VOTH34                                                           
*                                                                               
VOTH35   LR    R1,R3                                                            
         SR    R1,RE                                                            
         BNZ   VOTH36                                                           
         MVC   GERROR,=Y(BDCMLLN2)                                              
         B     CKSVTWA                                                          
*                                                                               
VOTH36   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
*                                                                               
         LA    R1,1(R1)            RESTORE LEN                                  
         CHI   R1,8                                                             
         BL    CMLENER                                                          
         BE    VOTH37                                                           
         CHI   R1,12                                                            
         BH    CMLENER                                                          
         OC    WORK,SPACES                                                      
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK                                    
         JNE   VCMLERR1                                                         
         OI    UNTCMLF,NUCMADF3                                                 
*                                                                               
VOTH37   OI    UNITSW1,UNTSW1NL    DON'T CK LEN                                 
         NI    REVALID,X'FF'-NOEQCCXT                                           
         MVC   SVCMLP,WORK         SAVE ISCI OR PACKED ID                       
         BRAS  RE,FCML                                                          
         NI    UNITSW1,X'FF'-UNTSW1NL                                           
*                                                                               
         BRAS  RE,VCMLAPR          GO CHECK STARCOM DATA                        
*                                                                               
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE                                      
         BZ    VOTH38                                                           
         TM    CMLFLAG1,NOAIR                                                   
         BO    NOAIRER2                                                         
         TM    CMLFLAG1,MAXDTE                                                  
         BO    MAXDTER2                                                         
         TM    CMLFLAG1,CADTE                                                   
         BO    CADTER2                                                          
*                                                                               
VOTH38   DS   0H                                                                
         CLC   UNTBBSN,SVCMLP      IS THERE A CHANGE?                           
         BE    *+8                                                              
         OI    UNTFLAG4,UNTFL4BC   SET ON BILLBOARD CHANGE                      
*                                                                               
         MVC   UNTBBSN,SVCMLP      SET CMML                                     
         XC    WORK,WORK                                                        
*                                                                               
         CLI   SVTNPR4,C'Y'        PAD WITH DASHES                              
         BNE   *+10                NO                                           
         MVC   WORK(8),=8C'-'                                                   
*                                                                               
         LA    R3,1(,R3)                                                        
         LR    RE,R3                                                            
VOTH39   CLI   0(R3),C'/'          CK END OF FIELD                              
         BE    VOTH40                                                           
         CLI   0(R3),C' '          CK END OF FIELD                              
         BNH   VOTH40                                                           
         LA    R3,1(,R3)                                                        
         B     VOTH39                                                           
         EJECT                                                                  
VOTH40   LR    R1,R3                                                            
         SR    R1,RE                                                            
         BZ    VOTH48              YES, CK POS                                  
*                                                                               
VOTH44   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
*                                                                               
         LA    R1,1(R1)            RESTORE LEN                                  
         CHI   R1,8                                                             
         BL    CMLENER                                                          
         BE    VOTH46                                                           
         CHI   R1,12                                                            
         BH    CMLENER                                                          
         OC    WORK,SPACES                                                      
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK                                    
         OI    UNTCMLF,NUCMADF4                                                 
*                                                                               
VOTH46   NI    REVALID,X'FF'-NOEQCCXT                                           
         MVC   SVCMLP,WORK         SAVE ISCI OR ADID                            
         BRAS  RE,FCML                                                          
*                                                                               
         CLC   UNTBBCN,SVCMLP      IS THERE A CHANGE                            
         BE    *+8                  NO                                          
         OI    UNTFLAG4,UNTFL4BC   SET ON BILLBOARD CHANGE                      
*                                                                               
         MVC   UNTBBCN,SVCMLP                                                   
         CLI   0(R3),C' '          CK END OF FIELD                              
         BNH   VOTH70              YES                                          
*                                                                               
VOTH48   LA    R3,1(R3)                                                         
         LR    RE,R3                                                            
         CLI   0(R3),C' '          CK END OF FIELD                              
         BNH   *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         LR    R1,R3                                                            
         SR    R1,RE                                                            
         CHI   R1,4                                                             
         BNH   *+14                                                             
         MVC   GERROR,=Y(BDPOSLN)                                               
         B     CKSVTWA                                                          
*                                                                               
         LTR   R1,R1               FIND ANYTHING                                
         BZ    VOTH70                                                           
*                                                                               
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,VOTHCLCP                                                      
         BE    *+8                                                              
         OI    UNTFLAG4,UNTFL4BC   SET ON BILLBOARD CHANGE                      
*                                                                               
         EX    R1,VOTHMVC                                                       
         B     VOTH70                                                           
*                                                                               
VOTHCLCP CLC   UNTBBPOS(0),0(RE)                                                
VOTHMVC  MVC   UNTBBPOS(0),0(RE)                                                
*                                                                               
VOTHCLR  XC    UNTBBSL(17),UNTBBSL                                              
         OI    UNTFLAG4,UNTFL4BC   SET ON BILLBOARD CHANGE                      
         B     VOTH70                                                           
*                                                                               
VOTHPACK PACK  DUB,WORK(0)                                                      
         EJECT                                                                  
* PRD ALLOCATION                                                                
*                                                                               
VOTHPRD  TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BNZ   *+14                 YES                                         
         MVC   GERROR,=Y(CSPRDNG)                                               
         B     CKSVTWA                                                          
*                                                                               
         CLI   1(R4),2                                                          
         BL    INVPRDER                                                         
         CLI   1(R4),3                                                          
         BH    INVPRDER                                                         
         CLI   1(R4),2                                                          
         BNE   *+8                                                              
         MVI   24(R4),C' '                                                      
*                                                                               
         LR    R0,R2                                                            
*                                                                               
         LA    R2,ELEM             FAKE                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0A01000184030001'                                     
         MVC   ELEM+8(3),22(R4)                                                 
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
         LR    R2,R0                                                            
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BE    VOTH56              BYPASS PRD                                   
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
VOTH56   LA    RE,(L'UNTCSPRO/3)                                                
         LA    RF,UNTCSPRO                                                      
VOTH57   CLC   0(3,RF),WORK        VALID PROD IN WORK                           
         BE    VOTH58                                                           
         LA    RF,3(,RF)                                                        
         CLI   0(RF),0                                                          
         BE    *+8                                                              
         BCT   RE,VOTH57                                                        
*                                                                               
         BRAS  RE,CSPRDER                                                       
*                                                                               
VOTH58   CLC   UNTPROD,WORK         SAME PROD?                                  
         BE    VOTH70                                                           
         MVC   UNTPROD,WORK                                                     
         OI    UNITSW,X'20'        SVTWA NEEDED                                 
         OI    UNTFLAG1,X'80'      FORCED REASSIGN                              
         OC    UNTCML1,UNTCML1                                                  
         BZ    VOTH70                                                           
         MVC   UNTCML1,REASSIGN                                                 
         B     VOTH70                                                           
*                                                                               
* UNIT                                                                          
*                                                                               
VOTHUNT  CLI   SVTNPR5,C'Y'       TRAFFIC DELETES ALLOWED                       
         BNE   VOTH90                                                           
*                                                                               
         LLC   R1,1(R4)            GET LENGTH OF SECOND FIELD                   
         BCTR  R1,0                                                             
         EX    R1,VOTHCLCD         DELETE                                       
         BNE   VOTH90                                                           
*                                                                               
* DELETE THIS UNIT, AND IF A NATIONAL UNIT WITH *                               
* FEEDS DELETE ANY FEEDS ASSOCIATED WITH IT     *                               
*                                                                               
         OI    UNTFLAG1,X'20'     SET ON DELETED                                
*                                                                               
         BAS   RE,ADJCTR           GO SUB FROM TOTUNITS/UNASGND                 
*                                                                               
         LH    RF,DLUNTS                                                        
         LA    RF,1(,RF)           ADD 1 TO DELETED UNITS                       
         STH   RF,DLUNTS                                                        
         TM    UNTFLAG1,X'02'     NATIONAL UNIT                                 
         BZ    VOTH70                                                           
         LA    R1,UNTNEXT                                                       
VOTH64   CLI   0(R1),0            END OF TABLE                                  
         BE    VOTH70                                                           
         CLC   UNTDSKAD,UNTDSKAD-UNTENT(R1) SAME                                
         BNE   VOTH70                                                           
         TM    UNTFLAG1-UNTENT(R1),X'01' MUST BE FEED                           
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    UNTFLAG1-UNTENT(R1),X'40' DELETE FEED                            
*                                                                               
         BAS   RE,ADJCTR           GO SUB FROM TOTUNITS/UNASGND                 
*                                                                               
         LA    R1,L'UNTENT(,R1)                                                 
         B     VOTH64                                                           
*                                                                               
VOTH70   LA    R4,64(R4)           POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         L     R1,SCANCT                                                        
         BCT   R1,VOTH10           FOR NUMBER OF BLOCKS FOUND                   
         OI    UNTFLAG4,UNTFLUPD                                                
         B     VOTHX10                                                          
*                                                                               
VOTH90   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OTHMSG+L'OTHHELP),OTHMSG                               
         CLI   SVTNPR5,C'Y'       TRAFFIC DELETES ALLOWED                       
         BNE   VOTH94                                                           
         MVC   CONHEAD+L'OTHMSG+L'OTHHELP-3(8),=C'/UNIT= *'                     
*                                                                               
VOTH94   TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    MYERROR                                                          
         MVC   UNTOTHR,SAVEOTHR    RESTORE OTHER FIELDS                         
         OC    UNTFLAG2,SAVEFLAG   SET ON IP IF NEEDED                          
         BAS   RE,SVTWA                                                         
         B     MYERROR                                                          
*                                                                               
         USING CMLBBEL,R6                                                       
MAXDTER2 MVC   CONHEAD(L'MAXDTMS2),MAXDTMS2                                     
         GOTO1 DATCON,DMCB,(3,CMLBBMXD),(5,CONHEAD+L'MAXDTM1+1)                 
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BAS   RE,SVTWA                                                         
*                                                                               
         GOTO1 ERREX2                                                           
*                                                                               
MAXDTMS2 DC    C'* ERROR * MAX USE DATE ='                                      
         DS    0H                                                               
CADTER2  MVC   GERROR,=Y(NOCLADTE) NO CLIENT APPROVAL DATE                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,9                                                           
         MVC   ELEM+1(8),WORK                                                   
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BAS   RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
*                                                                               
NOAIRER2 MVC   GERROR,=Y(NAPRTAIR) NOT APPROVED TO AIR                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,13                                                          
         MVC   ELEM+1(12),SVCML                                                 
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BAS   RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
ADJCTR   LH    RF,TOTUNITS                                                      
         BCTR  RF,0                SUB 1 FROM TOTUNITS                          
         STH   RF,TOTUNITS                                                      
         OC    UNTCML1,UNTCML1                                                  
         BZ    ADJCTR10                                                         
         CLC   UNTCML1,REASSIGN                                                 
         BE    ADJCTR10                                                         
         OC    UNTPROD2,UNTPROD2                                                
         BZR   RE                                                               
         OC    UNTCML2,UNTCML2                                                  
         BNZR  RE                                                               
ADJCTR10 LH    RF,UNASGND                                                       
         BCTR  RF,0                                                             
         STH   RF,UNASGND                                                       
         BR    RE                                                               
ADJCTR20 LH    RF,UNASGND                                                       
         LA    RF,1(RF)                                                         
         STH   RF,UNASGND                                                       
         BR    RE                                                               
ADJCTR30 LH    RF,PREVASS                                                       
         LA    RF,1(RF)                                                         
         STH   RF,PREVASS                                                       
         BR    RE                                                               
ADJCTR40 LH    RF,PREVASS                                                       
         BCTR  RF,0                                                             
         STH   RF,PREVASS                                                       
         BR    RE                                                               
*                                                                               
* CK IF MEDIA REQUIRES A BILLBOARD, AND THERE IS ONE *                          
*                                                                               
VOTHX10  DS    0H                                                               
*                                                                               
         TM    UNTFLAG2,X'04'      MEDIA REQUIRE BILLBOARD                      
         BZ    VOTHX                NO                                          
         TM    UNTFLAG1,X'60'      DELETE UNIT/FEED                             
         BNZ   VOTHX20              YES                                         
*                                                                               
         CLI   UNTBBSN,0           IS THERE A BILLBOARD                         
         BE    VOTHX15              NO                                          
*                                                                               
         CLC   UNTCML1,=C'REASSIGN'                                             
         BE    VOTHX                                                            
*                                                                               
         CLC   UNTCML2,=C'REASSIGN'                                             
         BE    VOTHX                                                            
*                                                                               
         CLC   UNTBBSN,=C'REASSIGN'                                             
         BE    VOTHX                                                            
*                                                                               
         CLC   UNTBBCN,=C'REASSIGN'                                             
         BE    VOTHX                                                            
*                                                                               
         NI    UNTFLAG1,X'FF'-X'80' SET OFF REASSIGN FLAG                       
         B     VOTHX                                                            
*                                                                               
VOTHX15  MVC   GERROR,=Y(MEDREQBB)                                              
         TM    UNITSW,X'20'                                                     
         BZ    TRAPERR2                                                         
         MVC   UNTOTHR,SAVEOTHR                                                 
         OC    UNTFLAG2,SAVEFLAG                                                
         BAS   RE,SVTWA                                                         
         B     TRAPERR2                                                         
VOTHX20  MVC   UNTOTHR,SAVEOTHR    RESTORE OTHER FIELDS                         
         OC    UNTFLAG2,SAVEFLAG   SET ON IP IF NEEDED                          
         B     EXIT                                                             
VOTHX    OI    4(R2),X'20'         SET VALIDATED                                
         CLC   SAVEOTHR+1(8),=CL8'VIGNETTE'   IF VIGNETTE DELETED               
         BNE   EXIT                                                             
         CLC   UNTOTHR+1(8),SAVEOTHR+1                                          
         BE    EXIT                                                             
         OI    UNITSW,X'20'                                                     
         NI    4(R2),X'FF'-X'20'         TURN OF VALIDATED BIT                  
         LA    R1,TRAOTH1H-TRACML1H      SET R2 TO PREV COML                    
         SR    R2,R1                                                            
         NI    4(R2),X'FF'-X'20'         TURN OF VALIDATED COM BIT              
         OI    REVALID,X'01'             SET NEED TO REVALID COML               
         XIT1  REGS=(R2)                                                        
OTHMSG   DC    C'* ERROR *'                                                     
OTHHELP  DC    C'VALID ENTRIES - POS/BB/IP/FEED= *'                             
VOTHCLCD CLC   22(0,R4),DELETED                                                 
*                                                                               
* 'OTHER' VALIDATION RTNS:                                                      
*   BYTES 00-09 = OTHER KEYWORD                                                 
*   BYTES 10-13 = A(VALI RTN)                                                   
*                                                                               
OTHTABLE DS    0CL14                                                            
         DC    CL10'POS       ',AL4(VOTHPOS)                                    
         DC    CL10'FEED      ',AL4(VOTHFEED)                                   
         DC    CL10'BB        ',AL4(VOTHBB)                                     
         DC    CL10'PRD       ',AL4(VOTHPRD)                                    
         DC    CL10'UNIT      ',AL4(VOTHUNT)                                    
         DC    CL10'VIGNETTE  ',AL4(VOTHVIG)                                    
         DC    CL10'IP        ',AL4(VOTHIP)                                     
         DC    X'FF'                                                            
         EJECT                                                                  
* RESET FILES TO SPOT *                                                         
*                                                                               
INITSPT  MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVI   LSTATUS+1,1                                                      
         MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         BR    RE                                                               
*                                                                               
* RESET FILES TO NET *                                                          
*                                                                               
INITNET  MVI   DATADISP+1,27       SET TO NET                                   
         MVI   LKEY+1,20                                                        
         MVI   LSTATUS+1,1                                                      
         MVI   SYSDIR,C'U'                                                      
         MVI   SYSDIR+1,C'N'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         BR    RE                                                               
*                                                                               
* RESET FILES TO NET *                                                          
*                                                                               
INITXSP  MVI   DATADISP+1,42       SET TO XSPOT                                 
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         MVI   SYSDIR,C'X'                                                      
         MVI   SYSDIR+1,C'S'                                                    
         MVI   SYSDIR+2,C'P'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         BR    RE                                                               
*                                                                               
* RTN TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                            
*                                                                               
         DS    0H                                                               
SVTWA    NTR1                                                                   
         LA    R1,=C'DMWRT'                                                     
         J     RDTWA10                                                          
*                                                                               
RDTWA    NTR1                                                                   
         LA    R1,=C'DMREAD'                                                    
*                                                                               
RDTWA10  BRAS  RE,COMTWA                                                        
         J     EXIT                                                             
         EJECT                                                                  
* CLEAR DISPLAY AREA OF SCREEN *                                                
*                                                                               
CLRSCRT  XC    TRAREV,TRAREV                                                    
         OI    TRAREVH+6,X'80'                                                  
*                                                                               
         XC    TRACOM,TRACOM                                                    
         OI    TRACOMH+6,X'80'                                                  
*                                                                               
         XC    TRAINF,TRAINF                                                    
         OI    TRAINFH+6,X'80'                                                  
*                                                                               
CLRSCR   LA    RF,TRAINF1H                                                      
*                                                                               
CLRSCR10 XC    8(L'TRAINF1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
*                                                                               
         LLC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         XC    8(L'TRACML1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
         NI    1(RF),X'FF'-X'20'   UNPROTECT CML FIELD                          
*                                                                               
         IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         XC    8(L'TRAOTH1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
         NI    1(RF),X'FF'-X'20'                                                
*                                                                               
         IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         XC    8(L'TRADTM1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
*                                                                               
         IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         LA    R0,TRATAGH                                                       
         CR    RF,R0                                                            
         BL    CLRSCR10                                                         
         BR    RE                                                               
         EJECT                                                                  
* GETEL                                                                         
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
* SPECIAL ERROR HANDLERS (OFTEN USED OR USING SUBST TABLES)                     
*                                                                               
SOLOER   MVC   GERROR,=Y(INVPGSO) SOLO/PIGGYBACK DOESN'T MATCH USE              
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
SOLOER10 XC    0(8,R4),0(R4)       CLEAR CML                                    
         B     CKSVTWA                                                          
         DROP  R5                                                               
*                                                                               
         USING CMLDTAEL,R6                                                      
INVSLNER MVC   GERROR,=Y(BDSPTLN)                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,4              L'SUBST TEXT + 1                             
         LLC   R1,BSLN                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+1(3),DUB                                                    
         MVI   ELEM+4,4            L'SUBST TEXT + 1                             
         LLC   R1,CMLSLN                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+5(3),DUB                                                    
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         B     CKSVTWA                                                          
         DROP  R6                                                               
CMLENER  MVC   GERROR,=Y(NOT812)   COMMERCIAL MUST BE 8-12 CHAR                 
         B     TRAPERR2                                                         
*                                                                               
INVPRDER MVI   ERROR,INVPROD                                                    
         J     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         J     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
         EJECT                                                                  
* VARIOUS ERROR EXITS                                                           
*                                                                               
TRAPERR  TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BAS   RE,SVTWA                                                         
         GOTO1 ERREX                                                            
*                                                                               
CKSVTWAP BRAS  RE,CURSPOS          POSITION THE CURSOR                          
*                                                                               
CKSVTWA  TM    UNITSW,X'20'        SVTWA NEEDED                                 
         JZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
*                                                                               
TRAPERR2 GOTO1 VTRAERR                                                          
*                                                                               
MYERROR  GOTO1 ERREX2                                                           
*                                                                               
DELERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DELMSG),DELMSG                                         
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BAS   RE,SVTWA                                                         
         GOTO1 ERREX2                                                           
*                                                                               
DELMSG   DC    C'* TO DELETE COMMERCIAL TYPE "DELETE"*'                         
*                                                                               
         EJECT                                                                  
DELETE   DS   0CL6                                                              
DELETED  DC    C'DELETED'                                                       
REASSIGN DC    CL9'REASSIGN '                                                   
*                                                                               
         LTORG                                                                  
         DROP  R7                                                               
*                                                                               
* COMMON RTN TO SAVE END OF UNITABLE ON TEMPSTR                                 
*                                                                               
COMTWA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=Y(ENDSYSD-(SVSTART+11046))                           
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,ASVSTOR                               
         CLI   8(R1),0                                                          
         BE    COMTWAX                                                          
         DC    H'0'                                                             
COMTWAX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE FILTERS DURING ASSIGN *                                              
*                                                                               
         USING UNTABLED,R5                                                      
FLTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    FLTRSW,X'FF'-X'80' RESET CLEAR ASGND CMLS                        
         XC    FILTERS,FILTERS                                                  
         XC    CURSPOS1,CURSPOS1                                                
         CLI   5(R2),0             ANY ENTRY                                    
         BE    FLTR89              NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    FLTR90              YES                                          
         CLI   5(R2),4                                                          
         BNH   FLTR02                                                           
         LA    R1,4                                                             
         B     FLTR04                                                           
FLTR02   LLC   R1,5(R2)                                                         
FLTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BE    FLTR90                                                           
         GOTO1 SCANNER,DMCB,(20,TRAOPTH),(X'80',BLOCK+64)                       
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR1            NO                                           
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
FLTR10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         MVI   HOLDSIGN,0                                                       
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    *+12                YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   *+12                NO, NETHER                                   
         MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
* GET ADDRESS OF FILTER VALIDATION RTN                                          
         LA    RF,FLTTABLE                                                      
         EX    R1,FLTRCLC                                                       
         BE    FLTRGO                                                           
         LA    RF,L'FLTTABLE(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-16                                                             
         B     FLTR90              INVALID FILTER                               
*                                                                               
FLTRCLC  CLC   12(0,R4),0(RF)                                                   
FLTRGO   L     RE,10(RF)                                                        
         A     RE,SPTR1CRR                                                      
         BR    RE                                                               
         EJECT                                                                  
* FILTER ON DATE                                                                
*                                                                               
FLTDATE  LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),DUB                                         
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERR              NO                                           
         CLC   STDATE,DUB         MUST BE WITHIN PERIOD                         
         BH    DTPERERR                                                         
         CLC   ENDATE,DUB                                                       
         BL    DTPERERR                                                         
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,DUB),(2,FLTRDATE)                                 
         CLM   R6,1,1(R4)          WAS THERE ONLY 1 DATE                        
         BE    FLTR18              YES                                          
         LA    R5,1(R6,R5)                                                      
         GOTO1 DATVAL,(R1),(0,(R5)),DUB                                         
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERR              NO                                           
         CLC   STDATE,DUB         MUST BE WITHIN PERIOD                         
         BH    DTPERERR                                                         
         CLC   ENDATE,DUB                                                       
         BL    DTPERERR                                                         
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,DUB),(2,FLTRDAT2)                                 
         CLC   FLTRDATE,FLTRDAT2                                                
         BNH   FLTR80                                                           
         MVC   GERROR,=Y(DATSEQER)                                              
         GOTO1 VTRAERR                                                          
*                                                                               
FLTR18   MVC   FLTRDATS,HOLDSIGN                                                
         B     FLTR80                                                           
         EJECT                                                                  
* FILTER ON PRODUCT                                                             
*                                                                               
FLTPRD   CLC   =C'POL',22(R4)      IS PRD=POL                                   
         BE    INVPDER              YES, ERROR                                  
         CLC   =C'ALL',22(R4)      IS PRD=ALL                                   
         BE    INVPDER              YES, ERROR                                  
         CLI   1(R4),3             MAX 3 CHAR                                   
         BNH   *+14                                                             
         MVC   GERROR,=Y(BDPRDLN)                                               
         J     CKSVTWA                                                          
*                                                                               
         CLI   1(R4),2                                                          
         BH    *+8                                                              
         MVI   24(R4),C' '                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0A01000184030001'                                     
         MVC   ELEM+8(3),22(R4)                                                 
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         LR    R0,R2               SAVE R2                                      
         LA    R2,ELEM                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
         LR    R2,R0                                                            
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BE    FLTR26               NO OKAY                                     
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
FLTR26   MVC   FLTRPROD,WORK                                                    
         B     FLTR80                                                           
*                                                                               
* SHOW UNIT COST                                                                
*                                                                               
FLTCOST  CLI   SVTN2PR8,C'Y'       OKAY TO SHOW UNIT COST?                      
         BNE   FLTR90               NO, ERROR                                   
         OI    UNITSW2,DCOST                                                    
         B     FLTR80                                                           
*                                                                               
* FILTER/SHOW BUY LINE NUMBERS OR ONLY ONE BUY LINE                             
*                                                                               
FLTLINE  OI    UNITSW2,LINENUM                                                  
         CLI   1(R4),0             SPECIFIC LINE # ENTERED?                     
         BE    FLTR80               NO                                          
         MVC   SVLINE,11(R4)       SAVE LINE # IN BINARY                        
         B     FLTR80                                                           
*                                                                               
* SHOW SECTIONAL FEED DESCRIPTION                                               
*                                                                               
FLTFEED  DS    0H                                                               
         CLI   1(RA),C'*'          THIS A DDS TERMINAL?                         
         BE    *+14                                                             
         CLC   =C'H9',AGENCY       IF AGENCY IS NOT STARCOM                     
         BNE   FLTR90               ERROR                                       
         OI    UNITSW2,FEEDSW                                                   
         B     FLTR80                                                           
*                                                                               
* DO NOT SHOW SECTIONAL FEED DESCRIPTION                                        
*                                                                               
FLTNOFD  DS    0H                                                               
         CLI   1(RA),C'*'          THIS A DDS TERMINAL?                         
         BE    *+14                                                             
         CLC   =C'H9',AGENCY       IF AGENCY IS NOT STARCOM                     
         BNE   FLTR90               ERROR                                       
         NI    UNITSW2,X'FF'-FEEDSW                                             
         B     FLTR80                                                           
*                                                                               
* FILTER ON COMMERCIAL                                                          
*                                                                               
FLTCML   MVC   WORK(8),22(R4)                                                   
         MVC   FLTCMLD(8),22(R4)   SAVE FOR RE-DISPLAY                          
         LA    R6,8(R4)            SET CURSOR POSN                              
         CLI   1(R4),8             MUST BE 8-12 CHAR                            
         BE    FLTCML10                                                         
         BL    FLTCMLE1                                                         
         CLI   1(R4),12                                                         
         BH    FLTCMLE1                                                         
*                                                                               
         LLC   R1,1(R4)            INPUT LENGTH                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLTCMLD(0),22(R4)                                                
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',22(R4)),WORK                                  
         BNE   FLTCMLE2                                                         
*                                                                               
FLTCML10 XC    KEY(13),KEY                                                      
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(3),BAGYMD   A-M/CLT                                       
         MVC   CMLKCML,WORK                                                     
         DROP  R1                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FLTCMLE2                                                         
*                                                                               
         MVC   FLTRCML,KEY+5                                                    
         B     FLTR80                                                           
*                                                                               
FLTCMLE1 BRAS  RE,CURSPOS          POSITION THE CURSOR                          
         MVC   GERROR,=Y(NOT812)                                                
         GOTO1 VTRAERR                                                          
*                                                                               
FLTCMLE2 BRAS  RE,CURSPOS          POSITION THE CURSOR                          
         MVI   ERROR,INVCOMM       NO SUCH COMMERCIAL FOR CLT                   
         J     TRAPERR4                                                         
         EJECT                                                                  
*                                                                               
* TURN SWITCH ON TO REPLACE COMMERCIAL W/TBA                                    
*                                                                               
FLTBASW  DS    0H                                                               
         MVC   CURSTBA,4(R4)       HOLD THE DISP OF 1ST SUB-FIELD               
         LA    R6,CURSTBA                                                       
*                                                                               
         LLC   R1,1(R4)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),22(R4)                                                   
*                                                                               
         CLI   1(R4),8             MUST BE 8-12                                 
         BL    CMLENERP                                                         
         BE    FLTBA04                                                          
         CLI   1(R4),12                                                         
         BH    CMLENERP                                                         
*                                                                               
         OC    WORK,SPACES                                                      
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK                                    
         JNE   VCMLERR1                                                         
         TM    FLTODOSW,TBASW                                                   
         BO    FLTSWX                                                           
*                                                                               
FLTBA04  MVC   SVCMLTBA,WORK                                                    
         OI    FLTODOSW,TBASW                                                   
         NI    UNITSW2,X'FF'-RCMLTBA   REPLACE COMMERCIAL W/TBA                 
         B     FLTR80                                                           
*                                                                               
FLTSWX   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DUPREQ1),DUPREQ1                                       
         B     ERREX2X                                                          
*                                                                               
* REPLACE COMMERCIAL W/TBA                                                      
*                                                                               
FLTBA    DS    0H                                                               
         TM    FLTODOSW,TBASW      REPLACE W/TBA ENTERED                        
         BZ    FLTR81               NO                                          
         NI    FLTODOSW,X'FF'-TBASW                                             
*                                                                               
         LA    R6,CURSTBA                                                       
*                                                                               
         GOTO1 ANY                                                              
         XC    KEY(13),KEY                                                      
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(3),BAGYMD  & BCLT                                         
*                                                                               
         MVC   CMLKCML,SVCMLTBA                                                 
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FLTBA02                                                          
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY(2),=X'0AC1'                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   FLTCMLE2                                                         
*                                                                               
FLTBA02  OI    UNITSW2,RCMLTBA     REPLACE CML WITH TBA                         
*                                                                               
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
         MVI   BYTE,0                                                           
*                                                                               
FLTBA05  CLC   SVCMLTBA,UNTCML1    IS IT THIS CML                               
         BE    FLTBA10                                                          
*                                                                               
         CLC   SVCMLTBA,UNTCML2                                                 
         BNE   FLTBA40                                                          
FLTBA10  DS    0H                                                               
         BRAS  RE,TFTR              FILTER UNIT TABLE                           
         BNE   FLTBA40                                                          
*                                                                               
         NI    UNTFLAG2,X'FF'-UNTFL2AS TURN OFF PREV ASGND BIT                  
         CLC   SVCMLTBA,UNTCML1                                                 
         BNE   FLTBA20                                                          
         OC    UNTCML1(16),UNTCML1                                              
         BZ    *+8                                                              
         BAS   RE,ADCTR20          INCR UNASGND COUNT                           
         XC    UNTCML1,UNTCML1     DELETE                                       
         MVI   BYTE,255                                                         
*                                                                               
         CLC   SVCMLTBA,UNTCML2                                                 
         BNE   *+14                                                             
FLTBA20  XC    UNTCML2,UNTCML2     DELETE                                       
         MVI   BYTE,255                                                         
         NI    UNTFLAG1,X'FF'-X'10'      CML FOR BOTH PRDS                      
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+10                YES, UNTREF=MAP YEAR/MAP CODE                
         XC    UNTREF,UNTREF                                                    
         MVI   UNTKEY,0                                                         
*                                                                               
FLTBA40  LA    R5,UNTNEXT                                                       
         CLI   0(R5),0                                                          
         BNE   FLTBA05                                                          
*                                                                               
         CLI   BYTE,255            ANY UNITS FOUND FOR THIS CML                 
         BE    FLTR81               YES                                         
         MVC   GERROR,=Y(NOUNT4CM)                                              
         LA    R2,TRAOPTH                                                       
         J     CKSVTWA                                                          
         EJECT                                                                  
* CLEAR ALL ASGND COMMERCIALS                                                   
*                                                                               
FLTCLR   DS    0H                                                               
         TM    FLTODOSW,CLRSW      CLEAR ALL ASGN CMLS?                         
         BZ    FLTR81               NO                                          
         NI    FLTODOSW,X'FF'-CLRSW                                             
*                                                                               
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
*                                                                               
FLTCLR10 BRAS  RE,TFTR             FILTER UNIT TABLE                            
         BNE   FLTCLR20                                                         
*                                                                               
         OC    UNTCML1(16),UNTCML1                                              
         BZ    FLTCLR20                                                         
         TM    UNTFLAG1,X'60'      DELETED FEED OR UNIT                         
         BNZ   FLTCLR20                                                         
         NI    UNTFLAG2,X'FF'-UNTFL2AS TURN OFF PREV ASGND BIT                  
         XC    UNTCML1,UNTCML1     DELETE                                       
         XC    UNTCML2,UNTCML2     DELETE                                       
         OI    UNTFLAG4,UNTFLDC1+UNTFLDC2 CML1/2 DELETED THIS SESSION           
         NI    UNTFLAG1,X'FF'-X'10'      CML FOR BOTH PRDS                      
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+10                YES, UNTREF=MAP YEAR/MAP CODE                
         XC    UNTREF,UNTREF                                                    
         MVI   UNTKEY,0                                                         
         BAS   RE,ADCTR20          GO COUNT UNASGND                             
*                                                                               
FLTCLR20 LA    R5,UNTNEXT                                                       
         CLI   0(R5),0                                                          
         BNE   FLTCLR10                                                         
*                                                                               
         B     FLTR81                                                           
         EJECT                                                                  
* FILTER ON SPOT LEN                                                            
*                                                                               
FLTLEN   TM    3(R4),X'80'         WAS SPOT LEN NUMERIC                         
         BZ    NUMERR1                                                          
         MVC   ELEM,TRAOPTH                                                     
         PACK  ELEM+4(1),3(1,R4)  NUM, ALPHA, HEX BITS                          
         MVC   ELEM+5(1),1(R4)    DATA LEN                                      
         MVC   ELEM+8(10),22(R4)     SPOT LEN                                   
         MVI   ERROPT,C'Y'                                                      
         LA    R2,ELEM                                                          
         GOTO1 VALISLN                                                          
         LA    R2,TRAOPTH                                                       
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   TRAPERR4            GO PRINT ERROR                               
         MVC   FLTRSLN,WORK                                                     
         B     FLTR80                                                           
*                                                                               
* RESTORE                                                                       
*                                                                               
FLTREST  CLI   SVTNPR5,C'Y'       TRAFFIC DELETES ALLOWED                       
         BNE   FLTR90              ERROR - SHOW HELP                            
         GOTO1 DATVAL,DMCB,22(R4),WORK                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,WORK),(2,WORK+6)                                  
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
FLTR54   TM    UNTFLAG1,X'60'      DELETED FEED OR UNIT                         
         BZ    FLTR58                                                           
         CLC   UNTADTEP,WORK+6                                                  
         BNE   FLTR58                                                           
         NI    UNTFLAG1,X'FF'-X'40'-X'20' SET OFF DELETE                        
         OI    UNITSW1,X'80'      RESTORE(S) FOUND                              
         MVI   WORK,0                                                           
         TM    UNTFLAG1,UNTFL1FD  THIS A FEED                                   
         BO    FLTR58                                                           
         LH    RF,DLUNTS           SUB FROM DEL UNITS                           
         BCTR  RF,0                                                             
         STH   RF,DLUNTS                                                        
         LH    RF,TOTUNITS                                                      
         LA    RF,1(,RF)           ADD 1 TO TOTUNITS                            
         STH   RF,TOTUNITS                                                      
*                                                                               
         TM    UNTFLAG1,X'60'      FEED OR UNIT DELETED                         
         BNZ   FLTR58                                                           
         OC    UNTCML1,UNTCML1     CK IF ASSIGN NEEDED                          
         BZ    FLTR56                                                           
         CLC   UNTCML1,REASIGN                                                  
         BE    FLTR56                                                           
         OC    UNTPROD2,UNTPROD2                                                
         BZ    FLTR58                                                           
         OC    UNTCML2,UNTCML2                                                  
         BNZ   FLTR58                                                           
FLTR56   LH    RF,UNASGND                                                       
         LA    RF,1(,RF)           ADD 1                                        
         STH   RF,UNASGND                                                       
FLTR58   LA    R5,UNTNEXT                                                       
         CLI   0(R5),0                                                          
         BNE   FLTR54                                                           
         CLI   WORK,0             WAS DELETED UNIT FOUND                        
         BE    FLTR80                                                           
         MVC   GERROR,=Y(BDRESDT)                                               
         J     CKSVTWA                                                          
         EJECT                                                                  
* TURN ON SWITCH TO SEED ALL CMMLS                                              
*                                                                               
FLPATTSW DS    0H                                                               
         OI    FLTODOSW,PATSW                                                   
         B     FLTR80                                                           
*                                                                               
* PATTERN - SEED ALL CMMLS                                                      
*                                                                               
FLTPATT  DS    0H                                                               
         TM    FLTODOSW,PATSW                                                   
         BZ    FLTR81                                                           
         NI    FLTODOSW,X'FF'-PATSW                                             
         BRAS  RE,BLPAT                                                         
         OI    FLTRSW,X'40'        PATTERNS ASSIGNED                            
         B     FLTR81                                                           
*                                                                               
* TURN ON SWITCH TO ASSIGN CMML                                                 
*                                                                               
FLACMLSW LA    R6,CURSACML                                                      
         MVC   CURSACML,8(R4)                                                   
*                                                                               
         LLC   R1,1(R4)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),22(R4)                                                   
*                                                                               
         NI    FLTODOSW,X'FF'-ACMLADID                                          
*                                                                               
         TM    FLTODOSW,ACMLSW     WAS ACML REQ ALREADY                         
         BO    FLTSWX               YES, DUPLICATE REQ (ERROR)                  
         OI    FLTODOSW,ACMLSW                                                  
*                                                                               
         CLI   1(R4),8             TEST 8 CHARS                                 
         BL    CMLENERP                                                         
         BE    FLAC04                                                           
         CLI   1(R4),12                                                         
         BH    CMLENERP                                                         
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK                                    
         BNE   FLTCMLE2                                                         
*                                                                               
         OI    FLTODOSW,ACMLADID  SET CMML IS ADID FLAG                         
*                                                                               
FLAC04   MVC   SVACML,WORK                                                      
         B     FLTR80                                                           
*                                                                               
* ASSIGN CMML                                                                   
*                                                                               
FLTACML  DS    0H                                                               
         TM    FLTODOSW,ACMLSW     WAS ASSIGN CML REQESTED                      
         BZ    FLTR81               NO                                          
         NI    FLTODOSW,X'FF'-ACMLSW                                            
*                                                                               
         LA    R6,CURSACML                                                      
*                                                                               
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=X'0A21'                                                  
         TM    FLTODOSW,ACMLADID   TEST CMML IS ADID                            
         BZ    *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
*                                                                               
         MVC   CMLKAM(3),BAGYMD                                                 
         MVC   CMLKCML,SVACML                                                   
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   FLTCMLE2                                                         
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'22'        CML NETWORK ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   FLACML20                                                         
*                                                                               
         USING CMLNETEL,R6                                                      
*                                                                               
FLACML10 DS    0H                                                               
*                                                                               
         CLI   CMLNETLN,6          OLD RECORD?                                  
         BE    FLACML13             YES                                         
         TM    CMLFLG,CMLEXNET     EXCLUDED NETWORKS?                           
         BZ    FLACML13                                                         
         CLC   NETWORK,CMLNET      IS THIS IT                                   
         BE    FLACMLER             YES ERROR                                   
         BRAS  RE,NEXTEL                                                        
         BE    FLACML10                                                         
         B     FLACML20                                                         
*                                                                               
FLACML13 CLC   NETWORK,CMLNET                                                   
         BE    FLACML20                                                         
         BRAS  RE,NEXTEL                                                        
         BE    FLACML10                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
FLACMLER MVC   GERROR,=Y(INVCMLN)  INVALID CML FOR THIS NETWORK                 
         LA    R6,CURSACML                                                      
         J     CKSVTWAP                                                         
*                                                                               
FLACML20 L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R2,R6                                                            
         USING CMLDTAEL,R2                                                      
         TM    CMLSTAT,X'80'       DELETED                                      
         BZ    *+18                                                             
         MVC   GERROR,=Y(CMLISDEL)                                              
         LA    R6,CURSACML                                                      
         J     CKSVTWAP                                                         
*                                                                               
         MVI   ELCODE,X'29'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLMPREL,R6                                                      
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
         MVI   BYTE,0                                                           
*                                                                               
FLTR72   DS    0H                                                               
         BRAS  RE,TFTR             FILTER UNIT TABLE                            
         BNE   FLTR78                                                           
         TM    UNTFLAG1,X'60'      DELETED UNIT                                 
         BNZ   FLTR78                                                           
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,UNTADTEP),(3,DUB)                                 
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,UNTADTE2),(3,DUB+5)                                 
*                                                                               
         OI    UNTFLAG2,UNTFL2AS   SET PREVIOUSLY ASSIGNED FLAG ON              
*                                                                               
         OC    UNTCML1,UNTCML1     ANY ASSIGNED CML                             
         BZ    *+12                                                             
         TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BZ    FLTR74                                                           
*                                                                               
         NI    UNTFLAG2,X'FF'-UNTFL2AS   TURN OFF PREV ASSIGNED FLAG            
*                                                                               
         LA    RF,UNTPROD                                                       
FLTR72C  BAS   RE,CKCML            SEE IF CML GOOD FOR THIS                     
         BNE   FLTR74                                                           
*                                                                               
         MVC   UNTCML1,SVACML                                                   
         OI    UNTFLAG1,X'08'      SET ON ASSIGNED THIS SESSION                 
         TM    FLTODOSW,ACMLADID   TEST CMML IS ADID                            
         BZ    *+8                                                              
         OI    UNTCMLF,X'80'       SET CMML IS ADID                             
         TM    UNTFLAG4,UNTFLDC1                                                
         BZ    *+8                                                              
         NI    UNTFLAG4,X'FF'-UNTFLDC1 TURN OFF CML1 DLTD THIS SESSION          
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+10                YES, UNTREF=MAP YEAR/MAP CODE                
         XC    UNTREF,UNTREF                                                    
         MVI   UNTKEY,0                                                         
         OI    UNITSW1,X'40'       SET ON CML ASSIGNED                          
         MVI   BYTE,255                                                         
         BAS   RE,ADCTR10          DECREMENT UNASGND COUNT                      
         BAS   RE,ADCTR30          INCR PREV ASGND COUNT                        
         TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BZ    FLTR78                                                           
         NI    UNTFLAG1,X'FF'-X'80' SET OFF REASSIGN FLAG                       
         CLC   UNTCML2,REASIGN                                                  
         BNE   FLTR78                                                           
         XC    UNTCML2,UNTCML2                                                  
         B     FLTR78                                                           
FLTR74   DS    0H                                                               
         TM    UNTFLAG3,UNTFL3TB                                                
         BO    FLTR78                                                           
         OC    UNTCML2,UNTCML2     ANY ASSIGNED CML                             
         BZ    *+12                                                             
         TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BZ    FLTR76                                                           
*                                                                               
         NI    UNTFLAG2,X'FF'-UNTFL2AS   TURN OFF PREV ASSIGNED FLAG            
*                                                                               
         LA    RF,UNTPROD2                                                      
         BAS   RE,CKCML            SEE IF CML GOOD FOR THIS                     
*                                                                               
         BNE   FLTR76                                                           
*                                                                               
         MVC   UNTCML2,SVACML                                                   
         OI    UNTFLAG1,X'04'      SET ON ASSIGNED THIS SESSION                 
         TM    FLTODOSW,ACMLADID   TEST CMML IS ADID                            
         BZ    *+8                                                              
         OI    UNTCMLF,X'40'       SET CMML IS ADID                             
         TM    UNTFLAG4,UNTFLDC2                                                
         BZ    *+8                                                              
         NI    UNTFLAG4,X'FF'-UNTFLDC2 TURN OFF CML2 DLTD THIS SESSION          
*                                                                               
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+10                YES, UNTREF=MAP YEAR/MAP CODE                
         XC    UNTREF,UNTREF                                                    
         MVI   UNTKEY,0                                                         
         OI    UNITSW1,X'40'       SET ON CML ASSIGNED                          
         MVI   BYTE,255                                                         
         TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BZ    FLTR78                                                           
         NI    UNTFLAG1,X'FF'-X'80' SET OFF REASSIGN FLAG                       
         CLC   UNTCML1,REASIGN                                                  
         BNE   FLTR78                                                           
         XC    UNTCML1,UNTCML1                                                  
         B     FLTR78                                                           
*                                                                               
* SEE IF COMML COVERS BOTH SPOTS                                                
*                                                                               
FLTR76   TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BO    *+24                                                             
         OC    UNTCML1,UNTCML1     ANY ASSIGNED CML                             
         BNZ   FLTR78                                                           
         OC    UNTCML2,UNTCML2     ANY ASSIGNED CML                             
         BNZ   FLTR78                                                           
*                                                                               
         LLC   R0,UNTSLN                                                        
         LLC   R1,UNTSLN2                                                       
         AR    R1,R0                                                            
         STC   R1,DUB+6            THIS EQUAL TO BOTH SPOT LENGTHS              
*                                                                               
         MVC   DUB+3(3),UNTPROD                                                 
         LA    RF,DUB+3                                                         
         BAS   RE,CKCML            SEE IF CML GOOD FOR THIS                     
*                                                                               
         BNE   FLTR78                                                           
*                                                                               
         MVC   DUB+3(3),UNTPROD2                                                
         LA    RF,DUB+3                                                         
         BAS   RE,CKCML            SEE IF CML GOOD FOR THIS                     
*                                                                               
         BNE   FLTR78                                                           
*                                                                               
         MVC   UNTCML1,SVACML                                                   
         MVC   UNTCML2,SVACML                                                   
         TM    FLTODOSW,ACMLADID   TEST CMML IS ADID                            
         BZ    *+8                                                              
         OI    UNTCMLF,X'C0'       SET CMML1 AND 2 ARE ADID                     
         OI    UNTFLAG1,X'1C'  SET ON ASSIGNED THIS SESS/CML FOR BOTH           
         NI    UNTFLAG1,X'FF'-X'80' SET OFF REASSIGN FLAG                       
         OI    UNITSW1,X'40'       SET ON CML ASSIGNED                          
         OI    UNITSW,X'20'        SVTWA NEEDED                                 
         MVI   BYTE,255                                                         
FLTR78   LA    R5,UNTNEXT                                                       
         CLI   0(R5),0                                                          
         BNE   FLTR72                                                           
         CLI   BYTE,255            ANY UNITS FOUND FOR THIS CML                 
         BE    FLTR81               YES                                         
         MVC   GERROR,=Y(NOUNT4CM)                                              
         LA    R2,TRAOPTH                                                       
         J     CKSVTWA                                                          
*                                                                               
* TURN OFF THE PROFILE SO WE CAN RE-ASSIGN PREV ASSIGNED CMLS                   
*                                                                               
FLTCLEAR MVI   SVTN1PR9,C'N'                                                    
         OI    FLTRSW,X'80'        CLEAR PREV ASSIGNED CMLS                     
         OI    FLTODOSW,CLRSW                                                   
         XC    TRAPAS,TRAPAS       CLR PREV ASSIGN=  FIELD                      
         OI    TRAPASH+6,X'80'                                                  
         B     FLTR80                                                           
*                                                                               
CKCML    NTR1                                                                   
         CLI   CMLSLN,X'FF'        ALL SPOT LEN'S                               
         BE    CKCML10                                                          
*                                                                               
         CLC   CMLSLN,3(RF)        RIGHT SPOT LEN                               
         BNE   CKCMLX                                                           
CKCML10  CLC   CMLRLSE,DUB+5       CK UNIT AIR DATE IN CML PERIOD               
         BH    CKCMLX                                                           
         CLC   CMLRCL,DUB                                                       
         BL    CKCMLX                                                           
         CLI   CMLMPRS,X'FF'       IS THIS COMML PRD=ALL                        
         BE    CKCMLX                                                           
         LLC   R0,1(R6)                                                         
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         LA    R1,CMLMPRS                                                       
CKCML20  CLC   0(3,RF),0(R1)       SAME PROD AS UNIT                            
         BE    CKCMLX                                                           
         LA    R1,3(,R1)                                                        
         BCT   R0,CKCML20                                                       
         LTR   RE,RE                                                            
CKCMLX   XIT1                                                                   
         DROP  R2,R6                                                            
         EJECT                                                                  
FLTR80   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,FLTR10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
FLTR81   DS    0H           SEE IF THERE ARE MORE FILTERS TO PROCESS            
         TM    FLTODOSW,TBASW                                                   
         BO    FLTBA                                                            
         TM    FLTODOSW,CLRSW                                                   
         BO    FLTCLR                                                           
         TM    FLTODOSW,ACMLSW                                                  
         BO    FLTACML                                                          
         TM    FLTODOSW,PATSW                                                   
         BO    FLTPATT                                                          
*                                                                               
         XC    TRAOPT,TRAOPT                                                    
         LA    R4,TRAOPT                                                        
         LR    R3,R4                                                            
         OC    FLTRCML,FLTRCML                                                  
         BZ    FLTR82                                                           
         MVC   0(3,R4),=C'CML'                                                  
         MVI   3(R4),C'='                                                       
*                                                                               
         MVC   4(12,R4),FLTCMLD                                                 
         LA    R4,15(,R4)                                                       
         CLI   0(R4),C' '          FIND LAST CHAR                               
         BNH   *+12                                                             
         LA    R4,1(R4)                                                         
         B     FLTR82                                                           
         BCT   R4,*-16                                                          
*                                                                               
FLTR82   CLI   FLTRDATE,0                                                       
         BE    FLTR84                                                           
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         MVC   0(4,R4),=C'DATE'                                                 
         MVI   4(R4),C'='                                                       
         CLI   FLTRDATS,0                                                       
         BE    *+14                                                             
         MVC   5(1,R4),FLTRDATS                                                 
         LA    R4,1(,R4)                                                        
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,FLTRDATE),(5,5(R4))                               
         LA    R4,13(R4)                                                        
         CLI   FLTRDAT2,0                                                       
         BE    FLTR84                                                           
         MVI   0(R4),C'-'                                                       
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,FLTRDAT2),(5,1(R4))                                 
         LA    R4,9(R4)                                                         
FLTR84   CLI   FLTRSLN,0                                                        
         BE    FLTR86                                                           
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         MVC   0(3,R4),=C'LEN'                                                  
         MVI   3(R4),C'='                                                       
         LLC   R0,FLTRSLN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R4),DUB                                                      
         LA    R4,7(,R4)                                                        
FLTR86   CLI   FLTRPROD,0                                                       
         BE    FLTR88                                                           
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         MVC   0(3,R4),=C'PRD'                                                  
         MVI   3(R4),C'='                                                       
         MVC   4(3,R4),FLTRPROD                                                 
FLTR88   OI    TRAOPTH+6,X'80'                                                  
FLTR89   OI    4(R2),X'20'         SET VALIDATED                                
FLTRX    XIT1                                                                   
FLTR90   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         LA    R2,TRAOPTH                                                       
         MVC   CONHEAD(L'FLTHELP),FLTHELP                                       
         LA    R1,CONHEAD+L'FLTHELP                                             
         CLI   SVTN2PR8,C'Y'       ALLOWED TO SHOW COST?                        
         BE    FLTR94                                                           
         MVC   CONHEAD+L'FLTHELP-5(5),SPACES                                    
         LA    R1,CONHEAD+L'FLTHELP-5                                           
FLTR94   DS    0H                                                               
         CLI   SVTNPR5,C'Y'       TRAFFIC DELETES ALLOWED                       
         BNE   ERREX2X                                                          
         MVC   CONHEAD+L'FLTHELP(8),=C'/REST= *'                                
ERREX2X  GOTO1 ERREX2                                                           
*                                                                               
* POSITION THE CURSOR WITHIN THE FIELD                                          
****** (R6 HOLDS THE DISP) ******                                               
*                                                                               
CURSPOS  DS    0H                                                               
         L     R1,SYSPARMS                                                      
         L     RF,0(R1)            RF = A(TIOB)                                 
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC+TIOBALRM                                       
         LA    R0,TRAOPTH                                                       
         STH   R0,TIOBCURD                                                      
         MVC   TIOBCURI,0(R6)                                                   
         DROP  RF                                                               
         MVI   PFKEY,0             IGNORE ANY PFKEY HIT                         
         BR    RE                                                               
*                                                                               
INVCMML  BRAS  RE,CURSPOS          POSITION THE CURSOR                          
         MVI   ERROR,INVCOMM       NO SUCH COMMERCIAL FOR CLT                   
         J     TRAPERR4                                                         
*                                                                               
CMLENERP BRAS  RE,CURSPOS          POSITION THE CURSOR                          
         MVC   GERROR,=Y(NOT812)   COMMERCIAL MUST BE 8 CHAR                    
         GOTO1 VTRAERR                                                          
*                                                                               
INVPDERR TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    INVPDER                                                          
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         J     TRAPERR4                                                         
*                                                                               
INVPDER  MVI   ERROR,INVPROD                                                    
         J     TRAPERR4                                                         
*                                                                               
MISSERR1 MVI   ERROR,MISSING                                                    
         J     TRAPERR4                                                         
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         J     TRAPERR4                                                         
*                                                                               
NUMERR1  MVI   ERROR,NOTNUM                                                     
         J     TRAPERR4                                                         
*                                                                               
DTPERERR MVC   GERROR,=Y(DTNINPER)                                              
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
* VARIOUS ERROR EXITS                                                           
*                                                                               
TRAPERR4 TM    UNITSW,X'20'        SVTWA NEEDED                                 
         JZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 ERREX                                                            
         J     FLTRX                                                            
*                                                                               
FLTHELP  DC    C'FTR=DATE/PRD/CML/TBA=CML/LEN/ACML/CLR/PAT=/COST'               
DUPREQ1  DC    C'* ERROR * ONE COMMERCIAL AT A TIME *'                          
REASIGN  DC    CL9'REASSIGN '                                                   
         EJECT                                                                  
* FILTER VALIDATION RTNS:                                                       
*   BYTES 00-09 = FILTER KEYWORD                                                
*   BYTES 10-13 = A(VALI RTN)                                                   
*                                                                               
FLTTABLE DS    0CL14                                                            
         DC    CL10'DATE      ',AL4(FLTDATE)                                    
         DC    CL10'PRD       ',AL4(FLTPRD)                                     
         DC    CL10'CML       ',AL4(FLTCML)                                     
         DC    CL10'COST      ',AL4(FLTCOST)                                    
         DC    CL10'LEN       ',AL4(FLTLEN)                                     
         DC    CL10'RESTORE   ',AL4(FLTREST)                                    
         DC    CL10'ACML      ',AL4(FLACMLSW)                                   
         DC    CL10'PATTERN   ',AL4(FLPATTSW)                                   
         DC    CL10'CLEAR     ',AL4(FLTCLEAR)                                   
         DC    CL10'CLR       ',AL4(FLTCLEAR)                                   
         DC    CL10'LINE      ',AL4(FLTLINE)                                    
         DC    CL10'FEED      ',AL4(FLTFEED)                                    
         DC    CL10'NOFEED    ',AL4(FLTNOFD)                                    
         DC    CL10'TBA       ',AL4(FLTBASW)                                    
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
ADCTR    LH    RF,TOTUNITS                                                      
         BCTR  RF,0                SUB 1 FROM TOTUNITS                          
         STH   RF,TOTUNITS                                                      
         OC    UNTCML1,UNTCML1                                                  
         BZ    ADCTR10                                                          
         CLC   UNTCML1,REASIGN                                                  
         BE    ADCTR10                                                          
         OC    UNTPROD2,UNTPROD2                                                
         BZR   RE                                                               
         OC    UNTCML2,UNTCML2                                                  
         BNZR  RE                                                               
ADCTR10  LH    RF,UNASGND                                                       
         BCTR  RF,0                                                             
         STH   RF,UNASGND                                                       
         BR    RE                                                               
ADCTR20  LH    RF,UNASGND                                                       
         LA    RF,1(RF)                                                         
         STH   RF,UNASGND                                                       
         BR    RE                                                               
ADCTR30  LH    RF,PREVASS                                                       
         LA    RF,1(RF)                                                         
         STH   RF,PREVASS                                                       
         BR    RE                                                               
ADCTR40  LH    RF,PREVASS                                                       
         BCTR  RF,0                                                             
         STH   RF,PREVASS                                                       
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
         EJECT                                                                  
*=======================================================                        
* VALIDATE COMMERCIAL                                                           
* ON ENTRY R4 POINTS TO CMML FIELD IN UNIT TABLE                                
*=======================================================                        
                                                                                
         USING UNTABLED,R5                                                      
VCML     NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R2),21            MAKE SURE IT'S A COMMERCIAL                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    REVALID,X'02'       REVALID P/B                                  
         BO    VCML01                                                           
*                                                                               
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BZ    VCML01                                                           
         MVC   SVCML,8(R2)                                                      
         B     VCML50              YES                                          
*                                                                               
VCML01   CLI   5(R2),0             ANY ENTRY                                    
         BE    VCML50                                                           
*                                                                               
         CLC   =CL8'VIGNETTE',UNTBBSN                                           
         BNE   VCML02                                                           
         OI    VIGNFLG,X'01'                                                    
*                                                                               
VCML02   GOTO1 ANY                                                              
         CLC   WORK(8),SPACES       ANY CML                                     
         BNH   VCML50               NO                                          
         CLC   WORK(8),=C'REASSIGN'                                             
         BE    VCML50                                                           
         CLC   WORK(6),=C'DELETED'  DELETE THIS CML?                            
         BNE   VCML06                                                           
*                                                                               
VCML02B  DS    0H                                                               
         TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BO    VCML04                                                           
         OC    0(8,R4),0(R4)       IS THERE A CML                               
         BNZ   VCML02C                                                          
         MVC   GERROR,=Y(BDDEL)                                                 
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
*                                                                               
VCML02C  DS    0H                                                               
         TM    UNTFLAG1,UNTFL1PB   CML FOR BOTH PRDS                            
         BZ    VCML04                                                           
*                                                                               
         LA    R0,UNTCML2                                                       
         CR    R0,R4               THIS SECOND OF PAIR                          
         BNE   VCML03                                                           
*                                                                               
         MVC   GERROR,=Y(DELPB)                                                 
         MVC   8(8,R2),0(R4)        RESTORE CML                                 
         OI    6(R2),X'80'                                                      
         OI    4(R2),X'20'         SET ON VALIDATED AGAIN                       
         TM    UNTCMLF,X'40'       TEST ADID                                    
         BZ    VCML02F                                                          
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'U',0(R4)),8(R2)                                  
*                                                                               
VCML02F  TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
*                                                                               
VCML03   NI    UNTFLAG1,X'FF'-X'10'      CML FOR BOTH PRDS                      
         XC    8(8,R4),8(R4)       DELETE PARTNER                               
         OI    UNTFLAG4,UNTFLDC2   CML2 DELETED THIS SESSION                    
         NI    UNTCMLF,X'FF'-X'40' TURN OFF ADID FLAG                           
*                                                                               
VCML04   XC    0(8,R4),0(R4)       DELETE                                       
         XC    8(8,R2),8(R2)       DELETE                                       
         LA    R0,UNTCML2                                                       
         CR    R4,R0               THIS CML2                                    
         BNE   *+16                                                             
         OI    UNTFLAG4,UNTFLDC2   CML2 DELETED THIS SESSION                    
         NI    UNTCMLF,X'FF'-X'40' TURN OFF ADID FLAGS                          
         B     VCML05                                                           
*                                                                               
         CR    R4,R0                                                            
         BH    VCML05              ITS CML3 ?                                   
         OI    UNTFLAG4,UNTFLDC1   CML1 DELETED THIS SESSION                    
         NI    UNTCMLF,X'FF'-X'80' TURN OFF ADID FLAGS                          
*                                                                               
VCML05   NI    UNTFLAG1,X'FF'-X'80' SET OFF REASSIGN NEEDED                     
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+10                YES, UNTREF=MAP YEAR/MAP CODE                
         XC    UNTREF,UNTREF                                                    
         MVI   UNTKEY,0                                                         
         B     VCML50                                                           
*                                                                               
VCML06   CLI   5(R2),9             CHECK FOR #NNNNNNNN                          
         BNE   VCML06A                                                          
         CLI   8(R2),C'#'                                                       
         BNE   VCML06A                                                          
         MVC   WORK(8),9(R2)                                                    
         MVC   8(8,R2),WORK        MOVE INPUT TO THE LEFT + X'00'               
         MVI   16(R2),0                                                         
         MVI   5(R2),8             SET NEW INPUT LENGTH                         
         OI    6(R2),X'80'         SEND IT BACK TO USER                         
         B     VCML20              AND IT'S AN ISCI!                            
*                                                                               
VCML06A  CLI   5(R2),8                                                          
         BH    VCML10              9-12 IS ADID                                 
         BL    VCML07                                                           
* INPUT LEN IS 8 - CHECK IF STARTS WITH 4 ALPHA                                 
* IF IT'S NOT ALPHA, SEND IT TO THE ADID ROUTINE                                
         OC    SVCML,SVCML         TEST HAVE PREVIOUS CMML                      
         BZ    VCML20                                                           
*MNCD                                                                           
         CLI   5(R2),8                                                          
         BE    VCML20              9-12 IS ADID                                 
*MNCD                                                                           
         LA    R0,4                                                             
*MNCD    LA    R0,2                ONLY CHECK FIRST 2 FOR COPY DOWN             
         LA    R1,8(R2)                                                         
*                                                                               
VCML06B  CLI   0(R1),C'A'                                                       
         BL    VCML08                                                           
         CLI   0(R1),C'Z'                                                       
         BH    VCML08                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VCML06B                                                       
         B     VCML20                                                           
*                                                                               
VCML07   CLI   5(R2),4             CAN BE 4                                     
         BL    *+14                                                             
         OC    SVCML,SVCML         IF THERE WAS A PREV VALID CML                
         BNZ   VCML08                                                           
*                                                                               
         MVC   GERROR,=Y(NOT812)                                                
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
*                                                                               
VCML08   MVC   SVCML+4(8),SPACES                                                
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVCML+4(0),8(R2)                                                 
*                                                                               
         MVC   8(12,R2),SVCML       IF ERROR, SEND THIS CML BACK                
         LA    R1,SVCML+11          POINT TO LAST CHAR                          
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,SVCML-1                                                       
         SR    R1,R0                                                            
         STC   R1,5(R2)            SET FIELD LENGTH                             
*                                                                               
         MVC   WORK(12),SVCML                                                   
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   5(R2),8             TEST ISCI INPUT                              
         BE    VCML20              YES                                          
*                                                                               
VCML10   CLI   5(R2),12            INPUT IS ADID                                
         BH    CMLENER1                                                         
                                                                                
         MVC   SVCML,WORK          SAVE EBCDIC CMML                             
         GOTO1 VTRPACK,DMCB,(C'P',WORK),SVCMLP                                  
         JNE   VCMLERR1                                                         
         MVI   SVCMLFLG,C'Y'       SET ADID                                     
         B     VCML22                                                           
*                                                                               
VCML20   MVI   SVCMLFLG,C'N'       SET ISCI CMML                                
         MVC   SVCML,WORK                                                       
         MVC   SVCMLP,SVCML        RETURN ISCI HERE TOO                         
*                                                                               
VCML22   NI    REVALID,X'FF'-NOEQCCXT                                           
         XC    SVPROD,SVPROD       RESET VALIDATE THIS PRD IN FCML              
         XC    WORK,WORK                                                        
         MVC   WORK(8),SVCMLP                                                   
*                                                                               
         BRAS  RE,FCML             READ COMMERCIAL RECORD                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'29'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLMPREL,R6                                                      
         CLI   CMLMPRS,X'FF'       IS THIS COMML PRD=ALL                        
         BE    VCML40                                                           
*                                                                               
         LLC   R0,CMLMPRLN                                                      
         BCTR  R0,R0                                                            
         BCTR  R0,R0                                                            
         LA    R1,CMLMPRS                                                       
*                                                                               
VCML30   CLC   QPRD,0(R1)          MATCH TO PROD                                
         BE    VCML34              YES                                          
         LA    R1,3(,R1)                                                        
         SHI   R0,2                                                             
         BCT   R0,VCML30                                                        
*                                                                               
VCML31   MVC   GERROR,=Y(BDUNTPRD)                                              
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
*                                                                               
VCML34   OC    QPRD2,QPRD2                                                      
         BZ    VCML40                                                           
         LLC   R0,CMLMPRLN                                                      
         BCTR  R0,R0                                                            
         BCTR  R0,R0                                                            
         LA    R1,CMLMPRS          START OF PROD LIST                           
*                                                                               
VCML36   CLC   QPRD2,0(R1)         MATCH TO PROD                                
         BE    VCML40              YES                                          
         LA    R1,3(R1)                                                         
         SHI   R0,2                                                             
         BCT   R0,VCML36                                                        
*                                                                               
         TM    VIGNFLG,X'04'       IS IT VIGNETTE                               
         BNZ   VCML38                                                           
         MVC   GERROR,=Y(PBNINCML) NO/ERROR                                     
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
*                                                                               
VCML38   XC    QPRD2,QPRD2         YES/FUDGE IT                                 
*                                                                               
VCML40   BRAS  RE,VCMLAPR          GO CHECK STARCOM DATA                        
*                                                                               
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BZ    VCML42                       NO                                  
         TM    CMLFLAG1,NOAIR                                                   
         BO    NOAIRERR                                                         
         TM    CMLFLAG1,MAXDTE                                                  
         BO    MAXDTER                                                          
         TM    CMLFLAG1,CADTE                                                   
         BO    CADTERR                                                          
*                                                                               
VCML42   OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
         LA    R1,SVCML            POINT TO EBCDIC CMML                         
         CLI   SVCMLFLG,C'Y'       TEST ADID INPUT THIS TIME                    
         BNE   *+8                                                              
         LA    R1,SVCMLP           POINT TO PACKED ADID                         
         CLC   0(8,R4),0(R1)       COMPARE TO CMML IN UNIT RECORD               
         BNE   *+12                YES                                          
         OI    REVALID,NONEW                                                    
         B     VCML50                                                           
*                                                                               
         NI    REVALID,X'FF'-NONEW                                              
         MVC   0(8,R4),WORK        SAVE VALIDATED CML                           
         CR    RB,RB                                                            
         B     VCMLX                                                            
*                                                                               
VCML50   LTR   RB,RB                                                            
*                                                                               
VCMLX    XIT1                                                                   
*                                                                               
         USING CMLBBEL,R6                                                       
MAXDTER  MVC   CONHEAD(L'MAXDTM1),MAXDTM1                                       
         GOTO1 DATCON,DMCB,(3,CMLBBMXD),(5,CONHEAD+L'MAXDTM1+1)                 
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
*                                                                               
         GOTO1 ERREX2                                                           
*                                                                               
MAXDTM1  DC    C'* ERROR * MAX USE DATE ='                                      
         DS    0H                                                               
CADTERR  MVC   GERROR,=Y(NOCLADTE) NO CLIENT APPROVAL DATE                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,9                                                           
         MVC   ELEM+1(8),WORK                                                   
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
*                                                                               
         BRAS  RE,SVTWA                                                         
*                                                                               
         GOTO1 VTRAERR                                                          
*                                                                               
NOAIRERR MVC   GERROR,=Y(NAPRTAIR) NOT APPROVED TO AIR                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,13                                                          
         MVC   ELEM+1(12),SVCML                                                 
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
*                                                                               
         BRAS  RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
*                                                                               
CMLENER1 MVC   GERROR,=Y(NOT812)   COMMERCIAL MUST BE 8-12 CHARS                
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
         DROP  R6                                                               
         EJECT                                                                  
* FURTHER VALIDATE CML (APPROVALS AND DATES IF ANY)                             
*                                                                               
VCMLAPR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    CMLFLAG1,X'FF'-(NOAIR+MAXDTE+CADTE) INIT ERR FLAGS               
*                                                                               
         MVI   ELCODE,X'90'        BORADCAST BUSINESS ELEMENT                   
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    VAPR10                                                           
         CLI   SVTN1PRD,C'Y'       IS THIS BRAND AGENCY                         
         BNE   VAPRX                                                            
         OI    CMLFLAG1,NOAIR      YES, NOT APPROVED TO AIR                     
         B     VAPRX                                                            
*                                                                               
         USING CMLBBEL,R6                                                       
VAPR10   DS    0H                                                               
         CLI   CMLBBBAG,C'Y'       IS BRAND AGY=Y                               
         BNE   VAPRX                NO                                          
         OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                             
         BZ    VAPR20                                                           
*                                                                               
         CLC   DUB+3(3),CMLBBMXD   COMPARE END AIR DATE TO MAX USE DTE          
         BNH   VAPR20                                                           
         OI    CMLFLAG1,MAXDTE     MAX DATE ERROR                               
         B     VAPRGX                                                           
*                                                                               
VAPR20   DS    0H                                                               
         CLI   CMLBBBAG,0          LEO CML?                                     
         BNE   *+12                                                             
         CLI   SVTN1PRD,C'Y'       IS THIS BRAND AGENCY                         
         BE    *+12                                                             
         CLI   CMLBBBAG,C'Y'       LEO B. CML                                   
         BNE   VAPRX                NO, DONE                                    
*                                                                               
         OC    CMLBBCAD,CMLBBCAD   ANY CLIENT APPROVAL DATE?                    
         BNZ   *+12                                                             
         OI    CMLFLAG1,CADTE       NO, ERROR                                   
         B     VAPRX                                                            
*                                                                               
         CLI   CMLBBAPR,C'Y'       BROADCAST BUSINESS APPROVAL?                 
         BNE   VAPR40                                                           
*                                                                               
         OC    CMLBBREF,CMLBBREF   ANY CML REFERENCE?                           
         BNZ   *+12                                                             
         OI    CMLFLAG1,NOAIR      NO, ERROR                                    
         B     VAPRX                                                            
*                                                                               
* CHECK IF CML FROM REFERENCE FIELD IS APPROVED TO AIR                          
*                                                                               
         MVC   WORK+56(8),WORK      SAVE ORIG COMML                             
         MVC   WORK(8),CMLBBREF                                                 
         OI    CMLFLAG1,JUSTAPR     NEED CML FOR APPROVALS                      
         NI    REVALID,X'FF'-NOEQCCXT                                           
         BRAS  RE,FCML              GO FIND COMMERCIAL                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    REVALID,RESOCML     YES, RESTORE ORIGINAL CML REC                
*                                                                               
         NI    CMLFLAG1,X'FF'-JUSTAPR  NEED CML FOR APPROVALS                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         BE    *+12                                                             
         OI    CMLFLAG1,NOAIR      NOT APPROVED TO AIR                          
         B     VAPRX                                                            
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
         CLI   CMLATAIR,C'Y'                                                    
         BE    *+12                                                             
         OI    CMLFLAG1,NOAIR      NOT APPROVED TO AIR                          
         B     VAPRX                                                            
*                                                                               
         MVC   WORK(8),WORK+56     RESTORE ORIG COMML                           
         OI    CMLFLAG1,JUSTAPR    JUST GET APPROVALS                           
*                                                                               
VAPR30   DS    0H                                                               
         MVC   SVANET,CMLANET      SAVE NET APPROVAL BITS                       
         BAS   RE,GSTATION                                                      
*                                                                               
         TM    REVALID,RESOCML     RESTORE ORIGINAL CML REC                     
         BZ    VAPRX                                                            
*                                                                               
         XC    SVPROD,SVPROD       RESET VALIDATE THIS PRD IN FCML              
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         BRAS  RE,FCML                                                          
         NI    REVALID,X'FF'-RESOCML                                            
         NI    REVALID,X'FF'-NOEQCCXT                                           
VAPRX    XIT1                                                                   
VAPRGX   XIT1  REGS=(R6)                                                        
*                                                                               
VAPR40   DS    0H                                                               
         CLI   CMLATAIR,C'Y'       APPROVED TO AIR                              
         BE    *+12                                                             
         OI    CMLFLAG1,NOAIR      NO, ERROR                                    
         B     VAPRX                                                            
*                                                                               
         NI    CMLFLAG1,X'FF'-JUSTAPR  TURN OFF JUST GET APPROVALS              
         B     VAPR30                                                           
*                                                                               
*        SEE THAT NET IS APPROVED                                               
*                                                                               
GSTATION NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         MVC   SVWORK,WORK                                                      
*                                                                               
         BRAS  RE,INITNET          CHANGE TO UNIT FILE                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STATRECD,R4         STATION RECORD                               
         MVI   STATKID,X'29'       RECORD ID                                    
         MVC   STATKAM,BAGYMD      AGY/MED                                      
*                                                                               
         MVC   STATKNET,NETWORK    VALIDATE THIS NETWORK ONLY                   
         OC    STATKNET,SPACES                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      IS THIS IT                                   
         BNE   GSTATX                                                           
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,2004(R6)                                                      
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         BE    GSTAT20                                                          
         DC    H'0'                                                             
*                                                                               
         USING STADATEL,R6                                                      
GSTAT20  CLC   STAIDATE,=X'FFFFFF' ANY INACTIVE DATE                            
         BE    GSTAT30              NO                                          
*                                                                               
         MVC   IDATE,STAIDATE                                                   
         XC    IDATE,=X'FFFFFF'    INVERT INACTIVE DATE                         
*                                                                               
         CLC   IDATE,STDATEB       IF INACTIVE IS BEFORE PER START              
         BL    GSTATX                                                           
*                                                                               
GSTAT30  DS    0H                                                               
         CLC   STAADATE,ENDATEP    ACTIVE DATE BEFORE PER END DATE              
         BH    GSTATX               YES                                         
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              AIO2+2004                                    
GSTAT40  MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING STACODEL,R6                                                      
*                                                                               
         MVC   SVCODE,STACODE      SAVE STATION CODE                            
*                                                                               
         MVC   WORK(8),SVANET      APPROVED NETWORKS                            
         NC    WORK(8),SVCODE      AGAINST THIS NETWORK CODE                    
         XC    SVCODE,WORK                                                      
         BZ    GSTATX                                                           
         OI    CMLFLAG1,NOAIR      NOT APPROVED TO AIR                          
*                                                                               
GSTATX   DS    0H                                                               
         BRAS  RE,INITSPT         CHANGE TO SPOT                                
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         GOTO1 HIGH                DUMMY READ HI FOR SEQ                        
*                                                                               
         MVC   WORK,SVWORK         RESTORE WORK                                 
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*===============================================================                
* FIND CMML AND CHECK FOR VALIDITY                                              
*===============================================================                
                                                                                
FCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITSPT         CHANGE TO SPOT                                
*                                                                               
         USING UNTABLED,R5                                                      
         LA    R4,KEY              BUILD CMML KEY                               
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,WORK                                                     
*                                                                               
         OC    WORK(8),WORK                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVWORK(8),SVWORK                                                 
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FCML01                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY(2),=X'0AC1'                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FCML01                                                           
*                                                                               
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BZ    VCMLERR1                                                         
         B     FCMLNEX                                                          
*                                                                               
FCML01   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   WORK(8),5(R6)                                                    
         TM    15(R6),CMLKSTA_PCKD   TEST CMML CODE IS PACKED                   
         BZ    FCML01X                                                          
         GOTO1 VTRPACK,DMCB,(C'U',5(R6)),WORK                                   
*                                                                               
FCML01X  TM    REVALID,RESOCML     JUST RESTORE ORIGINAL CML REC                
         BO    FCMLEQX                                                          
*                                                                               
         TM    CMLFLAG1,JUSTAPR    NEED CML FOR APPROVALS                       
         BO    FCMLEQX                                                          
*                                                                               
         MVI   ELCODE,X'22'        CML NETWORK ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   FCML04                                                           
*                                                                               
         USING CMLNETEL,R6                                                      
*                                                                               
FCML02   CLI   CMLNETLN,6          OLD RECORD?                                  
         BE    FCML03               YES                                         
         TM    CMLFLG,CMLEXNET     EXCLUDED NETWORKS?                           
         BZ    FCML03                                                           
         CLC   NETWORK,CMLNET      IS THIS IT                                   
         BNE   *+16                                                             
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BZ    FCMLERR                                                          
         B     FCMLNEX                                                          
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    FCML02                                                           
         B     FCML04                                                           
*                                                                               
FCML03   CLC   NETWORK,CMLNET                                                   
         BE    FCML04                                                           
         BRAS  RE,NEXTEL                                                        
         BE    FCML02                                                           
*                                                                               
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BO    FCMLNEX                                                          
*                                                                               
FCMLERR  MVC   GERROR,=Y(INVCMLN)  INVALID CML FOR THIS NETWORK                 
         J     CKSVTWA                                                          
*                                                                               
FCML04   L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       COMMERCIAL DELETED                           
         BZ    FCML04A                                                          
*                                                                               
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BO    FCMLNEX                                                          
*                                                                               
         MVC   GERROR,=Y(CMLISDEL)                                              
         J     CKSVTWA                                                          
*                                                                               
         FIXDT02                                                                
FCML04A  GOTO1 DATCON,DMCB,(2,UNTADTEP),(3,DUB)                                 
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,UNTADTE2),(3,DUB+3)                                 
*                                                                               
         CLC   CMLRLSE,DUB+3       SEE IF AIR DATE FALLS IN CML PERIOD          
         BH    CMLDTER             NO                                           
         CLC   CMLRCL,DUB                                                       
         BL    CMLDTER                                                          
*                                                                               
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BZ    *+10                                                             
         MVC   DUB(1),CMLSLN       RETURN LEN IN DUB                            
*                                                                               
         MVC   SVCMLSOL,CMLSOLO    SAVE SOLO/PIGGYBACK                          
*                                                                               
         CLI   SVCMLSOL,C'P'      PIGGYBACK                                     
         BE    FCML06                                                           
         CLI   SVCMLSOL,C'S'      SOLO                                          
         BE    FCML06                                                           
         MVI   SVCMLSOL,0          FORCE ALL ELSE TO NULLS                      
*                                                                               
FCML06   DS   0H                                                                
         CLI   JUSLEN,C'Y'         ONLY SET LENGTH                              
         BE    FCML08                                                           
*                                                                               
         OC    UNTPROD2,UNTPROD2     IS THIS CML FOR BOTH PRDS                  
         BNZ   FCML06C                YES                                       
*                                                                               
         CLI   SVCMLSOL,C'P'       MUST NOT BE P/B                              
         BNE   FCML07                                                           
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BO    FCMLNEX                                                          
         B     SOLOER1                                                          
*                                                                               
FCML06C  CLI   SVCMLSOL,C'S'       MUST NOT BE SOLO                             
         BNE   FCML07                                                           
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BO    FCMLNEX                                                          
         B     SOLOER1                                                          
*                                                                               
FCML07   DS    0H                                                               
         OC    UNTPROD2,UNTPROD2     IS THERE A PARTNER PRD                     
         BZ    FCML08                 NO                                        
         TM    UNITSW1,UNTSW1NL    THIS BILLBOARD                               
         BO    FCML08               YES, DO NOT CHECK TYPE                      
*                                                                               
         OC    SVTYPE,SVTYPE                                                    
         BNZ   *+14                                                             
         MVC   SVTYPE,CMLTYPE                                                   
         B     FCML08                                                           
*                                                                               
         CLC   SVTYPE,CMLTYPE                                                   
         BE    FCML08                                                           
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BO    FCMLNEX                                                          
         B     TYPERR                                                           
*                                                                               
FCML08   CLI   JUSLEN,C'Y'         ONLY SET LENGTH                              
         BNE   FCML10                                                           
         MVI   JUSLEN,0                                                         
         MVC   WORK(1),CMLSLN      RETURN COML LEN IN WORK                      
         B     FCMLX                                                            
*                                                                               
FCML10   TM    UNITSW1,UNTSW1NL    THIS BILLBOARD                               
         BO    FCML350              YES, NO LEN CHECKING                        
*                                                                               
         CLI   CMLSLN,X'FF'        THIS ALL SPOT LEN'S                          
         BE    FCML350                                                          
*                                                                               
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BZ    FCML12                                                           
*                                                                               
         OC    UNTPROD2,UNTPROD2  IS THERE A SECOND PROD                        
         BZ    FCML11              NO                                           
         CLI   UNTSLN2,0          IS THERE A SECOND LEN                         
         BE    FCML11F             NO, 1ST MUST COVER BOTH                      
*                                                                               
FCML11   TM    PRDMATSW,X'02'     LOOKING FOR PARTNER PROD                      
         BO    FCML11C                                                          
         CLC   UNTSLN,CMLSLN      IS THIS CML SAME SPOT LENGTH                  
         BE    FCML350                                                          
*                                                                               
         OC    UNTPROD2,UNTPROD2  IS THERE A SECOND PROD                        
         BZ    FCML300              NO                                          
         B     FCML11F                                                          
*                                                                               
FCML11C  CLC   UNTSLN2,CMLSLN     YES, MUST MATCH PRD2 LEN                      
         BE    FCML350                                                          
*                                                                               
FCML11F  LLC   R0,UNTSLN          GET TOTAL LEN                                 
         LLC   R1,UNTSLN2                                                       
         AR    R1,R0                                                            
         CLM   R1,1,CMLSLN         THIS EQUAL BOTH                              
         BE    FCML350                                                          
*                                                                               
         OC    UNTPROD2,UNTPROD2  IS THERE A SECOND PROD                        
         BZ    FCML300             NO/CHECK VIGNETTE                            
*                                                                               
         CLI   UNTSLN2,0          100 PERCENT TO PROD 1                         
         BE    FCML300             CHECK VIGNETTE                               
*                                                                               
         TM    PRDMATSW,X'02'     LOOKING FOR PARTNER PROD                      
         BZ    FCML18                                                           
*                                                                               
* IF UNIT LEN2 IS NOT EQUAL TO CML2 AND CML1 IS NOT ASSIGNED                    
* THEN UNIT LEN2 EQUALS CML LEN2 AND ADJUST UNIT LEN1                           
         OC    UNTCML1,UNTCML1                                                  
         BNZ   FCML18                                                           
*                                                                               
         LLC   RF,CMLSLN                                                        
         SR    R1,RF               TOT LEN MINUS CML LEN                        
         STC   R1,UNTSLN                                                        
         STC   RF,UNTSLN2                                                       
         B     FCML350                                                          
*                                                                               
FCML12   OC    UNTPROD2,UNTPROD2  IS THERE A SECOND PROD                        
         BZ    FCML14              NO                                           
         CLI   UNTSLN2,0          IS THERE A SECOND LEN                         
         BE    FCML16               NO, 1ST MUST COVER BOTH                     
*                                                                               
         TM    PRDMATSW,X'02'     LOOKING FOR PARTNER PROD                      
         BZ    FCML14                                                           
*                                                                               
         CLC   UNTSLN2,CMLSLN     IS THIS CML SAME SPOT LENGTH                  
         BE    FCML350                                                          
*                                                                               
* IF UNIT LEN2 IS NOT EQUAL TO CML2 AND CML1 IS NOT ASSIGNED                    
* THEN UNIT LEN2 EQUALS CML LEN2 AND ADJUST UNIT LEN1                           
         OC    UNTCML1,UNTCML1                                                  
         BNZ   FCML18                                                           
*                                                                               
         LLC   R0,UNTSLN          GET TOTAL LEN                                 
         LLC   R1,UNTSLN2                                                       
         AR    R1,R0                                                            
         LLC   RF,CMLSLN                                                        
         SR    R1,RF               TOT LEN MINUS CML LEN                        
         STC   R1,UNTSLN                                                        
         STC   RF,UNTSLN2                                                       
         B     FCML350                                                          
*                                                                               
FCML14   CLC   BSLN,CMLSLN         IS THIS CML SAME SPOT LENGTH                 
         BE    FCML20                                                           
*                                                                               
         OC    UNTPROD2,UNTPROD2  IS THERE A SECOND PROD                        
         BZ    FCML300              NO                                          
*                                                                               
FCML16   LLC   R0,BSLN             GET TOTAL LEN                                
         LLC   R1,BSLN2                                                         
         AR    R1,R0                                                            
         CLM   R1,1,CMLSLN         THIS EQUAL BOTH                              
         BE    FCML350                                                          
*                                                                               
         OC    UNTPROD2,UNTPROD2  IS THERE A SECOND PROD                        
         BZ    FCML300             NO/CHECK VIGNETTE                            
*                                                                               
         CLI   BSLN2,0            100 PERCENT TO PROD 1                         
         BE    FCML300             CHECK VIGNETTE                               
*                                                                               
FCML18   CLM   R1,1,CMLSLN        MUST BE MORE THAN CML SPOT LEN                
         BNL   *+16                                                             
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BO    FCMLNEX                                                          
         B     INVSLER1                                                         
*                                                                               
         LLC   R0,CMLSLN          GET CML SPOT LEN                              
         SR    R1,R0                                                            
         STC   R0,UNTSLN                                                        
         STC   R1,UNTSLN2                                                       
         STC   R0,BSLN                                                          
*                                                                               
FCML20   XC    QPRD2,QPRD2         PIGGYBACK NOT NEEDED FOR THIS CML            
         MVI   BSLN2,0                                                          
         B     FCML350                                                          
*                                                                               
FCML300  DS    0H                  VIGNETTE                                     
         BRAS  RE,CHKVIGNT                                                      
         BE    FCMLEQX                                                          
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BZ    INVSLER1                                                         
         B     FCMLNEX                                                          
*                                                                               
* CHECK THAT THIS COMMERCIAL COVERS ALL PRODUCTS                                
*                                                                               
FCML350  L     R6,AIO                                                           
         MVI   ELCODE,X'29'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PRDMATSW,X'01'      PRE-SET COVERS ALL PRDS                      
         USING CMLMPREL,R6                                                      
         CLI   CMLMPRS,X'FF'       ALL PRD=ALL                                  
         BNE   FCML400                                                          
         B     FCMLEQX                                                          
                                                                                
* SEE THAT UNIT PROD IS COVERED BY THIS CML                                     
                                                                                
FCML400  DS    0H                                                               
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BZ    FCMLEQX             VALIDATION DONE ELSE WHERE                   
*                                                                               
         OC    SVPROD,SVPROD       MATCH TO PROD                                
         BZ    FCMLEQX             NO, DONE ELSE WHERE                          
*                                                                               
         LLC   R0,CMLMPRLN                                                      
         BCTR  R0,R0                                                            
         BCTR  R0,R0                                                            
         LA    R1,CMLMPRS          START OF PROD LIST                           
FCML430  CLC   SVPROD,0(R1)        MATCH TO PROD                                
         BE    FCMLEQX             YES                                          
         LA    R1,3(,R1)                                                        
         SHI   R0,2                                                             
         BCT   R0,FCML430                                                       
*                                                                               
         TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BO    FCMLNEX              YES                                         
*                                                                               
         MVC   GERROR,=Y(BDUNTPRD)                                              
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
*                                                                               
FCMLNEX  LTR   RB,RB                                                            
         B     FCMLX10                                                          
*                                                                               
FCMLX    DS   0H                                                                
*        BRAS  RE,CKADID           SEE IF ADID CML, AND SV IF SO                
FCMLX10  XIT1                                                                   
*                                                                               
FCMLEQX  CR    RB,RB                                                            
         B     FCMLX                                                            
         DROP  R6                                                               
*                                                                               
ERREXITC DS   0H                                                                
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
TYPERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TYPERMSG),TYPERMSG                                     
         MVC   CONHEAD+L'TYPERMSG(4),=C'CML='                                   
         MVC   CONHEAD+L'TYPERMSG+4(8),WORK                                     
         B     ERRX                                                             
TYPERMSG DC    C'* PARTNER CML TYPE ERR '                                       
*                                                                               
CMLPRERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CMLPRMSG),CMLPRMSG                                     
         LA    RE,CONHEAD+1                                                     
         XC    CONHEAD+3(L'CMLPRMSG-3),SPACES                                   
         B     ERRX                                                             
CMLPRMSG DC    C'* NOT ALL PRODUCTS ARE COVERED BY THIS COMMERCIAL'             
*                                                                               
CMLLNERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CMLLNMSG),CMLLNMSG                                     
         XC    CONHEAD+3(L'CMLLNMSG-3),SPACES                                   
         B     ERRX                                                             
CMLLNMSG DC    C'*TOTAL COMMERCIAL LENGTH MUST EQUAL TOTAL UNIT LENGTH'         
*                                                                               
VCMLERR1 MVI   ERROR,INVCOMM       NO SUCH COMMERCIAL FOR CLT                   
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         JZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 ERREX                                                            
*                                                                               
ERRX     TM    UNITSW,X'20'        SVTWA NEEDED                                 
         JZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 ERREX2                                                           
         EJECT                                                                  
SOLOER1  MVC   GERROR,=Y(INVPGSO) SOLO/PIGGYBACK DOESN'T MATCH USE              
         B     ERRX01                                                           
*                                                                               
         USING CMLDTAEL,R6                                                      
INVSLER1 MVC   GERROR,=Y(BDSPTLN)                                               
*                                                                               
ERRX01   XC    ELEM,ELEM                                                        
         MVI   ELEM,4              L'SUBST TEXT + 1                             
         LLC   R1,BSLN                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+1(3),DUB                                                    
         MVI   ELEM+4,4            L'SUBST TEXT + 1                             
         LLC   R1,CMLSLN                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+5(3),DUB                                                    
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         J     CKSVTWA                                                          
         DROP  R6                                                               
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
CMLDTER  TM    REVALID,NOEQCCXT    NO ERROR EXIT                                
         BO    FCMLNEX                                                          
*                                                                               
         MVC   GERROR,=Y(BDCMLDT)                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,9              L'SUBST TEXT + 1                             
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,ELEM+1)                               
         CLI   CMLRCL,X'FF'                                                     
         BE    CMLDTERA                                                         
         MVI   ELEM+9,9            L'SUBST LEN + 1                              
         GOTO1 (RF),(R1),(3,CMLRCL),(5,ELEM+10)                                 
         B     *+14                                                             
CMLDTERA MVI   ELEM+9,4            L'SUBST LEN + 1                              
         MVC   ELEM+10(3),=C'UFN'                                               
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         J     CKSVTWA                                                          
         DROP  R4,R6                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* GET DAY AND TIME FROM PROGRAM RECORD                                          
*                                                                               
GETDTM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R4,KEYSAVE                                                       
         USING NPGKEY,R4                                                        
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,NETMKT                                                   
         MVC   NPGKPROG,PROGRAM                                                 
         MVC   NPGKEND,STDATEP                                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
*                                                                               
         L     R6,AIO1                                                          
         GOTO1 (RF),(R1),=C'GETREC',=C'SPTFIL',KEY+14,(R6),DMWORK               
*                                                                               
         MVI   ELCODE,X'92'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPGEL92,R6                                                       
         MVC   SVPDAY,NPGDAY                                                    
         MVC   SVPTIME,NPGTIME                                                  
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* FIND ORIG/CURRENT REV NUMBER*                                                 
*                                                                               
FREV     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITXSP          SET TO XSPOT                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REVXKEY,R4                                                       
KS       USING REVXKEY,KEYSAVE                                                  
*                                                                               
         MVC   REVXKID,=X'0A1D'                                                 
         MVC   REVXKAM(3),BAGYMD                                                
         MVC   REVXKNET,NETWORK                                                 
         MVC   REVXKPRG,PROGRAM                                                 
         MVC   REVXKPER,PERIOD                                                  
         CLI   MYTN2PR6,C'W'       PERIOD = WEEKLY                              
         BNE   *+18                                                             
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BNZ   *+10                YES, ALREADY SET                             
         MVC   REVXKPER,STDATEP                                                 
*                                                                               
         CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
         BNE   FREV01                                                           
         CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   REVXKPRD,VOPTPROD                                                
*                                                                               
FREV01   OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    *+10                                                             
         MVC   REVXKPGR(2),VOPTPRGR                                             
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         MVI   REVISION,0                                                       
         MVI   OREVNUM,0           REVISION FOR THE OTHER PERIOD                
         MVI   OREVNN,0                                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    FREV08                                                           
*                                                                               
FREV02   CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV30                                                           
*                                                                               
         OC    VOPTPROD,VOPTPROD   RUNNING BY PRODUCT                           
         BZ    FREV02C                                                          
         CLC   REVXKPRD,VOPTPROD                                                
         BNE   FREV04                                                           
         BE    FREV08                                                           
*                                                                               
FREV02C  OC    VOPTPRGR,VOPTPRGR   BY PRODUCT GROUP                             
         BZ    FREV02F                                                          
         CLC   REVXKPGR,VOPTPRGR                                                
         BNE   FREV04                                                           
         BE    FREV08                                                           
*                                                                               
FREV02F  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),KS.REVXKPRD PRD&PG               
         BZ    FREV08                                                           
*                                                                               
FREV04   DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     FREV02                                                           
*                                                                               
FREV08   MVC   SVLGKEY,KEY         SAVE LAST GOOD KEY                           
         OI    UNITSW,X'10'        SET NOT ORIG                                 
*                                                                               
FREV10   CLC   REVXKNUM,REVISION    VALID SEQ OF REVISION NUMBERS               
         BNH   *+10                                                             
         MVC   REVISION,REVXKNUM                                                
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
FREV14   DS   0H                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
                                                                                
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV20                                                           
*                                                                               
         CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
         BNE   FREV15                                                           
         CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BE    FREV15                                                           
         CLC   REVXKPRD,VOPTPROD                                                
         BNE   FREV14                                                           
         MVC   SVLGKEY,KEY         SAVE LAST GOOD KEY                           
         BE    FREV16                                                           
*                                                                               
FREV15   DS   0H                                                                
         OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    FREV16                                                           
         CLC   REVXKPGR(2),VOPTPRGR                                             
         BNE   FREV14                                                           
         MVC   SVLGKEY,KEY         SAVE LAST GOOD KEY                           
*                                                                               
FREV16   DS    0H                                                               
         MVC   SVLGKEY,KEY         SAVE LAST GOOD KEY                           
         LLC   RF,REVISION                                                      
         LA    RF,1(,RF)                                                        
         STC   RF,REVISION                                                      
         B     FREV10                                                           
*                                                                               
FREV20   MVC   KEY,SVLGKEY         RESTORE KEY OF LAST GOOD RECORD              
         GOTO1 HIGH                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
         CLI   MYTN2PR6,C'B'                                                    
         BNE   *+12                                                             
         OI    REVFLAG,REVBRD      TURN ON PERIOD IS BROAD MONTH                
         B     *+16                                                             
         CLI   MYTN2PR6,C'C'                                                    
         BNE   *+8                                                              
         OI    REVFLAG,REVCAL      TURN ON PERIOD IS CALENDAR MONTH             
*                                                                               
         TM    REVFLAG,X'80'       HAVE INSTRUCTIONS BEEN RUN                   
         BZ    FREV24              NO, SHOW THESE COMMENTS IF NONE              
         LLC   RF,REVISION                                                      
         LA    RF,1(,RF)                                                        
         STC   RF,REVISION                                                      
         OI    UNITSW,X'01'        SET NO REC FOR CUR REV #                     
         B     FREVX                                                            
*                                                                               
FREV24   MVI   ELCODE,X'40'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   FREVX                                                            
*                                                                               
         USING REVCMTEL,R6                                                      
         MVC   TRAREV,REVCMT                                                    
         OI    TRAREVH+6,X'80'                                                  
         BRAS  RE,NEXTEL                                                        
         BNE   FREVX                                                            
*                                                                               
         MVC   TRACOM,REVCMT                                                    
         OI    TRACOMH+6,X'80'                                                  
*                                                                               
         B     FREVX                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
* NO REV REC FOUND, CK IF OTHER FORMAT EXISTS *                                 
* IF OPTION WEEKLY, CK FOR MONTHLY REC        *                                 
*    OPTION MONTHLY, CK FOR WEEKLY REC        *                                 
*                                                                               
FREV30   MVC   KEY,KEYSAVE                                                      
         MVI   REVXKNUM,0          SET REVISION TO ZERO                         
         CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BE    *+10                                                             
         MVC   REVXKPRD,VOPTPROD                                                
*                                                                               
         OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    *+10                                                             
         MVC   REVXKPGR(2),VOPTPRGR                                             
*                                                                               
         CLI   MYTN2PR6,C'W'                                                    
         BE    FREV40                                                           
         MVC   WORK(6),STDATE                                                   
         GOTO1 GETDAY,DMCB,(0,STDATE),WORK+6                                    
         CLC   WORK+6(3),SPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),1             MUST BE MONDAY                               
         BE    FREV34                                                           
         LLC   R6,DMCB                                                          
         BCTR  R6,0                                                             
         LNR   R6,R6                                                            
         GOTO1 ADDAY,(R1),STDATE,WORK,(R6)                                      
*                                                                               
         FIXDT02                                                                
FREV34   GOTO1 DATCON,(R1),(0,WORK),(2,WORK+6)                                  
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    *+18                 NO                                          
         BRAS  RE,CONVPER                                                       
         MVC   REVXKPER,CONVDTE                                                 
         B     *+10                                                             
         MVC   REVXKPER,WORK+6                                                  
*                                                                               
         XC    SVLGKEY,SVLGKEY     INIT LAST GOOD KEY                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
FREV34B  CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV38               LOOK UP NEXT WEEK                           
*                                                                               
         OC    VOPTPROD,VOPTPROD   RUNNING BY PRODUCT                           
         BZ    FREV34C                                                          
         CLC   REVXKPRD,VOPTPROD                                                
         BNE   FREV34G                                                          
         BE    FREV35                                                           
*                                                                               
FREV34C  OC    VOPTPRGR,VOPTPRGR   BY PRODUCT GROUP                             
         BZ    FREV34F                                                          
         CLC   REVXKPGR,VOPTPRGR                                                
         BNE   FREV34G                                                          
         BE    FREV35                                                           
*                                                                               
FREV34F  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),KS.REVXKPRD PRD&PG               
         BZ    FREV35                                                           
*                                                                               
FREV34G  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     FREV34B                                                          
*---------------------------------------------                                  
* READ ALL REVISION RECORDS FOR THIS PERIOD                                     
* AND GET HIGHEST REVISION NUMBER USED                                          
*---------------------------------------------                                  
FREV35   CLC   OREVNUM,REVXKNUM    COMPARE OTHER REV # TO THIS                  
         BNL   *+10                                                             
         MVC   OREVNUM,REVXKNUM    SAVE THIS REV#                               
*                                                                               
         MVC   SVLGKEY,KEY         SAVE LAST GOOD KEY                           
*                                                                               
FREV35F  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV37               LOOK UP NEXT WEEK                           
*                                                                               
FREV36   CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BE    FREV36B                                                          
         CLC   REVXKPRD,VOPTPROD                                                
         BE    FREV35                                                           
         BNE   FREV35F             GET NEXT RECORD                              
FREV36B  OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    FREV36C                                                          
         CLC   REVXKPGR(2),VOPTPRGR                                             
         BNE   FREV35F             GET NEXT RECORD                              
         BE    FREV35                                                           
FREV36C  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),KS.REVXKPRD PRD&PGR              
         BZ    FREV35                                                           
         B     FREV35F             GET NEXT RECORD                              
*                                                                               
* RE-READ LAST GOOD RECORD                                                      
*                                                                               
FREV37   OC    SVLGKEY,SVLGKEY     ANY GOOD RECORD                              
         BZ    FREV38                                                           
         MVC   KEY,SVLGKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
*                                                                               
         CLC   OREVNUM,REVXKNUM    IS OTHER REV# HIGHER                         
         BH    FREV38               YES, DONE                                   
         BE    *+6                                                              
         DC    H'0'                OTHER REV# IS LOWER ?  BUG???                
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
*                                                                               
         CLI   REVDTALN,12         OLD REV REC                                  
         BE    *+20                                                             
         CLC   OREVNN,REVNNUM                                                   
         BNL   *+10                                                             
         MVC   OREVNN,REVNNUM      SAVE NET REV NUM FOR OTHER PERIOD            
*                                                                               
         TM    REVFLAG,X'80'       HAVE INSTRUCTIONS BEEN RUN                   
         BZ    FREV38              NO,LOOK UP NEXT WEEK                         
*                                                                               
         LLC   RF,OREVNUM          YES, UP REVISION NUMBER                      
         LA    RF,1(,RF)                                                        
         STC   RF,OREVNUM                                                       
*                                                                               
* LOOK UP NEXT WEEK IF NOT DONE WITH THIS MONTH                                 
*                                                                               
FREV38   MVC   KEY,KEYSAVE                                                      
         MVI   REVXKNUM,0          SET REVISION TO ZERO                         
         CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BE    *+10                                                             
         MVC   REVXKPRD,VOPTPROD                                                
*                                                                               
         OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    *+10                                                             
         MVC   REVXKPGR(2),VOPTPRGR                                             
         GOTO1 ADDAY,(R1),WORK,WORK,F'7' GET MONDAY OF NEXT WEEK                
*                                                                               
         CLC   STDATE+2(2),WORK+2  SAME MONTH?                                  
         BE    FREV34               YES                                         
*                                                                               
         CLI   MYTN2PR6,C'B'                                                    
         BNE   FREVX                                                            
*                                                                               
         CLC   ENDATE+2(2),=C'12'   FIX END OF YEAR ISSUE                       
         BNE   *+14                                                             
         CLC   WORK+2(2),=C'01'                                                 
         BE    FREVX                                                            
*                                                                               
         CLC   ENDATE+2(2),WORK+2  SAME MONTH?                                  
         BNL   FREV34               YES                                         
*                                                                               
         B     FREVX                                                            
         EJECT                                                                  
* CK FOR MONTHLY REV REC (SINCE USER=WEEKLY, ASSUME BROADCAST MONTH) *          
********* ASSUMING BROADCAST INTRODUCES A BUG!!!!!!                             
***** SO ASSUME NOTHING (LOOK FOR CALENDAR AND THEN BROADCAST MONTH) *          
*********************************************************************           
*                                                                               
FREV40   DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,STDATE),(3,WORK)                                  
         MVC   REVXKPER,WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    FREV42                                                           
*                                                                               
FREV40B  CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV50                                                           
*                                                                               
         OC    VOPTPROD,VOPTPROD   RUNNING BY PRODUCT                           
         BZ    FREV40C                                                          
         CLC   REVXKPRD,VOPTPROD                                                
         BNE   FREV40G                                                          
         BE    FREV42                                                           
*                                                                               
FREV40C  OC    VOPTPRGR,VOPTPRGR   BY PRODUCT GROUP                             
         BZ    FREV40F                                                          
         CLC   REVXKPGR,VOPTPRGR                                                
         BNE   FREV40G                                                          
         BE    FREV42                                                           
*                                                                               
FREV40F  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),KS.REVXKPRD PRD&PG               
         BZ    FREV42                                                           
*                                                                               
FREV40G  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     FREV40B                                                          
*---------------------------------------------                                  
* READ ALL REVISION RECORDS FOR THIS PERIOD                                     
* AND GET HIGHEST REVISION NUMBER USED                                          
*---------------------------------------------                                  
FREV42   CLC   OREVNUM,REVXKNUM    COMPARE OTHER REV # TO THIS                  
         BNL   *+10                                                             
         MVC   OREVNUM,REVXKNUM    SAVE THIS REV#                               
*                                                                               
         MVC   SVLGKEY,KEY                                                      
*                                                                               
FREV42F  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV45               LOOK UP NEXT WEEK                           
*                                                                               
FREV43   CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BE    FREV43B                                                          
         CLC   REVXKPRD,VOPTPROD                                                
         BE    FREV42                                                           
         BNE   FREV42F             GET NEXT RECORD                              
FREV43B  OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    FREV43C                                                          
         CLC   REVXKPGR(2),VOPTPRGR                                             
         BNE   FREV42F             GET NEXT RECORD                              
         BE    FREV42                                                           
FREV43C  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),KS.REVXKPRD PRD&PGR              
         BZ    FREV42                                                           
         B     FREV42F             GET NEXT RECORD                              
*                                                                               
* RE-READ LAST GOOD RECORD                                                      
*                                                                               
FREV45   OC    SVLGKEY,SVLGKEY                                                  
         BZ    FREV50                                                           
         MVC   KEY,SVLGKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
*                                                                               
         CLI   REVDTALN,12         OLD REV REC                                  
         BE    *+20                                                             
         CLC   OREVNN,REVNNUM                                                   
         BNL   *+10                                                             
         MVC   OREVNN,REVNNUM      SAVE NET REV NUM FOR OTHER PERIOD            
*                                                                               
         TM    REVFLAG,X'80'       HAVE INSTRUCTIONS BEEN RUN                   
         BZ    FREV50                                                           
*                                                                               
         LLC   RF,OREVNUM          YES, UP REVISION NUMBER                      
         LA    RF,1(,RF)                                                        
         STC   RF,OREVNUM                                                       
*                                                                               
FREV50   DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   REVXKNUM,0          SET REVISION TO ZERO                         
         CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BE    *+10                                                             
         MVC   REVXKPRD,VOPTPROD                                                
*                                                                               
         OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    *+10                                                             
         MVC   REVXKPGR(2),VOPTPRGR                                             
         MVC   WORK(6),STDATE                                                   
         GOTO1 VGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         CLC   WORK+8(2),WORK+14   START AND END MONTH SAME                     
         BE    FREV55                                                           
*                                                                               
* ONLY INCREMENT MONTH IF DAY IS OVER 21 *                                      
*                                                                               
         CLC   =C'20',WORK+2                                                    
         BL    FREV55                                                           
         GOTO1 ADDAY,(R1),WORK+6,WORK+6,F'15'                                   
FREV55   GOTO1 DATCON,(R1),(0,WORK+6),(3,WORK)                                  
*                                                                               
         CLC   REVXKPER,WORK                                                    
         BE    FREVX               DONE THIS PERIOD BEFORE                      
*                                                                               
         MVC   REVXKPER,WORK                                                    
         B     FREV60                                                           
*                                                                               
* CODE IS BYPASSED TO SEE WHAT IS THERE, PROD & PGRP CKD BEFORE                 
*NOP                                                                            
*        MVC   REVXKPER,PERIOD                                                  
*                                                                               
*        CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
*        BNE   FREV58                                                           
*        CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
*        BE    *+10                                                             
*        MVC   REVXKPRD,VOPTPROD                                                
*                                                                               
*REV58   DS   0H                                                                
*        OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
*        BZ    *+10                                                             
******   MVC   REVXKPRG,VOPTPRGR                                                
*                                                                               
FREV60   MVI   RDUPDATE,C'N'                                                    
         BRAS  RE,INITXSP          SET TO XSPOT                                 
         XC    SVLGKEY,SVLGKEY     INIT LAST GOOD KEY                           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    FREV62                                                           
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREVX                                                            
*                                                                               
FREV61   CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BE    FREV61B                                                          
         CLC   REVXKPRD,VOPTPROD                                                
         BE    FREV62                                                           
         BNE   FREV61F                                                          
*                                                                               
FREV61B  OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    FREV61C                                                          
         CLC   REVXKPGR(2),VOPTPRGR                                             
         BNE   FREV61F                                                          
         BE    FREV62                                                           
FREV61C  OC    REVXKPRD(L'REVXKPRD+L'REVXKPGR),KS.REVXKPRD PRD&PGR              
         BZ    FREV62                                                           
*                                                                               
FREV61F  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BNE   FREV65                                                           
         B     FREV61                                                           
*                                                                               
FREV62   CLC   OREVNUM,REVXKNUM    COMPARE OTHER REV # TO THIS                  
         BNL   *+10                                                             
         MVC   OREVNUM,REVXKNUM    SAVE THIS REV#                               
*                                                                               
         MVC   SVLGKEY,KEY         SAVE LAST GOOD KEY                           
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(REVXKNUM-REVXKEY),KEYSAVE                                    
         BE    FREV61                                                           
*                                                                               
* RE-READ LAST GOOD RECORD                                                      
*                                                                               
FREV65   OC    SVLGKEY,SVLGKEY                                                  
         BZ    FREVX                                                            
         MVC   KEY,SVLGKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'REVXKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                 MUST BE THERE                               
*                                                                               
         CLC   OREVNUM,REVXKNUM    COMPARE OTHER REV # TO THIS                  
         BH    FREVX                                                            
         BE    *+6                                                              
         DC    H'0'                OTHER REV IS LOWER ? BUG??                   
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
*                                                                               
         TM    REVFLAG,X'80'       HAVE INSTRUCTIONS BEEN RUN                   
         BZ    FREVX                                                            
*                                                                               
         LLC   RF,OREVNUM          YES, UP REVISION NUMBER                      
         LA    RF,1(,RF)                                                        
         STC   RF,OREVNUM                                                       
*                                                                               
FREVX    OI    4(R2),X'20'         SET VALIDATED                                
         BRAS  RE,INITNET          SET TO UNIT                                  
         CLC   OREVNUM,REVISION    COMPARE OTHER  REVISION #                    
         BNH   FREVXIT             TO THIS REVISION                             
         MVC   REVISION,OREVNUM    USE THE HIGHER NUMBER OF THE TWO             
*                                                                               
FREVXIT  XIT1                                                                   
         LTORG                                                                  
         DROP  KS                                                               
         DROP  R4                                                               
         EJECT                                                                  
* CHECKS OTHER FIELD FOR VIGNETTE AND SETS VIGNETTE LENGTH                      
*                                                                               
CHKVIGNT NTR1  BASE=*,LABEL=*                                                   
         USING UNTABLED,R5                                                      
         USING CMLDTAEL,R6                                                      
                                                                                
         CLC   UNTBBSN,=CL8'VIGNETTE' YES/IS IT VIGNETTE                        
         BE    *+18                                                             
         CLC   UNTBBCN,=CL8'VIGNETTE' YES/IS IT VIGNETTE                        
         BNE   CVT2                                                             
         OI    VIGNFLG,X'04'       DO NOT CHK PRODS                             
                                                                                
         OI    VIGNFLG,X'01'                                                    
                                                                                
         TM    UNTFLAG3,UNTFL3TB   IS IT TRI-BACK                               
         BZ    CVT1                                                             
         LLC   R1,CML2                                                          
         B     CVT1C                                                            
                                                                                
CVT1     LLC   R1,CMLSLN                                                        
         STC   R1,CML2             SET COMMMERCIAL LEN2                         
CVT1C    LLC   RE,CML1                                                          
         AR    R1,RE                                                            
         LLC   RE,CML3                                                          
         AR    R1,RE                                                            
                                                                                
         LLC   RE,UNTSLN                                                        
         LLC   RF,UNTSLN2                                                       
         AR    RE,RF                                                            
         LLC   RF,UNTSLN3                                                       
         AR    RE,RF                                                            
                                                                                
         CR    R1,RE                                                            
         BH    CVTERR                                                           
         B     CVTOK                                                            
                                                                                
CVT2     DS    0H                  FIRST PRD COMMERCIAL                         
         MVI   VIGNFLG,0                                                        
         CLI   SKIPVGN,C'Y'                                                     
         BNE   CVT3                                                             
         ST    R6,SVREG                                                         
         L     R6,NBAIO                                                         
         CLI   0(R6),X'04'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   1(1,R6),BAGYMD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'21'                                                     
         MVI   DATADISP+1,27       SET TO NET                                   
         BRAS  RE,GETEL                                                         
         BNE   CVTERR                                                           
         DROP  R6                                                               
         USING NUCMLEL,R6                                                       
         CLC   NUCMLBSN,=C'VIGNETTE'                                            
         BE    CVT8                                                             
         CLC   NUCMLBCN,=C'VIGNETTE'                                            
         BE    CVT8                                                             
         DROP  R6                                                               
*SM      B     CVTOK                                                            
         B     CVTERR                                                           
CVT3     DS    0H                                                               
         USING CMLDTAEL,R6                                                      
                                                                                
         LR    R4,R2                                                            
         LLC   R0,0(R4)            DOES OTHER HAVE VIGNETTE                     
         AR    R4,R0                                                            
         GOTO1 SCANNER,DMCB,(30,(R4)),(4,BLOCK+64)                              
         LLC   R1,DMCB+4                                                        
         LTR   R1,R1                                                            
         BZ    CVTERR              NO/ERROR                                     
         LA    R4,BLOCK+64                                                      
CVT5     LLC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,CVT7                                                          
         BE    CVT8                                                             
         B     *+10                                                             
CVT7     CLC   12(0,R4),=CL8'VIGNETTE'                                          
         LA    R4,54(R4)                                                        
         BCT   R0,CVT5                                                          
         B     CVTERR                                                           
CVT8     LLC   R0,BSLN             YES/CHECK LENGTHS                            
         LLC   R1,BSLN2                                                         
         AR    R1,R0                                                            
         LLC   R4,CMLSLN                                                        
         CR    R1,R4             COMPARE PRD LENGTHS VS COMMERCIAL LEN          
         BL    CVTERR              IF COM LEN > PRD LEN / ERROR                 
         STC   R4,CML1             SET LENGHT OF 1ST COMMERCIAL                 
         OI    VIGNFLG,X'01'       SET VIGNETTE FLAG                            
         OC    QPRD2,QPRD2                                                      
         BZ    *+8                                                              
         OI    VIGNFLG,X'04'       SKIP PMATPRER(ERROR) IN VCML                 
CVTOK    SR    R1,R1                                                            
         B     *+8                                                              
CVTERR   LA    R1,1                                                             
         LTR   R1,R1                                                            
         XIT1                                                                   
         DROP  R5,R6,RB,RC                                                      
         LTORG                                                                  
         EJECT                                                                  
VNET     NMOD1 0,**VNET**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         GOTO1 ANY                                                              
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BNE   VNX                                                              
         MVC   NETWORK,WORK                                                     
         L     R4,AIO                                                           
*                                                                               
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,NETMKT                                                        
*                                                                               
         MVC   SVMEDIA,STRTYPE                                                  
*                                                                               
VNX      CLI   8(R1),0                                                          
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE OPTIONS IN VALIDATE KEY RTN *                                        
*                                                                               
VOPT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,TRAOPTH          GET ANY DATES                                
         XC    TRAOPT2,TRAOPT2                                                  
         XC    MYTN2TMP,MYTN2TMP   SAVE ALL OPTIONS ENTERED FOR TN2             
         OI    TRAOPT2H+6,X'80'                                                 
*                                                                               
         XC    VOPTIONS,VOPTIONS                                                
         MVI   SVLINE,0                                                         
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPT84              NO                                           
         LA    R7,TRAOPT2                                                       
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPT06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VOPT02                                                           
         LA    R1,4                                                             
         B     VOPT04                                                           
VOPT02   LLC   R1,5(R2)                                                         
VOPT04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VOPT08                                                           
VOPT06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'VOPTHELP),VOPTHELP                                     
         CLI   SVTN2PRO+00,0       ANY PROD OR PROD GROUP EXPECTED              
         BE    VOPTERX2             NO                                          
         CLI   SVTN2PRO+00,C'0'    ANY PROD OR PROD GROUP EXPECTED              
         BE    VOPTERX2             NO                                          
         MVC   CONHEAD+L'VOPTHELP-3(7),=C'/PRD= *'                              
         CLI   SVTN2PRO+00,C'*'    EXPECTING PRODUCT                            
         BE    VOPTERX2                                                         
         MVC   CONHEAD+L'VOPTHELP-3+2(3),=C'GRP'                                
         B     VOPTERX2                                                         
VOPT08   GOTO1 SCANNER,DMCB,(20,TRAOPTH),(5,BLOCK+64)                           
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRA             NO                                          
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
VOPT10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
* GET ADDRESS OF OPTION VALIDATION RTN                                          
         LA    RF,OPTTABLE                                                      
         EX    R1,VOPTCLC                                                       
         BE    VOPTGO                                                           
         LA    RF,L'OPTTABLE(RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-16                                                             
         B     VOPT90                                                           
*                                                                               
VOPTCLC  CLC   12(0,R4),0(RF)                                                   
VOPTGO   L     RE,10(RF)                                                        
         A     RE,SPTR1CRR                                                      
         BR    RE                                                               
         EJECT                                                                  
* DATE                                                                          
*                                                                               
OPTDATE  TM    TRAPERH+4,X'20'     PERIOD VALIDATED                             
         BZ    VOPT80              NO, CK NEXT PASS                             
*                                                                               
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),ELEM                                        
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERRA              NO                                          
         CLC   STDATE,ELEM         MUST BE WITHIN PERIOD                        
         BH    VDTPERER                                                         
         CLC   ENDATE,ELEM                                                      
         BL    VDTPERER                                                         
         CLM   R6,1,1(R4)          WAS THERE ONLY 1 DATE                        
         BE    VDTPERER             YES                                         
         LA    R5,1(R6,R5)                                                      
         GOTO1 DATVAL,(R1),(0,(R5)),ELEM+6                                      
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERRA              NO                                          
         CLC   STDATE,ELEM+6       MUST BE WITHIN PERIOD                        
         BH    VDTPERER                                                         
         CLC   ENDATE,ELEM+6                                                    
         BL    VDTPERER                                                         
         CLC   ELEM(6),ELEM+6                                                   
         BH    VDTPERER                                                         
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,ELEM),(2,STDATEP)                                 
         FIXDT02                                                                
         GOTO1 (RF),(R1),(0,ELEM+6),(2,ENDATEP)                                 
         MVC   STDATE(12),ELEM                                                  
         OI    UNITSW1,X'02'       SET ON PARTIAL PERIOD                        
         B     VOPT80                                                           
*                                                                               
* WEEKLY                                                                        
*                                                                               
OPTWEEK  MVI   MYTN2PR6,C'W'                                                    
         MVI   MYTN2PRW,C'W'                                                    
                                                                                
         CLI   MYTN2PRC,0                                                       
         BNE   CALERR                                                           
         CLI   MYTN2PRB,0                                                       
         BNE   CALERR                                                           
                                                                                
         LA    R0,TRAOPT2+L'TRAOPT2-6                                           
         CR    R0,R7                                                            
         BH    *+12                                                             
         MVI   TRAOPT2+L'TRAOPT2-1,C'+'                                         
         B     VOPT82                                                           
         LA    R0,TRAOPT2                                                       
         CR    R0,R7                                                            
         BE    *+12                                                             
         MVI   0(R7),C','                                                       
         LA    R7,1(,R7)                                                        
         MVC   0(6,R7),=C'WEEKLY'                                               
         LA    R7,6(,R7)                                                        
         B     VOPT80                                                           
*                                                                               
* (BROADCAST) MONTHLY                                                           
*                                                                               
OPTBRD   MVI   MYTN2PR6,C'B'                                                    
         MVI   MYTN2PRB,C'B'                                                    
                                                                                
         CLI   MYTN2PRC,0                                                       
         BNE   CALERR                                                           
         CLI   MYTN2PRW,0                                                       
         BNE   CALERR                                                           
                                                                                
         LA    R0,TRAOPT2+L'TRAOPT2-5                                           
         CR    R0,R7                                                            
         BH    *+12                                                             
         MVI   TRAOPT2+L'TRAOPT2-1,C'+'                                         
         B     VOPT82                                                           
         LA    R0,TRAOPT2                                                       
         CR    R0,R7                                                            
         BE    *+12                                                             
         MVI   0(R7),C','                                                       
         LA    R7,1(,R7)                                                        
         MVC   0(5,R7),=C'BROAD'                                                
         LA    R7,5(,R7)                                                        
         B     VOPT80                                                           
*                                                                               
* (CALENDAR) MONTHLY                                                            
*                                                                               
OPTCAL   MVI   MYTN2PR6,C'C'                                                    
         MVI   MYTN2PRC,C'C'                                                    
                                                                                
         CLI   MYTN2PRB,0                                                       
         BNE   CALERR                                                           
         CLI   MYTN2PRW,0                                                       
         BNE   CALERR                                                           
                                                                                
         LA    R0,TRAOPT2+L'TRAOPT2-3                                           
         CR    R0,R7                                                            
         BH    *+12                                                             
         MVI   TRAOPT2+L'TRAOPT2-1,C'+'                                         
         B     VOPT82                                                           
         LA    R0,TRAOPT2                                                       
         CR    R0,R7                                                            
         BE    *+12                                                             
         MVI   0(R7),C','                                                       
         LA    R7,1(,R7)                                                        
         MVC   0(3,R7),=C'CAL'                                                  
         LA    R7,3(,R7)                                                        
         B     VOPT80                                                           
*                                                                               
* USE PATTERNS TO SEED COMMERCIALS                                              
*                                                                               
OPTPAT   OI    UNITSW1,X'10'                                                    
         B     VOPT80                                                           
*                                                                               
* SHOW BUY LINE NUMBERS OR ONLY ONE BUY LINE                                    
*                                                                               
OPTLINE  OI    UNITSW2,LINENUM                                                  
         CLI   1(R4),0             SPECIFIC LINE # ENTERED?                     
         BE    VOPT80               NO                                          
         MVC   SVLINE,11(R4)       SAVE LINE # IN BINARY                        
         B     VOPT80                                                           
*                                                                               
* DO NOT SHOW SECTIONAL FEED DESCRIPTION                                        
*                                                                               
OPTFEED  DS    0H                                                               
         CLI   1(RA),C'*'          THIS A DDS TERMINAL?                         
         BE    *+14                                                             
         CLC   =C'H9',AGENCY       IF AGENCY IS NOT STARCOM                     
         BNE   VOPT90               ERROR                                       
         NI    UNITSW2,X'FF'-FEEDSW                                             
         B     VOPT80                                                           
*                                                                               
* DO NOT SHOW SECTIONAL FEED DESCRIPTION                                        
*                                                                               
OPTNFED  DS    0H                                                               
         CLI   1(RA),C'*'          THIS A DDS TERMINAL?                         
         BE    *+14                                                             
         CLC   =C'H9',AGENCY       IF AGENCY IS NOT STARCOM                     
         BNE   VOPT90               ERROR                                       
         OI    UNITSW2,FEEDSW                                                   
         B     VOPT80                                                           
*MNV                                                                            
*                                                                               
* INCLUDE DIGITAL                                                               
*                                                                               
OPTDIGI  DS    0H                                                               
         OI    FLTODOSW,OVRDIGI                                                 
         B     VOPT80                                                           
*MNV                                                                            
*                                                                               
* BY PRODUCT                                                                    
*                                                                               
OPTPROD  CLI   SVTN2PRO+00,C'*'     TRAFFIC BY PRODUCT                          
         BNE   VOPT90               NO, PRD= IS INVALID                         
*                                                                               
         CLI   SVTN2PRO+00,C'A'    TRAFFIC BY PRODUCT GROUP                     
         BL    VOPT62               NO                                          
         CLI   SVTN2PRO+00,C'Z'    TRAFFIC BY PRODUCT GROUP                     
         BH    *+14                                                             
         MVC   GERROR,=Y(PRODNA)                                                
         B     VOPTTRAP                                                         
*                                                                               
VOPT62   DS    0H                                                               
         CLC   =C'POL',22(R4)      IS PRD=POL                                   
         BE    INVPRER              YES, ERROR                                  
         CLC   =C'ALL',22(R4)      IS PRD=ALL                                   
         BE    INVPRER              YES, ERROR                                  
         CLI   1(R4),3             MAX 3 CHAR                                   
         BH    BDPRDER                                                          
         OI    24(R4),C' '                                                      
         OC    VOPTPROD(4),VOPTPROD IF NON-ZERO, ALREADY HAD PRODUCT            
         BNZ   MULPRDER                                                         
*                                                                               
*                                                                               
         CLI   1(R4),2                                                          
         BH    *+8                                                              
         MVI   24(R4),C' '                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),=X'0A01000184030001'                                     
         MVC   ELEM+8(3),22(R4)                                                 
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         LR    R0,R2               SAVE R2                                      
         LA    R2,ELEM                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,0                                                         
         LR    R2,R0                                                            
*                                                                               
         CLI   ERROR,0             ANY ERROR?                                   
         BE    VOPT63               NO OKAY                                     
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
VOPT63   MVC   VOPTPROD,WORK       PROD                                         
         LA    R0,TRAOPT2+L'TRAOPT2-6                                           
         CR    R0,R7                                                            
         BH    *+12                                                             
         MVI   TRAOPT2+L'TRAOPT2-1,C'+'                                         
         B     VOPT82                                                           
         LA    R0,TRAOPT2                                                       
         CR    R0,R7                                                            
         BE    *+12                                                             
         MVI   0(R7),C','                                                       
         LA    R7,1(,R7)                                                        
         MVC   0(4,R7),=C'PRD='                                                 
         MVC   4(3,R7),VOPTPROD                                                 
         LA    R7,6(,R7)                                                        
         CLI   0(R7),C' '                                                       
         BNH   *+8                                                              
         LA    R7,1(,R7)                                                        
         B     VOPT80                                                           
         EJECT                                                                  
* BY PRODUCT GROUP                                                              
*                                                                               
OPTPGRP  CLI   SVTN2PRO+00,C'A'    TRAFFIC BY PRODUCT GROUP                     
         BL    VOPT90               NO, ERROR                                   
         CLI   SVTN2PRO+00,C'Z'    TRAFFIC BY PRODUCT GROUP                     
         BH    VOPT90               NO, ERROR                                   
*                                                                               
* VALIDATE PRODUCT GROUP *                                                      
*                                                                               
         BAS   RE,VPGR                                                          
         MVC   VOPTPRGR,SVPGRP     MOVE PRDGRP TO LOCAL STORAGE                 
*                                                                               
         MVI   TMPSTSW,C'Y'                                                     
         LA    R1,=C'DMWRT '                                                    
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)                                                 
         MVI   DMCB+8,4                                                         
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(TABSAVL)                                                 
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,AOPTPGRL,,(RF)                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R0,TRAOPT2+L'TRAOPT2-9                                           
         CR    R0,R7                                                            
         BH    *+12                                                             
         MVI   TRAOPT2+L'TRAOPT2-1,C'+'                                         
         B     VOPT82                                                           
         LA    R0,TRAOPT2                                                       
         CR    R0,R7                                                            
         BE    *+12                                                             
         MVI   0(R7),C','                                                       
         LA    R7,1(,R7)                                                        
         MVC   0(4,R7),=C'PGR='                                                 
*                                                                               
         MVC   4(1,R7),SVTN2PRO+00                                              
         MVC   DUB(2),VOPTPRGR                                                  
         UNPK  DUB+3(5),DUB(3)                                                  
         LLC   RE,PGRLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R7),DUB+3                                                    
         LA    R7,7(RE,R7)                                                      
*                                                                               
VOPT80   DS    0H                                                               
VOPT82   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
         TM    TRAPERH+4,X'20'     PERIOD VALIDATED                             
         BZ    VOPTXX              NO, CLR NEXT PASS                            
         XC    TRAOPT,TRAOPT                                                    
         OI    TRAOPTH+6,X'80'                                                  
VOPT84   CLI   SVTN2PRO+00,0       ANY PROD OR PROD GROUP EXPECTED              
         BE    VOPT88               NO                                          
         CLI   SVTN2PRO+00,C'0'    ANY PROD OR PROD GROUP EXPECTED              
         BE    VOPT88               NO                                          
         CLI   SVTN2PRO+00,C'*'    EXPECTING PRODUCT                            
         BNE   VOPT86                                                           
         CLI   VOPTPROD,0          ANY PROD ENTERED                             
         BNE   VOPT88                                                           
         MVC   GERROR,=Y(PRDREQD)                                               
         B     VOPTTRAP                                                         
VOPT86   DS   0H                                                                
         OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BNZ   VOPT88                                                           
         MVC   GERROR,=Y(PGRPREQD)                                              
         B     VOPTTRAP                                                         
VOPT88   OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
VOPTXX   OI    TRAOPTH+4,X'20'                                                  
         XIT1                                                                   
*                                                                               
VOPT90   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'VOPTMSG+L'VOPTHELP),VOPTMSG                            
         CLI   SVTN2PRO+00,0       ANY PROD OR PROD GROUP EXPECTED              
         BE    VOPTERX2             NO                                          
         CLI   SVTN2PRO+00,C'0'    ANY PROD OR PROD GROUP EXPECTED              
         BE    VOPTERX2             NO                                          
         MVC   CONHEAD+L'VOPTMSG+L'VOPTHELP-2(7),=C'/PRD= *'                    
*                                                                               
         CLI   SVTN2PRO+00,C'*'    EXPECTING PRODUCT                            
         BE    VOPTERX2                                                         
         MVC   CONHEAD+L'VOPTMSG+L'VOPTHELP-2+2(3),=C'GRP'                      
         B     VOPTERX2                                                         
         EJECT                                                                  
* VALIDATE PRODUCT GROUP *                                                      
*                                                                               
VPGR     NTR1                                                                   
         GOTO1 VALIPGR             CALL ROUTINE IN BASE                         
*                                                                               
         GOTO1 HIGH                ON RETURN, KEY HAS FIRST GROUP               
*                                                                               
VPGR20   LA    R0,(L'OPTPGRL/3)                                                 
         L     R5,AOPTPGRL                                                      
*                                                                               
VPGR40   MVC   0(3,R5),KEY+8                                                    
         LA    R5,3(R5)                                                         
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BNE   VOPTXX                                                           
         BCT   R0,VPGR40                                                        
         B     VMAXPGER            MORE PRODS THAT WIL FIT IN TABLE             
*                                                                               
CALERR   MVC   GERROR,=Y(CHCALBRD)                                              
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   ELEM,10             LEN OF TEXT PLUS 1                           
         MVI   ELEM+10,10          L'SUBST TEXT+1                               
                                                                                
         CLI   MYTN2PRW,C'W'                                                    
         BNE   CALERR10                                                         
         MVC   ELEM+1(9),=C'WEEKLY   '                                          
         CLI   MYTN2PRB,C'B'                                                    
         BNE   *+10                                                             
         MVC   ELEM+11(9),=C'BROADCAST'                                         
         CLI   MYTN2PRC,C'C'                                                    
         BNE   *+10                                                             
         MVC   ELEM+11(9),=C'CALENDAR '                                         
         B     VOPTTRAP                                                         
                                                                                
CALERR10 CLI   MYTN2PRB,C'B'                                                    
         BNE   CALERR20                                                         
         MVC   ELEM+1(9),=C'BROADCAST'                                          
         CLI   MYTN2PRC,C'C'                                                    
         BNE   *+10                                                             
         MVC   ELEM+11(9),=C'CALENDAR '                                         
         CLI   MYTN2PRW,C'W'                                                    
         BNE   *+10                                                             
         MVC   ELEM+11(9),=C'WEEKLY   '                                         
         B     VOPTTRAP                                                         
                                                                                
CALERR20 CLI   MYTN2PRC,C'C'                                                    
         BNE   CALERR20                                                         
         MVC   ELEM+1(9),=C'CALENDAR '                                          
         CLI   MYTN2PRB,C'B'                                                    
         BNE   *+10                                                             
         MVC   ELEM+11(9),=C'BROADCAST'                                         
         CLI   MYTN2PRW,C'W'                                                    
         BNE   *+10                                                             
         MVC   ELEM+11(9),=C'WEEKLY   '                                         
         B     VOPTTRAP                                                         
VMAXPGER MVC   GERROR,=Y(MAXPGRP)                                               
         B     VOPTTRAP                                                         
VDTPERER MVC   GERROR,=Y(DTNINPER)                                              
         B     VOPTTRAP                                                         
*                                                                               
PGRPERR  MVC   GERROR,=Y(BDPGRPLN)                                              
         B     VOPTTRAP                                                         
*                                                                               
BDPRDER  MVC   GERROR,=Y(BDPRDLN)                                               
*                                                                               
VOPTTRAP GOTO1 VTRAERR                                                          
*                                                                               
INVPRERR TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    INVPRER                                                          
         MVI   ERROR,SECLOCK       SECURITY LOCKOUT                             
         B     VOPTERX                                                          
INVPRER  MVI   ERROR,INVPROD                                                    
         B     VOPTERX                                                          
MISSERRA MVI   ERROR,MISSING                                                    
         B     VOPTERX                                                          
DATERRA  MVI   ERROR,INVDATE                                                    
VOPTERX  GOTO1 ERREX                                                            
MULPRDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MULPRDM1),MULPRDM1                                     
*                                                                               
VOPTERX2 GOTO1 ERREX2                                                           
*                                                                               
MULPRDM1 DC    C'* ERROR * ONLY 1 PRODUCT ALLOWED *'                            
*                                                                               
* OPTION VALIDATION RTNS:                                                       
*   BYTES 00-09 = OPTION KEYWORD                                                
*   BYTES 10-13 = A(VALI RTN)                                                   
*                                                                               
OPTTABLE DS    0CL14                                                            
         DC    CL10'DATE      ',AL4(OPTDATE)                                    
         DC    CL10'WEEKLY    ',AL4(OPTWEEK)                                    
         DC    CL10'BROADCAST ',AL4(OPTBRD)                                     
         DC    CL10'CALENDAR  ',AL4(OPTCAL)                                     
         DC    CL10'PATTERN   ',AL4(OPTPAT)                                     
         DC    CL10'PRD       ',AL4(OPTPROD)                                    
         DC    CL10'PGRP      ',AL4(OPTPGRP)                                    
         DC    CL10'LINE      ',AL4(OPTLINE)                                    
         DC    CL10'FEED      ',AL4(OPTFEED)                                    
         DC    CL10'NOFEED    ',AL4(OPTNFED)                                    
*MNV                                                                            
         DC    CL10'DIGITAL   ',AL4(OPTDIGI)                                    
*MNV                                                                            
         DC    X'FF'                                                            
*                                                                               
VOPTMSG  DC    C'* ERROR * '                                                    
VOPTHELP DC    CL34'OPTIONS DATE/WEEK/BROAD/CALENDAR *'                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE FEED - MAKE SURE NONE EXISTS FOR THIS DATE AND SQH *                 
*                                                                               
         DS    0H                                                               
VFED     NTR1  BASE=*,LABEL=*                                                   
         CLI   1(R4),4                                                          
         BH    FEEDLNER                                                         
         USING UNTABLED,R5                                                      
         LA    R1,UNITABLE                                                      
*                                                                               
VFED10   CLC   UNTCOM,0(R1)        AIRDATE/2/SQH/EST/SUB/DP/DSK ADDR            
         BNE   VFED14                                                           
         CLC   UNTFEED-UNTENT(,R1),22(R4) IS FEED SAME                          
         BNE   VFED14                                                           
         TM    UNTFLAG1-UNTENT(R1),X'40'      DELETED                           
         BZ    EQFEEDER                                                         
         NI    UNTFLAG1-UNTENT(R1),X'FF'-X'40' JUST RESTORE                     
         OI    UNITSW,X'40'        SET ON FEED ADDED (SORT UNIT TABLE)          
         B     VFED20                                                           
*                                                                               
VFED14   LA    R1,UNTNEXT-UNTENT(,R1)                                           
         CLI   0(R1),0                                                          
         BNE   VFED10                                                           
         LA    R0,UNITABLE                                                      
         AHI   R0,L'UNITABLE                                                    
         LA    RF,UNTNEXT-UNTENT(,R1)                                           
         CR    R0,RF                                                            
         BL    UNTSIZER                                                         
*                                                                               
* VALIDATE FEED TO EXIST *                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING FEEDKEY,RF                                                       
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,BAGYMD                                                   
         MVC   FEEDKNET,NETWORK                                                 
         MVC   FEEDKCLT,BCLT                                                    
         MVC   FEEDKFD,22(R4)                                                   
         LA    RE,FEEDKFD+3                                                     
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         MVI   0(RE),C' '                                                       
         BCT   RE,*-12                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VFED16                                                           
         MVC   KEY,KEYSAVE                                                      
         LA    RF,KEY                                                           
         XC    FEEDKCLT,FEEDKCLT                                                
         DROP  RF                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOFEEDER                                                         
VFED16   XC    0(L'UNTENT,R1),0(R1) CLEAR                                       
*                             AIRDATE/2/SQH/EST/SUB/DP/DISK ADDR                
         MVC   UNTCOM-UNTENT(,R1),UNTENT                                        
         MVC   UNTCOM2-UNTENT(,R1),UNTCOM2                                      
         MVC   UNTPROD-UNTENT(8,R1),UNTPROD PRD/SLN/PRD2/SLN2                   
         MVC   UNTFEED-UNTENT(,R1),22(R4)                                       
         MVI   UNTFLAG1-UNTENT(R1),X'01'                                        
         TM    UNTFLAG2,UNTFL2CS   TEST COPY SPLIT                              
         BZ    *+8                                                              
         OI    UNTFLAG2-UNTENT(R1),UNTFL2CS                                     
*                                                                               
         XC    UNTPROD-UNTENT(,R1),UNTPROD-UNTENT(R1) NEED TO ASGN PRD          
         MVC   UNTCSPRO-UNTENT(,R1),UNTCSPRO                                    
*                                                                               
VFED20   LH    R2,TOTUNITS                                                      
         LA    R2,1(,R2)           ADD 1 TO TOTUNITS                            
         STH   R2,TOTUNITS                                                      
*                                                                               
         OI    UNITSW,X'40'+X'20'  SET ON ADDED FEEDS AND SVTWA SWS             
*                                                                               
         OC    NEXTFEED,NEXTFEED                                                
         BNZ   VFED30                                                           
         LR    R0,R5                                                            
         LA    R1,UNITABLE                                                      
         SR    R0,R1                                                            
         ST    R0,NEXTFEED                                                      
*                                                                               
VFED30   TM    UNTFLAG1,UNTFL1FD   WAS THIS A FEED                              
         BO    VFEDX                                                            
         OI    UNTFLAG1,X'02'      SET ON NAT UNIT FLAG                         
VFEDX    XIT1                                                                   
*                                                                               
NOFEEDER MVC   GERROR,=Y(FEEDNF)                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM+1(4),22(R4)    SHOW BAD FEED                                
         MVI   ELEM,5              L'SUBST TEXT + 1                             
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         J     CKSVTWA                                                          
UNTSIZER MVC   GERROR,=Y(TOMNYUNT)                                              
         J     CKSVTWA                                                          
*                                                                               
EQFEEDER MVC   GERROR,=Y(FEEDEXST)                                              
         XC    ELEM,ELEM                                                        
         MVC   ELEM+1(4),22(R4)    SHOW BAD FEED                                
         MVI   ELEM,5              L'SUBST TEXT + 1                             
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         J     CKSVTWA                                                          
*                                                                               
FEEDLNER MVC   GERROR,=Y(BDFEEDLN)                                              
         J     CKSVTWA                                                          
         EJECT                                                                  
NETI     NTR1  BASE=*,LABEL=*                                                   
*        L     R3,AIO2                                                          
         USING NETBLOCKD,R3                                                     
*                                                                               
         LR    R0,R3               CLEAR NETBLOCK                               
         LA    R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    WORK,WORK           * READ TN PROFILE *                          
         MVC   WORK(4),=C'S0N0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY                         
         BZ    NETI10                                                           
*                                                                               
         TM    SECFLAG,NEPRDOFF    PRDS OFFICES NOT EQ                          
         BO    NETI10                                                           
*                                                                               
         MVC   WORK+11(1),SVPRDOFF USE PROD OFFICE                              
*                                                                               
NETI10   GOTO1 GETPROF,DMCB,WORK,NBUSER,DATAMGR                                 
*                                                                               
         MVC   NBSELAGY,AGENCY                                                  
         MVC   NBEFFAGY,AGENCY                                                  
         MVC   NBSELMED,QMED                                                    
         MVC   NBSELAM,BAGYMD                                                   
         MVC   NBSELCLI,QCLT                                                    
         MVC   NBSELCL2,BCLT                                                    
         MVC   NBSELNET,NETWORK                                                 
         MVC   NBSELPRG,ELEM+12                                                 
         CLI   ELEM,0                                                           
         BE    *+10                                                             
         MVC   NBSELSTR(12),ELEM                                                
*                                                                               
         MVC   NBSELPRD,=C'POL'   PER PZIR (SO C/S WILL WORK)                   
         MVI   NBSELPST,C'B'                                                    
         MVI   NBSELUOP,C'B'      GET MISSED (EST AND ACTUAL)                   
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'Q'                                                       
*        MVI   NBSEQ,C'D'                                                       
         MVI   NBSELTRF,C'B'      READ BOTH NETWORK AND TRAFFIC UNITS           
         OI    NBDMGOPT,X'08'      AND DELETED RECS                             
         MVI   NBMODE,NBPROCUN                                                  
         MVI   NBNOWRIT,C'N'       STOP LOCKING RECORDS                         
*                                                                               
         MVC   NBAIO,AIO3                                                       
         L     R1,SYSPARMS                                                      
         L     RF,16(,R1)           COMFACS ADDRESS                             
         ST    RF,NBACOM                                                        
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A27')                                 
         L     RF,0(,R1)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ANETIO                                                        
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A28')                                 
         L     RF,0(,R1)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,NBNETVAL                                                      
         XIT1                                                                   
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* BUILD LIST OF NETWORK UNITS AND FEEDS *                                       
*                                                                               
BLU      NMOD1 0,**+BLU**,R7       NOTE NEED TWO BASE REGISTERS                 
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         USING NETBLOCKD,R3                                                     
*                                                                               
         BRAS  RE,INITNET          SET FROM SPOT TO UNIT FILES                  
*                                                                               
         MVC   KEY,NBKEY                                                        
         OI    DMINBTS,X'08'       READ DELETED RECS                            
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' SET OFF READ DELETED RECS                    
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    UNITSW,X'FF'-X'80'-X'04'                                         
         L     RE,AIO2                                                          
         LA    RE,2000(RE)                                                      
         ST    RE,AESTBL                                                        
         LA    RF,1000                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
         SR    R1,R1                                                            
         ST    R1,NXTUNIT                                                       
*                                                                               
         SR    R4,R4              CLEAR UNIT COUNT                              
         XC    SVTS(6),SVTS       CLEAR TSUPP AND LEN                           
*                                                                               
* FIRST UNIT WAS READ IN VPROG RTN                                              
*                                                                               
BLU010   BRAS  RE,INITNET          SET TO NET                                   
         XC    CML1(3),CML1        CLEAR LENGTH CML1/CML2/CML3                  
         XC    SVTAG,SVTAG         CLEAR SAVE TAG                               
*                                                                               
         L     R6,NBAIO                                                         
*                                                                               
         CLI   SVLINE,0            LINE# ENTERED?                               
         BE    *+14                 NO                                          
         CLC   NBACTSUB,SVLINE     IS THIS IT                                   
         BNE   BLU082               NO                                          
*                                                                               
         NI    UNITSW2,X'FF'-UNITFDD INIT FEED NO NATIONAL                      
         XC    SVCSPROD,SVCSPROD                                                
*                                                                               
         TM    NBUNST3,X'40'       IS THIS A COPY SPLIT                         
         BZ    *+20                                                             
         OI    UNTFLAG2,UNTFL2CS   SET ON COPY SPLIT FLAG                       
         MVC   UNTCSPRO(3),NBPR1CL3                                             
         MVC   UNTCSPRO+3(3),NBPR2CL3                                           
*                                                                               
         MVI   ELCODE,X'19'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BLU010AC                                                         
*                                                                               
         USING NUPDED,R6                                                        
*                                                                               
         TM    NUPDEIND,X'40'      IS THIS A COPY SPLIT                         
         BZ    BLU010E              NO, BYPASS ALL C/S LOGIC                    
*                                                                               
         OI    UNTFLAG2,UNTFL2CS   SET ON COPY SPLIT FLAG                       
         MVC   UNTCSPRO(3),NBPR1CL3                                             
         MVC   UNTCSPRO+3(3),NBPR2CL3                                           
*                                                                               
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'7'                                                         
         CHI   R0,3                MUST BE A REMAINDER OF 3                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1                                                            
         CHI   R0,7                NO MORE THAN 6 PROD'SRODUCT                  
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    R1,NUPDEPR                                                       
         LA    RF,SVCSPROD                                                      
BLU010AA MVC   0(3,RF),0(R1)                                                    
         LA    R1,7(,R1)                                                        
         LA    RF,3(,RF)                                                        
         BCT   R0,BLU010AA                                                      
*                                                                               
         CLI   SVCSPROD,0                                                       
         BNE   BLU010E                                                          
         DC    H'0'                                                             
*                                                                               
BLU010AC DS    0H                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'14'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BLU010E                                                          
         USING NUPRDD,R6                                                        
*                                                                               
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'6'                                                         
         LR    R0,R1                                                            
         CHI   R0,1                MUST BE MORE THAN 1 PRODUCT                  
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R1,NUPRDPR                                                       
         LA    RF,SVCSPROD                                                      
*                                                                               
BLU010AD DS   0H                                                                
         LA    R2,NCLSTSIZ         FIND PROD                                    
         L     RE,ASVNCLST                                                      
*                                                                               
BLU010AE CLC   3(1,RE),0(R1)                                                    
         BE    BLU010AG                                                         
         LA    RE,4(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNH   *+8                                                              
         BCT   R2,BLU010AE                                                      
         DC    H'0'                                                             
BLU010AG DS    0H                                                               
         MVC   0(3,RF),0(RE)                                                    
         LA    R1,6(,R1)                                                        
         LA    RF,3(,RF)                                                        
         BCT   R0,BLU010AD                                                      
*                                                                               
         CLI   SVCSPROD,0                                                       
         BNE   BLU010E                                                          
         DC    H'0'                                                             
*                                                                               
BLU010E  L     R6,NBAIO                                                         
*                                                                               
         CLI   NBACTSUB,C'A'       THIS A SKED UNIT                             
         BNL   BLU010D              YES, NO ESTIMATE CK                         
*                                                                               
         BRAS  RE,CKEST            GO CK EST FOR COPY CDE N(BYPASS)             
         BE    BLU082               BYPASS                                      
*                                                                               
* CK PRODUCT OR PRODUCT GROUP FILTER *                                          
*                                                                               
BLU010D  DS    0H                                                               
         CLI   VOPTPROD,0                                                       
         BE    BLU010F                                                          
         CLC   NBPR1CL3,VOPTPROD                                                
         BE    BLU010F                                                          
         CLC   SVCSPROD(3),VOPTPROD                                             
         BNE   BLU082                                                           
*                                                                               
BLU010F  DS   0H                                                                
         OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    *+12                 NO                                          
         BAS   RE,FPG              FILTER ON PRODUCT GROUP                      
         BNE   BLU082                                                           
*                                                                               
* CHECK BRAND LEVEL SECURITY                                                    
*                                                                               
*        TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
*        BZ    BLU011                                                           
*                                                                               
* FIX HERE                                                                      
*        LA    R0,NCLSTSIZ         FIND PROD                                    
*        L     R1,ASVNCLST                                                      
*        SPACE                                                                  
*        LA    RF,NBPRD                                                         
*        CLI   NBPRD,0             ANY PROD                                     
*        BNE   BLU010H                                                          
*        SPACE                                                                  
*        LA    RF,SVCSPROD                                                      
*        CLI   SVCSPROD,0                                                       
*        BNE   *+6                                                              
*        DC    H'0'                UNALLOCATED ??!!                             
*        SPACE                                                                  
*LU010H  CLC   0(1,RF),3(R1)                                                    
*        BE    BLU011                                                           
*        LA    R1,4(R1)                                                         
*        CLI   0(R1),C' '                                                       
*        BNH   *+8                                                              
*        BCT   R0,BLU010H                                                       
*        MVI   ERROR,SECLOCK       SECURITY LOCK-OUT                            
*        B     BLU082                                                           
*                                                                               
BLU011   BRAS  RE,INITNET          SET FROM SPOT TO NET                         
*                                                                               
         TM    NBUNITST,X'42'     MISSED, TREAT AS DELETE                       
         BNZ   *+12                                                             
*                                                                               
         TM    NURSTAT-NUKEY(R6),X'80' DELETED UNIT                             
         BZ    BLU012                                                           
*                                                                               
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BLU082                                                           
         USING NUCMLEL,R6                                                       
         TM    NUCMLFLG,X'02'     MISSED/DELETED ON ORIG INSTR                  
         BO    BLU082                                                           
         TM    NUCMLFLG,X'10'     UNIT PRINTED ON INSTR                         
         BZ    BLU082                                                           
*                                                                               
         LH    RF,DLUNTS                                                        
         LA    RF,1(,RF)           ADD 1 TO DELETED UNITS                       
         STH   RF,DLUNTS                                                        
         B     BLU082             NEXT UNIT                                     
*                                                                               
         DROP  R6                                                               
*                                                                               
BLU012   TM    NBPACKST,X'20'     LOCKED                                        
         BZ    BLU012C             NO                                           
         OI    UNITSW,X'04'       SET ON FOUND LOCKED UNIT(S)                   
         B     BLU082                                                           
*                                                                               
BLU012C  OC    NBPR1CL3,NBPR1CL3   UNALLOCATED                                  
         BNZ   BLU012F                                                          
*                                                                               
         CLI   NBPRD,0             STD OLD PRD?                                 
         BNE   BLU012F                                                          
*                                                                               
         CLI   SVCSPROD,0          COPY SPLIT OR TRI-BACK PRD FOUND             
         BNE   BLU012F                                                          
*                                                                               
         OI    UNITSW,X'80'        SET ON SOME UNALLOCATED                      
         LH    R1,UNALUNTS                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,UNALUNTS                                                      
         B     BLU082                                                           
*                                                                               
BLU012F  L     R6,NBAIO                                                         
*                                                                               
* SEE IF THIS UNIT HAS 5 SECONDS TAG                                            
*                                                                               
         MVI   ELCODE,X'60'                                                     
         LA    R6,27(R6)                                                        
BLUNTG10 BRAS  RE,NEXTEL                                                        
         BNE   BLUNTGX                                                          
*                                                                               
         CLI   2(R6),C'Q'          IS THIS A TAG ELEMENT                        
         BNE   BLUNTG10                                                         
         CLI   3(R6),C'T'          T=TAG                                        
         BNE   BLUNTG10                                                         
*                                                                               
         PACK  DUB,4(1,R6)                                                      
         CVB   R0,DUB              TAGS IN BINARY                               
         MHI   R0,5                TIMES 5 (5 SECONDS PER TAG)                  
         LLC   R1,NBLEN            TOTAL UNIT LEN                               
         SR    R1,R0               MINUS TAG LEN                                
         STC   R1,NBLEN            = ACTUAL UNIT LENGTH                         
*                                                                               
         MVC   SVTAG,3(R6)         SAVE TAG (T1)                                
*                                                                               
BLUNTGX  DS    0H                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BLU013C                                                          
         USING NUCMLEL,R6                                                       
*                                                                               
         TM    NUCMLFL2,NUCMLFFD   FEED W/O NATIONAL ?                          
         BZ    BLU013C                                                          
         OI    UNITSW2,UNITFDD      YES                                         
         B     BLU049              SAVE BUILDING TABLE ENTRY TILL LATER         
*                                                                               
* THIS NTR1 IS USED FOR FEED WITHOUT NATIONAL                                   
*                                                                               
BLU013   NTR1                                                                   
*                                                                               
* BUILD TABLE ENTRY *                                                           
*                                                                               
BLU013C  XC    UNTENT(L'UNTENT+2),UNTENT                                        
         MVC   UNTADTEP,NBACTDAT                                                
         MVC   UNTADTE2,NBACTDAT   ASSUME USE UNIT DATE                         
         MVC   UNTSQH,NBACTSQH                                                  
         MVC   UNTEST,NBACTEST                                                  
         MVC   UNTSUB,NBACTSUB                                                  
         MVC   UNTDP,NBACTDP                                                    
         MVC   UNTDSKAD,NBKEY+21                                                
*                                                                               
         TM    UNTFLAG3,UNTFL3TB   IS IT TRI-BACK FLAG                          
         BO    BLU014AA             YES                                         
*                                                                               
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    BLU014AA             YES, DO NOT USE PROD HERE                   
*                                                                               
         MVC   UNTPROD,NBPR1CL3                                                 
         MVC   UNTPROD2,NBPR2CL3                                                
*                                                                               
         OC    NBPR1CL3,NBPR1CL3   IS THERE A PRODUCT?                          
         BNZ   BLU013I                                                          
*                                                                               
         CLI   NBPRD,0             IS THERE BINARY PRD?                         
         BE    BLU082               BYPASS                                      
*                                                                               
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
BLU013E  DS   0H                                                                
         CLC   NBPRD,3(R1)         THIS A VALID PROD CODE                       
         BE    BLU013G                                                          
         LA    R1,4(,R1)           BUMP PROD PTR                                
         CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   *+8                  YES, ERROR                                  
         BCT   R0,BLU013E                                                       
         DC    H'0'                                                             
BLU013G  DS    0H                                                               
         MVC   UNTPROD,0(R1)                                                    
BLU013I  DS    0H                                                               
         MVC   UNTPROD2,NBPR2CL3                                                
*                                                                               
         OC    NBPR2CL3,NBPR2CL3   IS THERE A PRODUCT?                          
         BNZ   BLU013O                                                          
*                                                                               
         CLI   NBPRD2,0            IS THERE BINARY PRD?                         
         BE    BLU014AF             NO PRD2                                     
*                                                                               
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
BLU013K  DS   0H                                                                
         CLC   NBPRD,3(R1)         THIS A VALID PROD CODE                       
         BE    BLU013M                                                          
         LA    R1,4(,R1)           BUMP PROD PTR                                
         CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   *+8                  YES, ERROR                                  
         BCT   R0,BLU013K                                                       
         DC    H'0'                                                             
BLU013M  DS    0H                                                               
         MVC   UNTPROD,0(R1)                                                    
BLU013O  DS    0H                                                               
         B     BLU014AF                                                         
*                                                                               
BLU014AA DS    0H                                                               
         OI    UNTFLAG2,UNTFL2CS   SET ON COPY SPLIT FLAG                       
         OC    NUCMPROD,NUCMPROD    3 CHAR PROD?                                
         BZ    *+14                                                             
         MVC   UNTPROD,NUCMPROD                                                 
         B     BLU014AF                                                         
*                                                                               
         CLI   NUCMLPRD,0          ANY OLD PROD?                                
         BE    BLU014AF             NO                                          
*                                                                               
         LA    R0,NCLSTSIZ                                                      
         L     R1,ASVNCLST                                                      
BLU014AC CLC   NUCMLPRD,3(R1)                                                   
         BE    BLU014AD                                                         
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,BLU014AC                                                      
         B     BLU014AF                                                         
BLU014AD MVC   UNTPROD,0(R1)                                                    
*                                                                               
BLU014AF DS    0H                                                               
         MVC   UNTSLN,NBLEN                                                     
         MVC   UNTEQVCT,CUREQVCT                                                
         MVC   UNTDAY,NBDAY                                                     
*                                                                               
         OC    SVTAG,SVTAG                                                      
         BZ    *+14                                                             
         MVI   UNTFEED+1,X'FF'                                                  
         MVC   UNTFEED+2(2),SVTAG  MOVE T(N) 00FFE3F1 (00FFT3)                  
*                                                                               
         OC    NBTIME,NBTIME       ANY TIME ENTERED?                            
         BZ    BLU014C              NO                                          
*                                                                               
         LA    R1,TIMETBL          START OF TIME TABLE                          
         LR    RF,R1                                                            
         LA    RE,L'TIMETBL(R1)    END OF TABLE                                 
*                                                                               
BLU014AG OC    0(4,R1),0(R1)       EMPTY?                                       
         BZ    BLU014A              YES                                         
         CLC   0(4,R1),NBTIME      SAME ENTRY                                   
         BE    BLU014B                                                          
         LA    R1,4(R1)                                                         
         CR    R1,RE                                                            
         BL    BLU014AG                                                         
*                                                                               
         B     UNTSZERB            TOO MANY UNIT                                
*                                                                               
BLU014A  MVC   0(4,R1),NBTIME      SAVE TIME IN TABLE                           
*                                                                               
BLU014B  SR    R1,RF               GET DISPLACEMENT INTO TIME TABLE             
         LA    R1,1(R1)                                                         
         STC   R1,UNTTIME          AND SAVE IT                                  
*                                                                               
BLU014C  CLI   SVMEDIA,C'N'        FOR MEDIA N ONLY                             
         BNE   *+12                                                             
         CLI   SVTN3PRO+4,C'Y'     TEST ASSIGN BY DAY                           
         BE    BLU014F             YES - SO IGNORE ROTATION                     
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,WORK)                                
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,WORK)                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         LLC   RF,0(R1)            GET DAY OF WEEK                              
*                                                                               
         XC    WORK,WORK                                                        
         LLC   R0,NBSDROT                                                       
         SLL   R0,25                                                            
         SR    R1,R1               CLEAR COUNTER                                
*                                                                               
         LTR   RF,RF                                                            
         BZ    BLU014D                                                          
         SLL   R0,1                ONLY COUNT DAYS AFTER NBACTDAT               
         BCT   RF,*-4                                                           
*                                                                               
BLU014D  LTR   R0,R0               COUNT UNTIL NO DAYS LEFT                     
         BZ    BLU014E                                                          
         SLL   R0,1                                                             
         BCT   R1,BLU014D                                                       
*                                                                               
BLU014E  LPR   R0,R1                                                            
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,UNTADTEP),(0,WORK)                                
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,WORK+6),(2,UNTADTE2)                              
*                                                                               
         MVC   SVUNTDTS,UNTADTEP                                                
*                                                                               
         CLC   UNTADTE2,ENDATEP    CAN NOT RUN OVER ENDATE                      
         BNH   *+10                                                             
         MVC   UNTADTE2,ENDATEP    FORCE TO ENDATE                              
*                                                                               
BLU014F  TM    NBUNITST,X'20'      ANY ACTUAL COST?                             
         BZ    *+14                                                             
         OI    UNTFLAG3,UNTFLCST                                                
         MVC   UNTACOST,NBACTUAL   SAVE COST IN TABLE                           
*                                                                               
         TM    NBUNITST,X'04'      PBF UNIT (BONUS)                             
         BZ    *+8                                                              
         OI    UNTFLAG3,UNTFLPFB                                                
*                                                                               
         TM    NBUNITST,X'01'      MAKE GOOD UNIT                               
         BZ    *+8                                                              
         OI    UNTFLAG3,UNTFLMG                                                 
*                                                                               
         TM    NBUNST3,X'02'       IS THIS AN ADU UNIT                          
         BZ    *+8                  NO                                          
         OI    UNTFLAG2,UNTFL2AD   SET ON ADU FLAG                              
*                                                                               
         TM    NBUNST3,X'40'       IS THIS A COPY SPLIT                         
         BO    *+12                                                             
         CLI   SVCSPROD,0                                                       
         BE    BLU017                                                           
*                                                                               
         XC    UNTPROD,UNTPROD                                                  
         XC    UNTPROD2,UNTPROD2                                                
         MVC   UNTCSPRO(3),NBPR1CL3   SAVE BOTH COPY SPLIT PRODS                
         MVC   UNTCSPRO+3(3),NBPR2CL3                                           
         TM    NBUNST3,X'40'       IS THIS A COPY SPLIT                         
         BZ    *+12                                                             
         OI    UNTFLAG2,UNTFL2CS   SET ON COPY SPLIT FLAG                       
         B     BLU014H                                                          
*                                                                               
BLU014H  CLC   NBPR1CL3,NBPR2CL3                                                
         BNE   *+10                                                             
         XC    UNTCSPRO+3(3),UNTCSPRO+3                                         
*                                                                               
* IF PRODUCT ELEMENT, THEN SAVE MULTI COPYSPLIT PRODUCTS *                      
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'19'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BLU015F                                                          
         USING NUPDEEL,R6                                                       
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'7'                                                         
         CHI   R0,3                MUST BE A REMAINDER OF 3                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1                                                            
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CHI   R0,7                MAX OF 6 PROD'S                              
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    R1,NUPDEPR                                                       
         LA    RF,UNTCSPRO                                                      
         B     BLUNT10C                                                         
*                                                                               
* IF PRODUCT ELEMENT, THEN SAVE MULTI COPYSPLIT PRODUCTS *                      
*                                                                               
BLU015F  DS   0H                                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'14'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BLU017                                                           
         USING NUPRDD,R6                                                        
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'6'                                                         
         CHI   R0,3                MUST BE A REMAINDER OF 3                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1                                                            
         CHI   R0,1                MUST BE MORE THAN 1 PRODUCT                  
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R1,NUPRDPR                                                       
         LA    RF,UNTCSPRO                                                      
***********************************************************                     
BLUPR00  LA    R2,NCLSTSIZ         FIND PROD                                    
         L     RE,ASVNCLST                                                      
*                                                                               
BLUPR10  CLC   0(1,R1),3(RE)                                                    
         BE    BLUPR20                                                          
         LA    RE,4(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNH   *+8                                                              
         BCT   R2,BLUPR10                                                       
         DC    H'0'                PROD NOT IN TABLE                            
*                                                                               
BLUPR20  MVC   0(3,RF),0(RE)                                                    
         B     *+10                                                             
BLUNT10C MVC   0(3,RF),0(R1)                                                    
         LA    RF,3(,RF)           BUMP IN SVCSPROD                             
         LA    R1,6(,R1)           TO NEXT PROD IN ELEM                         
         CLI   ELCODE,X'19'        NEW 3 CHAR PROD ELEMENT                      
         BNE   BLUPR30                                                          
         LA    R1,1(R1)            YES, BUMP ONE MORE IN ELEM                   
         BCT   R0,BLUNT10C                                                      
         B     *+8                                                              
BLUPR30  BCT   R0,BLUPR00                                                       
*                                                                               
***********************************************************                     
         B     BLU018   BOB HAD BLU058  ?????                                   
*                                                                               
* IF PARTNER, CALC SLN2 *                                                       
*                                                                               
BLU017   DS    0H                                                               
         OC    NBPR2CL3,NBPR2CL3   PARTNER                                      
         BZ    BLU018                                                           
*        THIS SURFACED WITH BACKDOOR COPY SPLIT                                 
         TM    UNTFLAG2,UNTFL2CS                                                
         BO    BLU018                                                           
*                                                                               
         LLC   R1,NBLEN1           LENGTH OF 1ST PRD                            
         CLI   NBLEN1,0            IF ZERO THEN                                 
         BNE   BLU017C                                                          
         SR    RE,RE                                                            
         LLC   RF,NBLEN            TOTAL LEN                                    
         D     RE,=F'2'                                                         
         STC   RF,UNTSLN2                                                       
         AR    RF,RE               ADD REMAINDER 1 IF ANY                       
         STC   RF,UNTSLN                                                        
         LR    R1,RF                                                            
         B     BLU018                                                           
*                                                                               
BLU017C  LLC   RF,NBLEN            TOTAL LENGTH                                 
         SR    RF,R1               MINUS PRD LEN 1                              
*                                                                               
BLU017F  STC   R1,UNTSLN                                                        
         STC   RF,UNTSLN2                                                       
*&&D0                                                                           
*        CLC   UNTPROD,UNTPROD2    ARE PROD IN ORDER?                           
*        BNH   BLU018                                                           
*                                                                               
*        OI    UNTFLAG2,UNTFL2SW   SET PRODS SWAPPED                            
*        BZ    BLU018                                                           
*        XC    UNTPROD,UNTPROD2                                                 
*        XC    UNTPROD2,UNTPROD                                                 
*        XC    UNTPROD,UNTPROD2                                                 
*                                                                               
*        XC    UNTSLN,UNTSLN2                                                   
*        XC    UNTSLN2,UNTSLN                                                   
*        XC    UNTSLN,UNTSLN2                                                   
*&&                                                                             
BLU018   MVI   BYTE,0              INIT TSUPP FOUND THIS TIME                   
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
BLU018C  BRAS  RE,NEXTEL                                                        
         BNE   BLU019                                                           
         CLI   2(R6),C'F'          THIS A BUY TYPE CODE                         
         BE    BLU018G                                                          
         CLI   2(R6),C'H'          TRAFFIC SUPPLIER?                            
         BNE   BLU018C              NO                                          
         TM    UNITSW2,UNITNTS     IF PRIOR UNIT WAS NON-TRA  SUPP              
         BO    TSUPPERR            THEN ERROR                                   
         MVI   BYTE,1              TSUPP FOUND THIS TIME                        
         B     BLU018K                                                          
*                                                                               
BLU018G  CLI   3(R6),C'O'          OPPORTUNISTIC                                
         BNE   *+8                                                              
         OI    UNTFLAG3,UNTFLOPR                                                
*                                                                               
         CLI   3(R6),C'S'          SCATTER                                      
         BNE   *+8                                                              
         OI    UNTFLAG3,UNTFLSCA                                                
*                                                                               
         CLI   3(R6),C'U'          UPFRONT                                      
         BNE   BLU018C                                                          
         OI    UNTFLAG3,UNTFLUPF                                                
         B     BLU018C                                                          
*                                                                               
BLU018K  DS    0H                                                               
         LLC   R1,1(R6)            GET LEN (MAX 5)                              
         AHI   R1,-3               MINUS OVERHEAD                               
*                                                                               
         OC    SVTS,SVTS                                                        
         BZ    BLU018M                                                          
         CLM   R1,1,SVTSLEN        SAME LEN                                     
         BNE   TSUPPERR                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   3(0,R6),SVTS                                                     
         BNE   TSUPPERR                                                         
         B     BLU018C                                                          
*                                                                               
BLU018M  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVTS(0),3(R6)       SAVE TRAFFIC SUPPLIER                        
         LA    R1,1(R1)                                                         
         STC   R1,SVTSLEN          SAVE TSUPP ENTRY LENGTH                      
         OI    UNITSW2,UNITTS      TURN ON FOUND TRAFFIC SUPPLIER               
         B     BLU018C                                                          
*                                                                               
BLU019   DS    0H                                                               
         CLI   BYTE,1              TSUPP FOUND                                  
         BE    *+16                 YES                                         
         TM    UNITSW2,UNITTS      IS TSUPP FLAG ON                             
         BO    TSUPPERR             YES, ERROR                                  
         OI    UNITSW2,UNITNTS     NON-TSUPP FOUND                              
*                                                                               
         MVI   ELCODE,X'22'        SEE IF UNIT BOUGHT AS FEED                   
         L     R6,NBAIO                                                         
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   UNTFEED,2(R6)                                                    
*                                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   BLU044                                                           
         USING NUCMLEL,R6                                                       
*                                                                               
         MVC   SVNUCFLG,NUCMLFLG   SAVE FLAG FROM 21 ELEM                       
*                                                                               
         TM    NUCMLFLG,X'E0'      REASSIGN NEEDED                              
         BZ    *+8                                                              
         OI    UNTFLAG1,X'80'                                                   
*                                                                               
         TM    NUCMLFLG,X'04'      BILLBOARD REQUIRED                           
         BZ    *+8                                                              
         OI    UNTFLAG2,X'04'                                                   
*                                                                               
         TM    NUCMLFLG,X'01'      THIS AN INVERT PRODUCT UNIT                  
         BZ    BLU019H              NO                                          
         TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BZ    BLU019C                                                          
         OC    UNTPROD2,UNTPROD2   IF NO PIGGYBACK PROD, DROP IP                
         BZ    BLU019H                                                          
*                                                                               
BLU019C  DS    0H                                                               
         OI    UNTFLAG2,X'10'                                                   
         OC    UNTPROD2,UNTPROD2   THERE HAD BETTER BE A PIGGY PROD             
         BNZ   BLU019H                                                          
         DC    H'0'                                                             
*                                                                               
BLU019H  CLI   SVTNPR5,C'Y'       PROFILE ON FOR TRAFFIC DELETES                
         BNE   BLU019J                                                          
*                                                                               
         TM    NUCMLFLG,X'08'      TRAFFIC DELETED UNIT                         
         BZ    BLU019J                                                          
         OI    UNTFLAG1,X'20'                                                   
         LH    RF,TRDLUNTS                                                      
         LA    RF,1(,RF)           ADD 1 TO DELETED UNITS                       
         STH   RF,TRDLUNTS                                                      
*                                                                               
BLU019J  TM    NUCMLFL2,NUCMLFFD   FEED W/O NATIONAL ?                          
         BZ    BLU019L              NO                                          
         XIT1                                                                   
*                                                                               
BLU019L  MVC   UNTREV,NUCMLREV                                                  
         MVC   UNTREF,NUCMLR3F                                                  
         MVC   UNTKEY,NUCMLKEY                                                  
*                                                                               
         MVI   UNTCMLF,0                                                        
         MVC   UNTCML1,NUCML1                                                   
         TM    NUCMADFL,NUCMADF1   TEST CMML IS ADID                            
         BZ    *+8                                                              
         OI    UNTCMLF,NUCMADF1    THEN SET IT                                  
*                                                                               
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLU019N                                                          
         MVC   UNTCML2,NUCML2                                                   
         TM    NUCMADFL,NUCMADF2   TEST CMML IS ADID                            
         BZ    *+8                                                              
         OI    UNTCMLF,NUCMADF2    THEN SET IT                                  
*                                                                               
BLU019N  OC    UNTCML1,UNTCML1                                                  
         BZ    BLU020                                                           
*                                                                               
         CLC   UNTCML1,UNTCML2                                                  
         BNE   BLU020                                                           
         OI    UNTFLAG1,UNTFL1PB   SET P/B CML FLAG                             
BLU020   DS    0H                                                               
*&&DO                                                                           
*        TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
*        BZ    BLU020X              NO                                          
*                                                                               
*        XC    UNTCML1,UNTCML2     SWAP CMLS ALSO                               
*        XC    UNTCML2,UNTCML1                                                  
*        XC    UNTCML1,UNTCML2                                                  
* NEED TO SWAP ADID FLAGS TOO!                                                  
*        MVC   BYTE,UNTCMLF                                                     
*        NI    BYTE,X'3F'                                                       
*        TM    UNTCMLF,X'80'                                                    
*        BZ    *+8                                                              
*        OI    BYTE,X'40'                                                       
*        TM    UNTCMLF,X'40'                                                    
*        BZ    *+8                                                              
*        OI    BYTE,X'80'                                                       
*        MVC   UNTCMLF,BYTE                                                     
*&&                                                                             
BLU020X  MVC   UNTOTHR(17),NUCMLBSL                                             
         MVC   UNTBBPOS,NUCMLBPS                                                
*                                                                               
         MVC   UNTPOS,NUCMLPOS                                                  
*                                                                               
         TM    NBUNST3,X'40'       IS THIS A COPY SPLIT                         
         BZ    BLU024               NO                                          
*                                                                               
         LA    RE,NUCMPROD         PRE-SET RE                                   
         OC    NUCMPROD,NUCMPROD   ALLOCATED                                    
         BNZ   BLU021                                                           
*                                                                               
         CLI   NUCMLPRD,0          UNLESS UNALLOCATED                           
         BE    BLU024               YES                                         
*                                                                               
         LA    R0,NCLSTSIZ                                                      
         L     R1,ASVNCLST                                                      
GPRD04   CLC   NUCMLPRD,3(R1)                                                   
         BE    GPRD06                                                           
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,GPRD04                                                        
         B     BLU024                                                           
GPRD06   LA    RE,0(R1)            PT TO 3 CHAR PROD                            
*                                                                               
BLU021   DS    0H                                                               
         LA    R0,(L'UNTCSPRO/3)                                                
         LA    R1,UNTCSPRO                                                      
*                                                                               
BLU022   CLC   0(3,RE),0(R1)       MUST BE 1 OF AVAIL PRODS                     
         BE    BLU023               YES                                         
         LA    R1,3(,R1)                                                        
         CLI   0(R1),0                                                          
         BE    BLU024                                                           
         BCT   R0,BLU022                                                        
         B     BLU024              LEAVE UNIT PRODUCT ZERO                      
*                                                                               
BLU023   MVC   UNTPROD,0(RE)       SAVE 3 CHAR PROD                             
*                                                                               
BLU024   TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BO    BLU034              YES, DISREGARD                               
*                                                                               
         OC    UNTCML1,UNTCML1                                                  
         BNZ   BLU024B                                                          
*                                                                               
         OC    UNTCML2,UNTCML2                                                  
         BZ    BLU044                                                           
*                                                                               
* CML1 IS UNASSIGNED NEED LEN OF CML2 FOR CORRECT UNIT LENGTHS                  
         MVC   WORK(8),UNTCML2                                                  
         MVI   JUSLEN,C'Y'                                                      
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         BRAS  RE,FCML             SEE IF COMML EXISTS                          
         BNE   BLU030                                                           
         LLC   RE,NBLEN            TOTAL LENGTH                                 
         LLC   RF,WORK             CML2 LEN                                     
         SR    RE,RF                                                            
         STC   RE,UNTSLN                                                        
         STC   RF,UNTSLN2                                                       
         B     BLU030                                                           
*                                                                               
* VALIDATE CML NOT DELETED, AND RELEASE/RECALL DATES OK                         
*                                                                               
BLU024B  CLC   UNTCML1,=C'REASSIGN'                                             
         BE    BLU028                                                           
         MVC   WORK(8),UNTCML1                                                  
         MVC   WORK+8(4),UNTADTEP  & UNTADTE2                                   
*                                                                               
         XC    SVTYPE,SVTYPE       CLEAR CML TYPE                               
*                                                                               
         MVI   CMLFLAG,1           VALIDATE CML1                                
         MVC   SVPROD,UNTPROD                                                   
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         NI    REVALID,X'FF'-RESOCML                                            
         BRAS  RE,FCML                                                          
         BNE   BLU028                                                           
*                                                                               
         MVC   CML1,DUB            SAVE CML1 LENGTH                             
*                                                                               
         TM    UNTFLAG1,UNTFL1PB   1 CML FOR BOTH PRODS                         
         BO    BLU024C                                                          
*                                                                               
         MVC   UNTSLN,DUB          SAVE CML1 LENGTH                             
         LLC   RE,NBLEN            TOTAL LENGTH                                 
         LLC   RF,DUB                                                           
         SR    RE,RF                                                            
         STC   RE,UNTSLN2                                                       
*                                                                               
BLU024C  BRAS  RE,VCMLAPR          GO CHECK STARCOM DATA                        
*                                                                               
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BNZ   BLU028                                                           
         TM    UNTFLAG3,UNTFL3TB   TRI-BACK?                                    
         BO    BLU026                                                           
         OC    UNTPROD2,UNTPROD2   PROD2?                                       
         BNZ   BLU026               YES                                         
         LLC   R1,CML1             CML LEN                                      
         LLC   R2,NBLEN            UNIT LEN                                     
         CLC   =C'VIGNETTE',UNTBBSN                                             
         BNE   BLU025                                                           
         LLC   RF,UNTBBSL                                                       
         AR    R1,RF                                                            
BLU025   CR    R1,R2                                                            
         BE    BLU026                                                           
         CLI   CML1,X'FF'          OKAY FOR ALL LENGTHS                         
         BNE   BLU028                                                           
*                                                                               
BLU026   TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BZ    *+10                                                             
         MVC   UNTSLN,DUB          SAVE CML LENGTH                              
*                                                                               
         MVI   PRDMATSW,0                                                       
*                                                                               
         MVI   CMLFLAG,1           VALIDATE CML1                                
         LA    R2,UNTPROD                                                       
         BRAS  RE,VPRD             VALIDATE PROD                                
         TM    PRDMATSW,X'01'      ALL PRDS COVERED ?                           
         BO    BLU030                                                           
         TM    PRDMATSW,X'20'                                                   
         BO    BLU027               PRD1 IS NOT COVERED                         
         CLI   UNTPROD2,0                                                       
         BE    BLU030                                                           
*                                                                               
         LA    R2,UNTPROD2         SEE IF PRD2 IS COVERED AS WELL               
         BRAS  RE,VPRD             VALIDATE PROD                                
         TM    PRDMATSW,X'20'                                                   
         BO    BLU030              ONLY PRD 1 IS COVERED BY THIS CML            
         OI    PRDMATSW,X'01'      TURN ON BOTH PRDS ARE COVERED                
         B     BLU030                                                           
*                                                                               
BLU027   OC    UNTPROD2,UNTPROD2                                                
         BZ    BLU028              PROD CHANGED SINCE LAST ASSIGNED             
*                                                                               
         NI    PRDMATSW,X'FF'-X'20'                                             
         LA    R2,UNTPROD2                                                      
         BRAS  RE,VPRD             VALIDATE PROD                                
         TM    PRDMATSW,X'20'                                                   
         BO    BLU028              PROD CHANGED SINCE LAST ASSIGNED             
         OI    PRDMATSW,X'10'      SWAP CMLS                                    
         B     BLU030                                                           
*                                                                               
BLU028   MVC   UNTCML1,=C'REASSIGN'                                             
         OI    UNTFLAG1,X'80'      SET REASSIGN FLAG                            
*                                                                               
BLU030   OC    UNTCML2,UNTCML2                                                  
         BZ    BLU032                                                           
         CLC   UNTCML2,=C'REASSIGN'                                             
         BE    BLU031                                                           
*                                                                               
         MVI   CMLFLAG,2           VALIDATE CML2                                
         MVC   SVPROD,UNTPROD2                                                  
         MVC   WORK(8),UNTCML2                                                  
         NI    REVALID,X'FF'-RESOCML                                            
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         OI    PRDMATSW,X'02'      LOOK FOR PRD/PTR                             
         BRAS  RE,FCML                                                          
         BNE   BLU031                                                           
*                                                                               
         MVC   CML2,DUB            SAVE CML2 LENGTH                             
*                                                                               
         BRAS  RE,VCMLAPR          GO CHECK STARCOM DATA                        
*                                                                               
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BNZ   BLU031                                                           
*                                                                               
         OC    UNTCML1,UNTCML1                                                  
         BZ    BLU030D             YES                                          
         CLC   =C'REASSIGN',UNTCML1                                             
         BE    BLU030D                                                          
         TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BO    BLU030D                                                          
         LLC   R1,CML1             CML1 LEN                                     
         CLC   UNTCML1,UNTCML2     CML1=CML2                                    
         BE    BLU030B             YES                                          
         LLC   R2,CML2                                                          
         AR    R1,R2               TOTAL CML LEN                                
*                                                                               
BLU030B  CLC   =C'VIGNETTE',UNTBBSN                                             
         BNE   BLU030C                                                          
         LLC   RF,UNTBBSL                                                       
         AR    R1,RF                                                            
*                                                                               
BLU030C  LLC   R2,NBLEN            UNIT LEN                                     
         CR    R1,R2                                                            
         BNE   BLU031                                                           
*                                                                               
BLU030D  TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BZ    BLU030F                                                          
         MVI   UNTSLN2,0                                                        
         CLC   UNTCML1,UNTCML2     SAME CMLS                                    
         BE    BLU030F                                                          
         MVC   UNTSLN2,DUB         SAVE CML LENGTH                              
*                                                                               
BLU030F  TM    PRDMATSW,X'10'      SWAP CMLS ?                                  
         BZ    *+12                                                             
         LA    R2,UNTPROD          PRD2 WAS COVERED BY CML1                     
         B     *+8                                                              
         LA    R2,UNTPROD2                                                      
         NI    PRDMATSW,X'FF'-X'20'                                             
         MVI   CMLFLAG,2           VALIDATE CML2                                
         BRAS  RE,VPRD             VALIDATE PROD                                
         TM    PRDMATSW,X'20'      NO MATCH ?                                   
         BZ    BLU032               OK TO GO ON                                 
         TM    PRDMATSW,X'10'      SWAP CML                                     
         BO    BLU031              PROD CHANGED SINCE LAST ASSIGNED             
         TM    PRDMATSW,X'01'      SEE IF CML1 COVERED BOTH PRDS                
         BZ    BLU031                                                           
         LA    R2,UNTPROD          MAYBE CML2 COVERS PRD1                       
         NI    PRDMATSW,X'FF'-X'20'                                             
         BRAS  RE,VPRD             VALIDATE PROD                                
         TM    PRDMATSW,X'20'      NO MATCH ?                                   
         BO    BLU031              PROD CHANGED SINCE LAST ASSIGNED             
         OI    PRDMATSW,X'10'      SWAP CML                                     
         B     BLU032                                                           
*                                                                               
BLU031   MVC   UNTCML2,=C'REASSIGN'                                             
         OI    UNTFLAG1,X'80'      SET REASSIGN FLAG                            
*                                                                               
BLU032   DS    0H                                                               
*                                                                               
BLULENX  TM    PRDMATSW,X'10'      SWAP CML                                     
         BZ    *+22                                                             
         XC    UNTCML1,UNTCML2                                                  
         XC    UNTCML2,UNTCML1                                                  
         XC    UNTCML1,UNTCML2                                                  
*                                                                               
         TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BO    BLU044                                                           
         B     BLU046                                                           
BLU034   OC    UNTCML1,UNTCML1                                                  
         BZ    BLU040                                                           
         MVC   UNTCML1,=C'REASSIGN'                                             
BLU040   OC    UNTPROD2,UNTPROD2                                                
         BZ    BLU042                                                           
         OC    UNTCML2,UNTCML2                                                  
         BZ    BLU042                                                           
         MVC   UNTCML2,=C'REASSIGN'                                             
BLU042   OI    UNTFLAG1,X'80'      SET REASSIGN NEEDED                          
*                                                                               
BLU044   TM    UNTFLAG1,X'60'      FEED OR UNIT DELETED                         
         BNZ   BLU046                                                           
*                                                                               
         LH    R1,UNASGND                                                       
         LA    R1,1(,R1)                                                        
         STH   R1,UNASGND                                                       
*                                                                               
BLU046   TM    UNTFLAG1,X'60'      FEED OR UNIT DELETED                         
         BNZ   *+8                                                              
         LA    R4,1(,R4)           ADD TO UNIT COUNT                            
*                                                                               
         TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BO    BLU048D                                                          
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS PREV ASSIGNED CMLS                    
         BNE   BLU048D              NO                                          
*                                                                               
         OI    UNTFLAG2,UNTFL2AS   SET PREVIOUSLY ASSIGNED FLAG ON              
*                                                                               
         OC    UNTCML1,UNTCML1     ASSIGNED CML1                                
         BZ    BLU047D                                                          
         CLC   UNTCML1,=C'REASSIGN'                                             
         BE    BLU047D                                                          
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLU048                                                           
         OC    UNTCML2,UNTCML2     ASSIGNED CML2                                
         BZ    BLU047D                                                          
         CLC   UNTCML1,=C'REASSIGN'                                             
         BNE   BLU048                                                           
*                                                                               
BLU047D  NI    UNTFLAG2,X'FF'-UNTFL2AS   TURN OFF PREV ASSIGNED FLAG            
*                                                                               
BLU048   DS    0H                                                               
         TM    UNTFLAG2,UNTFL2AS                                                
         BZ    BLU048D                                                          
         TM    UNTFLAG1,X'20'     TRAFFIC DELETE                                
         BO    BLU048D                                                          
         LH    R1,PREVASS          ADD UP PREV ASSIGNED CMLS                    
         LA    R1,1(,R1)                                                        
         STH   R1,PREVASS                                                       
*                                                                               
BLU048D  DS    0H                                                               
         OC    UNTBBSN,UNTBBSN     ANY BILLBOARD                                
         BZ    BLU048J                                                          
*                                                                               
         CLC   =C'VIGNETTE',UNTBBSN                                             
         BE    BLU048J                                                          
*                                                                               
         MVC   SVPROD,UNTPROD                                                   
         MVC   WORK(8),UNTBBSN                                                  
         OI    UNITSW1,UNTSW1NL    DON'T CK LEN FOR BB                          
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         BRAS  RE,FCML                                                          
         BNE   BLU048G                                                          
*                                                                               
         NI    UNITSW1,X'FF'-UNTSW1NL                                           
         TM    NUCMADFL,NUCMADF3                                                
         BZ    *+8                                                              
         OI    UNTCMLF,NUCMADF3                                                 
         B     BLU048J                                                          
*                                                                               
BLU048G  MVC   UNTBBSN,=C'REASSIGN'                                             
         OI    UNTFLAG1,X'80'      SET REASSIGN FALG                            
         NI    UNITSW1,X'FF'-UNTSW1NL                                           
*                                                                               
BLU048J  OC    UNTBBCN,UNTBBCN     ANY BILLBOARD COPY                           
         BZ    BLU048X                                                          
*                                                                               
         CLC   =C'VIGNETTE',UNTBBCN                                             
         BE    BLU048X                                                          
*                                                                               
         MVC   SVPROD,UNTPROD                                                   
         MVC   WORK(8),UNTBBCN                                                  
         OI    UNITSW1,UNTSW1NL    DON'T CK LEN FOR BB                          
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         BRAS  RE,FCML                                                          
         BNE   BLU048L                                                          
*                                                                               
         NI    UNITSW1,X'FF'-UNTSW1NL                                           
         TM    NUCMADFL,NUCMADF4                                                
         BZ    *+8                                                              
         OI    UNTCMLF,NUCMADF4                                                 
         B     BLU048X                                                          
*                                                                               
BLU048L  MVC   UNTBBCN,=C'REASSIGN'                                             
         OI    UNTFLAG1,X'80'      SET REASSIGN FALG                            
         NI    UNITSW1,X'FF'-UNTSW1NL                                           
*                                                                               
* SEE IF AT END OF TABLE *                                                      
*                                                                               
BLU048X  LA    R5,UNTNEXT                                                       
         C     R5,MAXUNT           AT END OF TABLE                              
         BNL   UNTSZERB                                                         
*                                                                               
* FIND ANY FEEDS AND IF THERE IS MAP YEAR MAP CODE *                            
*                                                                               
BLU049   DS    0H                                                               
         MVI   SVMYEAR,0           INIT MAP YEAR                                
         XC    SVMCODE,SVMCODE     AND MAP CODE                                 
*                                                                               
         MVI   ELCODE,X'18'        DATA ELEMENT                                 
         L     R6,NBAIO                                                         
         BRAS  RE,INITNET                                                       
         BRAS  RE,GETEL                                                         
         BNE   BLU049F                                                          
*                                                                               
         USING NUDTAD,R6                                                        
         CLI   NUDTAMYR,0          ANY MAP YEAR                                 
         BE    BLU049F                                                          
         OC    NUDTAMCD,NUDTAMCD   ANY MAP CODE                                 
         BZ    BLU049F                                                          
*                                                                               
         MVC   SVMYEAR,NUDTAMYR    SAVE MAP YEAR                                
         MVC   SVMCODE,NUDTAMCD    SAVE MAP CODE                                
*                                                                               
         DROP  R6                                                               
*                                                                               
BLU049F  MVI   ELCODE,X'23'                                                     
         L     R6,NBAIO                                                         
         BRAS  RE,GETEL                                                         
         BNE   BLU080                                                           
*                                                                               
         USING NUFDCEL,R6                                                       
BLU050   TM    NUFDCFL2,X'81'      THIS DELETED MEDIA BUY                       
         BNO   BLU051               NO                                          
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    BLU050                                                           
         B     BLU080               YES                                         
*                                                                               
BLU051   TM    UNITSW2,UNITFDD     FEED W/O NATIONAL ?                          
         BO    BLU052                                                           
*                                                                               
         LR    R1,R5               POINT                                        
         SHI   R1,L'UNTENT                                                      
*                                                                               
         OI    UNTFLAG1-UNTENT(R1),02    SET ON NATIONAL UNIT WITH FEED         
         OC    UNTFEED-UNTENT(,R1),UNTFEED-UNTENT(R1)                           
         BNZ   UNTFEDER                                                         
*                                                                               
BLU052   XC    UNTENT(L'UNTENT+2),UNTENT                                        
         TM    NUFDCFL2,X'81'      THIS DELETED MEDIA BUY                       
         BO    BLU079               YES                                         
*                                                                               
         TM    UNITSW2,UNITFDD     FEED NO NATIONAL                             
         BZ    *+16                                                             
         BAS   RE,BLU013           BUILD TABLE ENTRY                            
         OI    UNTFLAG4,UNTFLFDD   TURN ON FEED W/O NATIONAL                    
         B     BLU053                                                           
*                                                                               
         LR    R1,R5               POINT                                        
         AHI   R1,-L'UNTENT              TO LAST ENTRY                          
*                                                                               
         MVC   UNTENT(UNTCOM1),0(R1)                                            
         MVC   UNTDAY(L'UNTDAY+L'UNTTIME),UNTDAY-UNTENT(R1)                     
         MVC   UNTCSPRO,UNTCSPRO-UNTENT(R1)                                     
*                                                                               
         TM    SVNUCFLG,X'80'            NEED REASSIGN                          
         BZ    *+8                                                              
         OI    UNTFLAG1,X'80'                                                   
*                                                                               
         TM    SVNUCFLG,X'20'            NEED REASSIGN                          
         BZ    *+8                                                              
         OI    UNTFLAG1,X'80'                                                   
*                                                                               
         OI    UNTFLAG1,UNTFL1FD         SET ON FEED FOR NATIONAL UNIT          
*                                                                               
BLU053   XC    CML1(3),CML1        INIT CML LENGTHS                             
*                                                                               
         MVC   UNTREV,NUFDCREV                                                  
*                                                                               
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BZ    BLU053AA                                                         
*                                                                               
         OC    NUFDCR3F,NUFDCR3F   YES, PATTERN REFERENCE S/B ZERO              
         BZ    BLU053AC                                                         
         DC    H'0'                                                             
BLU053AA DS    0H                                                               
         MVC   UNTREF,NUFDCR3F                                                  
*                                                                               
BLU053AC DS    0H                                                               
         MVC   UNTKEY,NUFDCKEY                                                  
         MVC   UNTFEED,NUFDCFED                                                 
         MVC   UNTCML1,NUFDCML1                                                 
         TM    NUFDADFL,NUFDADF1   TEST CMML IS ADID                            
         BZ    *+8                                                              
         OI    UNTCMLF,NUCMADF1    THEN SET IT                                  
*                                                                               
         OC    UNTPROD2,UNTPROD2                                                
         BZ    *+10                                                             
         MVC   UNTCML2,NUFDCML2                                                 
         TM    NUFDADFL,NUFDADF2   TEST CMML IS ADID                            
         BZ    *+8                                                              
         OI    UNTCMLF,NUCMADF2    THEN SET IT                                  
*                                                                               
         MVC   UNTOTHR(17),NUFDCBSL                                             
         MVC   UNTPOS,NUFDCPOS                                                  
         MVC   UNTBBPOS,NUFDCBPS                                                
*                                                                               
         CLI   SVMYEAR,0           IS THIS SECTIONAL FEED                       
         BE    BLU053F                                                          
         MVC   UNTMYR,SVMYEAR      MAP YEAR                                     
*                                                                               
         LA    R1,MCODETBL         START OF MAP CODE TABLE                      
         LR    RF,R1                                                            
         LA    RE,L'MCODETBL(R1)   END OF TABLE                                 
*                                                                               
         OC    0(8,R1),0(R1)       EMPTY?                                       
         BZ    BLU053C              YES                                         
         CLC   0(8,R1),SVMCODE     SAME ENTRY                                   
         BE    BLU053D                                                          
         LA    R1,8(R1)                                                         
         CR    R1,RE                                                            
         BL    *-26                                                             
         DC    H'0'                MAKE MAP CODE TABLE BIGGER                   
*                                                                               
BLU053C  MVC   0(8,R1),SVMCODE     SAVE MAP CODE IN TABLE                       
*                                                                               
BLU053D  SR    R1,RF               GET DISP INTO MAP CODE TABLE                 
         LA    R1,1(R1)                                                         
         STC   R1,UNTMCDP          AND SAVE THE POINTER                         
*                                                                               
         OI    UNTFLAG4,UNTFLSEC   TURN ON SECTIONAL FLAG                       
*                                                                               
BLU053F  TM    NBUNST3,X'40'       IS THIS A COPY SPLIT                         
         BO    BLU053H              YES                                         
         TM    UNITSW2,UNITFDD     FEED NO NATIONAL                             
         BO    BLU058                                                           
         TM    UNTFLAG3-UNTENT(R1),UNTFL3TB    IS IT T/B                        
         BZ    BLU058               NO                                          
         OI    UNTFLAG3,UNTFL3TB   SET ON TRI-BACK FLAG                         
         B     *+8                                                              
BLU053H  OI    UNTFLAG2,UNTFL2CS   SET ON COPY SPLIT FLAG                       
*                                                                               
         XC    UNTPROD,UNTPROD                                                  
*                                                                               
         OC    NUFDPROD,NUFDPROD   PROD ASSIGNED?                               
         BNZ   BLU056               YES                                         
*                                                                               
         CLI   NUFDCPRD,0          IS FEED PROD ASSIGNED                        
         BE    BLU058               NO                                          
*                                                                               
         LA    R0,NCLSTSIZ                                                      
         L     R1,ASVNCLST                                                      
GPRD10   CLC   NUFDCPRD,3(R1)                                                   
         BE    GPRD20                                                           
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   R0,GPRD10                                                        
         B     BLU058                                                           
GPRD20   MVC   UNTPROD,0(R1)                                                    
         B     BLU058                                                           
*                                                                               
BLU056   MVC   UNTPROD,NUFDPROD                                                 
*                                                                               
BLU058   TM    NUFDCFL2,X'01'      THIS FROM MEDIA BUY                          
         BZ    *+8                  NO                                          
         OI    UNTFLAG2,X'01'      SET ON DO NOT DELETE FLAG                    
*                                                                               
         TM    UNTFLAG1,X'80'      ANY CHANGES                                  
         BO    BLU064              YES, DISREGARD                               
         EJECT                                                                  
* VALIDATE CML NOT DELETED, AND RELEASE/RECALL DATES OK                         
*                                                                               
         OC    UNTCML1,UNTCML1                                                  
         BZ    BLU074                                                           
         CLC   UNTCML1,=C'REASSIGN'                                             
         BE    BLU059                                                           
         MVC   WORK(8),UNTCML1                                                  
         MVC   WORK+8(4),UNTADTEP  & UNTADTE2                                   
*                                                                               
         XC    SVTYPE,SVTYPE       CLEAR CML TYPE                               
*                                                                               
         MVC   SVPROD,UNTPROD                                                   
         MVI   CMLFLAG,1           VALIDATE CML1                                
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         NI    REVALID,X'FF'-RESOCML                                            
         BRAS  RE,FCML                                                          
         BNE   BLU059                                                           
*                                                                               
         MVC   CML1,DUB                                                         
         BRAS  RE,VCMLAPR          GO CHECK STARCOM DATA                        
*                                                                               
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BNZ   BLU059                                                           
*                                                                               
         OC    UNTPROD2,UNTPROD2   IS IT P/B                                    
         BNZ   BLU058A             YES                                          
*                                                                               
         LLC   R1,NBLEN                                                         
         LLC   RE,CML1                                                          
         CR    R1,RE                                                            
         BNE   BLU059                                                           
*                                                                               
BLU058A  TM    UNTFLAG3,UNTFL3TB   IS IT TRI-BACK                               
         BZ    BLU058B                                                          
         MVI   CMLFLAG,1           PROCESS CML1                                 
         MVC   UNTSLN,DUB          SAVE CML LENGTH                              
*                                                                               
BLU058B  MVI   PRDMATSW,0                                                       
         LA    R2,UNTPROD                                                       
         BRAS  RE,VPRD             VALIDATE PROD                                
         TM    PRDMATSW,X'01'      ALL PRDS COVERED ?                           
         BO    BLU060                                                           
         TM    PRDMATSW,X'20'      PRD IS NOT COVERED                           
         BZ    *+16                                                             
         TM    UNTFLAG3,UNTFL3TB   IS IT TRI-BACK                               
         BZ    BLU058C              NO                                          
         B     BLU059              PROD CHANGED SINCE LAST ASGN                 
*                                                                               
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLU060                                                           
         LA    R2,UNTPROD2         SEE IF PRD2 IS COVERED AS WELL               
         BRAS  RE,VPRD             VALIDATE PROD                                
         TM    PRDMATSW,X'20'                                                   
         BO    BLU060                                                           
         OI    PRDMATSW,X'01'      TURN ON BOTH PRDS ARE COVERED                
         B     BLU060                                                           
BLU058C  OC    UNTPROD2,UNTPROD2                                                
         BZ    BLU059              PROD CHANGED SINCE LAST ASSIGNED             
*                                                                               
         NI    PRDMATSW,X'FF'-X'20'                                             
         LA    R2,UNTPROD2                                                      
         BRAS  RE,VPRD             VALIDATE PROD                                
         TM    PRDMATSW,X'20'                                                   
         BO    BLU059              PROD CHANGED SINCE LAST ASSIGNED             
         OI    PRDMATSW,X'10'      SWAP CMLS                                    
         B     BLU060                                                           
*                                                                               
BLU059   MVC   UNTCML1,=C'REASSIGN'                                             
         OI    UNTFLAG1,X'80'      SET REASSIGN FLAG                            
*                                                                               
BLU060   OC    UNTCML2,UNTCML2                                                  
         BZ    BLU062                                                           
         MVC   WORK(8),UNTCML2                                                  
         CLC   UNTCML2,=C'REASSIGN'                                             
         BE    BLU061                                                           
*                                                                               
         MVC   SVPROD,UNTPROD2                                                  
         MVI   CMLFLAG,2           VALIDATE CML2                                
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         NI    REVALID,X'FF'-RESOCML                                            
         BRAS  RE,FCML                                                          
         BNE   BLU061                                                           
*                                                                               
         MVC   CML2,DUB                                                         
         BRAS  RE,VCMLAPR          GO CHECK STARCOM DATA                        
*                                                                               
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BNZ   BLU061                                                           
*                                                                               
         TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BO    BLU060D                                                          
*                                                                               
         LLC   RE,CML1                                                          
         CLC   UNTCML1,UNTCML2                                                  
         BE    BLU060B                                                          
         LLC   R1,CML2                                                          
         AR    RE,R1               TOTAL CML LENGTH                             
BLU060B  LLC   R1,NBLEN                                                         
         CR    R1,RE               COMPARE UNIT LEN TO CML LENGTH               
         BNE   BLU061                                                           
         B     BLU060F                                                          
*                                                                               
BLU060D  MVI   UNTSLN2,0                                                        
         CLC   UNTCML1,UNTCML2     SAME CMLS                                    
         BE    BLU060F                                                          
         MVC   UNTSLN2,DUB         SAVE CML LENGTH                              
         MVI   CMLFLAG,2           PROCESS CML2                                 
*                                                                               
BLU060F  TM    PRDMATSW,X'10'      SWAP CMLS ?                                  
         BZ    *+12                                                             
         LA    R2,UNTPROD          PRD2 WAS COVERED BY CML1                     
         B     *+8                                                              
         LA    R2,UNTPROD2                                                      
         NI    PRDMATSW,X'FF'-X'20'                                             
         BRAS  RE,VPRD             VALIDATE PROD                                
*                                                                               
         TM    PRDMATSW,X'20'      PRD IS NOT COVERED                           
         BZ    BLU062                                                           
         TM    UNTFLAG3,UNTFL3TB   IS IT TRI-BACK                               
         BZ    BLU063               NO                                          
         B     BLU061              PROD CHANGED SINCE LAST ASGN                 
*                                                                               
         TM    PRDMATSW,X'10'      SWAP CML                                     
         BO    BLU061              PROD CHANGED SINCE LAST ASSIGNED             
         TM    PRDMATSW,X'01'      SEE IF CML1 COVERED BOTH PRDS                
         BZ    BLU061                                                           
         LA    R2,UNTPROD          MAYBE CML2 COVERS PRD1                       
         NI    PRDMATSW,X'FF'-X'20'                                             
         BRAS  RE,VPRD             VALIDATE PROD                                
         TM    PRDMATSW,X'20'      NO MATCH ?                                   
         BO    BLU061              PROD CHANGED SINCE LAST ASSIGNED             
         OI    PRDMATSW,X'10'      SWAP CML                                     
         B     BLU062                                                           
*                                                                               
BLU061   MVC   UNTCML2,=C'REASSIGN'                                             
         OI    UNTFLAG1,X'80'      SET REASSIGN FLAG                            
*                                                                               
BLU062   TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BZ    BLU063                                                           
*                                                                               
BLU063   TM    PRDMATSW,X'10'      SWAP CML                                     
         BZ    *+22                                                             
         XC    UNTCML1,UNTCML2                                                  
         XC    UNTCML2,UNTCML1                                                  
         XC    UNTCML1,UNTCML2                                                  
*                                                                               
BLU063F  TM    UNTFLAG1,X'80'                                                   
         BO    BLU074                                                           
         B     BLU076                                                           
BLU064   OC    UNTCML1,UNTCML1                                                  
         BZ    BLU070                                                           
         MVC   UNTCML1,=C'REASSIGN'                                             
BLU070   OC    UNTPROD2,UNTPROD2                                                
         BZ    BLU072                                                           
         OC    UNTCML2,UNTCML2                                                  
         BZ    BLU072                                                           
         MVC   UNTCML2,=C'REASSIGN'                                             
BLU072   OI    UNTFLAG1,X'80'                                                   
         TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BZ    BLU074                                                           
*                                                                               
BLU074   TM    UNTFLAG1,X'60'      FEED OR UNIT DELETED                         
         BNZ   BLU076                                                           
*                                                                               
         LH    R1,UNASGND                                                       
         LA    R1,1(,R1)                                                        
         STH   R1,UNASGND                                                       
*                                                                               
BLU076   TM    UNTFLAG1,X'60'      FEED OR UNIT DELETED                         
         BNZ   *+8                                                              
         LA    R4,1(,R4)           ADD TO UNIT COUNT                            
*                                                                               
         TM    UNTFLAG1,X'80'      REASSIGN NEEDED                              
         BO    BLU078D                                                          
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS PREV ASSIGNED CMLS                    
         BNE   BLU078D              NO                                          
*                                                                               
         OI    UNTFLAG2,UNTFL2AS   SET PREVIOUSLY ASSIGNED FLAG ON              
*                                                                               
         OC    UNTCML1,UNTCML1     ASSIGNED CML1                                
         BZ    BLU077D                                                          
         CLC   UNTCML1,=C'REASSIGN'                                             
         BE    BLU077D                                                          
         OC    UNTPROD2,UNTPROD2                                                
         BZ    BLU078                                                           
         OC    UNTCML2,UNTCML2     ASSIGNED CML2                                
         BZ    BLU077D                                                          
         CLC   UNTCML2,=C'REASSIGN'                                             
         BE    BLU077D                                                          
         TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BZ    BLU078                                                           
*                                                                               
BLU077D  NI    UNTFLAG2,X'FF'-UNTFL2AS   TURN OFF PREV ASSIGNED FLAG            
*                                                                               
BLU078   DS    0H                                                               
         TM    UNTFLAG2,UNTFL2AS                                                
         BZ    BLU078D                                                          
         TM    UNTFLAG1,X'20'     TRAFFIC DELETE                                
         BO    BLU078D                                                          
         LH    R1,PREVASS          ADD UP PREV ASSIGNED CMLS                    
         LA    R1,1(,R1)                                                        
         STH   R1,PREVASS                                                       
*                                                                               
BLU078D  DS    0H                                                               
         TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BO    BLU078X              YES, DONE                                   
*                                                                               
         OC    UNTBBSN,UNTBBSN     ANY BILLBOARD                                
         BZ    BLU078J                                                          
*                                                                               
         CLC   =C'VIGNETTE',UNTBBSN                                             
         BE    BLU078J                                                          
*                                                                               
         MVC   SVPROD,UNTPROD                                                   
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         MVC   WORK(8),UNTBBSN                                                  
         OI    UNITSW1,UNTSW1NL    DON'T CK LEN FOR BB                          
         BRAS  RE,FCML                                                          
         BNE   BLU078G                                                          
         MVC   UNTBBSN,WORK        MOVE 8 CHAR BB CML                           
         NI    UNITSW1,X'FF'-UNTSW1NL                                           
         B     BLU078J                                                          
*                                                                               
BLU078G  MVC   UNTBBSN,=C'REASSIGN'                                             
         OI    UNTFLAG1,X'80'      SET REASSIGN FALG                            
         NI    UNITSW1,X'FF'-UNTSW1NL                                           
*                                                                               
BLU078J  OC    UNTBBCN,UNTBBCN     ANY BILLBOARD COPY                           
         BZ    BLU078X                                                          
*                                                                               
         CLC   =C'VIGNETTE',UNTBBCN                                             
         BE    BLU078X                                                          
*                                                                               
         MVC   SVPROD,UNTPROD                                                   
         MVC   WORK(8),UNTBBCN                                                  
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         OI    UNITSW1,UNTSW1NL    DON'T CK LEN FOR BB                          
         BRAS  RE,FCML                                                          
         BNE   BLU078L                                                          
         MVC   UNTBBCN,WORK        MOVE 8 CHAR BB CML                           
         NI    UNITSW1,X'FF'-UNTSW1NL                                           
         B     BLU078X                                                          
*                                                                               
BLU078L  MVC   UNTBBCN,=C'REASSIGN'                                             
         OI    UNTFLAG1,X'80'      SET REASSIGN FALG                            
         NI    UNITSW1,X'FF'-UNTSW1NL                                           
*                                                                               
* SEE IF AT END OF TABLE *                                                      
*                                                                               
BLU078X  LA    R5,UNTNEXT                                                       
         C     R5,MAXUNT           AT END OF TABLE                              
         BNL   UNTSZERB                                                         
*                                                                               
BLU079   BRAS  RE,INITNET          SET TO UNIT                                  
         MVI   ELCODE,X'23'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    BLU052                                                           
         EJECT                                                                  
BLU080   DS   0H                                                                
         BAS   RE,GCUT             GO GET ANY CUTINS                            
*                                                                               
         MVI   SVMYEAR,0           CLEAR MAP YEAR                               
         XC    SVMCODE,SVMCODE       AND MAP CODE                               
*                                                                               
BLU082   DS   0H                                                                
         MVC   KEYSAVE,NBKEY                                                    
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'UNTDIR',KEYSAVE,KEY           
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    0(L'UNTENT+2,R5),0(R5)                                           
*                                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    BLU083                                                           
*                                                                               
         CLI   T216FFD+6,C'$'      USER SIGN ON OFFICE LIST                     
         BNE   *+12                                                             
         OI    NBINDS5,NBI5PLST    SET OFFICE LIST FLAG                         
         B     *+12                                                             
         CLI   T216FFD+6,C'*'      USER SIGN ON BY OFFICE                       
         BNE   BLU083               NO                                          
*                                                                               
         MVC   NBTRAPSC,T216FFD+7  BRAND LEVEL OFFICE(LIST)                     
BLU083   DS    0H                                                               
         GOTO1 ANETIO,DMCB,(R3)                                                 
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    BLU084                                                           
         DC    H'0'                                                             
BLU084   TM    NBSUBMSK,NBSBMCLI+NBSBMNET+NBSBMPRG                              
         BNZ   BLU090                                                           
         CLI   NBMODE,NBREQLST                                                  
         BE    BLU090                                                           
         CLI   NBMODE,NBPROCUN                                                  
         BNE   BLU080                                                           
         B     BLU010                                                           
BLU090   DS    0H                  NOW LOOK FOR EQUIVALENT PROGRAMS             
         LA    RE,EQVPTBL                                                       
         LA    RF,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         USING EQVPTBLD,RE                                                      
BLU092   OC    EQVENT,EQVENT       EMPTY ENTRY                                  
         BZ    BLU096                                                           
         CLI   EQVUSED,C'Y'        ENTRY USED ALREADY                           
         BNE   BLU094                                                           
         LA    RE,EQVNEXT                                                       
         BCT   RF,BLU092                                                        
*                                                                               
         B     BLU096                                                           
*                                                                               
BLU094   MVI   EQVUSED,C'Y'                                                     
         MVC   CUREQVCT,EQVCT      SAVE ENTRY CT                                
         MVC   ELEM(12),EQVSDT                                                  
*                                                                               
         CLC   EQVEDT,ENDATE       SEE IF END DATE PAST PERIOD END              
         BNH   *+10                                                             
         MVC   ELEM+6(6),ENDATE    CURTAIL END DATE                             
*                                                                               
         MVC   ELEM+12(6),EQVEPRG                                               
         DROP  RE                                                               
         L     R3,AIO2                                                          
         BRAS  RE,NETI                                                          
*                                                                               
         MVC   KEYSAVE,NBKEY                                                    
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'UNTDIR',KEYSAVE,KEY           
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLU095   DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    BLU095F                                                          
*                                                                               
         CLI   T216FFD+6,C'$'      USER SIGN ON OFFICE LIST                     
         BNE   *+12                                                             
         OI    NBINDS5,NBI5PLST    SET OFFICE LIST FLAG                         
         B     *+12                                                             
         CLI   T216FFD+6,C'*'      USER SIGN ON BY OFFICE                       
         BNE   BLU095F              NO                                          
*                                                                               
         MVC   NBTRAPSC,T216FFD+7  BRAND LEVEL OFFICE(LIST)                     
*                                                                               
BLU095F  GOTO1 ANETIO,DMCB,(R3)                                                 
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    BLU090                                                           
         CLI   NBMODE,NBPROCUN                                                  
         BNE   BLU095                                                           
         CLC   NBACTPRG,NBSELPRG                                                
         BNE   BLU090                                                           
         B     BLU010                                                           
*                                                                               
* LIST OF UNITS NOW BUILT, SEE IF LIST EMPTY, IF NOT DISPLAY                    
*                                                                               
BLU096   STH   R4,TOTUNITS                                                      
         AH    R4,TRDLUNTS                                                      
*                                                                               
         SR    R4,R4               COUNT # OF ENTRIES                           
         LA    R5,UNITABLE                                                      
*                                                                               
BLU096C  OC    0(L'UNTENT,R5),0(R5)                                             
         BZ    BLU096F             END OF TABLE                                 
         LA    R4,1(R4)                                                         
         LA    R5,UNTNEXT                                                       
         C     R5,MAXUNT           TABLE MAX                                    
         BNL   BLU096F                                                          
         B     BLU096C                                                          
*                                                                               
BLU096F  LTR   R4,R4                                                            
         BNZ   BLU098                                                           
         OC    DLUNTS,DLUNTS                                                    
         BNZ   BLUX                                                             
         TM    UNITSW,X'04'        ONLY LOCKED UNITS                            
         BO    LOCKERR                                                          
         B     NALLOCER                                                         
*                                                                               
* SORT ON AIRDATE, SQH, EST, SUB, DP, DISK ADDR, FEED *                         
*                                                                               
BLU098   GOTO1 XSORT,DMCB,UNITABLE,(R4),L'UNTENT,L'UNTSORT,0                    
*                                                                               
BLUX     XIT1                                                                   
*                                                                               
TSUPPERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TSUPPMS1),TSUPPMS1                                     
         GOTO1 ERREX2                                                           
         DROP  R5                                                               
         EJECT                                                                  
UNTSZERB MVC   GERROR,=Y(TOMNYUNT)                                              
         LA    R2,TRAPRGH                                                       
         B     ERREXIT1                                                         
UNTFEDER MVC   GERROR,=Y(FEDONFED)                                              
         USING UNTABLED,R5                                                      
         LR    R5,R1                                                            
         XC    ELEM,ELEM                                                        
         MVI   ELEM,9              L'GASUBST TEXT + 1                           
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,UNTADTEP),(5,ELEM+1)                              
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         DROP  R5                                                               
         B     ERREXIT1                                                         
NALLOCER TM    UNITSW,X'80'                                                     
         BZ    NDATER                                                           
         MVC   GERROR,=Y(NALLOCUN)                                              
         B     ERREXIT1                                                         
NDATER   MVC   GERROR,=Y(NOUNTPER)                                              
         B     ERREXIT1                                                         
LOCKERR  MVC   GERROR,=Y(PGMISLCK)                                              
         LA    R2,TRAPRGH                                                       
ERREXIT1 GOTO1 VTRAERR                                                          
         EJECT                                                                  
* GET ANY CUTINS *                                                              
*                                                                               
GCUT     NTR1                                                                   
*                                                                               
* SEE IF ANY CUTIN COMML ELEM                                                   
*                                                                               
         XC    BLOCK(256),BLOCK    CLEAR AREA FOR CUTIN COMMLS                  
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'17'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GCUT20                                                           
*                                                                               
         LLC   RF,1(R6)                                                         
         BCTR  RF,0                LESS 1 FOR MOVE                              
         EX    RF,GCUTMVC                                                       
         B     *+10                                                             
GCUTMVC  MVC   BLOCK(0),0(R6)                                                   
*                                                                               
         LA    R2,BLOCK+2                                                       
         LLC   R4,BLOCK+1                                                       
         SRL   R4,3                DIVIDE BY 8,DROP 2 EXTRA (REMAINDER)         
*                                                                               
GCUT10   DS    0H                                                               
         MVC   WORK(8),0(R2)                                                    
         MVC   WORK+8(4),SVUNTDTS    UNTADTEP & UNTADTE2                        
*                                                                               
         USING UNTABLED,R1                                                      
*                                                                               
         LR    R1,R5                                                            
         AHI   R1,-L'UNTENT      USE LAST ENTRY                                 
         MVC   0(20,R5),0(R1)    NEED SOME INFO FOR FCML                        
         DROP  R1                                                               
*                                                                               
         XC    SVPROD,SVPROD                                                    
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         NI    REVALID,X'FF'-RESOCML                                            
*                                                                               
         BRAS  RE,FCML                                                          
         B     GCUT16                                                           
*                                                                               
*===========> CODE BELOW IS NOP  <======================                        
*                                                                               
         BRAS  RE,VCMLAPR          GO CHECK STARCOM DATA                        
*                                                                               
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BZ    GCUT16                                                           
*===========>                    <=======================                       
GCUT16   DS    0H                                                               
         LA    R2,8(R2)                                                         
         BCT   R4,GCUT10                                                        
*                                                                               
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
*                                                                               
GCUT20   DS    0H                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'03'                                                     
         MVC   SVELCODE,ELCODE                                                  
         BRAS  RE,GETEL                                                         
         BNE   GCUTX                                                            
*                                                                               
         USING UNTABLED,R5                                                      
*                                                                               
GCUT30   DS    0H                                                               
         USING NUSPRD,R6                                                        
         CLI   NUSPRTYP,C'U'       BYPASS ALL OTHERS                            
         BNE   GCUT40                                                           
*                                                                               
         CLI   NUSPRLEN,NUSPRLN1   BYPASS OLD ELEMS                             
         BE    GCUT40                                                           
*                                                                               
         OC    NUSPRCIS,NUSPRCIS   ANY CUTIN STATION?                           
         BZ    GCUT40               NO                                          
*                                                                               
         CLI   NUSPRLEN,NUSPRLN4   FIXED LEN 3 CHAR ELEM                        
         BNE   GCUT31               NO                                          
*                                                                               
         OC    NUSPRTPC,NUSPRTPC   ANY 3 CHAR PRODUCT?                          
         BZ    GCUT40               NO                                          
*                                                                               
         XC    0(L'UNTENT+2,R5),0(R5)                                           
         B     GCUT33                                                           
*                                                                               
* NON 3 CHAR X'03' ELEM                                                         
*                                                                               
GCUT31   CLI   NUSPRTPR,0          ANY PRODUCT?                                 
         BE    GCUT40               NO                                          
*                                                                               
         XC    0(L'UNTENT+2,R5),0(R5)                                           
*                                                                               
         LA    R2,NCLSTSIZ         FIND 3 CHAR PROD                             
         L     RE,ASVNCLST                                                      
*                                                                               
GCUT31F  CLC   NUSPRTPR,3(RE)                                                   
         BE    GCUT32                                                           
         LA    RE,4(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNH   *+8                                                              
         BCT   R2,GCUT31F                                                       
         DC    H'0'                WHAT, NO PROD?                               
*                                                                               
GCUT32   MVC   SVPROD,0(RE)        MOVE 3 CHAR PROD                             
         B     *+10                                                             
GCUT33   MVC   SVPROD,NUSPRTPC                                                  
*                                                                               
         LR    R1,R5               POINT                                        
         AHI   R1,-L'UNTENT              TO LAST ENTRY                          
*                                                                               
         MVC   UNTENT(UNTCOM1),0(R1)  COPY COMMON STUFF                         
         MVC   UNTDAY(L'UNTDAY+L'UNTTIME),UNTDAY-UNTENT(R1)                     
         MVC   UNTPROD,SVPROD       MOVE 3 CHAR PROD                            
         XC    SVPROD,SVPROD                                                    
         XC    UNTPROD2,UNTPROD2                                                
         LLC   RE,UNTSLN                                                        
         LLC   RF,UNTSLN2                                                       
         AR    RF,RE                                                            
         STC   RF,UNTSLN                                                        
         MVI   UNTSLN2,0                                                        
*                                                                               
         TM    NUSPRSTA,X'80'      PRODUCT CHANGED?                             
         BO    *+12                 YES                                         
*                                                                               
         TM    UNTFLAG1-UNTENT(R1),X'80' NEED REASSIGN                          
         BZ    *+8                                                              
         OI    UNTFLAG1,X'80'                                                   
*                                                                               
         TM    UNTFLAG1-UNTENT(R1),X'20' TRAFFIC DELETE                         
         BZ    *+8                                                              
         OI    UNTFLAG1,X'20'                                                   
*                                                                               
         GOTO1 SWITCH,DMCB,=C'SPT',0                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
*                                                                               
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SPOTCAN                                                 
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,NUSPRCIS   MARKET/STATION                               
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   UNTPOS,STAPQSTA     ALL CONVERTED FOR 8 BYTE STATION             
         MVC   UNTCML3+3(5),NUSPRCIS  MARKET/STATION                            
*                                                                               
         MVC   UNTREV,NUSPRREV                                                  
*                                                                               
         GOTO1 SWITCH,DMCB,=C'NET',0                                            
*                                                                               
         OI    UNTFLAG4,UNTFLCUT   SET AS CUTIN                                 
*                                                                               
         TM    NUSPRSTA,X'80'      PROD CHANGED?                                
         BZ    *+8                                                              
         OI    UNTFLAG1,X'80'      SET NEED REASSIGN                            
*                                                                               
         XC    UNTFEED,UNTFEED                                                  
*                                                                               
         CLI   NUSPRLEN,NUSPRLN4   FIXED LEN 3 CHAR ELEM                        
         BNE   GCUT33G              NO                                          
         OC    NUSPRFED,NUSPRFED       FEED                                     
         BZ    GCUT34               NO FEED INFO                                
         MVC   UNTFEED,NUSPRFED                                                 
         OC    UNTFEED,SPACES                                                   
         B     GCUT34                                                           
*                                                                               
GCUT33G  CLI   1(R6),NUSPRFED-NUSPREL                                           
         BE    GCUT34               NO FEED INFO                                
*                                                                               
         LLC   R1,1(R6)            GET ELEM LEN                                 
         AHI   R1,-((NUSPRFED-NUSPREL)+1)                                       
         EX    R1,GCUTFD                                                        
         OC    UNTFEED,SPACES                                                   
         B     GCUT34                                                           
*                                                                               
GCUTFD   MVC   UNTFEED(0),NUSPRFED                                              
*                                                                               
GCUT34   DS    0H                                                               
         CLI   NUSPRCMI,0          IS THERE A COMML?                            
         BE    GCUT36               NO                                          
*                                                                               
         LLC   RF,NUSPRCMI         GET POINTER TO COMML                         
         BCTR  RF,0                                                             
         SLL   RF,3                TIMES 8                                      
         LA    R2,BLOCK+2(RF)                                                   
         MVC   UNTCML1,0(R2)                                                    
         OC    UNTCML1,UNTCML1     THERE HAD BETTER BE SOMETHING                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
* VALIDATE CML ONE MORE TIME WITH CORRECT PRODUCT                               
*                                                                               
         MVC   SVPROD,UNTPROD                                                   
         MVC   WORK(8),UNTCML1                                                  
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
         NI    REVALID,X'FF'-RESOCML                                            
         BRAS  RE,FCML                                                          
         BE    *+14                                                             
         MVC   UNTCML1,=C'REASSIGN'                                             
         B     GCUT36                                                           
*                                                                               
         CLC   WORK(8),UNTCML1     DID FCML CHANGE CMML IN WORK                 
         BE    GCUT36                                                           
         OI    UNTCMLF,X'80'       IF YES, IT'S AN ADID - SET FLAG              
*                                                                               
GCUT36   DS    0H                                                               
         CLI   SVMYEAR,0           IS THIS SECTIONAL FEED                       
         BE    GCUT39                                                           
         MVC   UNTMYR,SVMYEAR      MAP YEAR                                     
*                                                                               
         LA    R1,MCODETBL         START OF MAP CODE TABLE                      
         LR    RF,R1                                                            
         LA    RE,L'MCODETBL(R1)   END OF TABLE                                 
*                                                                               
         OC    0(8,R1),0(R1)       EMPTY?                                       
         BZ    GCUT36C              YES                                         
         CLC   0(8,R1),SVMCODE     SAME ENTRY                                   
         BE    GCUT36D                                                          
         LA    R1,8(R1)                                                         
         CR    R1,RE                                                            
         BL    *-26                                                             
         DC    H'0'                MAKE MAP CODE TABLE BIGGER                   
*                                                                               
GCUT36C  MVC   0(8,R1),SVMCODE     SAVE MAP CODE IN TABLE                       
*                                                                               
GCUT36D  SR    R1,RF               GET DISP INTO MAP CODE TABLE                 
         LA    R1,1(R1)                                                         
         STC   R1,UNTMCDP          AND SAVE THE POINTER                         
*                                                                               
         OI    UNTFLAG4,UNTFLSEC   TURN ON SECTIONAL FLAG                       
*                                                                               
GCUT39   DS    0H                                                               
*                                                                               
         LA    R5,UNTNEXT                                                       
*                                                                               
         C     R5,MAXUNT                                                        
         BNL   UNTSZERG                                                         
*                                                                               
         MVC   ELCODE,SVELCODE                                                  
*                                                                               
GCUT40   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    GCUT30                                                           
*                                                                               
GCUTX    XIT1  REGS=(R5)                                                        
         DROP  R5                                                               
         EJECT                                                                  
* FILTER ON PRODUCT GROUP *                                                     
*                                                                               
* FIX HERE                                                                      
FPG      NTR1                                                                   
         OC    NBPR1CL3,NBPR1CL3   ANY PRODUCT                                  
         BZ    FPGNE                NO, BYPASS                                  
*                                                                               
         LA    R0,(L'OPTPGRL/3)                                                 
         L     R1,AOPTPGRL                                                      
*                                                                               
FPG10    CLC   NBPR1CL3,0(R1)                                                   
         BE    FPGEQ                                                            
         LA    RE,(L'SVCSPROD/3)                                                
         LA    RF,SVCSPROD                                                      
FPG20    CLI   0(RF),0                                                          
         BE    FPG30                                                            
         CLC   0(3,R1),0(RF)                                                    
         BE    FPGEQ                                                            
         LA    RF,3(,RF)                                                        
         BCT   RE,FPG20                                                         
*                                                                               
FPG30    LA    R1,3(,R1)                                                        
         CLI   0(R1),0                                                          
         BNH   *+8                                                              
         BCT   R0,FPG10                                                         
FPGNE    DS    0H                                                               
         CR    RD,RB                                                            
         B     FPGX                                                             
FPGEQ    CR    RB,RB                                                            
FPGX     DS   0H                                                                
         B     BLUX                                                             
*                                                                               
UNTSZERG MVC   GERROR,=Y(TOMNYUNT)                                              
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    NOFEDSVG             NO                                          
*                                                                               
* SAVE TWA *                                                                    
*                                                                               
         LA    R1,=C'DMWRT'                                                     
         BRAS  RE,COMTWA                                                        
NOFEDSVG GOTO1 VTRAERR                                                          
         EJECT                                                                  
         LTORG                                                                  
TSUPPMS1 DC    C'* ERROR * ALL UNITS MUST HAVE SAME TRAFFIC SUPPLIER*'          
         DROP  R7                                                               
         EJECT                                                                  
* CHECK ESTIMATES, BYPASS THOSE WITH COPY CODE N *                              
* ESTIMATE TABLE ENTRY = 5 BYTES,                *                              
*                        1 EST, 3 PROD, 1 COPY CD*                              
*                                                                               
CKEST    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,255                                                           
         L     R1,AESTBL                                                        
CKEST10  CLI   0(R1),0             EMPTY ENTRY                                  
         BE    CKEST30                                                          
         CLC   NBACTEST,0(R1)      SAME EST                                     
         BNE   CKEST14                                                          
         CLC   NBPR1CL3,1(R1)      SAME PRODUCT                                 
         BE    CKEST20                                                          
*                                                                               
         CLC   SVCSPROD(3),1(R1)   SAME PRODUCT                                 
         BE    CKEST20                                                          
*                                                                               
CKEST14  LA    R1,5(,R1)                                                        
         BCT   R0,CKEST10                                                       
         DC    H'0'                                                             
CKEST20  CLI   4(R1),C'N'          BYPASS EST                                   
         B     CKESTX                                                           
CKEST30  DS    0H                                                               
         BRAS  RE,INITSPT          SET FROM NET TO SPOT                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD                                                  
*                                                                               
         OC    NBPR1CL3,NBPR1CL3   IS THERE APROD                               
         BZ    CKEST36                                                          
*                                                                               
         LA    RF,NBPR1CL3                                                      
         B     CKEST38                                                          
*                                                                               
CKEST36  LA    RF,=C'POL'                                                       
*                                                                               
CKEST38  MVC   KEY+4(3),0(RF)                                                   
         MVC   KEY+7(1),NBACTEST                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CKEST40                                                          
*                                                                               
         OC    NBPR1CL3,NBPR1CL3   IS THERE APROD                               
         BZ    CKEST50                                                          
         DC    H'0'                                                             
*                                                                               
CKEST40  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         USING ESTHDRD,R6                                                       
         MVC   0(1,R1),NBACTEST                                                 
         MVC   1(3,R1),NBPR1CL3                                                 
         MVC   4(1,R1),ECOPY                                                    
         B     CKEST20                                                          
*                                                                               
CKEST50  MVC   0(1,R1),NBACTEST    IF NO POL EST, SHOW                          
         XC    1(4,R1),1(R1)       CLEAR PROD & CODE                            
         B     CKEST20                                                          
*                                                                               
CKESTX   XIT1                                                                   
*                                                                               
DELPRDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DELPRDM1),DELPRDM1                                     
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(5,CONHEAD+13)                          
         GOTO1 ERREX2                                                           
DELPRDM1 DC    C'* UNIT AS OF XXX99/99 HAD DELETED PRODUCT *'                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE PRODUCT                                                              
*                                                                               
VPRD     NTR1  BASE=*,LABEL=*                                                   
         USING UNTABLED,R5                                                      
*                                                                               
         BRAS  RE,INITSPT                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'29'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLMPREL,R6                                                      
         OI    PRDMATSW,X'01'      COVERS ALL PRDS                              
         CLI   CMLMPRS,X'FF'       IS THIS COMML PRD=ALL                        
         BE    VPRDX               YES, COVERS ALL PRODUCTS                     
*                                                                               
         NI    PRDMATSW,X'FF'-X'01'                                             
         TM    UNTFLAG2,UNTFL2CS   IS IT COPY SPLIT                             
         BO    VPRD34                                                           
         TM    UNTFLAG3,UNTFL3TB   IS IT TRI-BACK                               
         BO    VPRD34                                                           
*                                                                               
         LLC   R0,CMLMPRLN                                                      
         BCTR  R0,R0                                                            
         BCTR  R0,R0                                                            
         LA    R1,CMLMPRS          START OF PROD LIST                           
VPRD30   CLC   0(3,R2),0(R1)       UNTPROD TO PROD                              
         BE    VPRDX               YES                                          
         LA    R1,3(,R1)                                                        
*        SHI   R0,7                WHY -7 ????                                  
         SHI   R0,2                                                             
         BCT   R0,VPRD30                                                        
*                                                                               
         TM    VIGNFLG,X'04'       IS IT VIGNETTE                               
         BNZ   VPRDX                                                            
         OI    PRDMATSW,X'20'      PRDS AND CML DO NOT MATCH                    
         B     VPRDX                                                            
*                                                                               
VPRD34   OC    UNTCSPRO,UNTCSPRO                                                
         BNZ   *+6                                                              
         DC    H'0'                NO C/S PRD FOUND                             
*                                                                               
         LA    R2,UNTCSPRO                                                      
*        LA    RF,UNTCSPRO+15                                                   
         LA    RF,UNTCSPRO+(L'UNTCSPRO-3)                                       
VPRD34C  DS    0H                                                               
         OC    0(3,RF),0(RF)                                                    
         BNZ   VPRD34E                                                          
         SHI   RF,3                                                             
         CR    R2,RF               AT THE START OF LIST                         
         BNH   VPRD34C                                                          
VPRD34E  DS    0H                                                               
         SR    RF,R2                                                            
         LA    RF,1(RF)            GET NUMBER OF C/S PRDS                       
VPRD35   LLC   R0,CMLMPRLN                                                      
         BCTR  R0,R0                                                            
         BCTR  R0,R0                                                            
*                                                                               
         LA    R1,CMLMPRS          START OF PROD LIST                           
*                                                                               
VPRD36   CLC   0(3,R2),0(R1)                                                    
         BE    VPRDX               YES                                          
         LA    R1,3(,R1)           BUMP IN CML PRDS                             
         SHI   R0,2                                                             
         BCT   R0,VPRD36                                                        
*                                                                               
         TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BO    VPRD44                                                           
*                                                                               
         LTR   RF,RF                                                            
         BZ    VPRD44                                                           
         B     VPRD42                                                           
*                                                                               
VPRD42   LA    R2,3(R2)                                                         
         BCT   RF,VPRD35                                                        
         B     VPRDX                                                            
*                                                                               
VPRD44   TM    VIGNFLG,X'04'       IS IT VIGNETTE                               
         BO    VPRDX                                                            
         OI    PRDMATSW,X'20'      BAD PRD CHANGED SINCE LAST ASSIGN            
VPRDX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE PROGRAM                                                              
*                                                                               
VPROG    NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         OC    NETWORK,NETWORK                                                  
         BZ    MISSNET                                                          
*                                                                               
         GOTO1 ANY                                                              
         MVC   PROGRAM,WORK                                                     
         MVI   CUREQVCT,0                                                       
*                                                                               
         BAS   RE,GTEQV            GET ANY EQUIVALENT PROGRAMS                  
*                                                                               
         MVC   ELEM(12),STDATE                                                  
         MVC   ELEM+12(6),PROGRAM                                               
*                                                                               
         MVC   BYTE,ELEM                                                        
         MVI   ELEM,0              VLD PRG REGARDLESS OF DATE FIRST             
*                                                                               
VPROG20  DS   0H                                                                
         L     R3,AIO2                                                          
         BRAS  RE,NETI                                                          
*                                                                               
VPROG30  DS    0H                                                               
         TM    SECFLAG,BLSSW       BRAND LEVEL SECURITY CLIENT                  
         BZ    VPROG35                                                          
*                                                                               
         CLI   T216FFD+6,C'$'      USER SIGN ON OFFICE LIST                     
         BNE   *+12                                                             
         OI    NBINDS5,NBI5PLST    SET OFFICE LIST FLAG                         
         B     *+12                                                             
         CLI   T216FFD+6,C'*'      USER SIGN ON BY OFFICE                       
         BNE   VPROG35              NO                                          
*                                                                               
         MVC   NBTRAPSC,T216FFD+7  BRAND LEVEL OFFICE(LIST)                     
*                                                                               
VPROG35  GOTO1 ANETIO,DMCB,(R3)                                                 
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    VPROG40                                                          
         DC    H'0'                                                             
VPROG40  CLI   NBMODE,NBREQLST                                                  
         BE    VPROG50                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   VPROG30                                                          
         CLC   NBACTPRG,NBSELPRG                                                
         BNE   VPROG50                                                          
*                                                                               
         CLI   ELEM,0                                                           
         BNE   *+14                                                             
         MVC   ELEM(1),BYTE                                                     
         B     VPROG20                                                          
*                                                                               
* HAVE FOUND 1 OK UNIT, SO MUST BE OK                                           
*                                                                               
VPROGX   OI    TRAPRGH+4,X'20'     VALIDATED                                    
         XIT1                                                                   
*                                                                               
* HAVE NOT FOUND GOOD UNIT ON REQUESTED CODE, SEE IF EQUIVALENTS *              
*                                                                               
VPROG50  DS    0H                  NOW LOOK FOR EQUIVALENT PROGRAMS             
         LA    RE,EQVPTBL                                                       
         LA    RF,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         USING EQVPTBLD,RE                                                      
VPROG54  OC    EQVENT,EQVENT       EMPTY ENTRY                                  
         BZ    PROGERR                                                          
         CLI   EQVUSED,C'Y'        ENTRY USED ALREADY                           
         BNE   VPROG60                                                          
         LA    RE,EQVNEXT                                                       
         BCT   RF,VPROG54                                                       
*                                                                               
         B     PROGERR                                                          
*                                                                               
VPROG60  MVI   EQVUSED,C'Y'                                                     
         MVC   ELEM(12),EQVSDT                                                  
         MVC   ELEM+12(6),EQVEPRG                                               
         MVC   CUREQVCT,EQVCT                                                   
*                                                                               
         B     VPROG20                                                          
         DROP  RE                                                               
         EJECT                                                                  
* GET EQUIVALENT PROGRAM RECORDS FOR CLIENT *                                   
*                                                                               
GTEQV    NTR1                                                                   
*                                                                               
         XC    EQVPTBL,EQVPTBL                                                  
*                                                                               
         LA    R3,EQVPTBL                                                       
         LA    R5,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         LA    R7,1                INITIALIZE COUNTER                           
         USING EQVPTBLD,R3                                                      
*                                                                               
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGEKEY,R4                                                        
         MVI   PGEKID,X'24'                                                     
         MVC   PGEKAM(3),BAGYMD    & BCLT                                       
         MVC   PGEKNET,NETWORK                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
GTEQV10  CLC   KEY(8),KEYSAVE                                                   
         BNE   GTEQVX                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    GTEQV24                                                          
         DC    H'0'                                                             
         USING PGEDTAEL,R6                                                      
*                                                                               
GTEQV20  BRAS  RE,NEXTEL                                                        
         BNE   GTEQV30                                                          
*                                                                               
GTEQV24  CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BE    GTEQV26                                                          
*                                                                               
         CLC   PROGRAM,PGEKPRG     THIS AN EQV PROG                             
         BNE   GTEQV26                                                          
*                                                                               
         CLC   PGESTR,ENDATEB      SEE IF WITHIN DATES                          
         BH    GTEQV26                                                          
         CLC   PGEEND,STDATEB                                                   
         BNL   EQVPRGER                                                         
*                                                                               
GTEQV26  CLC   PGEPROG,PROGRAM     THIS REQUESTED PROGRAM                       
         BNE   GTEQV20                                                          
*                                                                               
         CLC   PGESTR,ENDATEB      SEE IF WITHIN DATES                          
         BH    GTEQV20                                                          
         CLC   PGEEND,STDATEB                                                   
         BL    GTEQV20                                                          
*                                                                               
         CLC   PGESTR,STDATEB                                                   
         BNL   *+10                                                             
         MVC   PGESTR,STDATEB      FORCE DATES TO BE WITHIN PERIOD              
*                                                                               
         CLC   PGEEND,ENDATEB                                                   
         BNH   *+10                                                             
         MVC   PGEEND,ENDATEB                                                   
         GOTO1 DATCON,DMCB,(3,PGESTR),(0,EQVSDT)                                
         GOTO1 (RF),(R1),(3,PGEEND),(0,EQVEDT)                                  
         MVC   EQVEPRG,PGEKPRG                                                  
         STC   R7,EQVCT                                                         
         LA    R7,1(,R7)           ADD 1 TO ENTRY COUNT                         
         LA    R3,EQVNEXT                                                       
         BCT   R5,GTEQV20                                                       
         DC    H'0'                                                             
*                                                                               
GTEQV30  GOTO1 SEQ                                                              
         B     GTEQV10                                                          
*                                                                               
GTEQVX   DS    0H                                                               
         CLI   KEYSAVE+2,0         DID WE LOOK ACROSS ALL CLIENTS               
         BE    VPROGX               YES WE ARE DONE                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),KEYSAVE       ID/A/M                                      
         MVC   KEY+4(4),NETWORK     NETWORK                                     
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         B     GTEQV10                                                          
*                                                                               
         DROP  R3,R4,R6                                                         
*                                                                               
MISSNET  LA    R2,TRANETH                                                       
         MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
*                                                                               
PROGERR  DS    0H                                                               
         CLI   ELEM,0                                                           
         BE    *+18                                                             
         MVC   GERROR,=Y(NOUNTPER) NO UNIT FOR THIS PERIOD                      
         LA    R2,TRAPERH                                                       
         B     VPROGERX                                                         
*                                                                               
         MVC   GERROR,=Y(NOPROG)   NO PROGRAM FOUND                             
         LA    R2,TRAPRGH                                                       
         B     VPROGERX                                                         
*                                                                               
EQVPRGER MVC   GERROR,=Y(NOEQUIV)                                               
VPROGERX GOTO1 VTRAERR                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY UNITS AND CMLS IF ANY ASSIGNED *                                      
*                                                                               
DISUNT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    TRADYTM,TRADYTM                                                  
         XC    SVDYTLST,SVDYTLST                                                
         XC    SVDESC,SVDESC                                                    
         XC    SVFEED,SVFEED                                                    
         MVI   SVMYEAR,0                                                        
         MVI   SVMCODEP,0                                                       
*                                                                               
         LA    R2,TRADYTMH                                                      
         MVC   8(8,R2),=C'DAY/TIME'                                             
         XC    9(2,R2),SPACES                                                   
         XC    12(4,R2),SPACES                                                  
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 UNDAY,DMCB,SVPDAY,WORK                                           
         GOTO1 UNTIME,DMCB,SVPTIME,WORK+10                                      
         GOTO1 SQUASHER,DMCB,WORK,21                                            
         MVC   17(18,R2),WORK                                                   
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         XC    TRADYT2,TRADYT2     CLEAR DAY/TIME LIST FIELD                    
         OI    TRADYT2H+6,X'80'                                                 
         LA    R0,TRADYT2+9                                                     
         ST    R0,NEXTDYT          INITIALIZE AREA                              
         MVI   NXTDYTCT,0          CLEAR COUNTER                                
         MVC   TRADYT2(7),=C'EXCPTNS'                                           
         XC    TRADYT2+1(6),SPACES                                              
*                                                                               
         XC    TRATSUP,TRATSUP     TRAFFIC SUPPLIER FIELD                       
         OI    TRATSUPH+6,X'80'    TRANSMIT                                     
         OC    SVTS,SVTS           ANY TRAFFIC SUPP TO DISP                     
         BZ    *+20                 NO                                          
         LA    R2,TRATSUPH                                                      
         MVC   8(3,R2),=C'TS='                                                  
         MVC   11(5,R2),SVTS                                                    
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
* NUMBER OF LINES ON THE SCREEN *                                               
*                                                                               
         LA    R3,TRALINES                                                      
         LA    R5,UNITABLE                                                      
         CLI   0(R5),0            ANY UNITS AT ALL                              
         BNE   DU06                                                             
         OC    UNASGND,UNASGND    SHOULD BE ZERO                                
         BNZ   DU04                                                             
         OC    TOTUNITS,TOTUNITS  SHOULD BE ZERO                                
         BNZ   DU04                                                             
         OC    DLUNTS,DLUNTS      MUST BE SOME                                  
         BNZ   DUX10                                                            
DU04     DC    H'0'                                                             
*                                                                               
DU06     A     R5,NXTUNIT                                                       
         USING UNTABLED,R5                                                      
         LA    R2,TRAINF1H                                                      
*                                                                               
DU10     ST    R2,ATRAINFH         SAVE FIRST FIELD ADDRESS                     
         TM    UNTFLAG1,X'60'      DELETED FEED OR UNIT                         
         BNZ   *+12                                                             
         BRAS  RE,TFTR             FILTER UNIT TABLE                            
         BE    DU12                                                             
         LA    R5,UNTNEXT                                                       
         CLI   0(R5),0                                                          
         BNE   DU10                                                             
         B     DUX10                                                            
*                                                                               
DU12     TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BO    DU12F                                                            
*                                                                               
DU12D    OC    UNTPROD2,UNTPROD2   PARTNER                                      
         BZ    DU14                 NO                                          
*                                                                               
DU12F    CHI   R3,2                AT LEAST 2 LINES LEFT                        
         BL    DUX10               NO, WAIT FOR NEXT SCREEN                     
*                                                                               
         FIXDT02                                                                
DU14     GOTO1 DATCON,DMCB,(2,UNTADTEP),(4,8(R2))                               
*                                                                               
         CLI   SVLINE,0            SPECIFIC LINE # REQUESTED                    
         BNE   DU15                 YES                                         
         TM    UNITSW2,LINENUM     DISPLAY LINE#?                               
         BZ    DU15                                                             
*                                                                               
         CLI   UNTSUB,C'A'         IS THIS UNIT FROM SKED                       
         BNL   DU15C                                                            
*                                                                               
         MVI   13(R2),C'-'                                                      
         EDIT  (B1,UNTSUB),(3,14(R2)),ALIGN=LEFT                                
         MVI   17(R2),C' '                                                      
         OI    6(R2),X'80'                                                      
         B     DU18C                                                            
*                                                                               
DU15     CLI   UNTSUB,C'A'         IS THIS UNIT FROM SKED                       
         BL    DU16                                                             
*                                                                               
DU15C    MVC   13(1,R2),UNTSUB                                                  
*                                                                               
DU16     DS    0H                                                               
         TM    UNTFLAG2,UNTFL2AD   IS THIS AN ADU UNIT                          
         BZ    *+8                  NO                                          
         MVI   14(R2),C'D'                                                      
*                                                                               
         TM    UNTFLAG3,UNTFLPFB                                                
         BZ    *+8                                                              
         MVI   14(R2),C'B'                                                      
*                                                                               
         TM    UNTFLAG3,UNTFLMG    IS THIS A MAKE GOOD UNIT                     
         BZ    *+8                                                              
         MVI   14(R2),C'G'                                                      
*                                                                               
         TM    UNTFLAG1,X'03'      FEEDS IN NAT UNIT                            
         BZ    DU17                                                             
         MVI   15(R2),C'N'                                                      
         TM    UNTFLAG1,X'02'                                                   
         BO    DU18                                                             
         MVI   15(R2),C'F'                                                      
         TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BZ    DU18                                                             
         MVI   15(R2),C'C'                                                      
         B     DU18                                                             
DU17     DS    0H                                                               
         TM    UNTFLAG1,UNTFL1FD   SEE IF UNIT IS NOT A FEED                    
         BO    DU18                                                             
         TM    UNTFLAG2,UNTFL2CS   BUT IS A COPY SPLIT                          
         BZ    DU18                                                             
         MVI   15(R2),C'N'         THEN MUST BE NATIONAL                        
*                                                                               
DU18     DS   0H                                                                
         OI    6(R2),X'80'                                                      
*                                                                               
         TM    UNTFLAG4,UNTFLCUT   CUTIN?                                       
         BZ    *+10                                                             
         MVC   16(4,R2),UNTPOS     MOVE IN CALL LETTERS                         
*                                                                               
         OC    UNTFEED,UNTFEED                                                  
         BZ    DU18C                                                            
*                                                                               
         TM    UNTFLAG4,UNTFLCUT   CUTIN?                                       
         BO    DU18C                                                            
*                                                                               
         CLC   =X'00FFE3',UNTFEED  CHK FOR TAG                                  
         BNE   *+14                                                             
         MVC   15(2,R2),UNTFEED+2                                               
         B     *+10                                                             
         MVC   16(4,R2),UNTFEED                                                 
*                                                                               
         TM    UNTFLAG4,UNTFLFDD   FEED NO NATIONAL                             
         BZ    *+8                                                              
         MVI   15(R2),C'*'          YES                                         
*                                                                               
DU18C    DS    0H                                                               
         TM    UNTFLAG4,UNTFLCUT   CUTIN?                                       
         BZ    *+8                                                              
         MVI   15(R2),C'T'          YES                                         
*                                                                               
         LA    RF,(L'UNTCSPRO/3)                                                
         LA    RE,UNTCSPRO                                                      
         MVC   21(3,R2),UNTPROD                                                 
*        TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
*        BZ    *+10                                                             
******   MVC   21(3,R2),UNTPROD2   YES, SHOW PARTNER PROD                       
         LA    R6,23(,R2)                                                       
         CLI   0(R6),C' '                                                       
         BNH   DU26                                                             
         LA    R6,1(,R6)                                                        
*                                                                               
DU26     MVI   0(R6),C'-'                                                       
         EDIT  (B1,UNTSLN),(3,1(R6)),ALIGN=LEFT                                 
*        TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
*        BZ    DU26B                                                            
*******  EDIT  (B1,UNTSLN2),(3,1(R6)),ALIGN=LEFT                                
*                                                                               
DU26B    OC    UNTPROD2,UNTPROD2   PARTNER                                      
         BZ    DU28                                                             
DU27     DS   0H                                                                
         LA    R1,3(R6)                                                         
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  R1,0                                                             
         B     *-10                                                             
         MVI   1(R1),C'/'                                                       
DU28     LLC   R6,0(R2)                                                         
         AR    R6,R2                                                            
         MVC   8(8,R6),UNTCML1                                                  
*        TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
*        BZ    *+10                                                             
******   MVC   8(8,R6),UNTCML2                                                  
         OI    6(R6),X'80'         TRANSMIT                                     
         OI    4(R6),X'20'         VALIDATED                                    
*                                                                               
         CLC   UNTCML1,=CL8'REASSIGN'                                           
         BE    DU30                                                             
         TM    UNTCMLF,NUCMADF1                                                 
         BZ    DU28A                                                            
         GOTO1 VTRPACK,DMCB,(C'U',UNTCML1),8(R6)                                
*        TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
*        BZ    DU28A                                                            
******** GOTO1 VTRPACK,DMCB,(C'U',UNTCML2),8(R6)                                
*                                                                               
DU28A    TM    UNTFLAG1,X'08'      ASSIGNED THIS SESSION                        
         BZ    *+8                                                              
         MVI   20(R6),C'*'                                                      
*                                                                               
         OC    UNTREF,UNTREF       REF NONZERO (SOURCE IS PATTERN)              
         BZ    DU30                                                             
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    DU30                                                             
         MVI   20(R6),C'P'                                                      
                                                                                
* SCATTER/UPFRONT MOVED FOR ADIDS TO MAKE 'OTHER' FIELD BIGGER                  
                                                                                
DU30     DS    0H                                                               
         L     RE,ATRAINFH         GET ADDRESS OF INF FIELD                     
         TM    UNTFLAG3,UNTFLOPR   OPPORTUNISTIC                                
         BZ    *+12                                                             
         MVI   8+20(RE),C'O'                                                    
         B     DU30C                                                            
         TM    UNTFLAG3,UNTFLSCA   SCATTER?                                     
         BZ    *+12                                                             
         MVI   8+20(RE),C'S'                                                    
         B     DU30C                                                            
         TM    UNTFLAG3,UNTFLUPF   UPFRONT?                                     
         BZ    *+8                                                              
         MVI   8+20(RE),C'U'                                                    
*                                                                               
DU30C    LLC   R0,0(R6)            OTHER FIELD                                  
         AR    R6,R0                                                            
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
*                                                                               
         TM    UNITSW2,FEEDSW      SHOW FEED DESCRIPTION                        
         BZ    DU30H                                                            
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BZ    DU30H                                                            
*                                                                               
         CLC   SVMYEAR,UNTMYR                                                   
         BNE   DU30E                                                            
         CLC   SVFEED,UNTFEED                                                   
         BNE   DU30E                                                            
         CLC   SVMCODEP,UNTMCDP                                                 
         BE    DU30G                                                            
*                                                                               
DU30E    BRAS  RE,GDESC             YES, GET FEED DESCRIPTION                   
*                                                                               
DU30G    MVC   ELEM(L'SVDESC),SVDESC  MOVE IN DESCRIPTION                       
         B     DU76                                                             
*                                                                               
DU30H    TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BZ    DU40                                                             
         OC    UNTPROD,UNTPROD     PROD ASSIGNED                                
         BNZ   DU38                                                             
         MVC   0(5,R4),=C'PRD=?'                                                
*                                                                               
* SHOW PRODS AVAILABLE 'PRD=?AAA/BBB?' *                                        
*                                                                               
         LA    R4,5(,R4)                                                        
         LA    RF,(L'UNTCSPRO/3)   MAX COPY SPLITS                              
         LA    RE,UNTCSPRO                                                      
         B     DU32                                                             
*                                                                               
DU31     MVI   0(R4),C'/'                                                       
         LA    R4,1(,R4)                                                        
*                                                                               
DU32     DS   0H                                                                
         MVC   0(3,R4),0(RE)                                                    
         LA    RE,3(,RE)                                                        
         LA    R4,2(,R4)                                                        
         CLI   0(R4),C' '                                                       
         BNH   *+8                                                              
         LA    R4,1(,R4)                                                        
         CLI   0(RE),0                                                          
         BE    DU35                                                             
         BCT   RF,DU31                                                          
DU35     DS   0H                                                                
         MVI   0(R4),C'?'                                                       
         LA    R4,1(,R4)                                                        
         OI    6(R6),X'80'         FORCE TRANSMIT                               
*                                                                               
DU38     TM    UNTFLAG1,UNTFL1FD   IF THIS IS A FEED, ALL DONE                  
         BO    DU40                                                             
         TM    UNTFLAG1+L'UNTENT,X'01' NEXT UNIT MUST BE A FEED                 
         BO    DU40                                                             
         TM    UNTFLAG4,UNTFLFDD   FEED NO NATIONAL                             
         BO    DU40                                                             
*                                                                               
         TM    UNTFLAG4+L'UNTENT,UNTFLCUT CUT-IN?                               
         BZ    DU38G                        NO                                  
*                                                                               
* IS THERE A FEED FOR THIS UNIT                                                 
*                                                                               
         LR    R1,R5                                                            
DU38D    LA    R1,UNTNEXT-UNTENT(R1)      BUMP TO NEXT ENTRY                    
         CLI   0(R1),0             AT END OF TABLE                              
         BE    DU38G                                                            
*                                                                               
         TM    UNTFLAG1+L'UNTENT-UNTENT(R1),X'01' NEXT UNIT FEED ?              
         BO    DU40                                                             
         TM    UNTFLAG4-UNTENT(R1),UNTFLFDD   FEED NO NATIONAL                  
         BO    DU40                                                             
*                                                                               
         TM    UNTFLAG4+L'UNTENT-UNTENT(R1),UNTFLCUT CUT-IN?                    
         BO    DU38D                        YES, GET NEXT UNIT                  
*                                                                               
DU38G    LA    R1,ELEM                                                          
         CR    R4,R1                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         MVC   0(6,R4),=C'FEED=?'                                               
         OI    6(R6),X'80'         FORCE TRANSMIT                               
         LA    R4,6(,R4)                                                        
DU40     DS    0H                                                               
         CLC   UNTBBSN,=CL8'VIGNETTE'                                           
         BNE   DU42                                                             
         LA    R1,ELEM                                                          
         CR    R4,R1                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         MVC   0(8,R4),=CL8'VIGNETTE'                                           
         NI    UNTFLAG2,X'FF'-X'04'                                             
         LA    R4,8(,R4)                                                        
         CLI   UNTBBSL,X'FF'                                                    
         BE    DU42                                                             
         MVI   0(R4),C'='                                                       
         EDIT  (B1,UNTBBSL),(2,1(R4))                                           
         LA    R4,3(,R4)                                                        
*        B     DU76                                                             
*                                                                               
DU42     TM    UNTFLAG2,X'04'+X'10' BILLBOARD REQUIRED OR IP                    
         BNZ   *+14                                                             
         OC    UNTOTHR,UNTOTHR     OTHER FIELD                                  
         BZ    DU76                                                             
         TM    UNTFLAG3,UNTFL3TB   IS IT TRI-BACK                               
         BO    DU70                                                             
*                                                                               
         TM    UNTFLAG2,X'10'      THIS UNIT IP                                 
         BZ    DU43                                                             
         OC    UNTPROD2,UNTPROD2                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,ELEM                                                          
         CR    R4,R1                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         MVC   0(2,R4),=C'IP'                                                   
         LA    R4,2(R4)                                                         
*                                                                               
DU43     TM    UNTFLAG2,X'04'      BILLBOARD REQUIRED                           
         BO    *+12                                                             
         CLI   UNTBBSL,0                                                        
         BE    DU70                                                             
         CLC   =CL8'VIGNETTE',UNTBBSN                                           
         BE    DU70                                                             
*                                                                               
         LA    R1,ELEM                                                          
         CR    R4,R1                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         MVC   0(3,R4),=C'BB='                                                  
         CLI   UNTBBSL,0                                                        
         BNE   DU44                                                             
         MVC   3(8,R4),=CL8'REQUIRED'                                           
         LA    R4,11(R4)                                                        
         B     DU70                                                             
*                                                                               
DU44     EDIT  (B1,UNTBBSL),(3,3(R4)),ALIGN=LEFT                                
         LA    R4,6(R4)                                                         
*                                                                               
DU50     CLI   0(R4),C' '                                                       
         BH    DU52                                                             
         BCT   R4,DU50                                                          
*                                                                               
DU52     MVI   1(R4),C'/'                                                       
         MVC   2(8,R4),UNTBBSN                                                  
         TM    UNTCMLF,NUCMADF3                                                 
         BO    DU52A                                                            
*                                                                               
         LA    R4,9(R4)                                                         
         CLI   0(R4),C'-'                                                       
         BNE   DU52B                                                            
         MVI   0(R4),C' '                                                       
         BCT   R4,*-12                                                          
         B     DU52B                                                            
*                                                                               
DU52A    GOTO1 VTRPACK,DMCB,(C'U',UNTBBSN),2(R4)                                
         LA    R4,14(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
*                                                                               
DU52B    MVI   1(R4),C'/'                                                       
         LA    R4,2(R4)                                                         
*                                                                               
         OC    UNTBBCN,UNTBBCN     ANY COPY                                     
         BZ    DU60                                                             
         TM    UNTCMLF,NUCMADF4    TEST ADID                                    
         BO    DU54                                                             
         CLI   UNTBBCN+7,C'-'                                                   
         BE    DU53                                                             
*                                                                               
         MVC   0(8,R4),UNTBBCN                                                  
         LA    R4,8(R4)                                                         
         B     DU60                                                             
*                                                                               
DU53     MVC   0(8,R4),UNTBBCN                                                  
         LA    R4,7(R4)                                                         
         CLI   0(R4),C'-'                                                       
         BNE   *+12                                                             
         MVI   0(R4),C' '                                                       
         BCT   R4,*-12                                                          
         LA    R4,1(R4)                                                         
         B     DU60                                                             
*                                                                               
DU54     GOTO1 VTRPACK,DMCB,(C'U',UNTBBCN),0(R4)                                
         LA    R4,12(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,1(R4)                                                         
*                                                                               
DU60     OC    UNTBBPOS,UNTBBPOS                                                
         BZ    DU70                                                             
         MVI   0(R4),C'/'                                                       
         MVC   1(4,R4),UNTBBPOS                                                 
         LA    R4,5(R4)                                                         
*                                                                               
DU64     CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,DU64                                                          
         LA    R4,1(R4)                                                         
*                                                                               
DU70     OC    UNTPOS,UNTPOS                                                    
         BZ    DU75                                                             
*                                                                               
         TM    UNTFLAG4,UNTFLCUT   THIS A CUTIN                                 
         BO    DU75                 POS IS THE STATION                          
*                                                                               
         LA    R1,ELEM                                                          
         CR    R4,R1                                                            
         BE    DU74                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
DU74     DS    0H                                                               
         MVC   0(4,R4),=C'POS='                                                 
         MVC   4(4,R4),UNTPOS                                                   
         B     DU76                                                             
*                                                                               
DU75     DS    0H                                                               
         OC    UNTFEED,UNTFEED                                                  
         BZ    DU76                                                             
*                                                                               
         TM    UNTFLAG4,UNTFLCUT   THIS A CUTIN                                 
         BZ    DU76                 FEED IS SHOWN UNDER FEED COL                
*                                                                               
         LA    R1,ELEM                                                          
         CR    R4,R1                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         MVC   0(5,R4),=C'FEED='                                                
         MVC   5(4,R4),UNTFEED                                                  
*                                                                               
DU76     DS    0H                                                               
         MVC   8(L'TRAOTH1,R6),ELEM                                             
         OI    6(R6),X'80'         TRANSMIT                                     
         OI    4(R6),X'20'         VALIDATED                                    
*                                                                               
         TM    UNITSW2,FEEDSW      SHOW FEED DESCRIPTION                        
         BZ    DU76C                                                            
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BZ    DU76C                                                            
*                                                                               
         OI    1(R6),X'20'         PROTECTED                                    
*                                                                               
DU76C    LLC   R0,0(R6)                                                         
         AR    R6,R0               POINT TO PROTECTED FIELD AFTER OTHER         
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         TM    UNITSW2,DCOST       WAS COST REQUESTED                           
         BZ    DU76D                NO                                          
*                                                                               
         BAS   RE,ACOST                                                         
         CLC   ELEM+1(10),SPACES                                                
         BNH   DU76D                                                            
         MVC   8(L'TRADTM1,R6),ELEM   DISPLAY WHAT I CAN!                       
         OI    6(R6),X'80'                                                      
         B     DU80                                                             
                                                                                
*=============================================================                  
* DISPLAY *N IF DAY/TIME DIFFERENT FROM PROGRAM DAY TIME                        
* AND PUT *N= IN DAY/TIME 2 LINE IN HEADLINES                                   
*=============================================================                  
                                                                                
DU76D    MVC   DUB(1),UNTDAY                                                    
         LA    R4,TIMETBL          START OF TIME TABLE                          
         LLC   R1,UNTTIME          AND DISPLACEMENT+1                           
         BCTR  R1,0                                                             
         AR    R4,R1                                                            
         MVC   DUB+1(4),0(R4)      MOVE UNIT TIME                               
*                                                                               
         CLC   SVPDAY(5),DUB       TEST SAME DAY/TIME                           
         BE    DU80                YES, SKIP                                    
                                                                                
* SEE IF EXCEPTION DAY TIME IS IN LIST ALREADY                                  
                                                                                
         LA    RE,SVDYTLST                                                      
         LA    RF,11                                                            
*                                                                               
DU76E    CLC   DUB(5),0(RE)                                                     
         BE    DU76F                                                            
         OC    0(5,RE),0(RE)       TEST EOL                                     
         BZ    DU77                                                             
         LA    RE,5(RE)                                                         
         BCT   RF,DU76E                                                         
         DC    H'0'                NO MORE ROOM                                 
*                                                                               
DU76F    LA    RE,12                                                            
         SR    RE,RF               GIVES COUNT TO MATCH                         
         MVI   8(R6),C'*'                                                       
         STC   RE,9(R6)                                                         
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         DC    H'0'  <<<<<<<<<<<==== DEBUG                                      
         OI    9(R6),X'F0'         MAKE EBCDIC CHAR                             
         OI    6(R6),X'80'         SET XMT BIT                                  
         B     DU80                                                             
*                                                                               
DU77     MVC   0(5,RE),DUB         SAVE THIS ENTRY!                             
*                                                                               
         MVC   ELEM,SPACES                                                      
         MVI   ELEM,C'*'           DISPLAY *N=                                  
         LLC   RE,NXTDYTCT                                                      
         LA    RE,1(RE)                                                         
         STC   RE,NXTDYTCT                                                      
         STC   RE,ELEM+1                                                        
         OI    ELEM+1,X'F0'                                                     
         MVI   ELEM+2,C'='                                                      
*                                                                               
         MVC   8(2,R6),ELEM        DISPLAY IN FIELD AFTER OTHER                 
         OI    6(R6),X'80'                                                      
*                                                                               
         LA    R4,ELEM+3                                                        
         GOTO1 UNDAY,DMCB,UNTDAY,(R4)                                           
*                                                                               
         CLC   0(2,R4),=C'??'                                                   
         BNE   DU77A                                                            
         MVC   2(5,R4),SPACES      JUST DISPLAY TWO ?'S                         
         B     DU77B                                                            
*                                                                               
DU77A    LLC   RE,UNTDAY           TEST JUST ONE DAY                            
         SR    RF,RF                                                            
         SRDL  RE,1                                                             
         LTR   RF,RF               SHIFT UNTIL RF NONZERO                       
         BZ    *-6                                                              
         LTR   RE,RE               TEST ANY MORE DAY BITS LEFT                  
         BNZ   DU77B                                                            
         MVI   2(R4),C' '          DISPLAY TWO CHARS ONLY!                      
                                                                                
* IF TIME SAME AS PROGRAM TIME, JUST DISPLAY DAY                                
                                                                                
DU77B    CLC   SVPTIME,DUB+1                                                    
         BE    DU78                                                             
*                                                                               
         LA    R4,ELEM+14                                                       
         CLI   0(R4),C' '          FIND END OF DAY STRING                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'/'                                                       
         LA    R4,2(R4)                                                         
*                                                                               
DU77F    GOTO1 UNTIME,DMCB,DUB+1,(R4)                                           
*                                                                               
         GOTO1 SQUASHER,DMCB,ELEM,30                                            
*                                                                               
DU78     LA    R4,ELEM+29          FIND END OF STRING                           
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R0,ELEM-1                                                        
         SR    R4,R0               GIVES LENGTH OF OUTPUT                       
*                                                                               
         LA    R0,TRADYT2+L'TRADYT2  GET END OF FIELD ADDR                      
         L     RE,NEXTDYT            NEXT OUTPUT ADDR                           
         AR    RE,R4                 PLUS THIS OUTPUT LENGTH                    
         CR    RE,R0                 WILL IT FIT                                
         BH    DU80                  NO                                         
*                                                                               
         L     RE,NEXTDYT                                                       
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ELEM                                                     
*                                                                               
         LA    RE,2(RE,R4)         SET NEXT OUTPUT POSN                         
         ST    RE,NEXTDYT                                                       
*                                                                               
* CHECK FOR PARTNER *                                                           
*                                                                               
DU80     MVC   10+TRAINF2H-TRAINF1H(3,R2),=C'P/B' PRESET NEXT LINE              
*                                                                               
         OC    UNTPROD2,UNTPROD2                                                
         BNZ   DU81                                                             
         LA    R1,TRAINF2H-TRAINF1H(R2)                                         
         XC    10(3,R1),10(R1)                                                  
         B     DU90                                                             
*                                                                               
         MVC   10+TRAINF2H-TRAINF1H(3,R2),=C'T/B'                               
*                                                                               
DU81     LA    R2,TRAINF2H-TRAINF1H(R2)                                         
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         BCTR  R3,0                SUBTR FROM REMAINING LINES ON SCREEN         
*                                                                               
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    RF,(L'UNTCSPRO/3)                                                
         LA    RE,UNTCSPRO+3                                                    
*                                                                               
DU82     DS   0H                                                                
         MVC   21(3,R2),UNTPROD2                                                
*        TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
*        BZ    *+10                                                             
*****    MVC   21(3,R2),UNTPROD                                                 
         LA    R6,23(,R2)                                                       
         CLI   0(R6),C' '                                                       
         BNH   DU86                                                             
         LA    R6,1(,R6)                                                        
DU86     MVI   0(R6),C'-'                                                       
         EDIT  (B1,UNTSLN2),(3,1(R6)),ALIGN=LEFT                                
*        TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
*        BZ    DU86A                                                            
******** EDIT  (B1,UNTSLN),(3,1(R6)),ALIGN=LEFT                                 
*                                                                               
DU86A    LLC   R6,0(R2)                                                         
         AR    R6,R2                                                            
         MVC   8(8,R6),UNTCML2                                                  
*        TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
*        BZ    *+10                                                             
******%  MVC   8(8,R6),UNTCML1                                                  
         OI    6(R6),X'80'         TRANSMIT                                     
         OI    4(R6),X'20'         VALIDATED                                    
*                                                                               
         CLC   UNTCML2,=CL8'REASSIGN'                                           
         BE    DU88                                                             
         TM    UNTCMLF,NUCMADF2                                                 
         BZ    DU86B                                                            
         GOTO1 VTRPACK,DMCB,(C'U',UNTCML2),8(R6)                                
*        TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
*        BZ    DU86B                                                            
*******  GOTO1 VTRPACK,DMCB,(C'U',UNTCML1),8(R6)                                
*                                                                               
DU86B    TM    UNTFLAG1,X'04'      ASSIGNED THIS SESSION                        
         BZ    *+8                                                              
         MVI   20(R6),C'*'                                                      
*                                                                               
         OC    UNTREF,UNTREF       REF NONZERO (SOURCE IS PATTERN)              
         BZ    DU88                                                             
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+8                 YES, UNTREF=MAP YEAR/MAP CODE                
         MVI   20(R6),C'P'                                                      
*                                                                               
DU88     LLC   R0,0(R6)                                                         
         AR    R6,R0                                                            
         OI    1(R6),X'20'         PROTECT OTHER FIELD                          
         OI    6(R6),X'80'         TRANSMIT                                     
*                                                                               
         BCTR  R3,0                SUBTR FROM REMAINING LINES ON SCREEN         
*                                                                               
DU90     LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             AT END OF TABLE                              
         BE    DUX10                                                            
*                                                                               
         BCTR  R3,0                SUBTR FROM REMAINING LINES ON SCREEN         
*                                                                               
         LA    R2,TRAINF2H-TRAINF1H(R2)                                         
         LA    R0,TRATAGH                                                       
         CR    R2,R0                                                            
         BL    DU10                                                             
         EJECT                                                                  
* DISPLAY TOTALS LINE AND CONHEAD MESSAGE *                                     
*                                                                               
DUX10    BRAS  RE,TOT              DISPLAY TOTALS LINE                          
*                                                                               
         MVC   CONHEAD,DUMOREMS                                                 
*                                                                               
         CLC   TOTUNITS,NLINS      MORE UNITS THAN 1 SCREEN                     
         BH    DUX20               YES                                          
         XC    CONHEAD+36(24),CONHEAD+36                                        
*                                                                               
DUX20    OI    TRATAGH+6,X'01'+X'80' SET MODIFIED NEXT INPUT/TRANSMIT           
         LA    R2,TRACML1H                                                      
         OC    UNASGND,UNASGND     ANY UNITS WITHOUT CMLS                       
         BNZ   DUX40                                                            
         LA    R2,TRAOPTH                                                       
*                                                                               
         OC    TOTUNITS,TOTUNITS                                                
         BNZ   DUX40                                                            
*                                                                               
DUX30    MVC   CONHEAD,DUSAVEMS                                                 
*                                                                               
DUX40    LA    R1,CONHEAD+1                                                     
         LA    R0,L'CONHEAD-1                                                   
*                                                                               
DUX42    CLI   0(R1),C'A'                                                       
         BL    DUX44                                                            
         CLI   0(R1),C'Z'                                                       
         BH    DUX44                                                            
         XI    0(R1),C' '                                                       
*                                                                               
DUX44    LA    R1,1(R1)                                                         
         BCT   R0,DUX42                                                         
         GOTO1 ERREX2                                                           
         DROP  R5                                                               
NLINS    DC  AL2((TRATAGH-TRAINF1H)/(TRAINF2H-TRAINF1H))                        
DUMOREMS DC    CL60'NOW ASSIGN COMMERCIALS, OR TYPE SAVE, HIT ENTER TO C        
               SEE MORE'                                                        
DUSAVEMS DC    CL60'ENTER ANY REVISION COMMENTS AND TYPE SAVE'                  
         EJECT                                                                  
* GET ACTUAL COST                                                               
*                                                                               
         USING UNTABLED,R5                                                      
ACOST    NTR1                                                                   
         ICM   R0,15,UNTACOST                                                   
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         C     R0,=F'99999'      TEST MORE THAN 5 DIGITS                        
         BH    ACOST2                                                           
         MVI   ELEM,C'$'                                                        
         EDIT  (R0),(5,ELEM+1),ALIGN=LEFT                                       
         J     EXIT                                                             
*                                                                               
ACOST2   C     R0,=F'999999'     TEST MORE THAN 6 DIGITS                        
         BH    ACOST4                                                           
         EDIT  (R0),(6,ELEM),ALIGN=LEFT                                         
         J     EXIT                                                             
*                                                                               
ACOST4   SRDA  R0,32                                                            
         D     R0,=F'1000'                                                      
         LR    R0,R1                                                            
         EDIT  (R0),(5,ELEM),ALIGN=LEFT                                         
         LA    R4,ELEM                                                          
         AR    R4,R0                                                            
         MVI   0(R4),C'K'                                                       
         J     EXIT                                                             
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* DISPLAY  TOTALS LINE                                                          
*===============================================================                
                                                                                
TOT      NTR1  BASE=*,LABEL=*                                                   
         XC    TRAINF,TRAINF                                                    
         MVC   TRAINF(6),=C'UNITS='                                             
         LA    R5,TRAINF+6                                                      
         EDIT  (B2,TOTUNITS),(3,(R5)),ALIGN=LEFT                                
         MVC   TRAINF+11(11),=C'UNASSIGNED='                                    
         LA    R5,TRAINF+22                                                     
TOT05    EDIT  (B2,UNASGND),(3,(R5)),ALIGN=LEFT,ZERO=NOBLANK                    
         MVC   TRAINF+27(8),=C'UNALLOC='                                        
         LA    R5,TRAINF+35                                                     
         EDIT  (B2,UNALUNTS),(3,(R5)),ALIGN=LEFT,ZERO=NOBLANK                   
*                                                                               
         MVC   TRAINF+40(4),=C'DEL='                                            
         LA    R5,TRAINF+44                                                     
         LH    R0,DLUNTS                                                        
         AH    R0,TRDLUNTS                                                      
         EDIT  (R0),(3,(R5)),ALIGN=LEFT,ZERO=NOBLANK                            
TOT10    CLI   REVISION,0                                                       
         BNE   TOT20                                                            
         TM    UNITSW,X'10'        THIS REVISING PRIOR INSTR                    
         BO    TOT20               YES                                          
         MVC   TRAINF+48(4),=C'ORIG'                                            
         B     TOT30                                                            
TOT20    MVC   TRAINF+48(4),=C'REV='                                            
         LA    R5,TRAINF+52                                                     
         EDIT  (B1,REVISION),(3,(R5)),ZERO=NOBLANK,ALIGN=LEFT                   
TOT30    MVC   TRAINF+57(8),=C'PERIOD='                                         
         GOTO1 DATCON,DMCB,(0,STDATE),(4,TRAINF+64)                             
         MVI   TRAINF+69,C'-'                                                   
         GOTO1 (RF),(R1),(0,ENDATE),(4,TRAINF+70)                               
         OI    TRAINFH+6,X'80'                                                  
*                                                                               
         XC    TRAPAS,TRAPAS                                                    
         OI    TRAPASH+6,X'80'                                                  
         CLI   SVTN1PR9,C'Y'                                                    
         BNE   TOTXIT                                                           
         MVC   TRAPAS(12),=C'PREV ASSIGN='                                      
         LA    R5,TRAPAS+12                                                     
         EDIT  (B2,PREVASS),(3,(R5)),ZERO=NOBLANK,ALIGN=LEFT                    
         OI    TRAPASH+6,X'80'                                                  
TOTXIT   J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* FILTER UNITS ON ENTRIES IN OPTION FIELD WHILE DISPLAYING UNIT TABLE *         
*                                                                               
TFTR     NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO2             NETBLOCK ADDRESSIBILITY                      
*                                                                               
         USING UNTABLED,R5                                                      
         OC    FILTERS,FILTERS                                                  
         JZ    EXIT                                                             
*                                                                               
         OC    FLTRPROD,FLTRPROD   ANY PROD FILTER                              
         BZ    TFTR10                                                           
*                                                                               
         CLC   FLTRPROD,UNTPROD     PRODUCT ENTERED                             
         BE    TFTR10                                                           
*                                                                               
TFTR05   OC    UNTPROD2,UNTPROD2                                                
         BZ    TFTRNE                                                           
         CLC   FLTRPROD,UNTPROD2                                                
         BNE   TFTRNE                                                           
*                                                                               
TFTR10   CLI   FLTRSLN,0           SPOT LENGTH ENTERED                          
         BE    TFTR15                                                           
         CLC   FLTRSLN,UNTSLN                                                   
         BE    TFTR15                                                           
         CLI   UNTSLN2,0                                                        
         BNE   TFTRNE                                                           
         CLC   FLTRSLN,UNTSLN2                                                  
         BNE   TFTRNE                                                           
*                                                                               
TFTR15   CLI   SVLINE,0            LINE NUMBER ENTERED                          
         BE    TFTR20                                                           
         CLC   SVLINE,UNTSUB                                                    
         BNE   TFTRNE                                                           
*                                                                               
TFTR20   OC    FLTRDATE,FLTRDATE   ANY DATE LIMITS                              
         BZ    TFTR30                                                           
         CLI   FLTRDATS,0                                                       
         BE    TFTR22                                                           
         CLI   FLTRDATS,X'4C'      LESS THAN                                    
         BE    TFTR24                                                           
         CLI   FLTRDATS,X'6E'      GREATER THAN                                 
         BE    TFTR26                                                           
         DC    H'0'                                                             
         EJECT                                                                  
TFTR22   OC    FLTRDAT2,FLTRDAT2                                                
         BNZ   TFTR28                                                           
         CLC   FLTRDATE,UNTADTEP                                                
         BE    TFTR30                                                           
         B     TFTRNE                                                           
TFTR24   CLC   FLTRDATE,UNTADTEP                                                
         BNH   TFTR30                                                           
         B     TFTRNE                                                           
TFTR26   CLC   FLTRDATE,UNTADTEP                                                
         BNL   TFTR30                                                           
         B     TFTRNE                                                           
TFTR28   CLC   FLTRDATE,UNTADTEP                                                
         BH    TFTRNE                                                           
         CLC   FLTRDAT2,UNTADTEP                                                
         BL    TFTRNE                                                           
*                                                                               
TFTR30   OC    FLTRCML,FLTRCML     ONLY SHOW SPECIFIC COMML                     
         BZ    TFTREQ                                                           
         CLC   FLTRCML,UNTCML1                                                  
         BE    TFTREQ                                                           
         OC    UNTCML2,UNTCML2                                                  
         BZ    TFTRNE                                                           
         CLC   FLTRCML,UNTCML2                                                  
         BNE   TFTRNE                                                           
TFTREQ   CR    R1,R1                                                            
         J     EXIT                                                             
TFTRNE   LTR   RB,RB                                                            
         J     EXIT                                                             
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
* GET FEED DESCRIPTION                                                          
*                                                                               
GDESC    NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         USING UNTABLED,R5                                                      
*                                                                               
         MVI   SVMYEAR,0                                                        
         MVI   SVMCODEP,0                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING FEEDKEY,RF                                                       
         MVC   FEEDKID,=X'0A2B'                                                 
         MVC   FEEDKAM,BAGYMD                                                   
         MVC   FEEDKNET,NETWORK                                                 
         MVC   FEEDKCLT,BCLT                                                    
         MVC   FEEDKFD,UNTFEED                                                  
*                                                                               
         MVC   SVFEED,UNTFEED                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GDESC15                                                          
         MVC   KEY,KEYSAVE                                                      
         LA    RF,KEY                                                           
         XC    FEEDKCLT,FEEDKCLT                                                
         DROP  RF                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FEEDNFER                                                         
*                                                                               
GDESC15  DS    0H                                                               
         MVC   SVMYEAR,UNTMYR      SAVE THIS MAP YEAR                           
         MVC   SVMCODEP,UNTMCDP    AND THIS MAP CODE POINTER                    
*                                                                               
         LA    R4,MCODETBL         START OF MAP CODE TABLE                      
         LLC   R1,UNTMCDP          AND DISPLACEMENT+1                           
         AR    R4,R1                                                            
         BCTR  R4,0                R4 IS PONTING TO MAP CODE                    
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'        SECTIONAL FEED DESCRIPTION ELEM              
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
GDESC20  BRAS  RE,NEXTEL                                                        
         BNE   FEEDNFER            FEED DESCRIPTION NOT FOUND                   
*                                                                               
         USING FEEDSMEL,R6                                                      
*                                                                               
         TM    FEEDSFLG,FEEDSDEL   DELETED FEED                                 
         BO    GDESC20              YES                                         
*                                                                               
         CLC   UNTMYR,FEEDSMYR     SAME MAP YEAR                                
         BNE   GDESC20                                                          
         CLC   FEEDSMCD,0(R4)      SAME MAP CODE                                
         BNE   GDESC20                                                          
         MVC   SVDESC,FEEDSMDS     SAVE FEED DESCRIPTION                        
*                                                                               
         XIT1                                                                   
*                                                                               
FEEDNFER MVC   GERROR,=Y(FEEDNF)                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM+1(4),UNTFEED   SHOW BAD FEED *****YR/CODE******             
         XC    DUB,DUB                                                          
         MVC   DUB(1),UNTMYR       MAP YEAR                                     
         MVC   DUB+1(2),=X'0101'                                                
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(X'20',WORK)                                 
*                                                                               
         MVC   ELEM+5(2),WORK                                                   
*                                                                               
         LA    R4,MCODETBL         START OF MAP CODE TABLE                      
         LLC   R1,UNTMCDP          AND DISPLACEMENT+1                           
         AR    R4,R1                                                            
         BCTR  R4,0                R4 IS PONTING TO MAP CODE                    
*                                                                               
         MVI   ELEM+7,C'/'                                                      
         MVC   ELEM+8(8),0(R4)     MAP CODE                                     
*                                                                               
         MVI   ELEM,17             L'SUBST TEXT + 1                             
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    NOFEDSV1             NO                                          
*                                                                               
* SAVE TWA *                                                                    
*                                                                               
         LA    R1,=C'DMWRT '                                                    
         BRAS  RE,COMTWA                                                        
*                                                                               
NOFEDSV1 GOTO1 VTRAERR                                                          
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
* UPDATE UNIT RECORDS AND UPDATE OR CREATE REVISION RECORDS *                   
*                                                                               
         USING NETBLOCKD,R3                                                     
SRV      NMOD1 0,**+SRV**,R7                                                    
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         L     R3,AIO2                                                          
         AHI   R3,2900             ALLOW ROOM FOR UNITS > 2K                    
*                                                                               
         BRAS  RE,NETI             INIT NETBLOCK                                
*                                                                               
         MVI   NBSEQ,0                                                          
         MVI   NBSELTRF,0                                                       
         MVI   NBSELPST,0                                                       
         MVI   NBSELUOP,0                                                       
         MVC   NBADDAY,ADDAY                                                    
         MVC   NBCALLOV,CALLOV                                                  
         MVC   NBCLPACK,CLPACK                                                  
         MVC   NBCLUNPK,CLUNPK                                                  
         MVC   NBDM,DATAMGR                                                     
         MVC   NBDATCON,DATCON                                                  
         MVC   NBGETDAY,GETDAY                                                  
         MVC   NBHEXOUT,HEXOUT                                                  
         MVC   NBHELLO,HELLO                                                    
*        MVC   NBNETVAL                                                         
         MVC   NBGTPROF,GETPROF                                                 
         MVC   NBACTAM,BAGYMD                                                   
*                                                                               
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    SRV01                                                            
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    SRV01                                                            
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
SRV01    DS    0H                                                               
         BRAS  RE,FCHG             FIND IF ANY CHANGES MADE                     
*                                                                               
         BRAS  RE,FNREVN           FIND NETWORK REV NUMBER                      
*                                                                               
         CLC   REVISION,HIREVNN    PROG REV # MAY NOT BE > NET REV #            
         BNH   *+10                                                             
         MVC   HIREVNN,REVISION    USE PROG REV #                               
*                                                                               
* TEST IF REVISION COMMENTS ARE NEEDED *                                        
*                                                                               
         CLI   REVISION,0          NEED REV COMMENTS                            
         BE    SRV02                NOT FOR ORIGINALS                           
         LA    R2,TRAREVH                                                       
         CLI   TRAREVH+5,0             ANY INPUT                                
         BNE   SRV02                                                            
         NI    TRAOPTH+4,X'DF'     FORCE FILTER EDIT                            
         MVC   GERROR,=Y(REVCMTRQ)                                              
         B     SRVEREX                                                          
*                                                                               
SRV02    DS    0H                                                               
         L     RE,AIO3             CLEAR ADID TABLE                             
         MVC   0(4,RE),=C'*ID*'                                                 
         LA    RE,4(,RE)                                                        
         LA    RF,2496                                                          
         XCEF                                                                   
*                                                                               
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
*                                                                               
* SEE IF ONLY DELETED UNITS *                                                   
*                                                                               
         CLI   0(R5),0            ANY UNITS AT ALL                              
         BNE   SRV10                                                            
         OC    UNASGND,UNASGND    SHOULD BE ZERO                                
         BNZ   SRV04                                                            
         OC    TOTUNITS,TOTUNITS  SHOULD BE ZERO                                
         BNZ   SRV04                                                            
         OC    DLUNTS,DLUNTS      MUST BE SOME                                  
         BNZ   SRV60                                                            
SRV04    DC    H'0'                                                             
*                                                                               
* SEE IF ANY FORCED REASSIGNS OR UNASSIGNED COPY SPLIT PRODUCTS *               
* STILL LEFT TO DO                                              *               
*                                                                               
SRV10    TM    UNTFLAG1,X'60'      DELETED FEED OR UNIT                         
         BO    SRV28                                                            
         CLC   UNTCML1,=CL8'REASSIGN'                                           
         BE    SRV12                                                            
         CLC   UNTCML2,=CL8'REASSIGN'                                           
         BNE   *+8                                                              
SRV12    OI    UNITSW1,X'04'                                                    
*                                                                               
         TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BZ    SRV28                NO                                          
         TM    UNTFLAG1,02         NATIONAL UNIT                                
         BZ    SRV28                NO                                          
         LR    R1,R5                                                            
*                                                                               
* MOVE ALL PRDS TO WORK AREA ELIMINATING DUPLICATE PRDS                         
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,UNTCSPRO                                                      
         LA    RF,WORK                                                          
         LA    R0,(L'UNTCSPRO/3)                                                
         LR    R4,R0                                                            
         CLI   0(RE),0                                                          
         BE    SRV13X              NO MORE PRDS                                 
SRV13C   CLI   0(RF),0             EMPTY                                        
         BE    SRV13H               YES GO ADD PRD                              
         CLC   0(3,RE),0(RF)       SAME PRD                                     
         BE    SRV13F               YES GET NEXT PRD                            
         LA    RF,3(RF)            BUMP TO NEXT PRD IN WORK                     
         BCT   R0,SRV13C                                                        
*                                                                               
SRV13F   LA    RF,WORK                                                          
         LA    R0,(L'UNTCSPRO/3)                                                
         LA    RE,3(RE)            BUMP TO NEXT PRD IN UNTCSPRO                 
         CLI   0(RE),0             ANY MORE PRDS                                
         BE    SRV13X               NO                                          
         BCT   R4,SRV13C                                                        
         B     *+14                                                             
SRV13H   MVC   0(3,RF),0(RE)        SAVE PRD IN WORK                            
         B     SRV13F                                                           
*                                                                               
SRV13X   MVC   SVCSPROD,WORK                                                    
*                                                                               
*                                   UNASSIGNED UNIT?                            
         OC    UNTPROD-UNTENT(,R1),UNTPROD-UNTENT(R1)                           
         BZ    UNALLERR                                                         
*                                                                               
         LA    R0,(L'UNTCSPRO/3)                                                
         LA    RE,WORK                                                          
         LA    RF,SVCSPROD                                                      
SRV14    CLC   UNTPROD-UNTENT(,R1),0(RF)                                        
         BE    SRV16                                                            
         LA    RE,3(,RE)                                                        
         LA    RF,3(,RF)                                                        
         BCT   R0,SRV14                                                         
         DC    H'0'                                                             
SRV16    XC    0(3,RE),0(RE)                                                    
         LA    R1,L'UNTENT(,R1)                                                 
*                                                                               
* NOW CHECK ANY FEEDS *                                                         
*                                                                               
SRV20    CLC   UNTDSKAD,UNTDSKAD-UNTENT(R1) SAME                                
         BNE   SRV26                                                            
*                                                                               
         TM    UNTFLAG4-UNTENT(R1),UNTFLCUT  IF CUTIN BYPASS                    
         BO    SRV28                          YES                               
         TM    UNTFLAG1-UNTENT(R1),UNTFL1FD  THIS STILL A FEED                  
         BO    *+6                            YES                               
         DC    H'0' UNASSIGNED PROD?                                            
         OC    UNTPROD-UNTENT(,R1),UNTPROD-UNTENT(R1)                           
         BZ    UNALLERR                                                         
         TM    UNTFLAG2-UNTENT(R1),UNTFL2CS MUST BE COPY SPLIT                  
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,(L'UNTCSPRO/3)                                                
         LA    RE,WORK                                                          
         LA    RF,SVCSPROD                                                      
SRV21    CLC   UNTPROD-UNTENT(,R1),0(RF)                                        
         BE    SRV22                                                            
         LA    RE,3(,RE)                                                        
         LA    RF,3(,RF)                                                        
         BCT   R0,SRV21                                                         
         DC    H'0'                                                             
SRV22    XC    0(3,RE),0(RE)                                                    
*                                                                               
SRV24    LA    R1,L'UNTENT(,R1)                                                 
         B     SRV20                                                            
SRV26    OC    WORK(L'UNTCSPRO),WORK    ALL PRODUCTS USED                       
         BNZ   ONEPRDER                                                         
SRV28    LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             END OF TABLE                                 
         BNE   SRV10                                                            
*                                                                               
* GO TO SEE IF UNITS ARE LOCKED OUT - CONTROLLER WILL GIVE ERR IF SO *          
*                                                                               
         XC    DUB,DUB                                                          
         MVI   BYTE,0                                                           
         MVC   DUB(4),=C'TNET'                                                  
         GOTO1 VALILOC,0                                                        
*                                                                               
* IF RUNNING FOR ALL PRODUCTS, SEE THAT NONE OF THE PRODS ARE LOCKED            
*                                                                               
         CLI   SVTN2PRO+00,C'0'                                                 
         BE    *+12                                                             
         CLI   SVTN2PRO+00,0                                                    
         BNE   TLOCK130            NOT AN ALL PRODUCT RUN                       
*                                                                               
* GET PRODUCTS FOR THIS CLIENT                                                  
*                                                                               
         BRAS  RE,INITSPT                                                       
*                                                                               
         XC    SVCPROD,SVCPROD     INIT PROD                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DF1'      PROD REC                                    
         MVC   KEY+2(3),BAGYMD    & BCLT                                        
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
TLOCK115 CLC   KEY(5),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   KEY+5,X'FF'         LAST REC FOR THIS CLT                        
         BNE   *+14                                                             
         XC    SVCPROD,SVCPROD     CLEAR PROD                                   
         B     TLOCK1X              YES, DONE                                   
*                                                                               
         MVC   SVCPROD,KEY+6       SAVE 3 CHAR PROD                             
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
*******  MVC   DUB+6(1),3(R4)      THIS PRD LOCKED?                             
*                                                                               
         GOTO1 VALILOC,SVCPROD                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVI   DUB+2,X'FF'         NETWORK=ALL                                  
         MVC   DUB+3(1),SVMEDIA    MEDIA                                        
*******  MVC   DUB+6(1),3(R4)      THIS PRD LOCKED?                             
*                                                                               
         GOTO1 VALILOC,SVCPROD                                                  
*                                                                               
         GOTO1 SEQ                 GET NEXT PROD REC                            
         B     TLOCK115                                                         
*                                                                               
*        BCT   R5,TLOCK110                                                      
*                                                                               
* IF RUNNING BY PRODUCT, SEE THAT THIS PRD IS NOT LOCKED                        
*                                                                               
TLOCK130 CLI   SVTN2PRO+00,C'*'                                                 
         BNE   TLOCK150                                                         
         MVC   BYTE,SVTN2PRO+00                                                 
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
*******  MVC   DUB+6(3),VOPTPROD   THIS PRD LOCKED?                             
*                                                                               
         MVC   SVCPROD,VOPTPROD                                                 
         GOTO1 VALILOC,SVCPROD                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVI   DUB+2,X'FF'         NETWORK=ALL                                  
         MVC   DUB+3(1),SVMEDIA    MEDIA                                        
*******  MVC   DUB+6(3),VOPTPROD   THIS PRD LOCKED?                             
*                                                                               
         GOTO1 VALILOC,SVCPROD                                                  
*                                                                               
         B     TLOCK1X                                                          
*                                                                               
* IF RUNNING BY PGROUP, SEE THAT THIS PGROUP IS NOT LOCKED                      
*                                                                               
TLOCK150 OC    VOPTPRGR,VOPTPRGR   PRODUCT GROUP                                
         BNZ   *+6                                                              
         DC    H'0'                BUG CATCHER                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVC   BYTE,SVTN2PRO+00                                                 
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVC   DUB+2(4),TRANET     NETWORK                                      
         MVC   DUB+6(2),VOPTPRGR   THIS PGROUP LOCKED?                          
         GOTO1 VALILOC,0                                                        
         XC    DUB,DUB                                                          
         MVI   DUB,C'T'            TEST                                         
         MVI   DUB+1,X'FF'         NET (TRAFFIC)                                
         MVI   DUB+2,X'FF'         NETWORK=ALL                                  
         MVC   DUB+3(1),SVMEDIA    MEDIA                                        
         MVC   DUB+6(2),VOPTPRGR   THIS PGROUP LOCKED?                          
         GOTO1 VALILOC,0                                                        
*                                                                               
TLOCK1X  DS    0H                                                               
*                                                                               
         LA    R5,UNITABLE                                                      
         NI    UNITSW2,X'FF'-NOTIFYSW  INIT SEND NOTIFY                         
         MVI   BYTE,0              COUNT DELETED UNITS                          
         EJECT                                                                  
* UPDATE ALL UNIT RECORDS WITH NEW ASSIGNED COMMERCIALS *                       
*                                                                               
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
*                                                                               
SRV30    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKEY,R4                                                         
         MVI   NUKTYPE,04                                                       
         MVC   NUKAM(3),BAGYMD AND BCLT                                         
         MVC   NUKDATE,UNTADTEP                                                 
         MVC   NUKTIME,UNTSQH       START 1/4 HR                                
         MVC   NUKNET(10),NETWORK AND PROGRAM                                   
*                                                                               
         CLI   UNTEQVCT,0          IS THIS AN EQUIVALENT PROGRAM                
         BE    SRV31                NO                                          
         LLC   RE,UNTEQVCT                                                      
         BCTR  RE,0                                                             
         MHI   RE,EQVNEXT-EQVENT                                                
         LA    RE,EQVPTBL(RE)                                                   
         MVC   NUKPROG,EQVEPRG-EQVENT(RE) GET REAL PROG CODE                    
*                                                                               
SRV31    DS    0H                                                               
         MVC   NUKEST(3),UNTEST EST, SUB, DAYPART                               
         MVC   NUDA,UNTDSKAD                                                    
         L     R6,AIO1             USE AIO1                                     
         ST    R6,AIO                                                           
         OI    DMINBTS,X'08'       READ DELETED RECS                            
         GOTO1 GETREC                                                           
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         MVC   NBKEY,KEY                                                        
         ST    R6,NBAIO                                                         
         MVI   NBFUNCT,NBFVAL                                                   
         GOTO1 ANETIO,DMCB,(R3)                                                 
*                                                                               
* SET UP FOR CUTINS IF ANY                                                      
*                                                                               
         XC    BLOCK(256),BLOCK                                                 
         MVI   BLOCK,X'17'                                                      
         MVI   BLOCK+1,2                                                        
*                                                                               
         CLC   KEY(20),0(R6)                                                    
         BNE   SRVBUG                                                           
*                                                                               
         TM    22(R6),X'80'        DELETED REC                                  
         BZ    SRV31B                                                           
*                                                                               
         LLC   R1,BYTE             YES, INCREMENT COUNT                         
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         B     SRV40               AND BYPASS                                   
*                                                                               
SRV31B   XC    SVCSPROD,SVCSPROD                                                
*                                                                               
* BLOCK IS USED TO BUILD AN ELEMENT FOR CUTIN COMMERCIALS                       
*                                                                               
         MVI   ELCODE,X'17' FOR CUTIN COMMLS                                    
         GOTO1 REMELEM                                                          
*                                                                               
         MVI   ELCODE,X'14'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SRV31F                                                           
*                                                                               
         USING NUPRDD,R6                                                        
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'6'                                                         
         CHI   R0,3                MUST BE A REMAINDER OF 3                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1                                                            
         CHI   R0,1                MUST BE MORE THAN 1 PRODUCT                  
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R1,NUPRDPR                                                       
         LA    RF,SVCSPROD                                                      
SRV31C   MVC   0(1,RF),0(R1)                                                    
         LA    R1,6(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,SRV31C                                                        
*                                                                               
SRV31F   L     R6,AIO1             USE AIO1                                     
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
SRVBUG   DC    H'0'                                                             
         MVC   NBMAINEL(80),0(R6)                                               
*                                                                               
* SEE IF THIS UNIT HAS 5 SECONDS TAG                                            
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'60'                                                     
         LA    R6,27(R6)                                                        
*                                                                               
SRVTG10 BRAS   RE,NEXTEL                                                        
         BNE   SRVTGX                                                           
*                                                                               
         CLI   2(R6),C'Q'          IS THIS A TAG ELEMENT                        
         BNE   SRVTG10                                                          
         CLI   3(R6),C'T'          T=TAG                                        
         BNE   SRVTG10                                                          
*                                                                               
         PACK  DUB,4(1,R6)                                                      
         CVB   R0,DUB              TAGS IN BINARY                               
         MHI   R0,5                TIMES 5 (5 SECONDS PER TAG)                  
         LLC   R1,NBLEN            TOTAL UNIT LEN                               
         SR    R1,R0               MINUS TAG LEN                                
         STC   R1,NBLEN            = ACTUAL UNIT LENGTH                         
*                                                                               
SRVTGX   L     R6,AIO1                                                          
         LA    R6,27(R6)                                                        
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   SRV32                                                            
         GOTO1 REMELEM                                                          
*                                                                               
* BUILD AS IF NEW ELEMENT, AND COMPARE AS IF OLD FOR REV STORE *                
*                                                                               
SRV32    LA    R6,ELEM                                                          
         USING NUCMLEL,R6                                                       
         MVI   NUCMLEL,X'21'                                                    
         MVI   NUCMLELN,NUCMLEND-NUCMLEL                                        
*                                                                               
* SEE IF COMMERCIAL FOR THIS UNIT WAS ASSIGNED OR DELETED THIS SESSION          
* IF NO CHANGE THEN DO NOT RE-BUILD ENTIRE 21 ELEMENT                           
* (THIS WAY WE WILL BE ABLE TO HANDLE CONCURRENT UPDATES)                       
*                                                                               
         TM    UNTFLAG4,UNTFLUPD   ANYTHING TO UPDATE?                          
         BNZ   SRV32B               YES                                         
*                                                                               
SRV32A   TM    UNTFLAG1,X'0C'      CML1/2 ASGND THIS SESSION?                   
         BNZ   SRV32B                                                           
         TM    UNTFLAG4,UNTFLDC1+UNTFLDC2 CML1/2 DELETED THIS SESSN?            
         BNZ   SRV32B                                                           
         CLC   NUCMLBSL(17),UNTOTHR BB CHANGED                                  
         BZ    SRV34C               NO, CHK NEWLY ASGND FLAG                    
*                                                                               
SRV32B   TM    UNTFLAG4,UNTFLFDD   FEED NO NATIONAL                             
         BZ    *+8                  NO                                          
         OI    NUCMLFL2,NUCMLFFD                                                
*                                                                               
         MVC   SVCMLFLG,NUCMLFLG                                                
*                                                                               
* IF ANY CHANGES IN DELETED STATUS FORCE NEW REVISION *                         
*                                                                               
*        TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
*        BZ    SRV32C                                                           
*                                                                               
*        XC    UNTSLN,UNTSLN2                                                   
*        XC    UNTSLN2,UNTSLN                                                   
*        XC    UNTSLN,UNTSLN2                                                   
*                                                                               
*        XC    UNTPROD(4),UNTPROD2 SWAP BACK THE PRDS                           
*        XC    UNTPROD2(4),UNTPROD                                              
*        XC    UNTPROD(4),UNTPROD2                                              
*        NI    UNTFLAG2,X'FF'-X'20'                                             
*                                                                               
*        XC    UNTCML1,UNTCML2     AND SWAP BACK THE CMLS                       
*        XC    UNTCML2,UNTCML1                                                  
*        XC    UNTCML1,UNTCML2                                                  
*        MVC   HALF(1),UNTCMLF     NEED TO SWAP FLAGS TOO!                      
*        NI    HALF,X'3F'                                                       
*        TM    UNTCMLF,X'80'                                                    
*        BZ    *+8                                                              
*        OI    HALF,X'40'                                                       
*        TM    UNTCMLF,X'40'                                                    
*        BZ    *+8                                                              
*        OI    HALF,X'80'                                                       
*******  MVC   UNTCMLF,HALF                                                     
*                                                                               
* IF ANY CHANGES, FORCE NEW REVISION *                                          
*                                                                               
SRV32C   DS    0H                                                               
         TM    NUCMLFLG,X'20'      DATE CHANGED                                 
         BZ    SRV32D                                                           
         CLC   NUCMLREV,REVISION   UNIT REV HIGHER THAN CURR REV                
         BNH   SRV32D                                                           
         MVC   NUCMLREV,REVISION    YES, FORCE CURRENT REV                      
         OI    UNITSW2,NOTIFYSW    SEND NOTIFY - UNIT COPIED                    
         MVC   SVWORK(20),KEY       AND THE LAST KEY INVOLVED                   
         MVC   SVWORK+20(1),NBPACK                                              
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,SVWORK+(NUKDATE-NUKEY)),(4,SVWORK+21)             
*                                                                               
SRV32D   MVC   HALF,NUCMLFLG                                                    
         NI    HALF,X'E0'          SET OFF ALL BUT REASON FLAGS                 
         SR    R0,R0                                                            
         IC    R0,HALF                                                          
         SRL   R0,4                                                             
         STC   R0,HALF+1                                                        
*                                                                               
         TM    NUCMLFLG,X'08'      DELETED - ONLY SET IF TRAFFIC DEL            
         BZ    *+16                                                             
         TM    UNTFLAG1,X'20'      STILL DELETED                                
         BZ    SRV34                NO, RESTORED                                
         B     *+12                                                             
         TM    UNTFLAG1,X'20'      NOW DELETED                                  
         BO    SRV34                YES                                         
*                                                                               
         TM    UNTFLAG4,UNTFLFDD   FEED NO NATIONAL                             
         BO    SRV34O               YES                                         
*                                                                               
         CLC   NUCML1(16),UNTCML1                                               
         BE    SRV33                                                            
         CLC   NUCML1,UNTCML1                                                   
         BE    SRV32E                                                           
*        TM    UNTFLAG2,UNTFL2SW   WERE PRDS SWAPPED                            
*        BZ    *+14                                                             
*        CLC   NUCML1,UNTCML2                                                   
*******  BE    SRV32E                                                           
         TM    UNTFLAG1,X'08'      CML1 ASSIGNED THIS SESSION?                  
         BO    *+12                                                             
         TM    UNTFLAG4,UNTFLDC1   CML1 DELETED THIS SESSION?                   
         BZ    SRV32E               NO                                          
         OI    NUCMLTFL,NUCMLTF1   SET ON COMML 1 CHANGED                       
         OC    NUCMLTFL,HALF                                                    
*                                                                               
SRV32E   CLC   NUCML2,UNTCML2                                                   
         BE    SRV34                                                            
*        TM    UNTFLAG2,UNTFL2SW   WERE PRDS SWAPPED                            
*        BZ    *+14                                                             
*        CLC   NUCML2,UNTCML1                                                   
*******  BE    SRV34                                                            
         TM    UNTFLAG1,X'04'      CML2 ASSIGNED THIS SESSION?                  
         BO    *+12                                                             
         TM    UNTFLAG4,UNTFLDC2   CML2 DELETED THIS SESSION?                   
         BZ    SRV34                NO                                          
         OC    NUCMLTFL,HALF+1                                                  
         OI    NUCMLTFL,NUCMLTF2   SET ON COMML 2 CHANGED                       
         B     SRV34                                                            
*                                                                               
SRV33    TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BZ    *+14                 NO                                          
         CLC   NUCMPROD,UNTPROD                                                 
         BNE   SRV34                                                            
         CLC   NUCMLBSL(17),UNTOTHR                                             
         BNE   SRV34                                                            
         CLC   NUCMLPOS,UNTPOS                                                  
         BNE   SRV34                                                            
         CLC   NUCMLBPS,UNTBBPOS                                                
         BE    SRV34C                                                           
*                                                                               
SRV34    MVC   NUCMLREV,REVISION                                                
*                                                                               
         NI    UNTFLAG2,X'FF'-X'20'                                             
*                                                                               
         TM    NUCMLFLG,X'10'      BEEN PRINTED ON INSTRUCTIONS                 
         BZ    SRV34C                                                           
*                                                                               
         OI    NUCMLFL2,NUCMLFLP    NEEDS TO BE PRINTED ON INSTR FLAG           
*                                                                               
* CHK FOR NEWLY ASSIGNED CMLS                                                   
* SPECIAL CASE : IF CML HAS BEEN DELETED - FLAG IS ON                           
*                                                                               
SRV34C   DS    0H                                                               
         OC    NUCML1(16),NUCML1   IF THERE WAS NO CML                          
         BNZ   SRV34D                                                           
         OC    UNTCML1(16),UNTCML1 BUT THERE IS ONE NOW                         
         BZ    SRV34F                                                           
         CLC   =C'REASSIGN',UNTCML1                                             
         BE    SRV34F                                                           
         CLC   =C'REASSIGN',UNTCML2                                             
         BE    SRV34F                                                           
         TM    NUCMLFL2,NUCMLNEW   AND THE FLAG WAS OFF                         
         BO    *+12                                                             
         OI    NUCMLFL2,NUCMLNEW   THEN ITS A NEW CML                           
         B     SRV34F                                                           
         NI    NUCMLFL2,X'FF'-NUCMLNEW                                          
         B     SRV34F                                                           
*                                                                               
*                                  THERE WAS A CML                              
SRV34D   OC    UNTCML1(16),UNTCML1 AND THERE IS ONE NOW                         
         BZ    SRV34E                                                           
         CLC   =C'REASSIGN',UNTCML1                                             
         BE    SRV34E                                                           
         CLC   =C'REASSIGN',UNTCML2                                             
         BE    SRV34E                                                           
         TM    NUCMLFL2,NUCMLNEW   AND NEW FLAG WAS TURNED ON                   
         BZ    SRV34F                                                           
         CLC   UNTREV,REVISION     AT DIFFERENT REVISION                        
         BE    SRV34F                                                           
         NI    NUCMLFL2,X'FF'-NUCMLNEW THEN TURN OFF THE FLAG                   
         B     SRV34F                                                           
*                                                                               
* THERE WAS A CML BUT NOW ITS DELETED AND IF REVISION # CHANGED                 
* THEN TURN ON NEWLY ASGND FLAG                                                 
*                                                                               
SRV34E   CLC   UNTREV,REVISION     IF REV IS SAME                               
         BNE   *+12                                                             
         TM    NUCMLFL2,NUCMLNEW   AND NEW FLAG IS OFF                          
         BO    *+12                                                             
         OI    NUCMLFL2,NUCMLNEW   THEN TURN ON NEW FLAG(FAKE NEW ASGN)         
         B     SRV34F                                                           
         NI    NUCMLFL2,X'FF'-NUCMLNEW                                          
*                                                                               
SRV34F   DS    0H                                                               
         TM    UNTFLAG4,UNTFLUPD   ANYTHING TO UPDATE?                          
         BNZ   SRV34H               YES                                         
*                                                                               
SRV34G   TM    UNTFLAG1,X'0C'      CML1/2 ASGND THIS SESSION?                   
         BNZ   SRV34H                                                           
         TM    UNTFLAG4,UNTFLDC1+UNTFLDC2 CML1/2 DELETED THIS SESSN?            
         BNZ   SRV34H                                                           
         CLC   NUCMLBSL(17),UNTOTHR BB CHANGED                                  
         BZ    SRV38                NO, GO ADD ELEMENT                          
*                                                                               
SRV34H   DS    0H                                                               
         CLC   UNTCML1,=C'REASSIGN'                                             
         BE    SRV34I                                                           
*                                                                               
         TM    UNTFLAG1,X'08'      CML1 ASGND THIS SESSION                      
         BO    *+12                                                             
         TM    UNTFLAG4,UNTFLDC1   DELETED THIS SESSION                         
         BZ    *+10                                                             
         MVC   NUCML1,UNTCML1                                                   
*                                                                               
SRV34I   DS    0H                                                               
         CLC   UNTCML2,=C'REASSIGN'                                             
         BE    SRV34J                                                           
*                                                                               
         TM    UNTFLAG1,X'04'      CML2 ASGND THIS SESSION                      
         BO    *+12                                                             
         TM    UNTFLAG4,UNTFLDC2   DELETED THIS SESSION                         
         BZ    *+10                                                             
         MVC   NUCML2,UNTCML2                                                   
*                                                                               
SRV34J   DS    0H                                                               
         CLC   UNTBBSN,=C'REASSIGN'                                             
         BE    SRV34L                                                           
         CLC   UNTBBCN,=C'REASSIGN'                                             
         BE    SRV34L                                                           
*                                                                               
         TM    UNTFLAG4,UNTFLCUT   IS THIS CUTIN                                
         BO    SRV34N               YES                                         
*                                                                               
         MVC   NUCMLBSL(17),UNTOTHR                                             
*                                                                               
SRV34L   MVC   NUCMLPOS,UNTPOS                                                  
         MVC   NUCMLBPS,UNTBBPOS                                                
*                                                                               
SRV34N   CLC   =C'REASSIGN',NUCML1                                              
         BNE   *+10                                                             
         XC    NUCML1,NUCML1                                                    
         CLC   =C'REASSIGN',NUCML2                                              
         BNE   *+10                                                             
         XC    NUCML2,NUCML2                                                    
*                                                                               
SRV34O   TM    UNTFLAG1,X'80'      REASSIGN STILL NEEDED                        
         BO    *+8                                                              
*                (CHANGED UNIT LEN   PRD  DATE)                                 
         NI    NUCMLFLG,X'FF'-X'80'-X'40'-X'20' RESET SOME FLAGS                
*                  SET OFF TRAF DEL   I/P                                       
         NI    NUCMLFLG,X'FF'-X'08'-X'01'                                       
*                                                                               
         TM    UNTFLAG4,UNTFLFDD   FEED NO NATIONAL                             
         BO    SRV35                YES                                         
*                                                                               
         TM    UNTFLAG1,X'0C'      CML1 OR CML2 ASGND THIS SESSION              
         BNZ   *+12                                                             
         TM    UNTFLAG4,UNTFLDC1+UNTFLDC2 CML1/2 DELETED THIS SESSION           
         BZ    SRV34M                                                           
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+10                YES, UNTREF=MAP YEAR/MAP CODE                
         MVC   NUCMLR3F,UNTREF                                                  
         MVC   NUCMLKEY,UNTKEY                                                  
*                                                                               
SRV34M   TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BZ    SRV34P               NO                                          
         MVC   NUCMPROD,UNTPROD                                                 
         OI    UNTFLAG4,UNTFLUPD   SET TO UPDATE                                
         B     SRV35                                                            
SRV34P   DS    0H                                                               
         MVI   NUCMLPRD,0          UNIT IS NOT C/S, CLEAR C/S PROD              
         XC    NUCMPROD,NUCMPROD                                                
*                                                                               
SRV35    TM    UNTFLAG1,X'80'      REASSIGN STILL NEEDED                        
         BZ    SRV35C                                                           
*                                                                               
         CLI   UNTBBSN,0           IS THERE A BILLBOARD                         
         BE    SRV35B               NO                                          
*                                                                               
         CLC   UNTCML1,=C'REASSIGN'                                             
         BE    SRV35B                                                           
*                                                                               
         CLC   UNTCML2,=C'REASSIGN'                                             
         BNE   SRV35C              REASSIGN IS FOR BB NO NEED TO FLAG           
*                                                                               
SRV35B   TM    NUCMLFLG,X'80'+X'40'+X'20' ANY CHANGE FLAGS ON                   
         BNZ   *+8                                                              
         OI    NUCMLFLG,X'E0'      SET ALL CHANGED                              
*                                                                               
SRV35C   TM    UNTFLAG1,X'20'     TRAFFIC DELETE                                
         BZ    *+8                                                              
         OI    NUCMLFLG,X'08'     SET ON TRAFFIC DELETE                         
*                                                                               
         TM    UNTFLAG2,X'04'     MEDIA REQUIRES BILLBOARD                      
         BZ    *+8                                                              
         OI    NUCMLFLG,X'04'     SET ON BILLBOARD REQUIRED                     
*                                                                               
         TM    UNTFLAG2,X'10'      INVERT PRODUCT PRINTING ON INSTR             
         BZ    *+8                  NO                                          
         OI    NUCMLFLG,X'01'                                                   
*                                                                               
         TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BO    *+12                                                             
         TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BZ    SRV35X                                                           
*                                                                               
         OC    UNTPROD2,UNTPROD2   UNIT PROD2 MUST BE ZERO                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    NBPR2CL3,NBPR2CL3                                                
         BZ    SRV35F                                                           
*                                                                               
         OC    NBPR1CL3,NBPR1CL3                                                
         BZ    SRV35F                                                           
*                                                                               
         CLC   NBPR1CL3,UNTCSPRO      CK NBPRD 1 IS STILL THE SAME              
         BE    SRV36                                                            
         CLC   NBPR1CL3,UNTCSPRO+3    CK NBPRD 1 IS STILL THE SAME              
         BE    SRV36                                                            
         B     SRV35J                                                           
*                                                                               
SRV35F   LA    R0,L'UNTCSPRO/3     MAX COPY SPLITS                              
         LR    R1,R0                                                            
         LA    RE,UNTCSPRO                                                      
         TM    UNTFLAG3,UNTFL3TB   TRI-BACK                                     
         BZ    SRV35H                                                           
*                                                                               
         LA    RF,SVCSPROD                                                      
SRV35FC  CLC   0(3,RF),0(RE)       UNTCSPRD TO SVCSPROD                         
         BE    SRV35G                                                           
         LA    RE,3(RE)            BUMP IN SVCSPROD                             
         BCT   R0,SRV35FC                                                       
         B     SRV35J              PRD CHANGED                                  
*                                                                               
SRV35G   DS    0H                                                               
         LA    RF,3(RF)            BUMP IN UNTCSPRO                             
         LA    R0,(L'UNTCSPRO/3)                                                
         LA    RE,SVCSPROD                                                      
         BCT   R1,SRV35FC                                                       
         B     SRV36                                                            
*                                                                               
SRV35H   CLC   UNTPROD,0(RE)       SAME AS EXISTING CS PROD                     
         BE    SRV36                                                            
         LA    RE,3(,RE)                                                        
         BCT   R0,SRV35H                                                        
         B     SRV35J                                                           
*                                                                               
SRV35X   CLC   UNTPROD,NBPR1CL3                                                 
         BNE   SRV35J                                                           
*                                                                               
         TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BO    SRV36                YES                                         
*                                                                               
         CLC   UNTPROD2,NBPR2CL3                                                
         BE    SRV36                                                            
*                                                                               
SRV35J   OI    NUCMLFLG,X'40'      SET ON PROD CHANGED                          
*                                                                               
SRV36    LLC   RE,UNTSLN                                                        
         LLC   RF,UNTSLN2                                                       
         AR    RE,RF                                                            
*                                                                               
         TM    UNTFLAG3,UNTFL3TB   IS IT TRI-BACK                               
         BZ    SRV36C                                                           
*                                                                               
         LLC   RF,UNTSLN3                                                       
         AR    RE,RF                                                            
*                                                                               
SRV36C   CLM   RE,1,NBLEN                                                       
         BE    *+8                                                              
         OI    NUCMLFLG,X'80'      SET ON LEN CHANGED                           
*                                                                               
         TM    NUCMLFLG,X'E0'      ANY REASSIGNS STILL NEEDED                   
         BZ    *+8                                                              
         OI    UNITSW1,X'04'       SET ON REASSIGN STILL NEEDED                 
*                                                                               
         TM    NBUNITST,X'42'     MISSED, TREAT AS DELETE                       
         BNZ   SRV38                                                            
         L     R1,AIO1                                                          
         TM    NURSTAT-NUKEY(R1),X'80' DELETED UNIT                             
         BO    SRV38                                                            
         TM    NUCMLFLG,X'08'     TRAFFIC DELETE                                
         BO    SRV38                                                            
         NI    NUCMLFLG,X'FF'-X'02' RESET INST RUN FOR DEL FLAG                 
*                                                                               
SRV38    DS    0H                                                               
         CLC   UNTCML1,=C'REASSIGN'                                             
         BE    SRV38C                                                           
*                                                                               
         NI    NUCMADFL,X'FF'-NUCMADF1 SET OFF ADID FLAG                        
*                                                                               
         TM    UNTCMLF,NUCMADF1    TEST NEW CMML IS ADID                        
         BZ    SRV38A                                                           
         OI    NUCMADFL,NUCMADF1                                                
         B     SRV38B                                                           
*                                                                               
SRV38A   LA    R0,NUCMADF1         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,UNTCML1          POINT TO COMML                               
         LA    RF,NUCMADFL         ADDR OF FLAG BYTE                            
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
*                                                                               
SRV38B   MVC   NUCML1,UNTCML1                                                   
*                                                                               
SRV38C   DS    0H                                                               
         CLC   UNTCML2,=C'REASSIGN'                                             
         BE    SRV38F                                                           
*                                                                               
         NI    NUCMADFL,X'FF'-NUCMADF2 SET OFF ADID FLAG                        
         OC    UNTCML2,UNTCML2                                                  
         BZ    SRV38E                                                           
*                                                                               
         TM    UNTCMLF,NUCMADF2    TEST NEW CMML IS ADID                        
         BZ    SRV38D                                                           
         OI    NUCMADFL,NUCMADF2                                                
         B     SRV38E                                                           
*                                                                               
SRV38D   LA    R0,NUCMADF2         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,UNTCML2           POINT TO COMML                              
         LA    RF,NUCMADFL         ADDR OF FLAG BYTE                            
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
*                                                                               
SRV38E   MVC   NUCML2,UNTCML2                                                   
*                                                                               
SRV38F   DS    0H                                                               
         CLC   UNTBBSN,=C'REASSIGN'                                             
         BE    SRV38H                                                           
*                                                                               
         NI    NUCMADFL,X'FF'-NUCMADF3 SET OFF ADID FLAG                        
         LA    R0,NUCMADF3         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,UNTBBSN          POINT TO COMML                               
         LA    RF,NUCMADFL         AD-ID                                        
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
*                                                                               
         CLC   NUCMLBSN,UNTBBSN                                                 
         BE    *+10                                                             
         MVC   NUCMLBSN,UNTBBSN                                                 
*                                                                               
SRV38H   DS    0H                                                               
         TM    UNTFLAG4,UNTFLCUT   IS THIS CUTIN                                
         BO    SRV38K               YES                                         
*                                                                               
         CLC   UNTBBCN,=C'REASSIGN'                                             
         BE    SRV38K                                                           
*                                                                               
         NI    NUCMADFL,X'FF'-NUCMADF4 SET OFF ADID FLAG                        
         LA    R0,NUCMADF4         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,UNTBBCN          POINT TO COMML                               
         LA    RF,NUCMADFL         ADDR OF FLAG BYTE                            
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
*                                                                               
         CLC   NUCMLBCN,UNTBBCN                                                 
         BE    *+10                                                             
         MVC   NUCMLBCN,UNTBBCN                                                 
*                                                                               
SRV38K   GOTO1 ADDELEM                                                          
*                                                                               
         DROP  R6                                                               
         TM    UNTFLAG4,UNTFLFDD   FEED NO NATIONAL                             
         BO    SRV41                NO                                          
         EJECT                                                                  
* NOW SEE IF FEED ELEMENTS FOLLOW *                                             
*                                                                               
SRV40    LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             END OF TABLE                                 
         BE    SRV56                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         CLC   NUDA,UNTDSKAD       THIS SAME RECORD                             
         BNE   SRV56                                                            
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         TM    UNTFLAG4,UNTFLFDD   FEED NO NATIONAL                             
         BO    SRV41                YES                                         
         TM    UNTFLAG1,UNTFL1FD   MUST BE FEED                                 
         BO    SRV41                                                            
         TM    UNTFLAG4,UNTFLCUT   MUST BE CUTIN                                
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,SRCUT            SAVE CUTINS                                  
         B     SRV40                                                            
*                                                                               
SRV41    DS   0H                                                                
         L     R6,AIO                                                           
*                                                                               
         TM    22(R6),X'80'        DELETED REC                                  
         BO    SRV40                                                            
*                                                                               
         XC    ELEM,ELEM           IF 2 ELEMENTS, SAVE LAST ONLY                
         MVI   ELCODE,X'23'        FEED                                         
         BRAS  RE,GETEL                                                         
         BNE   SRV54                                                            
         USING NUFDCEL,R6                                                       
SRV42    CLC   UNTFEED,NUFDCFED    THIS THE ONE                                 
         BE    SRV44                                                            
         BRAS  RE,NEXTEL                                                        
         BE    SRV42                                                            
         B     SRV54                                                            
SRV44    LLC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,SRVMVC           SAVE ELEMENT                                 
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VRECUP,DMCB,(C'U',AIO),(R6)                                      
*                                                                               
         TM    UNTFLAG1,X'40'      THIS UNIT DELETED                            
         BO    SRV40                                                            
         LA    R6,ELEM                                                          
         USING NUFDCEL,R6                                                       
         MVI   NUFDCEL,X'23'                                                    
         MVI   NUFDCLEN,NUFDCELN                                                
*                                                                               
         TM    UNTFLAG4,UNTFLUPD   ANYTHING TO UPDATE?                          
         BNZ   SRV44K               YES                                         
*                                                                               
         TM    UNTFLAG3,UNTFL3TB   IS IT TRI-BACK                               
         BZ    SRV44G                                                           
*                                                                               
         TM    UNTTBFLG,UNTBCML3   CML3 ASGND THIS SESSION?                     
         BNZ   SRV44K                                                           
         TM    UNTTBFLG,UNTBDEL3   CML3 DELETED THIS SESSION                    
         BNZ   SRV44K                                                           
SRV44G   TM    UNTFLAG1,X'0C'      CML1/2 ASGND THIS SESSION?                   
         BNZ   SRV44K                                                           
         TM    UNTFLAG4,UNTFLDC1+UNTFLDC2 CML1/2 DELETED THIS SESSN?            
         BZ    SRV52                NO                                          
*                                                                               
SRV44K   TM    NUFDCFLG,X'20'      DATE CHANGED                                 
         BZ    *+20                                                             
         CLC   NUFDCREV,REVISION   UNIT REV HIGHER THAN CURR REV                
         BNH   *+10                                                             
         MVC   NUFDCREV,REVISION    YES, FORCE CURRENT REV                      
*                                                                               
         MVC   NUFDCFED,UNTFEED                                                 
*                                                                               
         CLC   NUFDCML1(16),UNTCML1                                             
         BE    SRV46                                                            
*                                                                               
         CLC   NUFDCML1,UNTCML1                                                 
         BE    SRV45                                                            
         TM    UNTFLAG1,X'08'      CML1 ASSIGNED THIS SESSION?                  
         BO    *+12                                                             
         TM    UNTFLAG4,UNTFLDC1   CML1 DELETED THIS SESSION?                   
         BZ    SRV45                NO                                          
         OC    NUFDCTFL,HALF                                                    
         OI    NUFDCTFL,NUFDCTF1   SET ON COMML 1 CHANGED                       
*                                                                               
SRV45    CLC   NUFDCML2,UNTCML2                                                 
         BE    SRV50                                                            
         TM    UNTFLAG1,X'04'      CML2 ASSIGNED THIS SESSION?                  
         BO    *+12                                                             
         TM    UNTFLAG4,UNTFLDC2   CML2 DELETED THIS SESSION?                   
         BZ    SRV50                NO                                          
*                                                                               
         OI    NUFDCTFL,NUFDCTF2   SET ON COMML 2 CHANGED                       
         OC    NUFDCTFL,HALF+1                                                  
         B     SRV50                                                            
*                                                                               
SRV46    CLC   NUFDCBSL(17),UNTOTHR SPOT LEN, CML1 AND CML2                     
         BNE   SRV50                                                            
         CLC   NUFDCPOS,UNTPOS                                                  
         BNE   SRV50                                                            
         CLC   NUFDCBPS,UNTBBPOS                                                
         BE    SRV52                                                            
SRV50    MVC   NUFDCREV,REVISION                                                
*                                                                               
         TM    SVCMLFLG,X'10'       BEEN PRINTED ON INSTRUCTIONS                
         BZ    SRV52                                                            
*                                                                               
         OI    NUFDCFL2,NUFDCFLP    NEEDS TO BE PRINTED ON INSTR FLAG           
*                                                                               
* CHK FOR NEWLY ASSIGNED CMLS                                                   
* SPECIAL CASE : IF CML HAS BEEN DELETED - FLAG IS ON                           
*                                                                               
SRV52    DS    0H                                                               
*                                                                               
         TM    UNTFLAG4,UNTFLUPD   ANYTHING TO UPDATE?                          
         BNZ   SRV52B               YES                                         
*                                                                               
         TM    UNTFLAG3,UNTFL3TB   IS IT TRI-BACK                               
         BZ    SRV52A                                                           
*                                                                               
         TM    UNTTBFLG,UNTBCML3   CML3 ASGND THIS SESSION?                     
         BNZ   SRV52B                                                           
         TM    UNTTBFLG,UNTBDEL3   CML3 DELETED THIS SESSION                    
         BNZ   SRV52B                                                           
SRV52A   TM    UNTFLAG1,X'0C'      CML1/2 ASGND THIS SESSION?                   
         BNZ   SRV52B                                                           
         TM    UNTFLAG4,UNTFLDC1+UNTFLDC2 CML1/2 DELETED THIS SESSN?            
         BZ    SRV53                NO, GO ADD ELEMENT                          
*                                                                               
SRV52B   OC    NUFDCML1(16),NUFDCML1 IF THERE WAS NO CML                        
         BNZ   SRV52D                                                           
         OC    UNTCML1(16),UNTCML1 BUT THERE IS ONE NOW                         
         BZ    SRV52F                                                           
         CLC   =C'REASSIGN',UNTCML1                                             
         BE    SRV52F                                                           
         CLC   =C'REASSIGN',UNTCML2                                             
         BE    SRV52F                                                           
         TM    NUFDCFL3,NUFDCNEW AND THE FLAG WAS OFF                           
         BO    *+12                                                             
         OI    NUFDCFL3,NUFDCNEW   THEN ITS A NEW CML                           
         B     SRV52F                                                           
         NI    NUFDCFL3,X'FF'-NUFDCNEW                                          
         B     SRV52F                                                           
*                                                                               
*                                  THERE WAS A CML                              
SRV52D   OC    UNTCML1(16),UNTCML1 AND THERE IS ONE NOW                         
         BZ    SRV52E                                                           
         CLC   =C'REASSIGN',UNTCML1                                             
         BE    SRV52E                                                           
         CLC   =C'REASSIGN',UNTCML2                                             
         BE    SRV52E                                                           
         TM    NUFDCFL3,NUFDCNEW   AND NEW FLAG WAS TURNED ON                   
         BZ    SRV52F                                                           
         CLC   UNTREV,REVISION     AT DIFFERENT REVISION                        
         BE    SRV52F                                                           
         NI    NUFDCFL3,X'FF'-NUFDCNEW THEN TURN OFF THE FLAG                   
         B     SRV52F                                                           
*                                                                               
* THERE WAS A CML BUT NOW ITS DELETED AND IF REVISION # CHANGED                 
* THEN TURN ON NEWLY ASGND FLAG                                                 
*                                                                               
SRV52E   CLC   UNTREV,REVISION     IF REV IS SAME                               
         BNE   *+12                                                             
         TM    NUFDCFL3,NUFDCNEW   AND NEW FLAG IS OFF                          
         BO    *+12                                                             
         OI    NUFDCFL3,NUFDCNEW   THEN TURN ON NEW FLAG(FAKE NEW ASGN)         
         B     SRV52F                                                           
         NI    NUFDCFL3,X'FF'-NUFDCNEW                                          
*                                                                               
SRV52F   DS    0H                                                               
         TM    UNTFLAG1,X'08'      CML1 ASGND THIS SESSION                      
         BO    *+12                                                             
         TM    UNTFLAG4,UNTFLDC1   DELETED THIS SESSION                         
         BZ    SRV52FC                                                          
*                                                                               
         CLC   UNTCML1,=C'REASSIGN'                                             
         BE    SRV52FC                                                          
         MVC   NUFDCML1,UNTCML1                                                 
*                                                                               
         NI    NUFDADFL,X'FF'-NUFDADF1 SET OFF ADID FLAG                        
         LA    R0,NUFDADF1         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,UNTCML1                                                       
         LA    RF,NUFDADFL         AD-ID                                        
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
         MVC   NUFDCML1,UNTCML1                                                 
*                                                                               
SRV52FC  DS    0H                                                               
         TM    UNTFLAG1,X'04'      CML2 ASGND THIS SESSION                      
         BO    *+12                                                             
         TM    UNTFLAG4,UNTFLDC2   DELETED THIS SESSION                         
         BZ    SRV52FE                                                          
*                                                                               
         CLC   UNTCML2,=C'REASSIGN'                                             
         BE    SRV52FE                                                          
         MVC   NUFDCML2,UNTCML2                                                 
*                                                                               
         NI    NUFDADFL,X'FF'-NUFDADF2 SET OFF ADID FLAG                        
         LA    R0,NUFDADF2         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,UNTCML2                                                       
         LA    RF,NUFDADFL         AD-ID                                        
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
*                                                                               
         CLC   NUFDCML2,UNTCML2                                                 
         BE    *+10                                                             
         MVC   NUFDCML2,UNTCML2                                                 
*                                                                               
SRV52FE  DS    0H                                                               
         CLC   =C'REASSIGN',NUFDCML1                                            
         BNE   *+10                                                             
         XC    NUFDCML1,NUFDCML1                                                
*                                                                               
         CLC   =C'REASSIGN',NUFDCML2                                            
         BNE   *+10                                                             
         XC    NUFDCML2,NUFDCML2                                                
*                                                                               
         CLC   UNTBBSN,=C'REASSIGN'                                             
         BE    SRV52J                                                           
         CLC   UNTBBCN,=C'REASSIGN'                                             
         BE    SRV52J                                                           
*                                                                               
         TM    UNTFLAG4,UNTFLCUT   IS THIS CUTIN                                
         BO    SRV52K               YES                                         
*                                                                               
         MVC   NUFDCBSL(17),UNTOTHR SPOT LEN, CML1 AND CML2                     
*                                                                               
         NI    NUFDADFL,X'FF'-NUFDADF3 SET OFF ADID FLAG                        
         LA    R0,NUFDADF3         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,NUFDCBSN         POINT TO COMML                               
         LA    RF,NUFDADFL         AD-ID                                        
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
*                                                                               
         NI    NUFDADFL,X'FF'-NUFDADF4 SET OFF ADID FLAG                        
         LA    R0,NUFDADF4         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,NUFDCBCN         POINT TO COMML                               
         LA    RF,NUFDADFL         AD-ID                                        
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
*                                                                               
SRV52J   MVC   NUFDCPOS,UNTPOS                                                  
         MVC   NUFDCBPS,UNTBBPOS                                                
SRV52K   MVI   NUFDCFLG,0          RESET FLAG                                   
*                                                                               
         NI    NUFDCFL2,X'FF'-X'80'                                             
*                                                                               
         TM    UNTFLAG1,X'40'      THIS FEED DELETED                            
         BZ    *+8                                                              
         OI    NUFDCFL2,X'80'                                                   
*                                                                               
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+10                YES, UNTREF=MAP YEAR/MAP CODE                
         MVC   NUFDCR3F,UNTREF                                                  
         MVC   NUFDCKEY,UNTKEY                                                  
*                                                                               
         TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BZ    SRV52L               NO                                          
         MVC   NUFDPROD,UNTPROD                                                 
         B     SRV53                                                            
SRV52L   DS    0H                                                               
         MVI   NUFDCPRD,0          UNIT IS NOT C/S, CLEAR C/S PROD              
         XC    NUFDPROD,NUFDPROD                                                
*                                                                               
SRV53    GOTO1 ADDELEM                                                          
         B     SRV40                                                            
*                                                                               
* ADD NEW ELEMENT HERE *                                                        
*                                                                               
SRV54    TM    UNTFLAG1,X'40'      THIS UNIT DELETED                            
         BO    SRV40                                                            
         LA    R6,ELEM                                                          
         USING NUFDCEL,R6                                                       
         MVI   NUFDCEL,X'23'                                                    
         MVI   NUFDCLEN,NUFDCELN                                                
         MVC   NUFDCFED,UNTFEED                                                 
*                                                                               
         MVC   NUFDCREV,REVISION                                                
*                                                                               
         LA    R0,NUFDADF1         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,UNTCML1                                                       
         LA    RF,NUFDADFL         AD-ID                                        
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
         MVC   NUFDCML1,UNTCML1                                                 
         OI    NUFDCFL3,NUFDCNEW                                                
*                                                                               
         LA    R0,NUFDADF2         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,UNTCML2                                                       
         LA    RF,NUFDADFL         AD-ID                                        
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
         MVC   NUFDCML2,UNTCML2                                                 
*                                                                               
         TM    UNTFLAG4,UNTFLCUT   IS THIS CUTIN                                
         BO    SRV54F                                                           
*                                                                               
         MVC   NUFDCBSL(17),UNTOTHR SPOT LEN, CML1 AND CML2                     
         LA    R0,NUFDADF3         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,NUFDCBSN         POINT TO COMML                               
         LA    RF,NUFDADFL         AD-ID                                        
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
*                                                                               
         LA    R0,NUFDADF4         SET FLAG BIT TO SET IF NEEDED                
         LA    R1,NUFDCBCN         POINT TO COMML                               
         LA    RF,NUFDADFL         AD-ID                                        
         BRAS  RE,STADID           GO SET ADID IF NEEDED                        
*                                                                               
         MVC   NUFDCPOS,UNTPOS                                                  
         MVC   NUFDCBPS,UNTBBPOS                                                
*                                                                               
SRV54F   MVI   NUFDCFLG,0          RESET FLAG                                   
         TM    UNTFLAG4,UNTFLSEC   IS THIS SECTIONAL FEED                       
         BO    *+10                YES, UNTREF=MAP YEAR/MAP CODE                
         MVC   NUFDCR3F,UNTREF                                                  
*                                                                               
         MVC   NUFDCKEY,UNTKEY                                                  
*                                                                               
         TM    UNTFLAG2,UNTFL2CS   COPY SPLIT                                   
         BZ    SRV55                NO                                          
*                                                                               
         MVC   NUFDPROD,UNTPROD                                                 
         OI    UNTFLAG4,UNTFLUPD   SET TO UPDATE                                
*                                                                               
SRV55    DS    0H                                                               
         GOTO1 ADDELEM                                                          
         B     SRV40                                                            
         DROP  R6                                                               
*                                                                               
SRVMVC   MVC   ELEM(0),0(R6) *EXECUTED*                                         
*                                                                               
SRV56    DS    0H                                                               
         CLC   BLOCK(2),=X'1702'  ANY 17 ELEM FOR CUTINS                        
         BE    SRV57                NO                                          
         CLI   BLOCK,X'17'         BETTER BE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM,BLOCK                                                       
         GOTO1 ADDELEM                                                          
*                                                                               
SRV57    DS    0H                                                               
         L     R6,AIO              SEE IF DELETED RECORD                        
         TM    22(R6),X'80'                                                     
         BO    SRV58                                                            
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
SRV58    CLI   0(R5),0             END OF TABLE                                 
         BNE   SRV30                                                            
*                                                                               
         TM    UNITSW2,NOTIFYSW    NOTIFY OF COPIED UNITS ?                     
         BZ    SRV60                                                            
         NI    UNITSW2,X'FF'-NOTIFYSW CLEAR NOTIFY FLAG                         
*                                                                               
* PRINT (CLT/NET/PROG/PERIOD/EST/PACKAGE/SUBLINE) IN THE NOTIFY                 
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         MVC   0(L'NOTEMSG,R1),NOTEMSG                                          
         LA    R1,L'NOTEMSG(,R1)                                                
         MVC   0(2,R1),AGENCY                                                   
         MVC   3(3,R1),QCLT                                                     
         MVC   7(4,R1),TRANET                                                   
         MVC   12(6,R1),TRAPRG                                                  
         MVC   20(8,R1),TRAPER                                                  
*                                                                               
         MVC   30(5,R1),=C'DATE='                                               
         MVC   35(5,R1),SVWORK+21                                               
*                                                                               
         MVC   43(4,R1),=C'E/P='                                                
         EDIT  (B1,SVWORK+(NUKEST-NUKEY)),(3,47(R1)),ALIGN=LEFT                 
*                                                                               
         MVI   50(R1),C'/'                                                      
         EDIT  (B1,SVWORK+20),(3,51(R1)),ALIGN=LEFT  PACKAGE                    
*                                                                               
         MVC   56(8,R1),=C'SUBLINE='                                            
         MVC   64(1,R1),SVWORK+(NUKSUB-NUKEY)                                   
*                                                                               
         CLI   SVWORK+(NUKSUB-NUKEY),C'A'  SUBLINE                              
         BNL   SRV59                                                            
*                                                                               
         EDIT  (B1,SVWORK+(NUKSUB-NUKEY)),(3,64(R1)),ALIGN=LEFT                 
*                                                                               
SRV59    DS    0H                                                               
         OC    ELEM,SPACES                                                      
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'NOTEMSG+72,ELEM)                       
*                                                                               
         XC    SVWORK,SVWORK                                                    
         XC    ELEM,ELEM                                                        
*                                                                               
SRV60    BRAS  RE,UPDREV            UDATE REVISION RECORD                       
*                                                                               
SRV80    MVC   GERROR,=Y(UNTUPDT)                                               
         TM    UNITSW1,X'04'       ANY REASSIGNS STILL NEEDED                   
         BZ    SRV84                                                            
         MVC   GERROR,=Y(SVUNTINC)                                              
SRV84    TM    UNITSW1,X'20'       ANY REASSIGNS FROM MEDIA UPDATE              
         BZ    SRV86                                                            
         MVC   GERROR,=Y(MEDCHUNT)                                              
SRV86    DS    0H                                                               
         CLI   BYTE,0              ANY DELETED UNITS                            
         BE    SRV90                                                            
         MVC   GERROR,=Y(UNTASDEL) UNITS ASSIGNED - SOME ARE DELETED            
         XC    ELEM,ELEM                                                        
         MVI   ELEM,4              L'SUBST TEXT + 1                             
         LLC   R1,BYTE                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+1(3),DUB                                                    
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
*                                                                               
*RV90    XC    NXTUNIT(EQVPTBL-NXTUNIT),NXTUNIT                                 
SRV90    LA    RE,NXTUNIT                                                       
         LA    RF,INITALL                                                       
         XCEF                                                                   
*                                                                               
         XC    EQVPTBL,EQVPTBL                                                  
         XC    UNITABLE(256),UNITABLE                                           
         XC    TRAOPT,TRAOPT                                                    
         OI    TRAOPTH+6,X'80'                                                  
         NI    TRACLTH+4,X'FF'-X'20' SET OFF VALIDATED                          
         NI    TRAOPTH+4,X'FF'-X'20'                                            
         MVC   TRAOPT1,=C'OPTIONS'                                              
         XC    TRAOPT1+1(6),SPACES                                              
         OI    TRAOPT1H+6,X'80'    TRANS                                        
*                                                                               
SRVX     XIT1                                                                   
         EJECT                                                                  
UNALLERR MVC   GERROR,=Y(CSNALLOC)                                              
         XC    ELEM,ELEM                                                        
         B     DISDATER                                                         
*                                                                               
ONEPRDER MVC   GERROR,=Y(ONEPROD)                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R0,(L'UNTCSPRO/3)                                                
         LA    RF,WORK                                                          
         CLI   0(RF),0                                                          
         BNE   *+14                                                             
         LA    RF,3(,RF)                                                        
         BCT   R0,*-12                                                          
         DC    H'0'                                                             
*                                                                               
ONEPRD14 DS    0H                                                               
         MVI   ELEM+9,4           L'SUBST TEXT + 1                              
         MVC   ELEM+10(3),0(RF)                                                 
         LA    R2,TRAOPTH          POINT TO OPTIONS FIELD                       
*                                                                               
DISDATER LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   ELEM,9              L'SUBST TEXT + 1                             
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,UNTADTEP),(5,ELEM+1)                              
         B     SRVEREX                                                          
*                                                                               
SRVEREX  TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    SRVEREXA                                                         
*                                                                               
         LA    R1,=C'DMWRT '                                                    
         BRAS  RE,COMTWA                                                        
*                                                                               
SRVEREXA GOTO1 VTRAERR                                                          
*                                                                               
*******************************************************                         
* SUBROUTINE TO SEE IF ANY CHANGES WERE AMDE TO UNITS *                         
*******************************************************                         
*                                                                               
FCHG     NTR1                                                                   
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
*                                                                               
         CLI   0(R5),0             EMPTY TABLE (DELETED UNITS ONLY)             
         BNE   FCHGX                NO ***** DISABLED THIS ROUTINE ***          
*                                                                               
         CLI   SVTN1PR5,C'C'       REPORT ON CHANGES ONLY                       
         BNE   FCHGX                                                            
         BE    FCHGERR                                                          
*                                                                               
FCHG10   DS   0H                                                                
         TM    UNTFLAG1,X'08'      CML 1 ASSIGNED THIS SESSION                  
         BO    FCHGX                OKAY TO SAVE                                
*                                                                               
         TM    UNTFLAG1,X'04'      CML 2 ASSIGNED THIS SESSION                  
         BO    FCHGX                OKAY TO SAVE                                
*                                                                               
         TM    UNTFLAG4,UNTFLDC1   CML 1 DELETED THIS SESSION                   
         BO    FCHGX                OKAY TO SAVE                                
*                                                                               
         TM    UNTFLAG4,UNTFLDC2   CML 2 DELETED THIS SESSION                   
         BO    FCHGX                OKAY TO SAVE                                
*                                                                               
         TM    UNTFLAG3,UNTFL3TB   IS THIS A TRI=BACK UNIT                      
         BZ    FCHG20               NO, BYPASS CKS                              
*                                                                               
         TM    UNTTBFLG,UNTBCML3   CML 3 ASSIGNED THIS SESSION                  
         BO    FCHGX                OKAY TO SAVE                                
*                                                                               
         TM    UNTTBFLG,UNTBDEL3   CML 3 DELETED THIS SESSION                   
         BO    FCHGX                OKAY TO SAVE                                
*                                                                               
FCHG20   DS   0H                                                                
         TM    UNTFLAG4,UNTFL4BC   WAS BILLBOARD CHANGED                        
         BO    FCHGX                OKAY TO SAVE                                
*                                                                               
         LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             END OF TABLE                                 
         BNE   FCHG10               NO                                          
*                                                                               
* NO CHANGES, NO NEED TO SAVE                                                   
*                                                                               
FCHGERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCHGMS1),NOCHGMS1                                     
         NI    TRACLTH+4,X'FF'-X'20'                                            
         LA    R2,TRAOPTH                                                       
         GOTO1 ERREX2                                                           
FCHGX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
NOCHGMS1 DC    C'* ERROR * NO CHANGES MADE, CANNOT SAVE *'                      
NOTEMSG  DC    C'AUTONOTE*MNAS,SMUR:COPIED UNITS AGY '                          
         DROP  R4,R5                                                            
         EJECT                                                                  
* UPDATE REVISION RECORD FOR THIS NETWORK, PROGRAM, PERIOD, REV # *             
*                                                                               
UPDREV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITXSP          SET TO XSPOT                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REVXKEY,R4                                                       
         MVC   REVXKID,=X'0A1D'                                                 
         MVC   REVXKAM(3),BAGYMD AND BCLT                                       
         MVC   REVXKNET(10),NETWORK AND PROGRAM                                 
         MVC   REVXKPER,PERIOD                                                  
         CLI   MYTN2PR6,C'W'       THIS WEEKLY PERIODS                          
         BNE   SRV61                                                            
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BNZ   SRV61                YES, DATE IS IN PERIOD                      
         MVC   REVXKPER,STDATEP                                                 
*                                                                               
SRV61    MVC   REVXKNUM,REVISION                                                
*                                                                               
         CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
         BNE   SRV62                                                            
         CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   REVXKPRD,VOPTPROD                                                
*                                                                               
SRV62    DS   0H                                                                
         OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    *+10                                                             
         MVC   REVXKPGR(2),VOPTPRGR                                             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         CLC   KEY(32),KEYSAVE                                                  
         BE    SRV64                                                            
*                                                                               
* BUILD NEW REV RECORD *                                                        
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVC   0(32,R6),KEYSAVE                                                 
         MVI   33(R6),42           LENGTH                                       
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING REVREVEL,R6                                                      
         MVI   REVREVEL,X'10'                                                   
         MVI   REVDTALN,(REVREVEQ-REVREVEL)                                     
         GOTO1 DATCON,DMCB,(5,0),(3,REVADATE)                                   
         MVC   REVNNUM,HIREVNN     NETWORK REVISION NUMBER                      
*                                                                               
         CLI   VOPTPRGR,0          RUNNING BY PRODUCT GROUP                     
         OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    *+8                                                              
         OI    REVFLAG,X'01'       SET ON REV BY PROD GROUP                     
*                                                                               
         CLI   MYTN2PR6,C'B'                                                    
         BNE   *+12                                                             
         OI    REVFLAG,REVBRD      TURN ON PERIOD IS BROAD MONTH                
         B     *+16                                                             
         CLI   MYTN2PR6,C'C'                                                    
         BNE   *+8                                                              
         OI    REVFLAG,REVCAL      TURN ON PERIOD IS CALENDAR MONTH             
*                                                                               
         GOTO1 ADDELEM                                                          
         B     SRV70                                                            
*                                                                               
SRV64    DS   0H                                                                
         GOTO1 GETREC                                                           
         CLC   KEY(32),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
         GOTO1 DATCON,DMCB,(5,0),(3,REVADATE)                                   
         NI    REVFLAG,X'FF'-X'40' SET OFF NET SEED RUN FOR REV                 
*                                                                               
         CLI   MYTN2PR6,C'B'                                                    
         BNE   *+12                                                             
         OI    REVFLAG,REVBRD      TURN ON PERIOD IS BROAD MONTH                
         B     *+16                                                             
         CLI   MYTN2PR6,C'C'                                                    
         BNE   *+8                                                              
         OI    REVFLAG,REVCAL      TURN ON PERIOD IS CALENDAR MONTH             
*                                                                               
         CLI   REVDTALN,12         OLD REVISION RECORD                          
         BE    SRV66                                                            
         CLC   REVNNUM,HIREVNN     THIS NET # > HIGHEST NET #                   
         BNH   *+6                                                              
         DC    H'0'                BUG CATCHER                                  
         MVC   REVNNUM,HIREVNN                                                  
*                                                                               
SRV66    MVI   ELCODE,X'40'                                                     
*                                                                               
         GOTO1 REMELEM                                                          
*                                                                               
SRV70    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING REVCMTEL,R6                                                      
         MVI   REVCMTEL,X'40'                                                   
         MVI   REVCMTLN,REVCMT+60-REVCMTEL                                      
         MVI   REVNUMB,1                                                        
         CLI   TRAREVH+5,0                                                      
         BE    SRV74                                                            
*                                                                               
         MVC   REVCMT,TRAREV                                                    
         GOTO1 ADDELEM                                                          
*                                                                               
SRV74    CLI   TRACOMH+5,0         SECOND CMT LINE ENTERED                      
         BE    SRV76                                                            
*                                                                               
         LA    R6,ELEM                                                          
         MVI   REVCMTEL,X'40'                                                   
         MVI   REVCMTLN,REVCMT+60-REVCMTEL                                      
         MVI   REVNUMB,2                                                        
         MVC   REVCMT,TRACOM                                                    
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
SRV76    CLC   KEY(32),KEYSAVE                                                  
         BNE   SRV78                                                            
*                                                                               
         BAS   RE,ACTELM           GO FIX F1 ACTIVITY ELEM                      
*                                                                               
         BAS   RE,BTIME            GO FIX F1 ACTIVITY ELEM                      
*                                                                               
         GOTO1 PUTREC                                                           
         B     SRVRX                                                            
*                                                                               
SRV78    DS   0H                                                                
         BAS   RE,ACTELM           GO FIX F1 ACTIVITY ELEM                      
         BAS   RE,BTIME            GO FIX F1 ACTIVITY ELEM                      
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
SRVRX    DS   0H                                                                
         BRAS  RE,INITNET          SET TO NET                                   
         XIT1                                                                   
*                                                                               
* CREATE/CHANGE ACTIVITY ELEM                                                   
*                                                                               
ACTELM   NTR1                                                                   
         MVI   ELCODE,X'F1'                                                     
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R6,ELEM                                                          
         USING ACTVD,R6                                                         
*                                                                               
         LA    R3,ACTVCHID                                                      
*                                                                               
         CLI   ELEM,X'F1'          ALREADY HAVE ELEM                            
         BE    ACTELM10             YES                                         
*                                                                               
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,20                                                       
         MVC   ACTVADDT,BTODAY     SET TODAYS DATE FOR ADD                      
*        MVI   ACTVCHNM,1          CHANGE NUMBER OF ZERO                        
         LA    R3,ACTVADID                                                      
*                                                                               
ACTELM10 DS    0H                                                               
         MVC   ACTVCHDT,BTODAY     SET TODAYS DATE FOR CHANGE                   
         AI    ACTVCHNM,1          UP THE CHANGE NUMBER                         
         MVC   0(2,R3),TWAORIG     MOVE IN ID                                   
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         BZ    ACTELM20                                                         
         MVC   0(2,R3),FAPASSWD    YES SO USE THIS ID                           
         OI    2(R3),X'80'                                                      
         DROP  R1                                                               
*                                                                               
ACTELM20 DS    0H                                                               
         OC    ASECBLK,ASECBLK     IS NEW SECURITY ACTIVE                       
         BZ    ACTELM30                                                         
         L     R1,ASECBLK          NEW SECURITY BLOCK                           
         USING SECD,R1                                                          
         MVC   ACTVSCID,SECPID     USER'S PERSONAL ID                           
         MVI   ACTVLEN,ACTVLENQ    NEW LENGTH                                   
         DROP  R1                                                               
ACTELM30 DS    0H                                                               
         CLI   ELEM,X'F1'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADDELEM                                                          
         B     SRVRX                                                            
         LTORG                                                                  
         DROP  R4,R6                                                            
*                                                                               
*                                                                               
* BUILD/UPDATE TIME ELEMENT                                                     
*                                                                               
BTIME    NTR1                                                                   
         BRAS  RE,INITXSP          SET TO XSPOT                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         BE    BTIM20                                                           
                                                                                
         XC    ELEM,ELEM                                                        
         USING REVTMEL,R6                                                       
         LA    R6,ELEM                                                          
         USING REVTMEL,R6                                                       
         MVI   REVTMEL,X'70'                                                    
         MVI   REVTMLN,REVTMEQ                                                  
         EDIT  (TIME,NOW),(8,WORK)                                              
         MVC   REVTIME(2),WORK                                                  
         MVC   REVTIME+2(2),WORK+3                                              
         MVC   REVTIME+4(2),WORK+6                                              
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(5,0),(2,REVDATE)                                    
         MVI   REVTPG,REVASSG                                                   
         GOTO1 ADDELEM                                                          
         B     BTIMXIT                                                          
                                                                                
BTIM20   DS    0H                                                               
         MVC   REVTIME2,REVTIME1                                                
         MVC   REVDATE2,REVDATE1                                                
         MVC   REVTPG2,REVTPG1                                                  
         MVC   REVTIME1,REVTIME                                                 
         MVC   REVDATE1,REVDATE                                                 
         MVC   REVTPG1,REVTPG                                                   
         EDIT  (TIME,NOW),(8,WORK)                                              
         MVC   REVTIME(2),WORK                                                  
         MVC   REVTIME+2(2),WORK+3                                              
         MVC   REVTIME+4(2),WORK+6                                              
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(5,0),(2,REVDATE)                                    
         MVI   REVTPG,REVASSG                                                   
                                                                                
BTIMXIT  XIT1                                                                   
*                                                                               
* SEE IF THIS COMML HAS ADID AND MOVE TO UNIT & SET FLAG IF SO                  
*                                                                               
STADID   NTR1  BASE=*,LABEL=*                                                   
         OC    0(8,R1),0(R1)       IS THERE ANY COMML?                          
         BZ    STADIDX              NO, BYPASS                                  
*                                                                               
         CLC   0(8,R1),=C'REASSIGN' BYPASS REASSIGNS                            
         BE    STADIDX               YES, BYPASS                                
*                                                                               
         CLC   0(8,R1),=C'VIGNETTE' BYPASS VIGNETTE                             
         BE    STADIDX               YES, BYPASS                                
*                                                                               
         STC   R0,HALF             SAVE FLAG BIT                                
*                                                                               
         L     R3,AIO3                                                          
         CLC   0(4,R3),=C'*ID*'                                                 
         BE    STADID10                                                         
         DC    H'0'                                                             
*                                                                               
STADID10 DS   0H                                                                
         LA    R3,4(R3)            START OF ADID TABLE                          
         LA    RE,200              200 * 16 = 3200                              
*                                                                               
STADID20 DS   0H                                                                
         OC    0(16,R3),0(R3)      EMPTY SLOT?                                  
         BZ    STADID50             END OF TABLE                                
*                                                                               
         CLC   0(8,R3),0(R1)       SAME COMML?                                  
         BE    STADID30                                                         
         CLC   8(8,R3),0(R1)       IS IT A REAL ADID                            
         BE    STADID30            YES-                                         
*                                                                               
         LA    R3,16(,R3)          CK NEXT                                      
         BCT   RE,STADID20                                                      
         DC    H'0'                NEED LARGER TABLE                            
STADID30 DS   0H                                                                
         OC    8(8,R3),8(R3)       ANY ADID                                     
         BZ    STADIDX              NO                                          
*                                                                               
         MVC   0(8,R1),8(R3)       SAVE PACKED AD-ID CODE                       
         OC    0(1,RF),HALF                                                     
*                                                                               
STADIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
STADID50 DS   0H                                                                
         MVC   0(8,R3),0(R1)       SAVE DUMMY COMML CODE                        
         ST    R1,DUB              SAVE ADDR OF COMML                           
         ST    RF,DUB+4            SAVE ADDR BYTE                               
*                                                                               
         MVC   WORK(L'KEY),KEY                                                  
*                                                                               
         BRAS  RE,INITSPT          SWITCH TO SPOT                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD CMML KEY                               
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,0(R1)                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    STADID52                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   CMLKID,=X'0AC1'                                                  
         GOTO1 HIGH                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
STADID52 L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   STADID56                                                         
                                                                                
* DO NOT PUT ADID IN FOR ISCI CMML UNLESS IT'S A REAL ADID                      
                                                                                
         USING CMLADIEL,R6                                                      
         GOTO1 VTRPACK,DMCB,(C'U',CMLADIDP),WORK+20                             
         CLI   WORK+20+8,C' '      REAL ADIDS ARE >8 CHARS                      
         BNH   STADID56            SO EXIT IF NOT                               
*                                                                               
         MVC   8(8,R3),CMLADIDP    SAVE PACKED ADID COMML CODE                  
*                                                                               
STADID56 DS    0H                                                               
         MVC   KEY,WORK                                                         
         BRAS  RE,INITNET          SWITCH BACK TO NET                           
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R1,DUB              SAVE ADDR OF COMML                           
         L     RF,DUB+4            SAVE ADDR BYTE                               
         B     STADID30                                                         
         EJECT                                                                  
* SAVE CUTINS IN 03 ELEMENT, AND BUILD A COMML X'17' ELEM                       
*                                                                               
         USING UNTABLED,R5                                                      
SRCUT    NTR1  BASE=*,LABEL=*                                                   
         OC    UNTCML1,UNTCML1     ANY CML ASSIGNED                             
         BNZ   SRCUT10                                                          
*                                                                               
         SR    R2,R2               SAVE NEW INDEX (NONE)                        
         B     SRCUT50                                                          
*                                                                               
SRCUT10  DS    0H                                                               
         LA    R0,31               MAX COMMLS                                   
         LA    R1,BLOCK+2                                                       
         LA    R2,1                COMML COUNT INTO ELEM                        
*                                                                               
SRCUT20  DS    0H                                                               
         OC    0(8,R1),0(R1)       EMPTY SLOT?                                  
         BZ    SRCUT40              YES                                         
*                                                                               
         CLC   UNTCML1,0(R1)       THIS SAME AS PREV                            
         BE    SRCUT50                                                          
         LA    R1,8(,R1)                                                        
         LA    R2,1(,R2)                                                        
         BCT   R0,SRCUT20                                                       
         DC    H'0'                                                             
*                                                                               
SRCUT40  DS    0H                                                               
         MVC   0(8,R1),UNTCML1                                                  
         LLC   RE,BLOCK+1                                                       
         LA    RE,8(,RE)                                                        
         STC   RE,BLOCK+1                                                       
*                                                                               
* NOW FIND 03 ELEM                                                              
*                                                                               
SRCUT50  DS    0H                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,03                                                        
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUSPRD,R6                                                        
SRCUT60  DS    0H                                                               
         CLI   NUSPRTYP,C'U'       BYPASS ALL OTHERS                            
         BNE   SRCUT70                                                          
*                                                                               
         CLI   NUSPRLEN,NUSPRLN1   BYPASS OLD ELEMS                             
         BE    SRCUT70                                                          
*                                                                               
         CLC   NUSPRCIS,UNTCML3+3  MARKET/STATION                               
         BNE   SRCUT70                                                          
*                                                                               
         STC   R2,NUSPRCMI         STORE INDEX                                  
         MVC   NUSPRREV,REVISION                                                
*                                                                               
SRCUTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
SRCUT70  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    SRCUT60                                                          
         DC    H'0'                SHOULD FIND ELEM                             
         DROP  R5,R6                                                            
         EJECT                                                                  
* FIND NETWORK REVISION NUMBER *                                                
* READ ALL REVISION RECORDS FOR THAT PERIOD FOR ALL PROGRAMS *                  
*                                                                               
FNREVN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   HIREVNN,0                                                        
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         BRAS  RE,INITXSP          SET TO XSPOT                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              WAS KEYSAVE                                  
         USING REVXKEY,R4                                                       
         MVC   REVXKID,=X'0A1D'                                                 
         MVC   REVXKAM(3),BAGYMD AND BCLT                                       
         MVC   REVXKNET(4),NETWORK                                              
         MVC   REVXKPRG,PROGRAM                                                 
         MVC   REVXKPER,PERIOD                                                  
*                                                                               
         CLI   MYTN2PR6,C'W'       USING WEEKLY PERIOD                          
         BNE   FNREV02                                                          
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BNZ   FNREV02              YES, DATE IN PERIOD                         
         MVC   REVXKPER,STDATEP                                                 
*                                                                               
FNREV02  CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
         BNE   FNREV03                                                          
         CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   REVXKPRD,VOPTPROD                                                
*                                                                               
FNREV03  OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    *+10                                                             
         MVC   REVXKPGR(2),VOPTPRGR                                             
*                                                                               
FNREVN05 DS   0H                                                                
*****    GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'UNTDIR',KEYSAVE,KEY                   
         GOTO1 HIGH                                                             
*                                                                               
FNREV05C CLC   KEY(REVXKPRG-REVXKEY),KEYSAVE                                    
         BNE   XFNREVN             MUST BE ORIGINAL                             
                                                                                
         CLI   SVTN2PRO+00,C'*'    TRAFFIC BY PRODUCT                           
         BNE   FNREV05F                                                         
                                                                                
         CLI   VOPTPROD,0          RUNNING BY PRODUCT                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   REVXKPRD,VOPTPROD                                                
         BNE   FNREVN20                                                         
         B     FNREV05K                                                         
                                                                                
FNREV05F OC    VOPTPRGR,VOPTPRGR   RUNNING BY PRODUCT GROUP                     
         BZ    FNREV05K                                                         
         CLC   REVXKPGR(2),VOPTPRGR                                             
         BNE   FNREVN20                                                         
                                                                                
FNREV05K MVC   KEYSAVE,KEY                                                      
                                                                                
         CLI   MYTN2PR6,C'W'       USING WEEKLY PERIOD                          
         BNE   FNREV05P                                                         
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    *+14                                                             
         CLC   REVXKPER,PERIOD     YES, SAME PERIOD                             
         B     *+10                                                             
         CLC   REVXKPER,STDATEP    SAME PERIOD                                  
         BE    FNREVN10             YES, GO GET NET REV #                       
         BH    FNREV05M                                                         
         MVC   REVXKPER,STDATEP                                                 
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    *+10                                                             
         MVC   REVXKPER,PERIOD                                                  
         B     *+10                                                             
FNREV05M MVC   REVXKPER,=X'FFFF'   FORCE NEXT PROGRAM                           
*                                                                               
*        XC    REVXKNUM(4),REVXKNUM CLEAR THE REST OF THE KEY                   
         MVI   REVXKNUM,0           CLEAR REV NUMBER                            
         B     FNREVN05                                                         
*                                                                               
FNREV05P CLC   REVXKPER,PERIOD                                                  
         BE    FNREVN10            GO GET NET REV #                             
         BH    *+14                                                             
         MVC   REVXKPER,PERIOD                                                  
         B     *+10                                                             
         MVC   REVXKPER,=X'FFFF'   FORCE NEXT PROGRAM                           
*                                                                               
         MVI   REVXKNUM,0           CLEAR REV NUMBER                            
*        XC    REVXKNUM(4),REVXKNUM CLEAR THE REST OF THE KEY                   
         B     FNREVN05                                                         
*                                                                               
FNREVN10 DS   0H                                                                
         L     R6,AIO                                                           
         MVC   AIO,AIO1                                                         
***      GOTO1 (RF),(R1),=C'GETREC',=C'UNTFIL',KEY+21,(R6),DMWORK               
         GOTO1 GETREC                                                           
*                                                                               
         ST    R6,AIO                                                           
         L     R6,AIO1                                                          
*                                                                               
         CLC   KEY(16),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,42(,R6)                                                       
         CLI   0(R6),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
         CLI   REVDTALN,12         OLD REV REC                                  
         BE    FNREVN20                                                         
         CLC   HIREVNN,REVNNUM                                                  
         BNL   FNREVN20                                                         
         MVC   HIREVNN,REVNNUM     SAVE REV NET NUM                             
*                                                                               
FNREVN20 DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
****     GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'UNTDIR',KEYSAVE,KEY                   
         GOTO1 SEQ                                                              
         B     FNREV05C                                                         
*                                                                               
XFNREVN  DS    0H                                                               
         CLC   HIREVNN,OREVNN                                                   
         BNL   *+10                                                             
         MVC   HIREVNN,OREVNN      SAVE HIGHEST NET REV #                       
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
* VALIDATE PERIOD - MONTH AND YR, AND DEVELOP                                   
*                   CALENDAR OR BROADCAST START/END DATES                       
*                                  BROADCAST MONTH                              
*                                                                               
VPER     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,TRAPERH          VALIDATE PERIOD                              
         XC    DATES,DATES                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERRB                                                         
*                                                                               
VPER02   LA    R4,TRAPER                                                        
         CLI   TRAPER,C'?'         QUESTION MARK HELP                           
         BNE   VPER04                                                           
         LA    R4,1(,R4)                                                        
*                                                                               
* MUST ENTER DATE - THEN WILL SHOW CALENDAR/BROADCAST START/END *               
*                                                                               
VPER04   GOTO1 DATVAL,DMCB,(0,(R4)),STDATE CK FOR M/D/Y                         
         OC    DMCB(4),DMCB                                                     
         BZ    VPER06                                                           
         CLI   MYTN2PR6,C'W'       INSTR PERIOD = WEEKLY                        
         BNE   PERDATER                                                         
*                                                                               
         GOTO1 GETDAY,(R1),(0,STDATE),WORK                                      
         CLI   0(R1),1             MUST BE MONDAY                               
         BNE   DAYDATER                                                         
         GOTO1 ADDAY,(R1),STDATE,ENDATE,F'6'                                    
         GOTO1 DATCON,(R1),(0,STDATE),(3,PERIOD)                                
*                                                                               
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    VPER30               NO                                          
         MVC   WORK(6),STDATE                                                   
         BRAS  RE,CONVPER                                                       
         MVC   PERIOD,CONVDTE                                                   
         B     VPER30                                                           
*                                                                               
VPER06   GOTO1 DATVAL,DMCB,(2,(R4)),STDATE                                      
         OC    DMCB(4),DMCB                                                     
         BZ    PERDATER                                                         
         CLI   MYTN2PR6,C'W'       INSTR PERIOD = WEEKLY                        
         BE    WKDATER                                                          
         GOTO1 DATCON,(R1),(0,STDATE),(3,PERIOD)                                
         CLI   MYTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BE    VPER20                                                           
         CLI   MYTN2PR6,C'C'       TRAFFIC OVERRIDE CALENDAR MONTH              
         BE    VPER10                                                           
         CLI   NBUSER+3,C'B'       BROADCAST MONTH                              
         BE    VPER20                                                           
         MVI   MYTN2PR6,C'C'       CALENDAR MONTH                               
*                                                                               
* CALC CALENDAR START/END OF MONTH DATES *                                      
*                                                                               
VPER10   MVC   STDATE+4(2),=C'01'                                               
         GOTO1 ADDAY,(R1),STDATE,ENDATE,F'31'                                   
VPER14   GOTO1 (RF),(R1),ENDATE,ENDATE,F'-1'                                    
         CLC   STDATE(4),ENDATE                                                 
         BNE   VPER14                                                           
         B     VPER30                                                           
*                                                                               
VPER20   DS    0H                                                               
         MVI   MYTN2PR6,C'B'       BROADCAST MONTH                              
         MVC   STDATE+4(2),=C'15'                                               
         MVC   WORK(6),STDATE                                                   
         GOTO1 VGTBROAD,DMCB,(1,WORK),STDATE,GETDAY,ADDAY                       
         FIXDT02                                                                
VPER30   GOTO1 DATCON,(R1),(0,STDATE),(2,STDATEP)                               
         FIXDT02                                                                
         GOTO1 (RF),(R1),(0,ENDATE),(2,ENDATEP)                                 
         GOTO1 (RF),(R1),(0,STDATE),(3,STDATEB)                                 
         GOTO1 (RF),(R1),(0,ENDATE),(3,ENDATEB)                                 
*                                                                               
         CLI   TRAPER,C'?'         QUESTION MARK HELP                           
         BNE   VPERX                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   GERROR,=Y(PERHLP)                                                
         MVI   GMSGTYPE,C'I'                                                    
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         STCM  R4,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R4),15            L'SUBST TEXT + 1                             
         MVC   1(14,R4),=C'CALENDAR MONTH'                                      
*                                                                               
         CLI   MYTN2PR6,C'W'       INSTR PERIOD = WEEKLY                        
         BE    VPER44                                                           
         CLI   MYTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BE    VPER42                                                           
         CLI   MYTN2PR6,C'C'       TRAFFIC OVERRIDE CALENDAR MONTH              
         BE    VPER46                                                           
         CLI   NBUSER+4,C'B'       BROADCAST MONTH                              
         BNE   VPER46                                                           
VPER42   MVI   0(R4),16            L'SUBST TEXT + 1                             
         MVC   1(15,R4),=C'BROADCAST MONTH'                                     
         B     VPER46                                                           
VPER44   MVI   0(R4),14            L'SUBST TEXT + 1                             
         MVC   1(13,R4),=C'CALENDAR WEEK'                                       
*                                                                               
VPER46   LLC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         MVI   0(R4),9             L'SUBST TEXT + 1                             
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,STDATEP),(5,1(R4))                                
         LA    R4,9(R4)                                                         
         MVI   0(R4),9             L'SUBST TEXT + 1                             
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,ENDATEP),(5,1(R4))                                  
         GOTO1 VTRAERR                                                          
*                                                                               
VPERX    OI    4(R2),X'20'         SET VALIDATED                                
VPERXX   XIT1                                                                   
*                                                                               
MISSERRB MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
PERDATER MVC   GERROR,=Y(ONLYMMYY)                                              
         CLI   MYTN2PR6,C'W'       INSTR PERIOD = WEEKLY                        
         BE    WKDATER                                                          
         B     VPERERX                                                          
DAYDATER MVC   GERROR,=Y(MONSTDT)                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         MVI   ELEM,4              L'SUBST TEXT + 1                             
         MVC   ELEM+1(3),WORK                                                   
         B     VPERERX                                                          
WKDATER  MVC   GERROR,=Y(WKDTER)                                                
VPERERX  GOTO1 VTRAERR                                                          
         LTORG                                                                  
*                                                                               
*                                                                               
****************************************************************                
* ON ENTRY WORK CONTAINS PERIOD (YYMMDD)                                        
* CONVERT PERIOD INTO CONVDTE:                                                  
* 1ST BYTE = YEAR                                                               
* 2ND BYTE = HOB MONTH AND LOB WEEK NUMBER FOR THAT MONTH                       
* EG. 1/27/20 CONVERT X'7814' YEAR= X'78' MONTH=1 WEEK=4                        
****************************************************************                
CONVPER  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(3,WORK+6) YYMMDD TO YMD                    
*                                                                               
         MVC   CONVDTE,WORK+6      SAVE YM                                      
*                                                                               
* CALC WEEK NUMBER FOR THIS MONTH                                               
         SR    R5,R5               INIT WEEK NUMBER                             
         MVC   DUB(6),WORK                                                      
CONV02   AHI   R5,1                INCR WEEK NUMBER                             
         GOTO1 ADDAY,(R1),DUB,DUB,F'-7'                                         
         CLC   WORK+2(2),DUB+2     SAME MONTH?                                  
         BE    CONV02                                                           
*                                                                               
         SLL   R5,28               WEEK NUMBER IN HOB                           
         LLC   R4,CONVDTE+1                                                     
         SLDL  R4,28               HOB MONTH/LOB WEEK NUMBER                    
         STCM  R4,8,CONVDTE+1      MONTH/WEEK NO (X'C4'= DEC/WEEK 4)            
         XIT1                                                                   
*                                                                               
*                                                                               
* PRINT ERROR MESSAGE TO INDICATE WHICH PRODUCTS ARE VALID FOR                  
* THIS COPY SPLIT UNIT.                                                         
*                                                                               
CSPRDER  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   GERROR,=Y(CSVALPRD)                                              
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST        A(SUBST TEXT)                                
         LA    RE,ELEM+1           MAX COPY SPLITS                              
         LA    RF,(L'UNTCSPRO/3)   MAX COPY SPLITS                              
         USING UNTABLED,R5                                                      
         LA    R4,UNTCSPRO                                                      
*                                                                               
CSPRD10  DS   0H                                                                
         MVC   0(3,RE),0(R4)                                                    
         LA    RE,2(,RE)                                                        
         CLI   0(RE),C' '                                                       
         BNH   *+8                                                              
         LA    RE,1(,RE)                                                        
*                                                                               
         MVI   0(RE),C'/'                                                       
         LA    RE,1(,RE)                                                        
*                                                                               
         LA    R4,3(,R4)                                                        
         CLI   0(R4),0                                                          
         BE    CSPRD40                                                          
         BCT   RF,CSPRD10                                                       
CSPRD40  LA    R0,ELEM                                                          
         SR    RE,R0                                                            
         BCTR  RE,0                SUBTRACT 1 FOR LAST SLASH                    
         STC   RE,ELEM                                                          
*                                                                               
         GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* SUBROUTINE TO READ PATTERN RECORDS FOR UNITABLE ENTRIES                       
*============================================================                   
                                                                                
BLPAT    NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         BAS   RE,PALPHA           MAKE SURE PRODS IN ALPHA ORDER               
*                                                                               
         TM    UNITSW1,X'02'       RUNNING FULL PERIOD                          
         BO    PATPERER             NO ONLY PART                                
*                                                                               
         L     R2,AIO3                                                          
         XC    0(L'PATENT+1,R2),0(R2) CLEAR ONE ENTRY                           
         ST    R2,DUB                                                           
*                                                                               
         BRAS  RE,INITXSP          SET TO XSPOT                                 
*                                                                               
* FIRST READ ANY ALL NETWORK PATTERNS AND MEDIA SPECIFIC PATTERNS *             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPTKEY,R4                                                        
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM(3),BAGYMD     A-M/CLT                                     
         MVI   NPTXR3F,X'A0'       BYPASS ANY SAVED PTNS                        
*                                                                               
BLP06    DS   0H                                                                
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(5),KEYSAVE      SAME TYPE/A-M/CLT                            
         BNE   BLP30                                                            
                                                                                
         CLC   KEY(9),KEYSAVE                                                   
         BNE   BLP20F                                                           
                                                                                
*                                                                               
         OC    KEY+5(27),KEY+5     THIS A PTN SEQ REC                           
         BZ    BLP10                                                            
*                                                                               
         CLI   KEY+NPTXR3F-NPTXKEY,X'A0'  THIS SAVED PTN                        
         BL    BLP10                                                            
         B     BLP20                                                            
*                                                                               
BLP10    DS   0H                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(5),KEYSAVE      SAME TYPE/A-M/CLT                            
         BNE   BLP30                                                            
                                                                                
         CLC   KEY(9),KEYSAVE                                                   
         BNE   BLP20F                                                           
                                                                                
*                                                                               
         OC    KEY+5(27),KEY+5     THIS A PTN SEQ REC                           
         BZ    BLP10                                                            
*                                                                               
         CLI   KEY+NPTXR3F-NPTXKEY,X'A0'     THIS SAVED PTN                     
         BL    BLP10                                                            
*                                                                               
BLP20    CLC   KEY(9),KEYSAVE                                                   
         BE    BLP24                                                            
         CLC   KEY(5),KEYSAVE      SAME TYPE/A-M/CLT                            
         BNE   BLP30                                                            
BLP20F   CLI   NPTXNET,X'00'       MEDIA SPECIFIC?                              
         BNE   BLP21                                                            
         CLI   NPTXNET+2,X'FF'     MEDIA SPECIFIC                               
         BNE   BLP21                                                            
         CLC   NPTXNET+1(1),SVMEDIA    THIS MEDIA?                              
         BNE   BLP20H                                                           
         CLI   NPTXR3F,X'A0'                                                    
         BL    BLP10                                                            
         B     BLP24                                                            
BLP20H   MVI   NPTXNET+3,X'FF'     SKIP THIS MEDIA AND DO READHI                
         B     BLP06                                                            
                                                                                
BLP21    CLC   NPTXNET,NETWORK                                                  
         BH    BLP30                                                            
         BNE   BLP21F                                                           
         CLI   NPTXR3F,X'A0'                                                    
         BL    BLP10                                                            
         B     BLP22                                                            
*                                                                               
* NOW READ NETWORK SPECIFIC PATTERN RECORDS *                                   
*                                                                               
BLP21F   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPTKEY,R4                                                        
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM(3),BAGYMD     A-M/CLT                                     
         MVC   NPTXNET(4),NETWORK     PRD/SLN  PTR/SLN                          
         B     BLP06                                                            
*                                                                               
BLP22    MVC   KEYSAVE,KEY         SAVE NETWORK SPECIFIC KEY                    
*                                                                               
BLP24    L     R6,AIO2             SET I/O AREA ADDRESS                         
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVKEY,KEY           SAVE PATTERN KEY                             
*                                                                               
         CLI   NPTXNET-NPTXKEY(R6),C'$' TEST NETWORK LIST REC                   
         BNE   *+12                                                             
         BRAS  RE,CKPATLST         SEE IF NETWORK DELETED                       
         BNE   BLP28                                                            
*                                                                               
         BAS   RE,BLENT           BUILD PATTERN TABLE ENTRY                     
*                                                                               
BLP28    DS    0H                                                               
         GOTO1 HIGH                GET BACK INTO SEQ READ FOR PATTERN           
         B     BLP10                                                            
*                                                                               
BLP30    BAS   RE,BLSORT           SORT PATTERN LIST                            
         CLI   ERROR,0                                                          
         BNE   BLPX                                                             
*                                                                               
BLPX     BRAS  RE,INITSPT                                                       
         BAS   RE,BALPHA           PUT PRODS BACK IN ITS ORDER                  
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*===================================================================            
* PUT P/B PRODUCTS IN ALPHA ORDER/SWAP PRODS/LEN/CMLS AND CML FLAGS             
*===================================================================            
                                                                                
*                                                                               
PALPHA   NTR1                                                                   
*                                                                               
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
         CLI   0(R5),0             END OF TABLE                                 
         JE    EXIT                                                             
*                                                                               
PA10     OC    UNTPROD2,UNTPROD2   ANY PROD2                                    
         BZ    PA20                                                             
         CLC   UNTPROD,UNTPROD2    ARE PROD IN ORDER?                           
         BNH   PA20                                                             
*                                                                               
         OI    UNTFLAG2,UNTFL2SW   SET PRODS SWAPPED                            
*                                                                               
         XC    UNTPROD,UNTPROD2                                                 
         XC    UNTPROD2,UNTPROD                                                 
         XC    UNTPROD,UNTPROD2                                                 
*                                                                               
         XC    UNTSLN,UNTSLN2                                                   
         XC    UNTSLN2,UNTSLN                                                   
         XC    UNTSLN,UNTSLN2                                                   
*                                                                               
         XC    UNTCML1,UNTCML2     SWAP CMLS ALSO                               
         XC    UNTCML2,UNTCML1                                                  
         XC    UNTCML1,UNTCML2                                                  
* NEED TO SWAP ADID FLAGS TOO!                                                  
         MVC   BYTE,UNTCMLF                                                     
         NI    BYTE,X'3F'                                                       
         TM    UNTCMLF,X'80'                                                    
         BZ    *+8                                                              
         OI    BYTE,X'40'                                                       
         TM    UNTCMLF,X'40'                                                    
         BZ    *+8                                                              
         OI    BYTE,X'80'                                                       
         MVC   UNTCMLF,BYTE                                                     
*                                                                               
PA20     LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             END OF TABLE                                 
         BNE   PA10                                                             
*                                                                               
         J     EXIT                                                             
*                                                                               
*=============================================                                  
* PUT P/B PRODUCTS BACK IN ITS ORIGINAL ORDER                                   
*=============================================                                  
*                                                                               
*                                                                               
BALPHA   NTR1                                                                   
*                                                                               
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
         CLI   0(R5),0             END OF TABLE                                 
         JE    EXIT                                                             
*                                                                               
BA10     TM    UNTFLAG2,UNTFL2SW   WERE PRODS SWAPPED                           
         BZ    BA20                                                             
*                                                                               
         NI    UNTFLAG2,X'FF'-UNTFL2SW   WERE PRODS SWAPPED                     
*                                                                               
         XC    UNTPROD,UNTPROD2                                                 
         XC    UNTPROD2,UNTPROD                                                 
         XC    UNTPROD,UNTPROD2                                                 
*                                                                               
         XC    UNTSLN,UNTSLN2                                                   
         XC    UNTSLN2,UNTSLN                                                   
         XC    UNTSLN,UNTSLN2                                                   
*                                                                               
         XC    UNTCML1,UNTCML2     SWAP CMLS ALSO                               
         XC    UNTCML2,UNTCML1                                                  
         XC    UNTCML1,UNTCML2                                                  
* NEED TO SWAP ADID FLAGS TOO!                                                  
         MVC   BYTE,UNTCMLF                                                     
         NI    BYTE,X'3F'                                                       
         TM    UNTCMLF,X'80'                                                    
         BZ    *+8                                                              
         OI    BYTE,X'40'                                                       
         TM    UNTCMLF,X'40'                                                    
         BZ    *+8                                                              
         OI    BYTE,X'80'                                                       
         MVC   UNTCMLF,BYTE                                                     
*                                                                               
BA20     LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             END OF TABLE                                 
         BNE   BA10                                                             
*                                                                               
         J     EXIT                                                             
*                                                                               
*=================================================================              
* SUBROUTINE TO BUILD A PATTERN LIST ENTRY FROM PATTERN RECORD                  
*=================================================================              
                                                                                
BLENT    NTR1                                                                   
         L     R2,DUB                                                           
         XC    0(L'PATENT+1,R2),0(R2)    CLEAR 1 ENTRY + 1 BYTE                 
*                                                                               
         LR    R4,R6                                                            
         USING NPTXKEY,R4                                                       
         OC    NPTXPROG,NPTXPROG     PROGRAM SPECIFIC                           
         BZ    BLE10                                                            
         CLC   PROGRAM,NPTXPROG   THIS PROGRAM                                  
         BE    BLE10                YES                                         
         CLI   NPTXPROG,X'FF'                                                   
         BNE   BLEX                                                             
*                                                                               
BLE10    L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
*                                                                               
         CLC   ENDATEB,NPTSTART    PERIOD END BEFORE PATTERN START              
         BL    BLEX                                                             
         CLC   STDATEB,NPTEND      PERIOD START AFTER PATTERN END               
         BH    BLEX                                                             
         MVI   ELEM+6,0                                                         
         TM    NPTSTAT,X'80'       TEST STATUS = DELETED                        
         BO    BLEX                 YES - IGNORE                                
*                                                                               
* DO NOT BUILD PATTERN RECORD FOR PREVIOUSLY ASSIGNED CML                       
*                                                                               
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
BLENT14  CLI   UNTADTEP,0          END OF TABLE                                 
         BE    BLEX                                                             
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS PREV ASSIGNED CMLS                    
         BNE   BLENT15                                                          
         TM    UNTFLAG2,UNTFL2AS   WAS THIS PREV ASSIGNED                       
         BO    BLENT16              YES BYPASS                                  
*                                                                               
         TM    UNTFLAG4,UNTFLCUT   IS IT A CUTIN                                
         BO    BLENT16              YES BYPASS                                  
*                                                                               
         TM    UNTFLAG4,UNTFLSEC   IS IT SECTIONAL FEED                         
         BO    BLENT16              YES BYPASS                                  
*                                                                               
BLENT15  DS    0H                                                               
         CLC   UNTPROD,NPTXPRD     THIS SAME PRD                                
         BNE   BLENT16                                                          
         CLC   UNTPROD2,NPTXPRD2   THIS SAME PTR                                
         BE    BLENT18                                                          
BLENT16  LA    R5,UNTNEXT                                                       
         B     BLENT14                                                          
*                                                                               
BLENT18  DS    0H                                                               
         L     R2,DUB                                                           
         LA    R2,0(,R2)            CLEAR HOB                                   
         USING PATABLED,R2                                                      
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE PATTERN KEY                          
                                                                                
* ADD ENTRY TO PATTERN LIST IF SPACE *                                          
                                                                                
         L     R0,AIO3                                                          
         AHI   R0,2000-L'PATENT+1+8                                             
         CR    R2,R0                     TEST ROOM IN LIST                      
         BH    NOPTNRM                                                          
*                                                                               
         XC    0(L'PATENT+1,R2),0(R2)    CLEAR 1 ENTRY + 1 BYTE                 
         LLC   RE,DUB              BUMP SEQUENCE NUMBER                         
         LA    RE,1(RE)                                                         
         STC   RE,DUB              AND SAVE                                     
         CLI   DUB,X'FF'                                                        
         BL    *+6                                                              
         DC    H'0'                NO MORE PATTERNS                             
*                                                                               
         OC    NPTXNET,NPTXNET     THIS ALL NETWORKS PATTERN                    
         BNZ   BLE20                NO                                          
         MVI   ELCODE,PATQALL      SET ALL NETS IND                             
         B     BLE50                                                            
*                                                                               
BLE20    CLI   NPTXNET,X'00'       IS THIS MEDIA SPECIFIC PATTERN               
         BNE   BLE22                NO                                          
         CLI   NPTXNET+2,X'FF'                                                  
         BNE   BLE22                NO                                          
         MVI   ELCODE,PATQMED      SET BY MEDIA                                 
*                                                                               
         CLI   NPTXPROG,X'FF'      NOW CHECK IF ALSO DAYPART                    
         BNE   BLE50                                                            
         MVI   ELCODE,PATQDPMD     SET BY MEDIA AND DAYPART                     
         MVC   PATDP,NPTXPROG+1                                                 
         B     BLE50                                                            
*                                                                               
BLE22    OC    NPTXPROG,NPTXPROG    THIS ALL PROGRAMS PATTERN                   
         BNZ   BLE24                 NO                                         
         MVI   ELCODE,PATQNET      SET ALL PGMS IND                             
         B     BLE50                                                            
*                                                                               
BLE24    CLI   NPTXPROG,X'FF'       THIS FEED AND/OR DAYPART CD                 
         BE    BLE30                YES                                         
         MVI   ELCODE,PATQPROG     SET AS PROGRAM PATTERN                       
         B     BLE50                                                            
*                                                                               
BLE30    OC    NPTXPROG+2(4),NPTXPROG+2 THIS A FEED PATTERN                     
         BNZ   BLE40                     YES                                    
         MVI   ELCODE,PATQDPT      SET AS DAYPART PATTERN                       
         B     BLE46                                                            
*                                                                               
BLE40    MVC   PATFEED,NPTXPROG+2                                               
*                                                                               
         CLI   NPTXPROG+1,0        THIS A FEED DAYPART PATTERN                  
         BNE   BLE44                     YES                                    
         MVI   ELCODE,PATQFEED     SET AS FEED ONLY                             
         B     BLE50                                                            
*                                                                               
BLE44    MVI   ELCODE,PATQFDDP     SET AS FEED AND DAYPART                      
*                                                                               
BLE46    MVC   PATDP,NPTXPROG+1                                                 
*                                                                               
BLE50    MVC   PATSEQ,DUB              MOVE SEQUENCE NUMBER                     
         MVC   PATREF,NPTS3QNO         REF/SUBLINE                              
         MVC   PATPROD1(8),NPTXPRD     PRD/SLN/PTR/SLN                          
         MVC   PATYPE,ELCODE           SET ENTRY TYPE                           
         MVC   PATDSKA,KEY+36          AND DISK ADDRESS                         
*                                                                               
         TM    NPTSTAT,NPTS_TIME       ELEM BIG ENOUGH TO HAVE TIME             
         BZ    *+10                    NO                                       
         MVC   PATSTIM(4),NPTSTIM      MOVE START/END TIMES                     
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(3,NPTSTART),(2,PATSTR)                              
         CLC   NPTEND,=X'FFFFFF'                                                
         BNE   BLE56                                                            
         MVC   PATEND,ENDATEP          MOVE END PERIOD DATE                     
         OI    PATFLG,PATFLUFN     SET FLAG                                     
         B     BLE60                                                            
*                                                                               
         FIXDT02                                                                
BLE56    GOTO1 DATCON,DMCB,(3,NPTEND),(2,PATEND)                                
*                                                                               
BLE60    LA    R2,L'PATENT(,R2)        NEXT PATTERN LIST ENTRY                  
         STCM  R2,7,DUB+1                                                       
BLEX     XIT1                                                                   
         DROP  R2,R4,R6                                                         
*                                                                               
PATPERER MVC   GERROR,=Y(PATPER)                                                
         B     VTERRX                                                           
*                                                                               
NOPTNRM  MVC   GERROR,=Y(MANYPATT)                                              
         LA    R2,TRAOPT1H                                                      
*                                                                               
VTERRX   GOTO1 VTRAERR                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* SEE IF NETWORK IN KEY+6 IS DELETED IN A 5B PATTERN LIST ELEM                  
*================================================================               
                                                                                
CKPATLST NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'5B'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CKPAT2   BRAS  RE,NEXTEL                                                        
         JNE   NEQXIT                                                           
*                                                                               
         CLC   NPTXNET-NPTXKEY+KEY(4),2(R6)   RIGHT NETWORK ELEM                
         BNE   CKPAT2                                                           
*                                                                               
         TM    6(R6),X'80'         TEST NETWORK DELETED                         
         JO    NEQXIT                                                           
         J     EQXIT                                                            
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* SUBROUTINE TO APPLY PATTERN TO UNITS                                          
*================================================================               
                                                                                
BLSORT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AIO3                                                          
         SR    R0,R0                                                            
         LR    R1,R2                                                            
*                                                                               
BLS10    CLI   0(R1),0                                                          
         BE    BLS14                                                            
         LA    R1,L'PATENT(,R1)                                                 
         BCTR  R0,0                                                             
         B     BLS10                                                            
*                                                                               
BLS14    LPR   R0,R0                                                            
         BNZ   *+16                                                             
         CLI   SVTN1PR9,C'Y'       BYPASS PREV ASSIGNED CMLS                    
         BNE   PATERR               NO, NO PATTERNS                             
         BE    BLSX                                                             
*                                                                               
         GOTO1 QSORT,DMCB,(R2),(R0),L'PATENT,L'PATSRT,0                         
*                                                                               
* NOW REDO PAT SEQNUMS                                                          
*                                                                               
         L     R2,AIO3                                                          
         USING PATABLED,R2                                                      
         LA    R4,1                                                             
*                                                                               
BLS15    STC   R4,PATSEQ                                                        
         LA    R4,1(R4)                                                         
         LA    R2,PATNEXT                                                       
         BCT   R0,BLS15                                                         
*                                                                               
         LA    R5,UNITABLE                                                      
         USING UNTABLED,R5                                                      
         OI    UNITSW1,X'08'       SET ON NO PATTERNS FOUND                     
*                                                                               
* GO THRU UNIT TABLE, ASSIGNING AND MARKING PATTERNS USED *                     
*                                                                               
BLS20    L     R2,AIO3             START OF PATTERN TABLE                       
         SR    R6,R6               CLEAR PATTERN POINTER                        
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS PREV ASSIGNED CMLS                    
         BNE   *+12                                                             
         TM    UNTFLAG2,UNTFL2AS   WAS THIS PREV ASSIGNED                       
         BO    BLS44                YES - SKIP UNIT                             
*                                                                               
         TM    UNTFLAG4,UNTFLSEC   IS IT SECTIONAL FEED                         
         BO    BLS44                YES - SKIP UNIT                             
*                                                                               
         TM    UNTFLAG4,UNTFLCUT   IS IT A CUTIN                                
         BO    BLS44                YES - SKIP UNIT                             
                                                                                
* MOVE UNIT TIME FROM TABLE ENTRY TO FULL SO IT'S AVAILABLE                     
                                                                                
         LA    R4,TIMETBL          START OF TIME TABLE                          
         LLC   R1,UNTTIME          AND DISPLACEMENT+1                           
         BCTR  R1,0                                                             
         AR    R4,R1                                                            
         MVC   FULL,0(R4)          MOVE UNIT TIME                               
*                                                                               
BLS22    DS    0H                                                               
         OC    UNTFEED,UNTFEED     THIS A FEED UNIT                             
         BZ    BLS24                NO                                          
         CLC   =X'00FFE3',UNTFEED  CHK FOR TAG                                  
         BE    BLS24               ITS A TAG NOT A FEED                         
*                                                                               
         CLI   PATYPE,PATQFEED     THIS A FEED OR FEED/DP PATTN                 
         BH    BLS40                                                            
         CLC   PATFEED,UNTFEED                                                  
         BNE   BLS40                                                            
*                                                                               
         CLI   PATDP,0             DAYPART CODE ALSO                            
         BE    BLS30                NO                                          
         CLC   PATDP,UNTDP                                                      
         BE    BLS30                                                            
         B     BLS40                                                            
*                                                                               
BLS24    DS    0H                                                               
BLS26    OC    PATFEED,PATFEED     THIS A FEED PATTERN                          
         BNZ   BLS40                YES                                         
         CLI   PATDP,0                                                          
         BE    BLS30                                                            
         CLC   PATDP,UNTDP                                                      
         BNE   BLS40                                                            
                                                                                
*=============================================================                  
* FIND LEAST USED PATTERN ENTRY FOR THIS UNIT                                   
*=============================================================                  
                                                                                
BLS30    CLC   UNTPROD(8),PATPROD1   PATTERN FOR RIGHT PRDS                     
         BNE   BLS40                 NO - NEXT PATTERN                          
*                                                                               
         CLC   UNTADTEP,PATEND     UNIT START DATE AFTER PATTERN END            
         BH    BLS40                                                            
         CLC   UNTADTE2,PATSTR     UNIT END BEFORE PATTERN START                
         BL    BLS40                                                            
*                                                                               
         OC    PATSTIM,PATSTIM      TEST PATTERN HAS TIME                       
         BZ    BLS36                NO - USE IT                                 
*                                                                               
         CLC   UNTADTEP,UNTADTE2     TEST ONE DAY UNIT                          
         BNE   BLS36                 NO - USE IT                                
                                                                                
* IT'S A ONE DAY UNIT                                                           
                                                                                
         MVC   MYPSTIM(4),PATSTIM  MOVE PATTERN TIMES                           
         CLC   MYPSTIM,=H'2400'    TEST FOR MIDNIGHT                            
         BNE   *+10                                                             
         XC    MYPSTIM,MYPSTIM                                                  
*                                                                               
         CLC   MYPSTIM,=H'600'     TEST PAT STARTS AFTER 6A                     
         BL    BLS31A                                                           
         LH    RE,MYPETIM                                                       
         CLC   MYPETIM,=H'600'     AND PAT ENDS BEFORE 6A                       
         BH    *+8                                                              
         LA    RE,2400(RE)                                                      
         STH   RE,MYPETIM                                                       
*                                                                               
BLS31A   LA    RE,TIMETBL          START OF TIME TABLE                          
         LLC   RF,UNTTIME          AND DISPLACEMENT+1                           
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         MVC   MYUSTIM(4),0(RE)    MOVE UNIT TIMES                              
*                                                                               
         CLC   MYPSTIM,=H'600'     TEST PAT STARTS AFTER 6A                     
         BL    BLS31B              NO                                           
*                                                                               
         LH    RE,MYUSTIM                                                       
         CLC   MYUSTIM,=H'600'     TEST UNIT STARTS AFTER 6A                    
         BNL   *+8                                                              
         LA    RE,2400(RE)                                                      
         STH   RE,MYUSTIM                                                       
*                                                                               
         LH    RE,MYUETIM                                                       
         CLC   MYUETIM,=H'600'                                                  
         BH    *+8                                                              
         LA    RE,2400(RE)                                                      
         STH   RE,MYUETIM                                                       
*                                                                               
BLS31B   CLC   UNTADTEP,PATSTR       UNIT DATE MATCH PATT START                 
         BNE   BLS32                                                            
         CLC   PATSTR,PATEND         TEST ONE DAY PATTERN                       
         BE    BLS31C                                                           
         CLC   MYUETIM,MYPSTIM       UNIT END TIME BEFORE PATT START            
         BL    BLS40                 YES - SKIP                                 
         B     BLS36                 ELSE USE IT                                
*                                                                               
* ONE DAY UNIT AND ONE DAY PATTERN                                              
*                                                                               
BLS31C   CLC   MYUSTIM,MYPETIM       UNIT START TIME AFTER PATT END             
         BH    BLS40                 YES - SKIP                                 
         CLC   MYUETIM,MYPSTIM       UNIT END TIME BEFORE PATT START            
         BL    BLS40                                                            
         B     BLS36                 USE THIS PATTERN                           
*                                                                               
* ONE DAY UNIT NOT ON PATTERN START DATE                                        
*                                                                               
BLS32    CLC   UNTADTEP,PATEND       UNIT DATE MATCH PATTERN END                
         BL    BLS36                 BEFORE END DATE - USE IT                   
         CLC   MYUSTIM,MYPETIM       UNIT START TIME AFTER PATTERN END          
         BH    BLS40                 YES - SKIP                                 
*                                                                               
BLS36    MVC   WORK(2),UNTADTEP    MOVE UNIT START/END                          
         MVC   WORK+2(2),UNTADTE2                                               
*                                                                               
         CLC   WORK(2),PATSTR      UNIT START BEFORE PATTERN START              
         BNL   *+10                NO                                           
         MVC   WORK(2),PATSTR      YES - SAVE PATTERN START                     
*                                                                               
         CLC   WORK+2(2),PATEND    UNIT END BEFORE PATTERN END                  
         BL    *+10                NO                                           
         MVC   WORK+2(2),PATEND    YES - SAVE PATTERN END                       
*                                                                               
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,WORK),WORK+8                                      
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,WORK+2),WORK+14                                     
         GOTO1 PERVERT,DMCB,WORK+8,WORK+14                                      
         LH    R0,DMCB+8                                                        
         STC   R0,PATCOVER         AND SAVE IT                                  
*                                                                               
         LTR   R6,R6               TEST FOUND AN ENTRY YET                      
         BNZ   *+6                 YES                                          
         LR    R6,R2               SAVE PATTERN TABLE ENTRY ADDRESS             
*                                                                               
         CR    R6,R2               SO CAN PATCH                                 
         BE    *+6                                                              
         DC    X'0700'                                                          
*                                                                               
         CLC   PATYPE,PATYPE-PATENT(R6) TEST SAME LEVEL PATTERNS                
         BNE   BLS37                    NO                                      
*                                                                               
         LLC   RE,PATCNT                GET NEW USE COUNT                       
         LA    RF,1000                                                          
         MR    RE,RE                    SCALE UP                                
         LLC   R0,PATCOVER              GET NEW DAY COUNT                       
         DR    RE,R0                    CALCULATE COUNT/DAYS                    
         ST    RF,DUB                   AND SAVE IT                             
*                                                                               
         LLC   RE,PATCNT-PATENT(R6)     GET OLD USE COUNT                       
         LA    RF,1000                  SCALE UP                                
         MR    RE,RE                                                            
         LLC   R0,PATCOVER-PATENT(R6)   GET OLD DAY COUNT                       
         DR    RE,R0                                                            
*                                                                               
         C     RF,DUB                   COMPARE RATIOS                          
         BH    BLS38                    OLD HIGHER - USE NEW                    
         BL    BLS40                    OLD LOWER - SKIP IT                     
         LH    R0,DMCB+8                EQUAL - RESTORE R0 AND GO ON            
*                                                                               
BLS37    CLC   PATCOVER,PATCOVER-PATENT(R6)  NEW PATT COVER MORE DAYS?          
         BL    BLS40                    NO SKIP IT                              
         BH    BLS38                    IF HIGH - USE IT                        
*                                                                               
         CLC   PATYPE,PATYPE-PATENT(R6)   COMPARE PATTERN TYPES                 
         BH    BLS40                      HI IS LESS SPECIFIC - SKIP            
         BL    BLS38                      LO IS MORE SPECIFIC - USE IT          
         CLC   PATCNT,PATCNT-PATENT(R6)   COMPARE COUNTERS                      
         BH    BLS40                                                            
         BL    BLS38                                                            
* IF EQUAL USAGE, USE EARLIER START DATE                                        
         CLC   PATSTR,PATSTR-PATENT(R6)   IS NEW STDATE BEFORE OLD?             
         BH    BLS40                      NO - KEEP OLD                         
         BL    BLS38                                                            
* IF SAME START DATE, USE EARLIER START TIME                                    
         CLC   PATSTIM,PATSTIM-PATENT(R6) IS NEW STTIME BEFORE OLD?             
         BH    BLS40                                                            
*                                                                               
BLS38    LR    R6,R2               SAVE ENTRY WITH LOWEST COUNT                 
*                                                                               
BLS40    LA    R2,PATNEXT                                                       
         CLI   0(R2),0                                                          
         BNE   BLS22                                                            
*                                                                               
         LTR   R6,R6               TEST FOUND ANY ENTRY                         
         BZ    BLS42               NO                                           
         LR    R2,R6                                                            
         LLC   RE,PATCNT                                                        
         LA    RE,1(RE)                                                         
         STC   RE,PATCNT                                                        
         OI    PATFLG,PATFLUSD     MARK PATTERN USED                            
         MVC   UNTPSEQ,PATSEQ                                                   
*                                                                               
BLS42    DS    0H                                                               
BLS44    LA    R5,UNTNEXT                                                       
         CLI   UNTADTEP,0          END OF UNITS                                 
         BNE   BLS20                                                            
         EJECT                                                                  
         L     R2,AIO3             PATTERN TABLE                                
*                                                                               
BLS50    LA    R5,UNITABLE                                                      
                                                                                
         TM    PATFLG,PATFLUSD     WAS PATTERN USED                             
         BZ    BLS74                NO                                          
         MVI   BYTE,0                                                           
         CLI   PATYPE,PATQNET      NET PAT                                      
         BE    BLS52                                                            
         CLI   PATYPE,PATQALL      ALL NET PAT                                  
         BNE   *+12                                                             
         OI    BYTE,X'10'                                                       
         B     BLS52                                                            
         CLI   PATYPE,PATQMED      MEDIA SPECIFIC                               
         BNE   *+12                                                             
         OI    BYTE,X'08'                                                       
         B     BLS52                                                            
         CLI   PATYPE,PATQDPMD     MEDIA SPECIFIC                               
         BNE   *+12                                                             
         OI    BYTE,X'28'                                                       
         B     BLS52                                                            
         CLI   PATYPE,PATQDPT      DAYPART SPECIFIC                             
         BNE   *+12                                                             
         OI    BYTE,X'20'                                                       
         B     BLS52                                                            
         CLI   PATYPE,PATQPROG     PROG SPECIFIC                                
         BNE   *+12                                                             
         OI    BYTE,X'80'                                                       
         B     BLS52                                                            
         CLI   PATYPE,PATQFEED     FEED ONLY                                    
         BNE   *+12                                                             
         OI    BYTE,X'40'                                                       
         B     BLS52                                                            
         CLI   PATYPE,PATQFDDP     FEED AND DAYPART                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    BYTE,X'60'                                                       
*                                                                               
BLS52    MVC   KEY+36(4),PATDSKA                                                
         MVC   KEY(2),=X'0A61'     SET KEY TYPE                                 
         XC    KEY+2(30),KEY+2                                                  
         MVI   RDUPDATE,C'N'                                                    
*        L     R6,AIO1                                                          
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
         MVI   ADIDFLAG,C'Y'                                                    
         TM    NPTSTAT,NPTS_ADID                                                
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         MVI   ELCODE,X'30'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,2(,R6)                                                        
         MVI   ELCODE,X'32'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,UNITABLE                                                      
         LLC   R0,1(R6)                                                         
         LA    R1,2(R6)                                                         
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         XC    ELEM,ELEM                                                        
         STC   R0,ELEM                                                          
         LA    RE,ELEM+1                                                        
*                                                                               
BLS54    ICM   RF,1,0(R1)                                                       
         SLL   RF,28                                                            
         SRL   RF,28                                                            
         CLI   0(R1),C'J'                                                       
         BL    *+8                                                              
         LA    RF,9(,RF)           J=10, K=11                                   
         BCTR  RF,0                                                             
         STC   RF,0(,RE)                                                        
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   R0,BLS54                                                         
BLS60    LLC   R0,ELEM                                                          
         LA    R1,ELEM+1                                                        
*                                                                               
BLS62    DS    0H                                                               
         BRAS  RE,TFTR             FILTER UNIT TABLE                            
         BNE   BLS70                                                            
*                                                                               
         XC    SVTYPE,SVTYPE                                                    
*                                                                               
         CLI   SVTN1PR9,C'Y'       BYPASS PREV ASSIGNED CMLS                    
         BNE   *+12                                                             
         TM    UNTFLAG2,UNTFL2AS   WAS THIS PREV ASSIGNED                       
         BO    BLS70                YES BYPASS                                  
*                                                                               
         TM    UNTFLAG4,UNTFLSEC   IS IT SECTIONAL FEED                         
         BO    BLS70                YES BYPASS                                  
*                                                                               
         TM    UNTFLAG4,UNTFLCUT   IS IT A CUTIN                                
         BO    BLS70                THEN BYPASS                                 
*                                                                               
         TM    UNTFLAG2,UNTFL2PS   THIS UNIT ALREADY SEEDED                     
         BO    BLS70                YES                                         
         CLC   PATSEQ,UNTPSEQ      THIS UNIT FOR THIS PAT                       
         BNE   BLS70                                                            
         XC    WORK,WORK                                                        
         MVC   WORK+8(4),UNTADTEP  & UNTADTE2                                   
*                                                                               
         BAS   RE,VCMLPRD          VALIDATE CML, PRD AND LEN                    
*                                                                               
         LLC   RE,0(R1)            ROT LEN                                      
         SLL   RE,4                TIMES 16                                     
         LA    RF,0(RE,R4)         THIS COMML                                   
*                                                                               
         OC    PATPROD2,PATPROD2   PIGGYBACK PROD                               
         BZ    *+20                 NO                                          
         OC    8(8,RF),8(RF)       IS THERE A SECOND CML                        
         BNZ   *+10                 YES                                         
         MVC   8(8,RF),0(RF)       SET UP FOR BOTH PRODS                        
*                                                                               
         OC    UNTCML1(16),UNTCML1 ANY COMML ASSIGNED                           
         BZ    BLS66                YES                                         
         CLC   UNTCML1(16),0(RF)   THESE DIFFERENT COMMLS                       
         BNE   BLS66                YES, CHANGE, NEW REVISION                   
         CLC   UNTREF,PATREF       THIS NEW PAT REF?                            
         BE    BLS68                NO, NO CHANGE, NO NEW REVISION              
*                                                                               
BLS66    OI    UNTFLAG1,X'08'                                                   
         TM    UNTFLAG4,UNTFLDC1                                                
         BZ    *+8                                                              
         NI    UNTFLAG4,X'FF'-UNTFLDC1 TURN OFF CML1 DLTD THIS SESSION          
*                                                                               
         OC    8(8,RF),8(RF)       ANY P/B COMML                                
         BZ    BLS67                NO                                          
         OI    UNTFLAG1,X'04'                                                   
         TM    UNTFLAG4,UNTFLDC2                                                
         BZ    *+8                                                              
         NI    UNTFLAG4,X'FF'-UNTFLDC2 TURN OFF CML2 DLTD THIS SESSION          
*                                                                               
BLS67    NI    UNITSW1,X'FF'-X'08' SET OFF NO UPDATES SW                        
*                                                                               
         MVC   UNTCML1(16),0(RF)                                                
         BAS   RE,BLSSETAD         SET ADID/ISCI AS REQUIRED                    
         B     BLS68                                                            
*                                                                               
BLSSETAD NTR1                                                                   
*                                                                               
         NI    UNTCMLF,X'FF'-X'80'-X'40'                                        
*                                                                               
         OC    UNTCML1,UNTCML1                                                  
         BZ    BLSSETAX                                                         
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   BLSSETA4                                                         
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'U',UNTCML1),WORK                                 
         CLI   WORK+8,C' '                                                      
         BH    BLSSETA2                                                         
         MVC   UNTCML1,WORK        NOT REAL ADID, SO USE UNPACKED               
         B     BLSSETA4                                                         
*                                                                               
BLSSETA2 OI    UNTCMLF,X'80'       SET CMML1 IS PACKED                          
*                                                                               
BLSSETA4 OC    UNTCML2,UNTCML2     TEST FOR CMML                                
         BZ    BLSSETAX                                                         
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   BLSSETAX                                                         
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'U',UNTCML2),WORK                                 
         CLI   WORK+8,C' '                                                      
         BH    BLSSETA6                                                         
         MVC   UNTCML2,WORK                                                     
         B     BLSSETAX                                                         
*                                                                               
BLSSETA6 OI    UNTCMLF,X'40'       SET CMML2 IS PACKED                          
*                                                                               
BLSSETAX XIT1                                                                   
*                                                                               
BLS68    MVC   UNTREF,PATREF       SAVE NEW PTN SEQ NO                          
         MVC   UNTKEY,BYTE         OVERLAY PTTN SEQ WITH KEY MASK               
         OI    UNTFLAG1,X'08'      CML ASSIGNED THIS SESSION                    
         NI    UNTFLAG1,X'FF'-X'80' SET OFF REASSIGN NEEDED                     
         OI    UNTFLAG2,UNTFL2PS   THIS UNIT NOW SEEDED                         
         TM    UNTFLAG1,X'60'      DELETED FEED OR UNIT                         
         BNZ   *+14                                                             
         LH    RF,UNASGND                                                       
         BCTR  RF,0                                                             
         STH   RF,UNASGND                                                       
         LA    R1,1(,R1)                                                        
         BCTR  R0,0                                                             
BLS70    LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             END OF UNIT TABLE                            
         BE    BLS74                YES                                         
         LTR   R0,R0                                                            
         BNZ   BLS62                                                            
         B     BLS60                                                            
BLS74    LA    R2,PATNEXT                                                       
         CLI   0(R2),0             END OF PATTERN TABLE                         
         BNE   BLS50                NO                                          
*                                                                               
         LA    R5,UNITABLE                                                      
BLS76    NI    UNTFLAG2,X'FF'-UNTFL2PS SET OFF THIS UNIT NOW SEEDED             
         LA    R5,UNTNEXT                                                       
         CLI   0(R5),0             END OF UNIT TABLE                            
         BNE   BLS76                NO                                          
BLSX     XIT1                                                                   
         EJECT                                                                  
* CHECK THAT PRD AND PRD LEN IS SAME IN COMMERCIAL                              
*                                                                               
         DS    0H                                                               
VCMLPRD  NTR1                                                                   
*                                                                               
         ST    R5,SVREG1           SAVE R5 (POINTS TO UNIT TABLE)               
*                                                                               
         L     R6,AIO2             PATTERN RECORD                               
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   PRDMATSW,0                                                       
*                                                                               
         USING NPTCMLEL,R6                                                      
*                                                                               
         LLC   R0,NPTCMLLN         GET ELEM LEN                                 
         SRL   R0,4                DIV BY 16=NO OF CMML PRS                     
         LA    R5,NPTCML           1ST CMML                                     
VCMLPR10 CLI   0(R5),C'*'          IS IT DELETED                                
         BE    VCMLPR90             YES                                         
*                                                                               
         MVC   WORK(8),0(R5)                                                    
*                                                                               
VCMLPR15 DS    0H                                                               
         XC    DUB(4),DUB                                                       
         XC    SVPROD,SVPROD       RESET VALIDATE THIS PRD IN FCML              
         NI    REVALID,X'FF'-RESOCML                                            
         OI    REVALID,NOEQCCXT    RETURN ON NE CC IF ERROR                     
*                                                                               
         ST    R5,SVREG2           SAVE R5 (POINTS TO CML)                      
*                                                                               
         L     R5,SVREG1           RESTORE R5 (PT TO UNTABLE)                   
         BRAS  RE,FCML                                                          
         BNE   NOCMLERR            CML NOT FOUND                                
*                                                                               
         MVC   FULL,DUB            SAVE CML LENGTH                              
         BRAS  RE,VCMLAPR          GO CHECK STARCOM DATA                        
         L     R5,SVREG2                                                        
*                                                                               
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE  ANY ERRORS                          
         BZ    VCMLPR17                                                         
         TM    CMLFLAG1,NOAIR                                                   
         BO    NOAIRER1                                                         
         TM    CMLFLAG1,MAXDTE                                                  
         BO    MAXDTER1                                                         
         TM    CMLFLAG1,CADTE                                                   
         BO    CADTER1                                                          
*                                                                               
VCMLPR17 DS    0H                                                               
         L     R4,AIO2             PATTERN RECORD                               
         USING NPTXKEY,R4                                                       
*                                                                               
         L     R6,AIO1             COMMERCIAL RECORD                            
         MVI   ELCODE,X'29'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLMPREL,R6                                                      
         CLC   =XL3'2903FF',CMLMPREL  IS THIS COMML PRD=ALL                     
         BE    VCMLPR30                YES, COVERS ALL PRODUCTS                 
         LLC   RF,CMLMPRLN                                                      
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LR    RE,RF                                                            
         LA    R2,CMLMPRS          START OF PROD LIST                           
VCMLPR20 CLC   NPTXPRD,0(R2)       MATCH PAT PROD TO CML PRD                    
         BE    VCMLPR30            YES                                          
         LA    R2,3(,R2)                                                        
         BCT   RF,VCMLPR20                                                      
         OC    NPTXPRD2,NPTXPRD2   IS THERE A PARTNER PROD                      
         BZ    BADPRDER            NO, MUST MATCH PROD 1                        
         B     VCMLPR50            SEE IF 2 MATCHES                             
VCMLPR30 CLC   NPTXSLN,FULL        IS THIS CML SAME SPOT LENGTH                 
         BE    VCMLPR40            YES                                          
         OC    NPTXPRD2,NPTXPRD2   IS THERE A PARTNER PROD                      
         BZ    LENERR               NO, MUST MATCH PROD 1                       
         LLC   RE,NPTXSLN                                                       
         LLC   RF,NPTXSLN2                                                      
         AR    RE,RF                                                            
         CLM   RE,1,FULL                                                        
         BE    VCMLPR40                                                         
         OC    NPTXPRD2,NPTXPRD2   IS THERE A PARTNER PROD                      
         BZ    LENERR              NO, MUST MATCH PROD 1                        
         B     VCMLPR50                                                         
*                                                                               
VCMLPR40 CLI   NPTXPRD2,0          IS THERE A PARTNER PROD                      
         BE    VCMLPR90            NO, JUST GET OUT                             
         OI    PRDMATSW,X'80'                                                   
*                                                                               
VCMLPR50 CLC   =XL3'2903FF',CMLMPREL  IS THIS COMML PRD=ALL                     
         BE    VCMLPR70                YES, COVERS ALL PRODUCTS                 
*                                                                               
         LA    R2,CMLMPRS          START OF PROD LIST                           
VCMLPR60 CLC   NPTXPRD2,0(R2)      IS THIS PRD 2                                
         BE    VCMLPR70                                                         
         LA    R2,3(,R2)                                                        
         BCT   RE,VCMLPR60                                                      
         TM    PRDMATSW,X'80'      WAS OTHER PROD FOUND                         
         BO    VCMLPR80            YES                                          
         B     BADPRDER                                                         
*                                                                               
VCMLPR70 OI    PRDMATSW,X'40'                                                   
         CLC   NPTXSLN2,FULL       IS THIS CML SAME SPOT LENGTH                 
         BE    VCMLPR80            YES                                          
         TM    PRDMATSW,X'C0'      WERE BOTH PRODS FOUND                        
         BNO   LENERR              NO                                           
*                                                                               
         LLC   RE,NPTXSLN                                                       
         LLC   RF,NPTXSLN2                                                      
         AR    RE,RF                                                            
         CLM   RE,1,FULL                                                        
*NOP     BNE   LENERR                                                           
         BE    *+12                                                             
         NI    PRDMATSW,X'FF'-X'40' RESET PARTNER FOUND                         
         B     VCMLPR80                                                         
                                                                                
         OI    PRDMATSW,X'08'      SET ON BOTH PROD COVERED                     
         B     VCMLPR90                                                         
*                                                                               
VCMLPR80 DS    0H                                                               
         TM    PRDMATSW,X'C0'      WERE BOTH PRODUCTS FOUND                     
         BO    VCMLPR90             YES                                         
         TM    PRDMATSW,X'08'      BOTH PRODUCTS COVERED                        
         BO    VCMLPR90             YES                                         
         TM    PRDMATSW,X'02'      DID WE LOOK FOR PRD/PTR                      
         BO    BADPRDER            YES, AND DIDN'T FIND IT                      
         OI    PRDMATSW,X'02'      LOOK FOR PRD/PTR                             
         OC    8(8,R5),8(R5)       MAKE SURE THERE IS A 2ND CMML                
         BZ    BADPRDER                                                         
         MVC   WORK(8),8(R5)                                                    
         B     VCMLPR15                                                         
*                                                                               
VCMLPR90 LA    R5,16(R5)                                                        
         BCT   R0,VCMLPR10         VLD NEXT CML                                 
*                                                                               
         BRAS  RE,INITXSP          SET TO XSPOT                                 
         B     VCMLPRX                                                          
*                                                                               
         USING CMLBBEL,R6                                                       
MAXDTER1 MVC   CONHEAD(L'MAXDTMS1),MAXDTMS1                                     
         GOTO1 DATCON,DMCB,(3,CMLBBMXD),(5,CONHEAD+L'MAXDTM1+1)                 
*                                                                               
         TM    UNITSW,X'20'        SVTWA NEEDED                                 
         BZ    *+8                                                              
         BRAS  RE,SVTWA                                                         
         GOTO1 ERREX2                                                           
*                                                                               
MAXDTMS1 DC    C'* ERROR * MAX USE DATE ='                                      
         DS    0H                                                               
CADTER1  MVC   GERROR,=Y(NOCLADTE) NO CLIENT APPROVAL DATE                      
         B     NOAIRERX                                                         
*                                                                               
NOAIRER1 MVC   GERROR,=Y(NAPRTAIR) NOT APPROVED TO AIR                          
*                                                                               
NOAIRERX XC    ELEM,ELEM                                                        
         MVI   ELEM,13                                                          
         MVC   ELEM+1(12),WORK                                                  
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
         BRAS  RE,CKSVTWA                                                       
         DC    H'0'                                                             
*                                                                               
NOCMLERR DS 0H                                                                  
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         STCM  R3,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R3),13            L'SUBST TEXT + 1                             
         MVC   1(12,R3),WORK                                                    
         MVI   13(R3),8            L'SUBST TEXT + 1                             
         MVC   14(7,R3),DUB                                                     
         MVC   GERROR,=Y(CMLERRER)   CML ERROR                                  
         SR    R2,R2                                                            
         B     VCMLERR                                                          
*                                                                               
LENERR   DS 0H                                                                  
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         STCM  R3,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R3),13            L'SUBST TEXT + 1                             
         MVC   1(12,R3),WORK                                                    
         MVC   GERROR,=Y(CMLPATER)   PAT LEN NOT EQUAL CML LEN                  
         SR    R2,R2                                                            
         B     VCMLERR                                                          
*                                                                               
BADPRDER DS    0H                                                               
         MVC   GERROR,=Y(PATPROD) PAT PROD NOT IN CML                           
*                                                                               
         USING NPTXKEY,R4                                                       
         L     R4,AIO2             PAT RECORD                                   
         LA    RE,NPTXPRD                                                       
         TM    PRDMATSW,X'80'      WAS 1ST PRD FOUND                            
         BZ    *+8                  NO                                          
         LA    RE,NPTXPRD2                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         STCM  R3,7,GASUBST        A(SUBST TEXT)                                
         MVI   0(R3),4             L'SUBST TEXT + 1                             
         MVC   1(3,R3),0(RE)                                                    
*                                                                               
         MVI   4(R3),13            L'SUBST TEXT + 1                             
         MVC   5(12,R3),WORK                                                    
         SR    R2,R2                                                            
*                                                                               
VCMLERR  GOTO1 VTRAERR                                                          
         DC    H'0'                                                             
*                                                                               
VCMLPRX  XIT1                                                                   
         EJECT                                                                  
* PRINT ERROR REPORT *                                                          
*                                                                               
PATERR   LA    R2,TRAPRGH                                                       
         MVC   GERROR,=Y(NOPATT)                                                
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
         LTORG                                                                  
TABSAV   DS    0H                                                               
OPTPGRL  DS    XL1500              PROD GROUP PRODUCT LIST (150 PRD)            
TABSAVL  EQU   *-TABSAV                                                         
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNPAT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNREV                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNFEED                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE SPTRNEQPRG                                                     
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
********+INCLUDE SPTRNSTA                                                       
*          DATA SET SPTRNSTA   AT LEVEL 009 AS OF 04/30/01                      
STATRECD DSECT        **** NET STATION RECORD ****                              
*                                                                               
STATKEY  DS    0XL20                                                            
STATKID  DS    X'29'               RECORD ID                                    
STATKAM  DS    XL1                 AGENCY/MEDIA                                 
STATKNET DS    CL4                 NETWORK STATION                              
         DS    XL14                SPARE                                        
*                                                                               
         DS    XL2                 RECORD LENGTH                                
         DS    XL4                 CONTROL                                      
         DS    XL1                                                              
*                                                                               
STADATEL DS    X'10'               STATION DATA ELEMENT                         
STADATLN DS    AL1(STADATEQ-STADATEL) ELEMENT LENGTH                            
STASEQ   DS    XL1                 SEQUENCE NUM INVERTED (LIFO)                 
STAIDATE DS    XL3                 INACTIVE DATE (INVERTED)                     
STAADATE DS    XL3                 ACTIVE DATE                                  
STAMDATE DS    XL3                 MAINT DATE (DATE ADDED/CHANGED)              
         DS    XL4                 SPARE                                        
STADATEQ EQU   *                                                                
*                                                                               
STACODEL DS    X'20'               STATION CODE ELEMENT                         
STACODLN DS    AL1(STACODEQ-STACODEL) ELEMENT LENGTH                            
STACODE  DS    XL8                 STATION CODE (80/40/20/10....)               
STACODEQ EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPGENPROG                                                      
         PRINT OFF                                                              
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRABCD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
         PRINT OFF                                                              
NETBLOCKD      DSECT                                                            
       ++INCLUDE NETBLOCKN                                                      
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
AOPTPGRL DS    A                                                                
ASVSTOR  DS    A                                                                
SPTR1CRR DS    A                                                                
VGTBROAD DS    A                                                                
QSORT    DS    A                                                                
VTRPACK  DS    A                                                                
ANETIO   DS    F                                                                
MAXUNT   DS    F                                                                
NEXTFEED DS    F                                                                
AESTBL   DS    F                                                                
CMLASNCT DS    H                                                                
*                                                                               
* START OF SAVED STORAGE (6144)                                                 
*                                 (18K) CKPT SCREEN SAVED STARTING              
SAVESTOR EQU   SVSTART+11324-SYSD 18432-2048-5020 = 11364 *                     
*              FOR SOME REASON, ONLY 11324 IS SAVED???                          
*                                                                               
NXTUNIT  DS    F                  FIRST UNIT TO DISPLAY NEXT SCREEN             
SCANCT   DS    F                                                                
ATRAINFH DS    A                                                                
NEXTDYT  DS    A                   A(NEXT DAY TIME)                             
TOTUNITS DS    H                                                                
UNASGND  DS    H                  UNASSIGNED UNITS (NO CMMLS)                   
UNALUNTS DS    H                  UNALLOCATED UNITS (NO PROD)                   
TRDLUNTS DS    H                  DELETED UNITS (BY TRAFFIC)                    
DLUNTS   DS    H                  DELETED UNITS                                 
PREVASS  DS    H                  PREVIOUSLY ASSIGNED CMLS                      
CVRDAYS  DS    H                                                                
SVREG1   DS    F                  SAVE REGISTER                                 
SVREG2   DS    F                  SAVE REGISTER                                 
SVREG    DS    F                  SAVE ANY REGISTER                             
*                                                                               
NXTDYTCT DS    X                  DAY/TIME COUNTER                              
FLTRSW   DS    X                  80 - CLEAR ASGND CMLS                         
*                                 40 - PATTERNS ASSIGNED                        
*                                 20 - CMLS CLEARED                             
*                                                                               
*                                                                               
*                                                                               
* STDATE, ENDATE, STDATEP, AND ENDATEP MUST BE TOGETHER AND IN ORDER            
*                                                                               
DATES    DS    0CL25                                                            
PERIOD   DS    XL3                                                              
STDATE   DS    CL6                                                              
ENDATE   DS    CL6                                                              
STDATEB  DS    XL3                                                              
ENDATEB  DS    XL3                                                              
STDATEP  DS    XL2                                                              
ENDATEP  DS    XL2                                                              
*                                                                               
CONVDTE  DS    XL2                                                              
*                                                                               
MYPSTIM  DS    H                                                                
MYPETIM  DS    H                                                                
MYUSTIM  DS    H                                                                
MYUETIM  DS    H                                                                
*                                                                               
SVCSPROD DS    XL(L'UNTCSPRO)      1ST PROD OF COPY SPLIT PRODS                 
*                                                                               
UNITSW   DS    XL1                                                              
*                                  80 - FOUND UNALLOCATED PRODUCT-BLUNT         
*                                  40 - FEEDS ADDED, RESORT UNITABLE            
*                                  20 - VALID CML FOUND, SAVE TWA               
*                                  10 - FOUND REV 0 REC NOT ORIG                
*                                  08 - BUY ACTIVITY REPORT                     
*                                  04 - LOCKED UNITS FOUND                      
*                                  02 - TABLE BUILT                             
*                                  01 - NO REC FOR CURRENT REVISION #           
UNITSW1  DS    XL1                                                              
*                                  80 - RESTORE FOUND IN FILTER                 
*                                  40 - ACML DONE, SVTWA NEEDED                 
*                                  20 - MEDIA FORCED REASSIGN NEEDED            
*                                       (OCCURRED WHILE DOING ASSIGN)           
*                                  10 - USE PATTERNS TO SEED UNITS              
*                                  08 - NO PATTNS FOUND SEED WITH PATTM         
*                                  04 - SRV RTN - REASSIGNS STILL LEFT          
*                                  02 - PARTIAL PERIOD -                        
*                                       ENTERED DATE= IN VOPT                   
UNTSW1NL EQU                       01 - IGNORE SPOT LEN IN FCML FOR             
*                                       BILLBOARD SLIDE                         
*                                                                               
CURSPOS1 DS   0XL2                 KEEP THESE                                   
CURSTBA  DS    XL1                    THREE FIELDS                              
CURSACML DS    XL1                          TOGETHER                            
*                                                                               
OREVNUM  DS    XL1                 REVISION FOR THE OTHER PERIOD                
OREVNN   DS    XL1                 NET REV FOR THE OTHER PERIOD                 
REVISION DS    XL1                                                              
HIREVNN  DS    XL1                 KEEPS HIGHEST NETWORK REV NUMBER             
SVMEDIA  DS    C                   NETWORK MEDIA                                
SVCMLFLG DS    X                                                                
SVELCODE DS    X                   SAVE ELCODE                                  
*                                                                               
SVTYPE   DS    XL4                 CML TYPE                                     
SVPROD   DS    CL3                 SAVE PRODUCT                                 
*                                                                               
MYTN2TMP DS    0CL3                SAVE TEMPORARY OPTIONS                       
MYTN2PRW DS    CL1                 SV OPTION WEEKLY                             
MYTN2PRB DS    CL1                 SV OPTION BROADCAST                          
MYTN2PRC DS    CL1                                                              
MYTN2PR6 DS    CL1                                                              
*                                                                               
SVWORK   DS    CL64                                                             
SVCODE   DS    XL8                                                              
SVANET   DS    XL8                 SAVED BITS FOR APPROVED NETS                 
IDATE    DS    XL3                 INACTIVE DATE FROM STATION APPROVAL          
NETMKT   DS    H                                                                
NETWORK  DS    CL4                                                              
PROGRAM  DS    CL6                                                              
CUREQVCT DS    XL1                 CURRENT EQV TABLE COUNT                      
SVCML    DS    CL12                                                             
SVCMLP   DS    XL8                                                              
SVPDAY   DS    CL1                 SAVE PROGRAM DAY                             
SVPTIME  DS    CL4                  AND PROGRAM TIME                            
SAVEOTHR DS    CL25                                                             
SVTAG    DS    CL2                 SAVE TAG                                     
SAVEFLAG DS    CL1                                                              
SVTS     DS    CL5                  SAVE TRAFFIC SUPPLIER                       
SVTSLEN  DS    CL1                  TSUPP ENTRY LEN                             
SVLINE   DS    CL1                  SAVE LINE #                                 
SVMYEAR  DS    XL1                  MAP YEAR                                    
SVMCODEP DS    XL1                  MAP CODE POINTER                            
SVMCODE  DS    CL8                  MAP CODE                                    
SVFEED   DS    CL4                  FEED                                        
SVDESC   DS    CL60                 SAVE SECTIONAL FEED DESCRIPTION             
SVDYTLST DS    XL55                 SAVE DAY/TIME EXCEPTIONS                    
*                                                                               
FILTERS  DS   0CL(FLTRLEN)                                                      
FLTRPROD DS    CL3                                                              
FLTRCML  DS    CL8                                                              
FLTRSLN  DS    XL1                                                              
FLTRDATE DS    XL2                                                              
FLTRDAT2 DS    XL2                                                              
FLTRDATS DS    CL1                                                              
SVVIGNLN DS    CL1                                                              
SVCMLTBA DS    CL8                                                              
SVACML   DS    CL8                                                              
UNITSW2  DS    XL1                                                              
UNITTS   EQU   X'80'               TRAFFIC SUPPLIER UNIT                        
DCOST    EQU   X'40'               DISPLAY UNIT COST                            
RCMLTBA  EQU   X'20'               REPLACE COMMERCIAL WITH TBA                  
UNITNTS  EQU   X'10'               NON-TRAFFIC SUPPLIER UNIT                    
UNITFDD  EQU   X'08'               FEED NO NATIONAL                             
LINENUM  EQU   X'04'               LINE # ENTERED IN OPTIONS                    
FEEDSW   EQU   X'02'               SHOW SECTIONAL FEED DESCRIPTION              
NOTIFYSW EQU   X'01'               SEND NOTIFY - UNIT COPIED (CM,DATE)          
*                                                                               
FLTODOSW DS    XL1                 FILTERS LEFT TO DO SWITCH                    
*MNV                                                                            
OVRDIGI  EQU   X'04'                OVERRIDE DIGITAL                            
*MNV                                                                            
ACMLADID EQU   X'08'                ACML IS ADID                                
TBASW    EQU   X'10'                REPLACE CML WITH TBA                        
ACMLSW   EQU   X'20'                ASSIGN COMMERCIAL                           
PATSW    EQU   X'40'                SEED COMMERCIAL -PATTERN                    
CLRSW    EQU   X'80'                CLEAR PREV ASSIGNMENTS                      
*                                                                               
FLTCMLD  DS    CL12                CML TO DISPLAY                               
FLTRLEN  EQU   *-FLTRPROD                                                       
*                                                                               
SVNUCFLG DS    XL1                 SAVE NUCMLFLG FROM 21 ELEM                   
*                                                                               
PRDMATSW DS    XL1                 80 - PRODUCT FOUND IN COMMERCIAL             
*                                  40 - PARTNER FOUND IN CML                    
*                                  20 - NO MATCH BETWEEN PRD AND CML            
*                                  10 - SWAP CMLS                               
*                                  08 - PRD/PTR SPOT LENS=CML LEN               
*                                       -CML IS FOR PIGGYBACK PAIR              
*                                  02 - LOOKING FOR PTR                         
*                                  01 - CML COVERS ALL PRDS                     
*                                                                               
CMLFLAG  DS    CL1                 1=CML1, 2=CML2, 3=CML3                       
*                                                                               
CML1     DS    CL1                 CMLSLN STORED HERE                           
CML2     DS    CL1                   "       "     :                            
CML3     DS    CL1                                                              
SVCMLSOL DS    CL1                 HAS CMLSOLO FROM LAST COMML FCML             
VIGNFLG  DS    CL1                                                              
SKIPVGN  DS    CL1                                                              
JUSLEN   DS    CL1                 JUST VALIDATE LEN SWITCH                     
REVALID  DS    CL1                                                              
*        EQU   X'01'               REVALIDATE CML                               
*        EQU   X'02'               REVALIDATE P/B                               
NOREVALD EQU   X'04'               CML DELETED DO NOT REVALIDATE                
NONEW    EQU   X'08'               SAME CML AS BEFORE NO NEW ASGN               
RESOCML  EQU   X'10'               RESTORE ORIGINAL CML                         
NOEQCCXT EQU   X'20'               NOT EQUAL CONDITION CODE EXIT                
*                                                                               
VOPTIONS DS   0CL70                                                             
VOPTPROD DS    CL3                 LIMIT TO PROD XXX                            
VOPTPRD  DS    XL1                                                              
VOPTPRGR DS    XL2                 PROD GROUP (LETTER IS IN PROFILE)            
TMPSTSW  DS    CL1                                                              
*                                                                               
HOLDSIGN DS    CL1                                                              
*                                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL20                                                             
*                                                                               
SVCPROD  DS    CL3                 SAVE 3 CHAR PRODUCT FOR VALILOC              
SVLGKEY  DS    CL(L'KEYSAVE)       SAVE LAST GOOD KEY                           
*                                                                               
         DS    0D                                                               
EQVPTBL  DS    CL(10*L'EQVENT)  ALLOW 10 PROGRAMS                               
INITALL  EQU   EQVPTBL-NXTUNIT     INITIALIZE MY STORAGE                        
*                                                                               
MCODETBL DS    CL(50*8)        50 MAP CODE ENTRIES FOR SECTIONAL FEEDS          
*                                                                               
TIMETBL  DS    CL(100*TIMENT)  ALLOW 100 ENTRIES                                
TIMENT   EQU   4                                                                
*                                                                               
SVPATSEQ DS    CL8                                                              
SVUNTDTS DS   0XL4                                                              
SVUNTSTR DS    XL2                                                              
SVUNTEND DS    XL2                                                              
CMLFLAG1 DS    XL1                                                              
JUSTAPR  EQU   X'20'               JUST GET APPROVALS FOR THIS CML              
NOAIR    EQU   X'08'               NOT APPROVED TO AIR                          
MAXDTE   EQU   X'04'               MAX DATE ERROR                               
CADTE    EQU   X'02'               CLT APPROVAL DATE ERROR                      
*                                                                               
SVFLAG   DS    XL1                                                              
CONVSW   EQU   X'20'               CONVERTED RECORDS                            
*                                                                               
         DS    0D                                                               
UNITABLE DS    CL(16472)        ROOM FOR 168 98 BYTE ENTRIES                    
*                                                                               
         DS    CL2              END MARKER                                      
*                                                                               
ENDSYSD  EQU   *       IF THIS ADDRESS EXCEEDS 65B8, PAST END OF SYSD           
*                                  LSYSD IS 65B8 (26040) WITH 44,000            
*                                  GRABBED BY T21600                            
*                                                                               
TRALINES EQU   (TRATAGH-TRAINF1H)/(TRAINF2H-TRAINF1H)                           
         EJECT                                                                  
* UNIT TABLE IS SORTED ON 1ST 16 BYTES AFTER BEING BUILT *                      
*                                                                               
UNTABLED DSECT                                                                  
UNTENT   DS    0CL(UNTNEXT-UNTSORT)                                             
*                                                                               
* KEEP UNTADTEP/UNTSQH TOGETHER AND IN ORDER                                    
*                                                                               
UNTSORT  DS   0CL16                                                             
UNTCOM   DS   0CL12                                                             
UNTADTEP DS    XL2                AIR DATE                                      
UNTADTE2 DS    XL2                AIR DATE PLUS ROTATION DAYS                   
UNTSQH   DS    XL1                STARTING QUARTER HOUR                         
*                                                                               
UNTEST   DS    XL1                ESTIMATE                                      
UNTSUB   DS    XL1                SUB-LINE 0-192 REAL, 193-233 TRAFFIC          
UNTDP    DS    CL1                DAYPART                                       
UNTDSKAD DS    XL4                DISK ADDRESS                                  
UNTFEED  DS    CL4                FEED  (ALSO USED FOR 5 SEC TAGS)              
UNTPROD  DS    CL3                PRODUCT                                       
UNTSLN   DS    XL1                UNIT LENGTH                                   
UNTPROD2 DS    CL3                PARTNER PRODUCT                               
UNTSLN2  DS    XL1                PARTNER LENGTH                                
UNTCOM1  EQU   *-UNTCOM                                                         
*                                                                               
UNTCML1  DS    CL8                ASSIGNED COMMERCIAL                           
UNTCML2  DS    CL8                ASSIGNED COMMERCIAL P/B                       
UNTCMLF  DS    CL1                X'80'=CML1 IS ADID, X'40'=CML2                
UNTFLAG1 DS    XL1                80 REASSIGN NEEDED                            
*                                 40 DELETED FEED                               
*                                 20 DELETED UNIT                               
UNTFL1PB EQU   X'10'              10 CML COVERS BOTH PRDS                       
*                                 08 CML1 ASSIGNED THIS SESSION                 
*                                 04 CML2 ASSIGNED THIS SESSION                 
*                                 02 NATIONAL UNIT WITH FEEDS                   
UNTFL1FD EQU   X'01'              01 FEED TO A NATIONAL UNIT                    
*                                                                               
UNTFLAG2 DS    XL1                                                              
UNTFL2PS EQU   X'80'              80 UNIT WAS SEEDED FROM PATTERN               
UNTFL2CS EQU   X'40'              40 UNIT IS COPY SPLIT                         
UNTFL2SW EQU   X'20'              20 UNIT PRODS SWAPPED TO ALPHA ORDER          
*                                     FROM UNIT RECORD                          
*                                 10 INVERT PRD/CML PRINTING ON INSTR           
UNTFL2AS EQU   X'08'              08 PREVIOUSLY ASSIGNED CML                    
*                                 04 BILLBOARD REQUIRED SET IN BUY              
UNTFL2AD EQU   X'02'              02 ADU UNIT (NO COST ALLOWED)                 
*                                 01 FEED ASSIGNED BY MEDIA DEPT                
UNTREF   DS    XL3                 PATTERN REF WHEN SEEDED FROM PTTNS           
*                                                                               
* RE-USE UNTREF FOR SECTIONAL MAP YR/MAP CODE IF SECTIONAL FLAG IS ON           
         ORG   UNTREF                                                           
UNTMYR   DS    XL1                 MAP YEAR                                     
UNTMCDP  DS    XL1                 MAP CODE POINTER                             
         ORG   UNTREF+L'UNTREF                                                  
*                                                                               
UNTPSEQ  DS   0XL1                 PATTERN SEQ                                  
* UNTPSEQ REPLACED IN UPDATE WITH KEY                                           
UNTKEY   DS    XL1                 80 - PROG SPEC                               
*                                  60 - FEED & DPT SPEC                         
*                                  40 - FEED SPEC                               
*                                  20 - DAYPART SPEC                            
*                                  10 - ALL NET                                 
*                                  08 - MEDIA SPEC                              
*                                  00 - NET SPEC                                
UNTOTHR  DS    0CL25                                                            
UNTBBSL  DS    XL1                BILLBOARD LENGTH                              
UNTBBSN  DS    CL8                BILLBOARD SLIDE NUMBER                        
UNTBBCN  DS    CL8                BILLBOARD COPY NUMBER                         
UNTPOS   DS    CL4                POSITION                                      
UNTBBPOS DS    CL4                BILLBOARD POSITION                            
*** REUSE SOME BB FIELDS FOR TRI-BACK INFO                                      
         ORG   UNTBBCN                                                          
UNTCML3  DS    CL8                ASSIGNED COMMERCIAL 3 FOR TRI-BACKS           
         ORG   UNTBBPOS           LEAVE AS IS (USED FOR UNIT POSITION)          
UNTSLN3  DS    XL1                PRD3 LENGTH                                   
UNTTBFLG DS    XL1                **THIS FLAG IS FOR TRI-BACKS ONLY ***         
UNTBCML3 EQU   X'80'              80 CML3 ASSIGNED THIS SESSION                 
UNTB3BCM EQU   X'40'              40 THIS CML COVERS ALL 3 PRODS                
UNTB2BCM EQU   X'20'              20 THIS CML COVERS 2 OF 3 PRODS               
UNTBRVLD EQU   X'10'              10 RE-VALIDATE CMLS                           
UNTBDEL3 EQU   X'08'              08 CML3 DELETED THIS SESSION                  
*                                 04                                            
*                                 02                                            
*                                 01                                            
         DS    XL2                                                              
*                                                                               
UNTCOM2  DS   0CL26                                                             
UNTCSPRO DS    XL18               ALL PRODS FOR COPY SPLIT UNIT                 
UNTEQVCT DS    XL1                IF NON-ZERO, EQV PROG CD IN EQVPTBL           
UNTDAY   DS    CL1                DAY BITS (0=SPARE 1=MON.. 7=SUN)              
UNTTIME  DS    CL1                PT TO MILITARY START-END TIME TBL             
UNTACOST DS    CL4                ACTUAL UNIT COST                              
*                                                                               
UNTFLAG3 DS    XL1                                                              
UNTFL3TB EQU   X'80'              80 TRI-BACK UNIT                              
UNTFLGPD EQU   X'40'              40 PARTIAL DATES                              
UNTFLSCA EQU   X'20'              20 SCATTER UNIT                               
UNTFLUPF EQU   X'10'              10 UPFRONT UNIT                               
UNTFLOPR EQU   X'08'              08 OPPORTUNISTIC                              
UNTFLPFB EQU   X'04'              04 PFB UNIT (BONUS)                           
UNTFLMG  EQU   X'02'              02 MAKE-GOOD                                  
UNTFLCST EQU   X'01'              01 ACTUAL COST ENTERED                        
*                                                                               
UNTREV   DS    XL1                UNIT REVISION NUMBER                          
*                                                                               
UNTFLAG4 DS    XL1                                                              
UNTFLFDD EQU   X'80'               FEED NO NATIONAL                             
UNTFLDC1 EQU   X'40'               CML1 DELETED THIS SESSION                    
UNTFLDC2 EQU   X'20'               CML2 DELETED THIS SESSION                    
UNTFLUPD EQU   X'10'               UNIT UPDATED, UPDATE UNIT RECORD             
UNTFLCUT EQU   X'08'               CUTIN - BY STATION FOR STARCOM               
UNTFLSEC EQU   X'04'               SECTIONAL FEED                               
UNTFL4BC EQU   X'02'               CHANGE TO BILLBOARD                          
*                                                                               
UNTNEXT  EQU   *                                                                
         EJECT                                                                  
* DSECT FOR PATTERN LIST ENTRIES *                                              
*                                                                               
PATABLED DSECT                                                                  
PATENT   DS    0XL36                                                            
PATSRT   DS    0XL13                                                            
PATYPE   DS    XL1                                                              
PATQALL  EQU   64                 ALL NETWORK                                   
PATQMED  EQU   32                 MEDIA                                         
PATQDPMD EQU   24                 MEDIA AND DAYPART                             
PATQNET  EQU   16                 NETWORK                                       
PATQDPT  EQU   08                 DAYPART                                       
PATQPROG EQU   04                 PROGRAM                                       
PATQFEED EQU   02                 FEED                                          
PATQFDDP EQU   01                 FEED AND DAYPART                              
*                                                                               
PATFEED  DS    CL4                FEED                                          
PATSTR   DS    XL2                START DATE                                    
PATEND   DS    XL2                END DATE                                      
PATSTIM  DS    XL2                START TIME                                    
PATETIM  DS    XL2                END TIME                                      
*                                                                               
PATDP    DS    CL1                DAYPART                                       
PATSEQ   DS    XL1                                                              
PATREF   DS    XL3                REF NO                                        
PATPROD1 DS    CL3                PROD                                          
PATULN   DS    XL1                UNIT LEN                                      
PATPROD2 DS    CL3                PROD 2                                        
PATULN2  DS    XL1                UNIT LEN                                      
PATDSKA  DS    XL4                                                              
PATFLG   DS    XL1                                                              
PATFLUSD EQU   X'80'              PATTERN USED                                  
*        EQU   X'40'              COMMLS 1ST ASSIGNED (NO PREV)                 
*        EQU   X'20'              COMMLS CHANGED (DIFF FROM PREV)               
PATFLUFN EQU   X'10'              PATTERN END DATE UFN                          
*        EQU   X'08'                                                            
*        EQU   X'04'                                                            
*        EQU   X'02'                                                            
*        EQU   X'01'                                                            
PATCNT   DS    XL1                                                              
PATCOVER DS    XL1                                                              
         DS    XL3                                                              
PATNEXT  EQU   *                                                                
*                                                                               
* DSECT FOR EQUIVALENT PROGRAM CODE ENTRIES                                     
*                                                                               
EQVPTBLD DSECT                                                                  
EQVENT   DS   0CL(EQVNEXT-EQVEPRG)                                              
EQVEPRG  DS    CL6                EQUIVALENT PROGRAM CODE                       
EQVSDT   DS    CL6                START DATE                                    
EQVEDT   DS    CL6                END                                           
EQVUSED  DS    XL1                                                              
EQVCT    DS    XL1                ENTRY COUNTER                                 
EQVNEXT  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPTRA1C   11/09/20'                                      
         END                                                                    
