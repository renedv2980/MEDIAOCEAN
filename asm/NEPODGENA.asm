*          DATA SET NEPODGENA  AT LEVEL 010 AS OF 05/01/02                      
*          DATA SET NEPODGEN   AT LEVEL 015 AS OF 06/08/00                      
*PHASE T00A54A,*                                                                
NEPODGEN TITLE 'T00A54 - RESEARCH WRITER GENERAL ROUTINES'                      
         PRINT NOGEN                                                            
T00A54   CSECT                                                                  
         REQUS                                                                  
         USING *,RF                                                             
         DS    16384X              RESERVE 16K                                  
         ORG   T00A54                                                           
GEN      NTR1                                                                   
         DROP  RF                                                               
         LR    RB,RF                                                            
         USING T00A54,RB,RA,R7,R6                                               
         B     *+12                                                             
         DC    CL8'**GEN***'                                                    
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R7,2048(RA)                                                      
         LA    R7,2048(R7)                                                      
         LA    R6,2048(R7)                                                      
         LA    R6,2048(R6)                                                      
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         USING GETTXTD,GETTXTCB                                                 
         USING PODBD,PODBOOK                                                    
*                                                                               
         LA    RE,RELOC                                                         
         S     RE,RELOC                                                         
         ST    RE,PGNR                                                          
*                                                                               
         MVC   DUMPDBA,=C'**DBA***'                                             
*                                                                               
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VVALUSER                                                         
         B     VVALMED                                                          
         B     VVALCLT                                                          
         B     VVALBOOK                                                         
         B     VPDSERR                                                          
         B     VVALDEMO                                                         
         B     VVALNET                                                          
         B     VVALDYTM                                                         
         B     VVPRGHUT                                                         
         B     VVALPROG                                                         
         B     VVALOPTS                                                         
         B     VVALTITS                                                         
         B     VVALFILT                                                         
         B     VCHEFILT                                                         
*                                                                               
         B     VVALLEFT                                                         
         B     VVALRGHT                                                         
         B     VVALMID                                                          
         B     VVALROWS                                                         
         B     VVALCOLS                                                         
         B     VVALPEX                                                          
*                                                                               
         B     VINTDRON                                                         
         B     VVRWDRON                                                         
         B     VGRWDRON                                                         
         B     VVCLDRON                                                         
         B     VGCLDRON                                                         
         B     VVCMDRON                                                         
         B     VGCMDRON                                                         
         B     VGUSDRON                                                         
         B     VWRPDRON                                                         
*                                                                               
         B     VINTDRIV                                                         
*                                                                               
         B     VGENHEAD                                                         
*                                                                               
         B     VGENEDCT                                                         
*                                                                               
         B     VNUMERIC                                                         
         B     VPACK                                                            
         B     VCURSERR                                                         
         B     VERRXIT                                                          
         B     VEXIT                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE USER AGENCY                                         *         
***********************************************************************         
*                                                                               
VVALUSER MVC   PDQAGY,AGENCY                                                    
         MVC   AGYSIGN,SPACES                                                   
         XC    AGYALPHA,AGYALPHA                                                
         MVI   AGYNUM,X'FF'        ASSUME NOT NUMERIC                           
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         L     R1,ATWA                                                          
         MVC   CTIKID+8(2),10(R1)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R4,CTIDATA                                                       
         SR    R3,R3                                                            
*                                                                               
VUSE02   CLI   0(R4),0                                                          
         BE    VUSE14                                                           
         CLI   0(R4),X'30'                                                      
         BE    VUSE06                                                           
         CLI   0(R4),X'02'                                                      
         BE    VUSE08                                                           
         CLI   0(R4),X'06'                                                      
         BE    VUSE10                                                           
         CLI   0(R4),X'21'         SYSTEM AUTHORIZATION ELEMENT                 
         BE    VUSE12                                                           
*                                                                               
VUSE04   IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     VUSE02                                                           
*                                                                               
         USING CTDSTD,R4                                                        
VUSE06   MVC   USERNAME,CTDSTNAM                                                
         MVC   USERADDR,CTDSTADD                                                
         B     VUSE04                                                           
*                                                                               
         USING CTDSCD,R4                                                        
VUSE08   MVC   AGYSIGN,CTDSC                                                    
         MVC   WORK(3),AGYSIGN+2                                                
         NC    WORK(3),=X'F0F0F0'                                               
         CLC   WORK(3),=X'F0F0F0'                                               
         BNE   VUSE04                                                           
         MVC   AGYNUM,AGYSIGN+2                                                 
         B     VUSE04                                                           
*                                                                               
         USING CTAGYD,R4                                                        
VUSE10   MVC   AGYALPHA,CTAGYID                                                 
         B     VUSE04                                                           
*                                                                               
         USING CTSYSD,R4                                                        
VUSE12   CLI   CTSYSNUM,2          SPOT                                         
         BNE   VUSE04                                                           
         MVC   PDSSENUM,CTSYSSE    COPY SE NUMBER                               
         B     VUSE04                                                           
*                                                                               
VUSE14   XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         GOTO1 GETFACT,DMCB,FACTWRK                                             
         L     RF,DMCB                                                          
         LA    R4,FACTWRK                                                       
         MVC   0(80,R4),0(RF)                                                   
         USING FACTSD,R4                                                        
         MVC   PDSYSTEM,FASYS                                                   
         MVC   PDOVSYS,FAOVSYS                                                  
         MVC   LNDEMCOD,=H'3'      3 CHAR DEMO CODES                            
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         CLI   PDOVSYS,2           SPOT SYSTEM?                                 
         BNE   *+14                                                             
         MVC   RCPROG(2),=C'SP'    PREFIX FOR REPORT                            
         B     VUSE16                                                           
         CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+20                                                             
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT                            
         MVC   LNDEMCOD,=H'4'      NETWORK USES 4 CHAR DEMO CODES               
         B     VUSE16                                                           
         CLI   PDOVSYS,8           REP SYSTEM?                                  
         BNE   *+10                                                             
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT                            
*                                                                               
VUSE16   DS    0C                                                               
         MVI   OFFLINE,C'N'                                                     
         TM    FATFLAG,X'01'                                                    
         BNO   VUSEX                                                            
         MVI   OFFLINE,C'Y'                                                     
*                                                                               
VUSEX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE MEDIA                                               *         
***********************************************************************         
*                                                                               
VVALMED  XC    KEY,KEY             GET AGENCY RECORD                            
*                                                                               
         LA    R4,KEY                                                           
         USING AGYHDRD,R4                                                       
         CLI   PDOVSYS,8           REP, FORGET ABOUT                            
         BNE   VMED06                                                           
*                                                                               
* --- HAVE TO CHECK IF OFFLINE, IF SO, OPEN FILE                                
* HAVE TO SWITCH SYSTEMS FOR REP                                                
         CLI   OFFLINE,C'Y'        ARE WE OFFLINE?                              
         BNE   VMED04              NO, COULD USE SWITCH                         
         CLI   PDSSENUM,0          NEED A SE NUM TO SWITCH                      
         BNZ   VMED02                GOOD, USE SIDS                             
         MVI   PDSIDOPT,C'X'                                                    
         B     VMEDX                                                            
*                                                                               
VMED02   ICM   RF,15,PDAUTL                                                     
         MVC   4(1,RF),PDSSENUM    SPOT SE NUM                                  
                                                                                
         L     R1,=A(FLIST)                                                     
         A     R1,PGNR                                                          
         ST    R1,DMCB+8                                                        
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',,AIO1                           
         B     VMED06                                                           
*                                                                               
VMED04   GOTO1 SWITCH,DMCB,(0,=C'SPT'),0      SWITCH TO SPOT                    
         CLI   4(R1),0                                                          
         BZ    VMED06              NEED IT TO GET AGYMED FOR SIDS               
         MVI   PDSIDOPT,C'X'       CAN'T USE SIDS                               
         CLI   4(R1),2             SWITCHED BUT NOT OPENED?                     
         BNE   VMEDX                                                            
         GOTO1 SWITCH,DMCB,(PDSYSTEM,0),0   SWITCH BACK                         
         CLI   4(R1),0                                                          
         BE    VMEDX                                                            
         DC    H'0'                                                             
*                                                                               
* --- HAVE TO SET A FLAG FOR SID AVAILABILITY                                   
VMED06   MVI   PDSIDOPT,C'N'                                                    
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         LA    R2,KEY+28                                                        
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   PDQAGY,AGENCY                                                    
         LR    R5,R4                                                            
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         B     VMED08+4                                                         
*                                                                               
VMED08   BAS   RE,NEXTEL                                                        
         BNE   BADMED                                                           
         CLI   2(R5),C'N'          NETWORK TV?                                  
         BE    VMED10                                                           
         CLI   2(R5),C'T'          SPOT TV?                                     
         BNE   VMED08                                                           
*                                                                               
VMED10   MVC   PDBAGYMD,3(R5)      AGENCY/MEDIA CODE                            
         CLI   PDOVSYS,8           HAVE TO SWITCH BACK TO REP                   
         BNE   VMEDX                                                            
         CLI   OFFLINE,C'Y'        RETURN TO ORIGINAL SYSTEM                    
         BNE   VMED12                                                           
         ICM   RF,15,PDAUTL                                                     
         MVC   4(1,RF),PDSYSTEM                                                 
         B     VMEDX                                                            
*                                                                               
VMED12   GOTO1 SWITCH,DMCB,(PDSYSTEM,0),0                                       
         CLI   4(R1),0                                                          
         BZ    VMEDX                                                            
         DC    H'0'                                                             
*                                                                               
VMEDX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CLIENT                                              *         
***********************************************************************         
*                                                                               
VVALCLT  GOTO1 ANY                                                              
*                                                                               
VCLIX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE BOOK                                                *         
*                 INPUT:  A(INPUT SCREEN)                             *         
*                 OUTPUT: PODBOOK - LIST OF BOOKS                     *         
***********************************************************************         
*                                                                               
VVALBOOK DS    0H                                                               
         GOTO1 =A(VALBK),DMCB,(R2),(R9),(RC),RR=PGNR                            
         L     RE,APODBKL                                                       
         MVC   PODBD(PODBLNQ),0(RE)                                             
*                                                                               
VBOOKX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE DEMO                                                *         
***********************************************************************         
*                                                                               
VVALDEMO GOTO1 ANY                                                              
         LA    RE,BLOCK                                                         
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         GOTO1 SCANNER,PARAS,(R2),(12,BLOCK),C',=,.'                            
*                                                                               
*--IF SOURCE IS NTI OR P NAD DEMOS ARE INVALID EX. (67.RWMN1224)                
         L     RE,APODBKL                                                       
         ZIC   RF,PODBKNUM                                                      
         LTR   RF,RF                                                            
         BZ    BADDEM                                                           
*                                                                               
VDEM02   CLC   8(3,RE),=CL3'NAD'   NAD AND NHI                                  
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'NAW'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'NHW'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'TCA'   TCAR                                         
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'MPA'                                                
         BE    VDEM10                                                           
         CLC   8(3,RE),=CL3'TP'    THIS COVERS TP,T4 AND DPT                    
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'PAV'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'IUN'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'PIV'                                                
         BNE   VDEM04                                                           
         MVI   PODNADBK,X'FF'                                                   
         B     VDEM10                                                           
*                                                                               
VDEM04   CLC   1(3,RE),=CL3'CSI'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'BBM'                                                
         BE    VDEM10                                                           
         CLC   1(3,RE),=CL3'SRC'                                                
         BE    VDEM10                                                           
         LA    RE,PODBLNQ(RE)                                                   
         CLI   0(RE),X'FF'                                                      
         BNE   VDEM02                                                           
*                                                                               
         LA    RE,BLOCK                                                         
         LA    RF,12                                                            
         MVI   FIELDERR,1                                                       
*                                                                               
VDEM06   CLI   0(RE),0                                                          
         BE    VDEM10                                                           
         CLI   1(RE),0             IS DEMO IN NAD FORMAT                        
         BE    VDEM08              NO BYPASS                                    
         CLI   7(RE),171           USER DEMO                                    
         BE    VDEM10                                                           
         TM    2(RE),X'80'         IS THERE NUMERIC PREFIX                      
         BNZ   BADDEM              YES ERROR                                    
*                                                                               
VDEM08   LA    RE,32(RE)                                                        
         AI    FIELDERR,1                                                       
         BCT   RF,VDEM06                                                        
*                                                                               
*--IF BOOK PRIOR TO SEP/88 THEN C'O'(TP PUT)                                    
*--OR A C'Q'(TP SHARE)                                                          
VDEM10   MVI   FIELDERR,1                                                       
         TM    PODBKOPT,X'80'      IS THERE A BOOK PRIOR TO SEP/87              
         BZ    VDEM14              NO                                           
*                                                                               
         LA    RE,BLOCK                                                         
         LA    RF,12                                                            
*                                                                               
VDEM12   CLI   0(RE),0                                                          
         BE    VDEM14                                                           
         CLI   12(RE),C'O'                                                      
         BE    BADDEM                                                           
         CLI   12(RE),C'Q'                                                      
         BE    BADDEM                                                           
         CLI   22(RE),C'O'                                                      
         BE    BADDEM                                                           
         CLI   22(RE),C'Q'                                                      
         BE    BADDEM                                                           
         LA    RE,32(RE)                                                        
         AI    FIELDERR,1                                                       
         BCT   RF,VDEM12                                                        
*                                                                               
VDEM14   L     R4,PDADEMTB                                                      
         USING PDDMBUFF,R4         DEMO BUFFER                                  
         LA    R5,DBLOCKA                                                       
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   PODBMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         CLI   PDOVSYS,3           FOR NET                                      
         BNE   *+8                                                              
         MVI   DBSELMED,C'N'       SET SELMED TO N                              
         MVC   DBFILE,PODBFIL                                                   
*                                                                               
         ST    R5,PARAS+8                                                       
         CLI   PDOVSYS,2                                                        
         BNE   *+8                                                              
         MVI   PARAS+8,C'S'        SPOTPAK CALL                                 
         CLI   PDOVSYS,3           ONLY NET CAN USE 4 BYTE DEMOS                
         BNE   *+8                                                              
         MVI   DBDEMTYP,C'4'                                                    
         GOTO1 DEMOVAL,PARAS,(NFLDS,(R2)),(12,PODDEMWK),,0                      
         MVC   FIELDERR,0(R1)                                                   
         CLI   4(R1),0                                                          
         BE    BADDEM                                                           
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         MVC   PODDMWLN,ACTUAL                                                  
         MVC   PODDMNUM,ACTUAL                                                  
         MVI   PODDMENT,1                                                       
         ZIC   RE,PODDMWLN                                                      
         LA    RF,PODDEMWK                                                      
*                                                                               
VDEM16   CLI   0(RF),0                                                          
         BNE   *+8                                                              
         MVI   0(RF),X'01'                                                      
         AH    RF,LNDEMCOD                                                      
         BCT   RE,VDEM16                                                        
*                                                                               
         MVI   0(RF),X'FF'         PUT END OF TABLE MARK                        
         MVC   PODDEMO(49),PODDEMWK                                             
         L     RE,APODBKL                                                       
         ZIC   RF,PODBKNUM                                                      
*                                                                               
VDEM18   CLI   0(RE),X'FF'                                                      
         BE    VDEMX                                                            
         CLC   1(3,RE),=CL3'NMI'                                                
         BE    VDEM20                                                           
         CLC   1(3,RE),=CL3'EMI'                                                
         BE    VDEM20                                                           
         LA    RE,PODBLNQ(RE)                                                   
         B     VDEM18                                                           
*                                                                               
VDEM20   LA    RF,PODDEMO                                                       
*                                                                               
VDEM22   CLI   0(RF),X'FF'                                                      
         BE    VDEMX                                                            
         CLI   1(RF),C'T'                                                       
         BE    VDEM24                                                           
         CLI   1(RF),C'R'                                                       
         BNE   BADDEM2                                                          
*                                                                               
VDEM24   AH    RF,LNDEMCOD                                                      
         B     VDEM22                                                           
*                                                                               
VDEMX    B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE NETWORK                                             *         
***********************************************************************         
*                                                                               
VVALNET  DS    0H                                                               
         MVI   PDMGROPT,C'N'                                                    
         MVI   PDZZZMED,C'N'       FOR ZZZ NETS                                 
         CLI   5(R2),0             ANY NETWORK GIVEN?                           
         BNE   VNET02              YES                                          
         CLC   =C'TP ',PODBFIL     NO NETWORK/STATION VALID FOR THESE           
         BE    VNETX               FILES IF MARKETS REQUESTED (VALOPT)          
         CLC   =C'IUN',PODBFIL                                                  
         BE    VNETX                                                            
         CLC   =C'TPT',PODBFIL                                                  
         BE    VNETX                                                            
         CLC   =C'PAV',PODBFIL                                                  
         BE    VNETX                                                            
         CLC   =C'WTP',PODBFIL                                                  
         BE    VNETX                                                            
*                                                                               
VNET02   GOTO1 ANY                 GENERATE ERROR IF NO DATA                    
         USING T325FFD,R1                                                       
         GOTO1 SCANNER,PARAS,(R2),(14,BLOCK),C',=,('                            
         CLI   4(R1),0                                                          
         BE    BADNET                                                           
         MVI   BYTE,0                                                           
         MVC   PODNTNUM,4(R1)                                                   
         ZIC   R0,4(R1)            LOOP COUNTER                                 
*                                                                               
         LA    R3,PDSIDMKT                                                      
         LA    R4,PODNET                                                        
         LA    R5,BLOCK                                                         
*                                                                               
VNET03   TM    2(R5),X'80'         SIDMKT?                                      
         BNO   VNET04                                                           
         CLI   PODNTNUM,7          MAXIMUM NUMBER ALLOWED IS 7                  
         BH    BADNET2                                                          
         MVC   0(2,R3),6(R5)       SAVE SIDMKT                                  
         MVC   0(5,R4),12(R5)                                                   
         B     VNET24                                                           
*                                                                               
VNET04   TM    13(R5),C'0'         MARKET GROUP?                                
         BNO   VNET10                                                           
         CLI   12(R5),C'G'         MARKET GROUP DEF    G => ID <= Z             
         BL    VNET10                                                           
         CLI   12(R5),C'Z'                                                      
         BH    VNET10                                                           
         LA    RE,13(R5)                                                        
         ZIC   R1,0(R5)            LENGTH OF STATION                            
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    VNET08                                                           
*                                                                               
VNET06   TM    0(RE),C'0'          HAS TO BE NUMERIC                            
         BNO   VNET10              NOT, REGULAR STATION                         
         LA    RE,1(RE)                                                         
         BCT   R1,VNET06                                                        
*                                                                               
VNET08   MVC   0(5,R4),12(R5)      COPY MARKET GROUP                            
         MVI   PDMGROPT,C'Y'                                                    
         B     VNET24                                                           
*                                                                               
VNET10   CLC   =C'MENU=',12(R5)     COULD BE A SET RECORD                       
         BE    VNET54                                                           
         CLC   12(3,R5),=C'ZZZ'    OR ALL STATIONS                              
         BNE   VNET12              OR A REAL STATION                            
         MVI   BYTE,X'FF'                                                       
         MVC   0(3,R4),12(R5)                                                   
         OI    3(R4),X'40'                                                      
         MVC   4(1,R4),16(R5)                                                   
         MVC   PDZZZMED,PODBEXT+4                                               
         MVC   PDZZZTYP,4(R4)                                                   
         CLI   16(R5),C'S'         SYNDICATION                                  
         BNE   *+12                                                             
         MVI   PDZZZTYP,C'M'                                                    
         B     VNET14                                                           
         CLI   16(R5),C'H'         HISPANIC                                     
         BE    VNET14                                                           
         CLI   16(R5),C'C'         CABLE                                        
         BNE   *+12                                                             
         MVI   PDUSECAB,C'Y'                                                    
         B     VNET14                                                           
         MVI   4(R4),C'T'                                                       
         CLI   16(R5),C'N'         NETWORK                                      
         BE    VNET14                                                           
         L     R1,ATWA                                                          
         MVC   CONHEAD+29(5),12(R5)                                             
         B     BADNET                                                           
*                                                                               
VNET12   XC    WORK,WORK                                                        
         MVC   WORK+45(1),0(R5)                                                 
         MVC   WORK+48(10),12(R5)                                               
         CLI   WORK+49,C'-'        REPLACE DASH WITH UNDELINE                   
         BNE   *+8                                                              
         MVI   WORK+49,C'_'                                                     
         GOTO1 SCANNER,PARAS,WORK+40,(1,WORK),C',=$-'                           
         CLI   WORK+13,C'_'        REPLACE UNDELINE WITH DASH                   
         BNE   *+8                                                              
         MVI   WORK+13,C'-'                                                     
         MVC   0(4,R4),WORK+12                                                  
         MVI   4(R4),C'T'          DEFAULT MEDIA                                
         CLI   WORK+1,0                                                         
         BE    *+10                                                             
         MVC   4(1,R4),WORK+22                                                  
         CLI   WORK+22,C'C'        CABLE STATION                                
         BNE   *+8                                                              
         MVI   PDUSECAB,C'Y'                                                    
         MVI   5(R4),0                                                          
*                                                                               
*--VALIDATE BOOK TYPE                                                           
VNET14   L     R1,ATWA                                                          
         MVI   CONHEAD+29,C'('                                                  
         MVC   CONHEAD+30(1),22(R5)                                             
         CLI   4(R4),C'C'          IF CABLE, NO BOOK TYPE UNLESS                
         BNE   VNET16                 TYPE=U                                    
         CLI   PODBBTY,C'U'                                                     
         BNE   VNET22                                                           
         CLI   PODBMED,C'N'                                                     
         BNE   VNET22                                                           
*                                                                               
VNET16   CLC   PODBEXT(3),=C'NHW'  HISPANIC WEEKLY DOESN'T FORCE                
         BE    VNET20                                                           
         CLC   PODBEXT(3),=C'TCA'  TCAR WEEKLY DOESN'T FORCE                    
         BE    VNET20                                                           
         CLC   =C'NAD',PODBFIL     NAD FILES DON'T FORCE                        
         BE    VNET20                                                           
         CLC   =C'NAW',PODBFIL     NAD WEEKLY FILES DON'T FORCE                 
         BE    VNET20                                                           
         CLC   =C'IUN',PODBFIL     IUN FILES DON'T FORCE                        
         BE    VNET20                                                           
         CLI   PODBMED,C'T'        SPOT FILES HAVE DIFF. BOOK TYPES             
         BE    VNET20                                                           
         CLI   PODBMED,C'D'                                                     
         BE    VNET20                                                           
         CLI   PODBMED,C'C'        CANADA TOO                                   
         BE    VNET20                                                           
         CLI   PODBMED,C'W'        DON'T FORGET WEEKLY                          
         BE    VNET20                                                           
         MVI   5(R4),C'A'                                                       
         CLI   1(R5),0                                                          
         BE    VNET22                                                           
         CLI   22(R5),C'A'                                                      
         BE    VNET18                                                           
         CLI   22(R5),C'S'                                                      
         BE    VNET18                                                           
         CLI   22(R5),C'O'                                                      
         BE    VNET18                                                           
         CLI   22(R5),C'D'                                                      
         BE    VNET18                                                           
         CLI   22(R5),C'I'                                                      
         BNE   BADNET                                                           
*                                                                               
VNET18   MVC   5(1,R4),22(R5)      BOOKTYPE                                     
         B     VNET22                                                           
*                                                                               
VNET20   CLI   1(R5),0                                                          
         BE    VNET22                                                           
         CLI   22(R5),C'A'         SPOT BOOK TYPES:                             
         BL    BADNET                HAS TO BE IN A-Z                           
         CLI   22(R5),C'Z'                                                      
         BH    BADNET                                                           
         CLI   23(R5),C')'         ONLY 1 CHAR                                  
         BNE   BADNET                                                           
         B     VNET18                                                           
*                                                                               
VNET22   CLI   PODBMED,C'N'        NO SPILL FOR NETWORK                         
         BE    VNET24                                                           
         XC    WORK,WORK           SPILL MARKETS                                
         MVC   WORK+45(1),0(R5)                                                 
         MVC   WORK+48(10),12(R5)                                               
         GOTO1 SCANNER,PARAS,WORK+40,(1,WORK),C',=$/'                           
         CLI   WORK+1,0            NO SPILL MARKET?                             
         BZ    VNET24                                                           
         TM    WORK+3,X'80'        HAS TO BE A NUMBER                           
         BNO   BADNET                                                           
         SR    RF,RF                                                            
         ICM   RF,15,WORK+8                                                     
         STCM  RF,3,6(R4)          SAVE SPILL MARKET                            
         CLI   3(R4),C'/'          TAKE CARE OF 3CHAR STA                       
         BNE   *+8                                                              
         MVI   3(R4),C' '                                                       
*                                                                               
VNET24   LA    R5,32(R5)                                                        
         LA    R4,PODNETL(R4)                                                   
         LA    R3,2(R3)                                                         
         BCT   R0,VNET03                                                        
*                                                                               
         OC    PDSIDMKT,PDSIDMKT                                                
         BNZ   VNET26                                                           
         CLI   PDMGROPT,C'Y'                                                    
         BNE   VNET28                                                           
         CLI   PODNTNUM,1                                                       
         BH    BADNET                                                           
*                                                                               
VNET26   MVI   0(R4),X'FF'                                                      
         B     VNETX                                                            
*                                                                               
VNET28   MVI   0(R4),X'FF'                                                      
*                                                                               
*--CHECK STATIONS AGAINST NTI CODES                                             
         LA    R3,DBLOCKA                                                       
         USING DBLOCKD,R3                                                       
         LA    R4,PODNET                                                        
*                                                                               
VNET30   L     R5,APODBKL                                                       
         CLC   8(3,R5),=C'NAD'     NO CABLE NAD                                 
         BNE   VNET32                                                           
         CLI   PODBBTY,C'U'        EXCEPT IF USER FILE SPECIFIED                
         BE    VNET32                                                           
         CLI   4(R4),C'C'                                                       
         BE    BADNTBK                                                          
*                                                                               
VNET32   CLC   0(3,R4),=CL3'ZZZ'    DONT VALIDATE "ALL" STATION                 
         BE    VNET50                                                           
         XC    DBLOCK,DBLOCK                                                    
         OC    6(2,R4),6(R4)       ANY SPILL MARKET?                            
         BZ    *+10                                                             
         MVC   DBSELRMK,6(R4)                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBAREC,AIO                                                       
         MVC   DBSELAGY,AGENCY                                                  
*                                                                               
VNET34   CLC   8(3,R5),=CL3'EVN'   DONT VALIDATE PIV TYPES                      
         BE    VNET50                                                           
         CLC   8(3,R5),=CL3'MPA'                                                
         BE    VNET50                                                           
         MVI   DBFUNCT,DBVLNBK                                                  
         L     RF,=A(DBFUNTAB)                                                  
         A     RF,PGNR                                                          
*                                                                               
VNET36   CLC   1(5,R5),0(RF)                                                    
         BE    VNET38                                                           
         LA    RF,5(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   VNET36                                                           
         MVI   DBFUNCT,DBVLSTBK                                                 
*                                                                               
VNET38   MVC   DBSELSTA,0(R4)                                                   
         L     RF,=A(DBNADTAB)                                                  
         A     RF,PGNR                                                          
*                                                                               
VNET40   CLC   1(5,R5),0(RF)                                                    
         BE    VNET42                                                           
         LA    RF,7(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   VNET40                                                           
         B     VNET46                                                           
*                                                                               
VNET42   CLC   8(3,R5),=C'NAD'       NAD SYNDICATION                            
         BNE   *+8                                                              
         CLI   DBSELSTA+4,C'S'                                                  
         BNE   *+12                                                             
         MVI   DBSELSTA+4,C'M'                                                  
         B     VNET46                                                           
         CLI   DBSELSTA+4,C'S'     NTI SYNDICATION                              
         BE    VNET46                                                           
         CLI   DBSELSTA+4,C'H'     HISPANIC                                     
         BNE   VNET44                                                           
         CLC   PODBEXT(3),=C'NHW'                                               
         BE    *+10                                                             
         CLC   PODBEXT,=C'NHT  '                                                
         BE    *+10                                                             
         CLC   PODBEXT,=C'NAD  '                                                
         BE    *+10                                                             
         CLC   PODBEXT,=C'NAW  '                                                
         BE    *+10                                                             
         MVC   DBSELSTA+3(1),5(RF)     FOR NAD-T,NAD-D, NAW-T & NAW-D           
         MVI   DBSELSTA+4,C'H'                                                  
         B     VNET46                                                           
*                                                                               
VNET44   MVC   DBSELSTA+3(2),5(RF)                                              
*                                                                               
VNET46   CLC   PODBEXT(3),=C'BBM'                                               
         BNE   VNET47                                                           
         MVI   DBFUNCT,DBGETTLB                                                 
         MVC   DBBTYPE,PODBBTY                                                  
         CLI   DBBTYPE,C'W'        WEEKLY BOOK TYPE?                            
         BNE   VNET47              NO                                           
         MVI   DBBEST,C'M'         YES, ASSUME M/Y REQUEST                      
         CLI   SVBBMWK,C'Y'        WAS IS M/D/Y?                                
         BNE   *+8                 NO                                           
         MVI   DBBEST,C'N'         YES, DBBEST                                  
*                                                                               
VNET47   MVC   DBFILE,8(R5)                                                     
         MVC   DBSELMED,7(R5)                                                   
         MVC   DBSELSRC,6(R5)                                                   
         MVC   DBSELBK,12(R5)                                                   
         TM    DBSELBK+1,X'80'                                                  
         BNO   *+8                                                              
         NI    DBSELBK+1,X'7F'     EXTRACT ESTIMATE BIT                         
*                                                                               
         LA    RE,SETNFF           SET FOR FLAT FILE                            
         ST    RE,DBEXTEND                                                      
         CLC   =C'NHW-H',1(R5)                                                  
         BE    *+10                                                             
         CLC   =C'NHT-H',1(R5)                                                  
         BE    *+10                                                             
         CLC   =C'NHI-T',1(R5)                                                  
         BNE   *+10                                                             
         MVC   DBFILE,=C'NTI'                                                   
                                                                                
         CLC   PODBEXT(3),=C'TCA'  TCAR WEEKLY NEEDS BOOK TYPE                  
         BNE   *+8                                                              
         MVI   DBBTYPE,C'V'                                                     
                                                                                
         CLC   DBFILE,=C'IUN'                                                   
         BE    VNET50                                                           
         GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         MVC   DBFILE,8(R5)                                                     
         XC    DBEXTEND,DBEXTEND                                                
         CLI   DBERROR,0                                                        
         BE    VNET50                                                           
*                                                                               
*     TRY FOR SPILL AND/OR BOOK TYPE IF REQUESTED                               
         MVC   DBSELMK,6(R4)       SET KEY MARKET                               
         MVC   DBBTYPE,5(R4)       SET BOOK TYPE                                
         CLI   DBBTYPE,C'A'                                                     
         BNL   VNET48                                                           
         CLI   PODBBTY,C'A'                                                     
         BL    VNET48                                                           
         MVC   DBBTYPE,PODBBTY                                                  
*                                                                               
VNET48   GOTO1 DEMAND,DMCB,DBLOCK,0                                             
         MVC   DBFILE,8(R5)                                                     
         CLI   DBERROR,0                                                        
         BNE   VNET52                                                           
*                                                                               
VNET50   LA    R4,PODNETL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BE    VNET58                                                           
         B     VNET30                                                           
*                                                                               
VNET52   DS    0H                                                               
         LA    R5,PODBLNQ(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BE    BADNTBK                                                          
         B     VNET34                                                           
*                                                                               
         USING RSETD,RE                                                         
VNET54   CLI   PDOVSYS,8                                                        
         BNE   BADNET                                                           
         LA    RE,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,AGENCY                                                  
         MVC   RSETKSET,=C'MS'     I'M CONFUSED HERE                            
         MVC   RSETKID(4),17(R5)   GET PAST "MENU="                             
         OC    RSETKID(4),SPACES                                                
         DROP  RE                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI  '),=C'REPDIR  ',KEY,KEY,0             
         CLC   KEY(RSETKID+4-RSETKTYP),KEYSAVE                                  
         BNE   BADNET                                                           
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=CL8'REPFIL',KEY+28,AIO1,     X        
               DMWORK,0                                                         
         USING RSETD,RE                                                         
         L     RE,AIO1             SET TO IO AREA                               
         LA    RE,RSETELEM         DETERMINE NUMBER OF ENTRIES                  
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         USING RSETMEMD,RE                                                      
         SR    R0,R0                                                            
         ZIC   R1,RSETMELN                                                      
         ZIC   RF,RSETMLEN                                                      
         DR    R0,RF                                                            
         LA    RE,RSETMEMB                                                      
*                                                                               
VNET56   MVC   0(5,R4),0(RE)       SEED ENTRIES IN STATION LIST                 
         AR    RE,RF                                                            
         LA    R4,PODNETL(R4)                                                   
         BCT   R1,VNET56                                                        
*                                                                               
         SHI   R4,PODNETL                                                       
         B     VNET24                                                           
*                                                                               
*--READ STATION RECORD TO VALIDATE NETWORK                                      
VNET58   L     R5,APODBKL                                                       
         LA    R4,PODNET                                                        
*                                                                               
VNET60   CLI   0(R5),X'FF'                                                      
         BE    VNET66                                                           
         CLC   8(3,R5),=CL3'EVN'                                                
         BE    VNET62                                                           
         LA    R5,PODBLNQ(R5)                                                   
         B     VNET60                                                           
*                                                                               
VNET62   CLC   0(3,R4),=CL3'ZZZ'    DONT VALIDATE "ALL" STATION                 
         BE    VNET66                                                           
         MVC   KEY(17),=C'SNABC NAA00000000'                                    
         USING STAREC,R3                                                        
         LA    R3,KEY                                                           
         MVC   STAKCALL(4),0(R4)                                                
         MVC   STAKAGY,AGENCY                                                   
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=C'STATION'                                             
         GOTO1 HIGH                                                             
         XC    6(2,R4),6(R4)                                                    
         CLC   KEY(15),KEYSAVE                                                  
         BNE   VNET64                                                           
         L     R3,AIO                                                           
         PACK  DUB,SMKT            CONVERT 'MARKET' NUMBER                      
         CVB   R1,DUB                                                           
         STH   R1,DUB                                                           
         MVC   6(2,R4),DUB         MARKET                                       
*                                                                               
VNET64   LA    R4,PODNETL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   VNET62                                                           
         XC    FILENAME,FILENAME                                                
         DROP  R3                                                               
*                                                                               
VNET66   ZIC   R3,PODNTNUM                                                      
         LA    R4,PODNET                                                        
*                                                                               
VNET68   DS    0H                                                               
         BAS   RE,NEWSTCHK                                                      
         BNZ   BADNTBK                                                          
         LA    R4,PODNETL(R4)                                                   
         CLI   0(R4),X'FF'                                                      
         BNE   VNET68                                                           
         L     R1,ATWA                                                          
         MVC   CONHEAD,SPACES      CLEAR ERROR AREA                             
*                                                                               
VNETX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL VALNET ROUTINES                                   *         
***********************************************************************         
*                                                                     *         
*        CHECK SOURCE BOOKS                                           *         
*                                                                     *         
NEWSTCHK NTR1                                                                   
         MVC   DUB(3),=CL3'PIV'                                                 
         GOTO1 CHKSRCE,DMCB,(1,DUB)                                             
         BZ    NEWS02                                                           
         CLC   0(3,R4),=CL3'ZZZ'                                                
         BE    NEWSERR                                                          
*                                                                               
NEWS02   MVC   DUB(3),=CL3'NAD'                                                 
         GOTO1 CHKSRCE,DMCB,(3,DUB)                                             
         BZ    NEWS04              SOURCE NOT FOUND                             
         CLI   5(R4),C'S'          TEST SEASON TO DATE                          
         BE    NEWSERR                                                          
*                                                                               
NEWS04   MVC   DUB(3),=CL3'NTI'                                                 
         GOTO1 CHKSRCE,DMCB,(3,DUB)                                             
         BZ    NEWSX               SOURCE NOT FOUND                             
         CLI   5(R4),C'S'          TEST SEASON TO DATE                          
         BE    NEWSERR                                                          
*                                                                               
NEWSX    SR    RE,RE                                                            
*                                                                               
NEWSERR  LTR   RE,RE                                                            
         B     XIT                                                              
*                                                                     *         
*        CHECK BOOK TABLE FOR SOURCE                                  *         
*                                                                     *         
CHKSRCE  NTR1                                                                   
         ZIC   R3,0(R1)                                                         
         BCTR  R3,0                                                             
         L     R4,0(R1)                                                         
         L     R5,APODBKL                                                       
*                                                                     *         
CHKS02   EX    R3,*+12                                                          
         BE    CHKSX                                                            
         BNE   CHKS04                                                           
         CLC   0(0,R4),1(R5)                                                    
*                                                                     *         
CHKS04   LA    R5,PODBLNQ(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BNE   CHKS02                                                           
         SR    R4,R4               SOURCE NOT FOUND                             
*                                                                               
CHKSX    LTR   R4,R4                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE DAY/TIME                                            *         
***********************************************************************         
*                                                                               
VVALDYTM GOTO1 ANY                                                              
         MVI   NADUSE,C'N'         NAD NOT USED                                 
         L     R5,APODBKL          CHECK IF NAD USED                            
         ST    R2,SAVER2           R2 GETS REUSED HERE                          
*                                                                               
VDAY02   CLI   0(R5),X'FF'         END OF BOOKS?                                
         BE    VDAY06                                                           
         CLC   =C'NHT-D',1(R5)     WAS NHT-D USED?                              
         BE    *+10                  .....OR                                    
         CLC   =C'NAD-D',1(R5)     WAS NAD-D USED?                              
         BNE   VDAY04              NO, NEXT BOOK                                
         MVI   NADUSE,C'Y'                                                      
         B     VDAY06                                                           
*                                                                               
VDAY04   LA    R5,PODBLNQ(R5)                                                   
         B     VDAY02                                                           
*                                                                               
VDAY06   XC    PODDAYTM,PODDAYTM                                                
         LA    R4,PODDAYTM                                                      
         LA    RE,BLOCK                                                         
         LA    RF,480                                                           
         XCEFL                                                                  
         XC    HALF,HALF           USE HALF TO STORE PREV & CURR DAY            
*                                                                               
         GOTO1 SCANNER,PARAS,(20,(R2)),(10,BLOCK),C',=,/'                       
         CLI   4(R1),0                                                          
         BE    BADDAY                                                           
         LA    R5,BLOCK                                                         
*                                                                               
         CLI   0(R5),0             END OF TABLE                                 
         BE    VDAY30              YES EXIT                                     
*                                                                               
VDAY08   MVI   BYTE,2              ASSUME FIELD IS DIVIDED                      
         MVI   0(R4),X'FF'         CHECK FOR 'ALL'                              
         MVI   HALF+1,X'FF'        HOLD ONTO THE POTENTIAL CURRENT DAY          
         CLC   12(4,R5),=C'DALL'                                                
         BE    VDAY24              BUMP BLOCK                                   
         CLI   NADUSE,C'Y'                                                      
         BE    BADDAY                                                           
*                                                                               
*--CHECK FOR DAY TIME MACRO DEFINITIONS                                         
*                                                                               
         L     R3,=A(DYTMMAC)                                                   
         A     R3,PGNR                                                          
*                                                                               
VDAY10   CLC   0(6,R3),12(R5)                                                   
         BE    VDAY12                                                           
         LA    R3,17(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BE    VDAY16                                                           
         B     VDAY10                                                           
*                                                                               
*---LOAD IN MACRO DAY AND TIME                                                  
VDAY12   MVC   0(10,R4),6(R3)                                                   
         CLC   0(5,R3),=C'PRIME'                                                
         BNE   VDAY14                                                           
         L     RF,APODBKL                                                       
         CLC   1(3,RF),=C'PIV'                                                  
         BE    VDAY14                                                           
         MVC   0(10,R4),=X'7E07D008FC01076C08FC' NORMAL PRIME                   
*                                                                               
VDAY14   XC    HALF,HALF           START PREV & CURR DAY AGAIN                  
         ZIC   RE,16(R3)                                                        
         AR    R4,RE                                                            
         B     VDAY28                                                           
*                                                                               
VDAY16   XC    WORK,WORK                                                        
         MVC   WORK+45(1),0(R5)    MOVE FIELD VALUE                             
         MVC   WORK+48(10),12(R5)  MOVE FIELD VALUE                             
         GOTO1 SCANNER,PARAS,WORK+40,(1,WORK),C',=,-'                           
         CLI   WORK,0                                                           
         BE    BADDAY                                                           
         CLI   WORK+1,0                                                         
         BNE   VDAY20                                                           
         L     R3,=A(DAYLIST)                                                   
         A     R3,PGNR                                                          
*                                                                               
VDAY18   MVC   0(1,R4),9(R3)                                                    
         MVC   HALF+1(1),9(R3)     HOLD ONTO THE POTENTIAL CURRENT DAY          
         SR    RE,RE                                                            
         ICM   RE,1,WORK                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK+12(0),0(R3)                                                 
         BE    VDAY24                                                           
         LA    R3,10(R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   VDAY18                                                           
*                                                                               
* MIGHT BE REGULAR EXPRESSION                                                   
         GOTO1 DAYVAL,DMCB,(WORK,WORK+12),DMCB+20,DMCB+21                       
         MVC   0(1,R4),DMCB+20                                                  
         CLI   DMCB+20,0                                                        
         BE    VDAY22                                                           
         OI    0(R4),X'80'                                                      
         MVC   HALF+1(1),0(R4)     HOLD ONTO THE CURRENT DAY                    
         B     VDAY24                                                           
*                                                                               
*--CHECK DAY RANGES (MONDAY-FRIDAY OR MONDAY-SATURDAY)                          
VDAY20   SR    RE,RE                                                            
         ICM   RE,1,WORK                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK+12(0),=CL6'MONDAY'                                          
         BNE   VDAY22                                                           
*                                                                               
         MVI   0(R4),X'7C'                                                      
         MVI   HALF+1,X'7C'        HOLD ONTO THE POTENTIAL CURRENT DAY          
         SR    RE,RE                                                            
         ICM   RE,1,WORK+1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK+22(0),=CL6'FRIDAY'                                          
         BE    VDAY24                                                           
*                                                                               
         MVI   0(R4),X'7F'                                                      
         MVI   HALF+1,X'7F'        HOLD ONTO THE POTENTIAL CURRENT DAY          
         SR    RE,RE                                                            
         ICM   RE,1,WORK+1                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK+22(0),=CL6'SUNDAY'                                          
         BE    VDAY24                                                           
*                                                                               
VDAY22   CLI   1(R5),0             IF THIS IS A DIVIDED FIELD,                  
         BNE   BADDAY               THEN THE DAY IS INVALID                     
         CLI   HALF,0              IF NO PREVIOUS DAY,                          
         BE    BADDAY               THEN WE MUST HAVE A VALID DAY               
         MVC   0(1,R4),HALF        ELSE, WE CAN USE PREVIOUS DAY                
         MVI   BYTE,1              ASSUME DATA IN SINGLE FIELD IS TIME,         
         MVC   HALF+1(1),HALF       UPDATE THE CURRENT TO PREV DAY,             
*                                                                               
VDAY24   MVC   HALF(1),HALF+1      CURR DAY BECOMES PREV DAY                    
         LA    R2,12(R5)           LET R2 POINT TO DATA IN FIELD                
         ZIC   R3,0(R5)                                                         
         CLI   BYTE,1              IS THIS A SINGLE FIELD?                      
         BE    VDAY26               YES                                         
         LA    R2,10(R2)            NO, RESET R2 AND L(DATA)                    
         ZIC   R3,1(R5)                                                         
*                                                                               
VDAY26   MVI   1(R4),X'FF'         CHECK FOR 'ALL'                              
         CLC   0(4,R2),=C'TALL'                                                 
         BNE   VDAY27                                                           
         CLI   DAYPOPT,C'Y'        DO WE HAVE DAYPART OPTION?                   
         BE    BADTIME2            YES, TALL NOT VALID                          
         B     VDAY28                                                           
*                                                                               
VDAY27   CLI   NADUSE,C'Y'                                                      
         BE    BADTIME                                                          
         LTR   R3,R3                                                            
         BZ    BADTIME                                                          
         GOTO1 TIMVAL,PARAS,((R3),0(R2)),1(R4)                                  
         CLI   0(R1),X'FF'                                                      
         BE    BADTIME                                                          
*                                                                               
         OC    3(2,R4),3(R4)       TEST FOR END TIME                            
         BNZ   *+10                                                             
         MVC   3(2,R4),1(R4)       NO-SET END TIME=START TIME                   
*                                                                               
         MVC   DUB,1(R4)           ADJUST TIME FOR 12-6AM                       
         LH    R1,DUB                                                           
         CH    R1,=H'600'                                                       
         BNL   *+8                                                              
         AH    R1,=H'2400'                                                      
         STH   R1,DUB                                                           
         LH    R1,DUB+2                                                         
         CH    R1,=H'600'                                                       
         BH    *+8                                                              
         AH    R1,=H'2400'                                                      
         STH   R1,DUB+2                                                         
         CLC   DUB+2(2),DUB        ENSURE END GT OR EQ START                    
         BL    BADTIME                                                          
         MVC   1(4,R4),DUB                                                      
*                                                                               
VDAY28   LA    R5,42(R5)           BUMP TO NEXT BLOCK                           
         CLI   0(R5),0                                                          
         BE    VDAY30                                                           
         LA    R4,5(R4)                                                         
         B     VDAY08                                                           
*                                                                               
VDAY30   LA    R4,5(R4)                                                         
         MVI   0(R4),X'99'         SET END OF TABLE MARK                        
         LA    RE,PODDAYTM                                                      
         LA    RF,PODDAYTM                                                      
*                                                                               
VDAY32   OC    1(4,RE),1(RE)                                                    
         BNZ   VDAY38                                                           
         CLI   0(RE),X'99'                                                      
         BE    VDAY40                                                           
*                                                                               
VDAY34   OC    1(4,RF),1(RF)                                                    
         BNZ   VDAY36                                                           
         LA    RF,5(RF)                                                         
         CLI   0(RF),X'99'                                                      
         BNE   VDAY34                                                           
         B     BADTIME                                                          
*                                                                               
VDAY36   MVC   1(4,RE),1(RF)                                                    
         LA    RE,5(RE)                                                         
         CR    RE,RF                                                            
         BE    VDAY32                                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
         B     VDAY36                                                           
*                                                                               
VDAY38   LA    RE,5(RE)                                                         
         LA    RF,5(RF)                                                         
         CLI   0(RE),X'99'                                                      
         BNE   VDAY32                                                           
*                                                                               
VDAY40   CLI   PODNADBK,X'F0'      IF SPOT BOOK AND                             
         BNE   VDAY42                                                           
         CLI   PODBMED,C'T'         TV REQUESTED,                               
         BE    VDAYX                THEN DONT CHECK OVERLAP                     
         CLI   PODBMED,C'C'         TV REQUESTED,                               
         BE    VDAYX                THEN DONT CHECK OVERLAP                     
*                                                                               
VDAY42   BAS   RE,CKOVLAP          CHECK DAY TIME OVERLAP                       
         BNZ   DTOVER              IF OK EXIT                                   
*                                                                               
VDAYX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL VALDYTM ROUTINE                                   *         
***********************************************************************         
*                                                                     *         
*        CHECK FOR DAY/TIME OVERLAPS                                  *         
*                                                                     *         
CKOVLAP  NTR1                                                                   
         LA    R3,PODDAYTM                                                      
*                                                                               
CKOV02   LA    R4,PODDAYTM                                                      
*                                                                               
CKOV04   CR    R3,R4               ARE WE AT SAME LOCATION                      
         BE    CKOV08              YES BYPASS                                   
         CLI   0(R3),X'FF'         IS DAY=ALL                                   
         BE    CKOV06                                                           
         CLI   0(R4),X'FF'         IS DAY=ALL                                   
         BE    CKOV06                                                           
         CLC   0(1,R3),0(R4)       ARE DAYS THE SAME                            
         BNE   CKOV08              NO BYPASS                                    
*                                                                               
CKOV06   CLI   1(R3),X'FF'         IS TIME=ALL                                  
         BE    CKOVERR                                                          
         CLI   1(R4),X'FF'         IS TIME=ALL                                  
         BE    CKOVERR                                                          
         CLC   1(2,R3),3(R4)       IS START1 GREATER THEN END2                  
         BNL   CKOV08              YES BYPASS                                   
         CLC   3(2,R3),1(R4)       IS END1 LESS THEN START2                     
         BNH   CKOV08              YES BYPASS                                   
         B     CKOVERR             ELSE ERROR CONDITION                         
*                                                                               
CKOV08   LA    R4,5(R4)                                                         
         CLI   0(R4),X'99'         IS TABLE TWO AT END                          
         BNE   CKOV04                                                           
         LA    R3,5(R3)                                                         
         CLI   0(R3),X'99'         IS TABLE ONE AT END                          
         BNE   CKOV02                                                           
         SR    R3,R3                                                            
*                                                                               
CKOVERR  LTR   R3,R3                                                            
*                                                                               
CKOVX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        PICK UP HUTS FOR PROGRAM                                     *         
*              INPUTS              HUTTIME REQUESTED START/END        *         
*                                  PDDAY6 REQUESTED DAY NUMBER        *         
*              OUTPUT              HUT RETURNED IN HUT                *         
***********************************************************************         
*                                                                               
VVPRGHUT CLI   HUTSW,0             INITIALIZE FIRST TIME THROUGH                
         BNE   *+8                                                              
         BAS   RE,HUTINIT                                                       
         MVI   HUTSW,1                                                          
         ZIC   R3,PDDAY                                                         
         MH    R3,=H'96'                                                        
         LA    R3,HUTVALS(R3)      PICK UP HUTS FOR DAY                         
         MVC   DBLOCKA(96),0(R3)                                                
         LA    R4,KEY                                                           
         BAS   RE,GETAHUTS                                                      
         MVC   0(96,R3),DBLOCKA    ROUTINE UPDATES VALUES                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL ROUTINES FOR VPRGHUT                              *         
***********************************************************************         
*                                                                     *         
*        LOAD UP HUT VALUES                                           *         
*                                                                     *         
HUTINIT  NTR1                                                                   
         LA    R3,HUTVALS                                                       
         LA    R4,9                                                             
*                                                                               
HUTI02   XC    0(96,R3),0(R3)                                                   
         LA    R3,96(R3)                                                        
         BCT   R4,HUTI02                                                        
*                                                                               
HUTIX    B     XIT                                                              
*                                                                     *         
*        COMPUTE HUTS                                                 *         
*                                                                     *         
GETAHUTS NTR1                                                                   
         LA    R2,HUTTIME          CONVERT TIME TO QUARTERS                     
         LA    R3,HUTQ                                                          
         BAS   RE,GETQ                                                          
         MVC   HUTQ+1(1),HUTQ                                                   
         LA    R2,HUTTIME+2                                                     
         LA    R3,HUTQ+1                                                        
         OC    0(2,R2),0(R2)                                                    
         BZ    *+8                                                              
         BAS   RE,GETQ                                                          
*                                                                               
         SR    R2,R2               ADD HUTS IN R2                               
         LA    R3,1                COUNT IN R3                                  
*                                                                               
GETA02   ZIC   R1,HUTQ                                                          
         SRL   R1,1                                                             
         SLL   R1,1                                                             
         LA    R1,DBLOCKA(R1)      LOOK UP HUT FOR THIS 1/2 HOUR                
         BAS   RE,GETDHUTS                                                      
         AH    R2,0(R1)                                                         
         AI    HUTQ,2                                                           
         CLC   HUTQ(1),HUTQ+1                                                   
         BNL   GETA04                                                           
         LA    R3,1(R3)                                                         
         B     GETA02                                                           
*                                                                               
GETA04   LR    R0,R2               AVERAGE HUTS                                 
         SRDA  R0,31                                                            
         DR    R0,R3                                                            
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         STH   R1,HUT                                                           
*                                                                               
GETAX    B     XIT                                                              
*                                                                     *         
*        GET QUARTERS                                                 *         
*                                                                     *         
GETQ     NTR1                                                                   
         LH    R1,0(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CH    R1,=H'6'                                                         
         BNL   *+8                                                              
         LA    R1,24(R1)                                                        
         SH    R1,=H'6'                                                         
         SLL   R1,2                                                             
         LR    R2,R1                                                            
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R2,R1                                                            
         STC   R2,0(R3)                                                         
*                                                                     *         
GETQX    B     XIT                                                              
*                                                                     *         
*        REFRESH HUT VALUE FOR SPECIFC PDDAY HUTQ                     *         
*                                                                     *         
GETDHUTS NTR1                                                                   
         OC    0(2,R1),0(R1)       HAVE WE LOOKED THIS UP BEFORE                
         BNZ   GETDX                                                            
         LR    R2,R1                                                            
         LA    R5,DBLOCKA+100      SET UP BLOCK FOR GETHUT                      
         USING GETHUTD,R5                                                       
         XC    GHBLOCK,GHBLOCK                                                  
         MVI   GHPREVYR,C'N'       DON'T TRY PREV. YR. HUTS                     
         MVC   GHREPDAY,PDDAY                                                   
         MVC   GHQUARTS,HUTQ                                                    
         MVC   GHQUARTS+1,HUTQ                                                  
         MVI   GHSCHEME,X'FE'      PRESET FROM YEAR RECORDS                     
         MVI   GHAVE,C'W'          SET UP FOR WEEKLY HUTS                       
         MVI   GH52,C'Y'           PRESET 52 WEEK OPTION                        
         CLI   HUT52,0                                                          
         BE    *+10                                                             
         MVC   GH52,HUT52                                                       
         MVC   GHBKTYPE,HUTTYPE    HUT TYPE (D, A, I OR C)                      
         MVC   GHSURVY,PDHUTTYP                                                 
*                                  DEFAULT IS ASCRIBED                          
         CLI   GHBKTYPE,0                                                       
         BNE   *+8                                                              
         MVI   GHBKTYPE,C'A'                                                    
*                                                                               
         CLI   GHBKTYPE,C'D'                                                    
         BNE   *+8                                                              
         MVI   GHBKTYPE,C'O'       (GETHUT MUST'NT CHANGE 'D' TO X'00')         
         MVC   GHBOOKS(2),PDSBOOK                                               
         MVC   GHBOOKS+2(2),PDSBOOK                                             
*                                                                               
*--CONVERT BOOK TO DEMO FILE WEEK                                               
         L     RE,=A(YRINDX)                                                    
         A     RE,PGNR                                                          
*                                                                               
GETD02   CLC   GHBOOKS(1),0(RE)    EXCEPTION YEAR                               
         BE    *+8                                                              
         CLI   0(RE),X'FF'         DEFAULT                                      
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     GETD02                                                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(RE)          CONVERT THE WEEK                             
         L     RE,=A(YRINDX)                                                    
         A     RE,PGNR                                                          
         AR    RE,R0                                                            
         ZIC   RF,GHBOOKS+1                                                     
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         MVC   GHBOOKS+1(1),0(RE)                                               
         MVC   GHBOOKS+2(2),GHBOOKS                                             
*                                                                               
         MVC   GHCOMFCS,ACOMFACS                                                
         MVC   GHAGY,AGENCY        FOR LOCKOUTS                                 
         CLI   HUTSCHEM,0                                                       
         BE    GETD04                                                           
         MVC   GHSCHEME,HUTSCHEM   AGENCY SCHEME REQUESTED                      
         MVC   GHAGYMED,PDBAGYMD                                                
*                                                                               
GETD04   GOTO1 GETHUT,DMCB,(R5)                                                 
         MVC   0(2,R2),GHHUT                                                    
         CLI   HUTSCHEM,0                                                       
         BE    GETDX                                                            
         GOTO1 HIGH                NEED TO RESTORE SEQUENCE                     
*                                                                               
GETDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PROGRAM PERIOD                                      *         
***********************************************************************         
*                                                                               
VVALPROG DS    0H                                                               
         XC    PODPSTRT(4),PODPSTRT                                             
         CLI   5(R2),0                                                          
         BNE   VVALPR02                                                         
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BNE   INVPROG                                                          
         B     VVALPRX                                                          
*                                                                               
VVALPR02 GOTO1 DATVAL,PARAS,(0,8(R2)),WORK                                      
         OC    PARAS(4),PARAS                                                   
         BZ    INVPROG                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,PODPSTRT)                                
         MVC   PROGSTRT(6),WORK    SAVE EBCDIC FORMAT                           
*                                                                               
         BAS   RE,BUMP             GET END DATE                                 
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         MVC   PODPEND,PODPSTRT    IF NO END START AND END EQUAL                
         B     VVALPR04                                                         
*                                                                               
         GOTO1 DATVAL,PARAS,(0,8(R2)),WORK                                      
         OC    PARAS(4),PARAS                                                   
         BZ    INVPROG                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,PODPEND)                                 
*                                                                               
VVALPR04 CLC   PODPSTRT,PODPEND                                                 
         BH    INVPROG                                                          
*                                                                               
VVALPRX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                             *         
***********************************************************************         
*                                                                               
VVALOPTS DS    0H                                                               
         CLI   0(R1),X'FF'                                                      
         BE    VVALOP2                                                          
         GOTO1 =A(VALOPXNS),DMCB,0,(R2),(R9),(RC),RR=PGNR                       
         B     VVALOPX                                                          
*                                                                               
VVALOP2  GOTO1 =A(VALOPXNS),DMCB,(X'FF',DUB),(R2),(R9),(RC),RR=PGNR             
*                                                                               
         CLI   PDSIDOPT,C'Y'       ARE WE USING SID?                            
         BNE   VVALOPX                                                          
         CLI   PDSDPER,0                                                        
         BE    PERERR2                                                          
*                                                                               
VVALOPX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE TITLE                                               *         
***********************************************************************         
*                                                                               
VVALTITS MVC   TITLE,SPACES                                                     
         MVC   TITLE(22),=C'RESEARCH REPORT WRITER'                             
         CLI   5(R2),0                                                          
         BE    VTIT02                                                           
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
VTIT02   GOTO1 CENTER,DMCB,TITLE,64                                             
*                                                                               
VTITX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE FILTERS                                             *         
***********************************************************************         
*                                                                               
VVALFILT L     RE,APDPDFLT         CLEAR PROGRAM/NETWORK TABLE (PDF)            
         LA    RF,L'PDPDFLT                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,APDPRGTT         CLEAR DATE/TIMES TABLE (PDF)                 
         LA    RF,L'PDPRGTT                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,APDPRGTT                                                      
         STCM  RE,15,PRGTPTR       SAVE TABLE ADDRESS                           
*                                                                               
         L     RE,APDSUBPT         CLEAR SUB PROGRAM TABLE (SUBPTYPE)           
         LA    RF,L'PDSUBPT                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,APDSUBPT                                                      
         MVI   0(RE),X'FF'         SET END MARKER                               
*                                                                               
         XC    PODNTFLT(147),PODNTFLT   CLEAR AREAS                             
         MVI   PODNTFLT,X'FF'      SET END OF FILTERS MARK                      
         MVI   PODNDFLT,X'FF'                                                   
         MVI   PODNHFLT,X'FF'                                                   
         MVI   PODPVFLT,X'FF'                                                   
         XC    PDHISPNM,PDHISPNM                                                
         MVI   PDHISPLN,0                                                       
         CLI   5(R2),0                                                          
         BE    VFILX                                                            
*                                                                               
         XCEFL MYBLOCK,800                                                      
         GOTO1 SCANNER,DMCB,(20,(R2)),(X'8E',MYBLOCK),0 14 LINES MAX            
         ZIC   R0,4(R1)                                                         
         LA    R3,MYBLOCK                                                       
         LTR   R0,R0                                                            
         BZ    BADFILT                                                          
         MVI   FIELDERR,1                                                       
         B     VFIL04                                                           
*                                                                               
VFIL02   CLI   1(R3),0                                                          
         BE    VFIL08                                                           
*                                                                               
VFIL04   L     R4,APDPDFLT         ADDRESS PDF TABLE                            
         CLC   12(3,R3),=C'PDF'    PDF FILTER IN USE?                           
         BE    VFIL40              YES                                          
         LA    R4,PODNTFLT                                                      
         CLC   12(3,R3),=C'NTI'    NTI USES THIS AREA                           
         BE    VFIL06                                                           
         LA    R4,PODNDFLT                                                      
         CLC   12(3,R3),=C'NAD'    NAD & NHT USE A SEPARATE AREA                
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'NHT'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'MPA'                                                 
         BE    VFIL06                                                           
         CLC   12(2,R3),=C'TP'                                                  
         BE    VFIL06                                                           
         CLC   12(2,R3),=C'T4'                                                  
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'PAV'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'IUN'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'DPT'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'CSI'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'BBM'                                                 
         BE    VFIL06                                                           
         CLC   12(3,R3),=C'SRC'                                                 
         BE    VFIL06                                                           
         LA    R4,PODPVFLT                                                      
         CLC   12(3,R3),=C'PIV'                                                 
         BE    VFIL06                                                           
         CLC   =C'PNAME',12(R3)                                                 
         BE    VFIL16                                                           
         CLC   12(4,R3),=C'SUBPTYPE'                                            
         BE    VFIL50                                                           
         B     VFIL18                                                           
*                                                                               
VFIL06   MVC   0(1,R3),1(R3)                                                    
         MVC   12(10,R3),22(R3)                                                 
         LA    R0,11                                                            
*                                                                               
VFIL08   ZIC   RF,0(R3)                                                         
         LTR   RF,RF                                                            
         BZ    VFIL14                                                           
         LR    R1,R4                                                            
         LA    RE,12(R3)                                                        
*                                                                               
VFIL10   CLI   0(RE),C'-'          NEGATE?                                      
         BNE   VFIL12                                                           
         LTR   RF,RF                                                            
         BZ    BADFILT                                                          
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BZ    BADFILT                                                          
         LA    RE,1(RE)                                                         
         NI    0(RE),X'BF'                                                      
*                                                                               
VFIL12   MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,VFIL10                                                        
*                                                                               
         LA    R4,6(R4)            AND ADDRESS THE NEXT AREA                    
         MVI   0(R4),X'FF'                                                      
*                                                                               
VFIL14   LA    R3,42(R3)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VFIL02                                                        
         MVI   0(R4),X'FF'                                                      
         B     VFILX                                                            
*                                                                               
VFIL16   BAS   RE,BMPSTRG          BUMP THRU TO FIND LENGTH                     
         BNE   BADPREF                                                          
         ZIC   R1,PDHISPNM+1                                                    
         LA    RF,8(R2)                                                         
         ZIC   RE,PDHISPNM                                                      
         AR    RF,RE                                                            
         SR    R1,RE                                                            
         STC   R1,PDHISPLN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PDHISPNM(0),0(RF)                                                
         GOTO1 HISPNAME,DMCB,(PDHISPLN,PDHISPNM),0,SCANNER                      
         CLI   DMCB,0              MATCH?                                       
         BNZ   BADPREF             NO, ERRORS                                   
*                                                                               
         LTR   R0,R0                                                            
         BNZ   VFIL14                                                           
         MVI   0(R4),X'FF'                                                      
         B     VFILX                                                            
*                                                                               
VFIL18   CLC   12(3,R3),=C'NOR'    OPTION TO SUPPRESS (NOR) LINES               
         BNE   VFIL20               PAV ONLY                                    
         MVC   PDNOROPT,22(R3)                                                  
         CLI   22(R3),C'N'                                                      
         BE    VFIL36                                                           
         CLI   22(R3),C'Y'                                                      
         BE    VFIL36                                                           
         B     BADFILT                                                          
*                                                                               
VFIL20   CLC   12(3,R3),=C'WK '    NUMBER OF WEEKS OPTION                       
         BNE   VFIL22                                                           
         CLI   22(R3),C'0'         LEAD MUST BE NUMERI                          
         BL    BADFILT                                                          
         CLI   22(R3),C'9'                                                      
         BH    BADFILT                                                          
         MVC   PDWKOPT,22(R3)      GET THE NUMBER OF WEEKS                      
         NI    PDWKOPT,X'0F'                                                    
         CLI   23(R3),C'+'         N OR MORE                                    
         BNE   *+8                                                              
         OI    PDWKOPT,X'40'                                                    
         CLI   23(R3),C'-'         N OR LESS                                    
         BNE   *+8                                                              
         OI    PDWKOPT,X'80'                                                    
         CLI   23(R3),C'/'         ACTIVITY IN THESE WEEKS                      
         BNE   *+8                                                              
         OI    PDWKOPT,X'20'                                                    
         B     VFIL36                                                           
*                                                                               
VFIL22   CLC   12(2,R3),=C'PT'     FILTER SID PROGRAM TYPES                     
         BNE   VFIL24                                                           
         CLI   1(R3),8             MAX OF 8                                     
         BH    BADFMX8                                                          
         MVC   PDSDPTPL,22(R3)                                                  
         B     VFIL36                                                           
*                                                                               
VFIL24   CLC   12(2,R3),=C'DT'     FILTER SID DAYPARTS                          
         BNE   VFIL26                                                           
         CLI   1(R3),8             MAX OF 8                                     
         BH    BADFMX8                                                          
         MVC   PDSDDPTL,22(R3)                                                  
         B     VFIL36                                                           
*                                                                               
VFIL26   CLC   12(3,R3),=C'GAA'    OPTION SUPPRESS/INCLUDE GAA LINES            
         BNE   VFIL28               SYNDICATION ONLY                            
         MVC   PDGAAOPT,22(R3)                                                  
         CLI   22(R3),C'N'                                                      
         BE    VFIL36                                                           
         CLI   22(R3),C'Y'                                                      
         BE    VFIL36                                                           
         B     BADFILT                                                          
*                                                                               
VFIL28   CLC   12(3,R3),=C'EFF'                                                 
         BNE   BADPREF                                                          
         XC    PODPSTRT(4),PODPSTRT                                             
         LA    RF,22(R3)                                                        
         LA    RE,WORK+20                                                       
*                                                                               
VFIL30   CLI   0(RF),C'-'                                                       
         BE    VFIL32                                                           
         CLI   0(RF),C' '                                                       
         BNH   VFIL32                                                           
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     VFIL30                                                           
*                                                                               
VFIL32   GOTO1 DATVAL,PARAS,(0,WORK+20),WORK                                    
         OC    PARAS(4),PARAS                                                   
         BZ    BADFILT                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,PODPSTRT)                                
         MVC   PROGSTRT(6),WORK    SAVE EBCDIC FORMAT                           
         LA    RF,22(R3)                                                        
*                                                                               
VFIL34   CLI   0(RF),C' '                                                       
         BNH   VFIL36                                                           
         CLI   0(RF),C'-'                                                       
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     VFIL34                                                           
*                                                                               
         LA    RF,1(RF)                                                         
         GOTO1 DATVAL,PARAS,(0,(RF)),WORK                                       
         OC    PARAS(4),PARAS                                                   
         BZ    BADFILT                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,PODPSTRT+2)                              
*                                                                               
VFIL36   LA    R3,42(R3)           BUMP TO NEXT FILTER                          
         AI    FIELDERR,1                                                       
         BCT   R0,*+8              IS IT THE END                                
         B     VFILX               YES - EXIT                                   
         CLI   1(R3),0             IS IT A KEYWORD                              
         BE    BADPREF             NO-ITS AN ERROR                              
         B     VFIL02                                                           
*                                                                               
VFIL38   CLI   1(R3),0             IS THERE A SECOND FIELD?                     
         BE    VFIL42              NO, CONTINUE ON                              
         B     VFIL04              YES, START OVER                              
*                                                                               
VFIL40   L     RF,APODMKTL         SAVE A(MARKET LIST TABLE)                    
         STCM  RF,15,PODMPTR                                                    
         CLI   0(RF),X'FF'         NO, USING MRKT OPTION?                       
         BNE   OPTCON              YES, ERROR                                   
         OC    PDMGRP,PDMGRP       NO, USING MRKGRP OPTION?                     
         BNZ   OPTCON              YES, ERROR                                   
*                                                                               
         MVC   0(1,R3),1(R3)       MOVE LENGTH AND DATA OF FIRST ITEM           
         MVC   12(10,R3),22(R3)                                                 
*                                                                               
VFIL42   ZIC   RF,0(R3)            GET LENGTH OF ENTRY                          
         LTR   RF,RF                                                            
         BZ    VFIL48                                                           
         LR    R1,R4               SAVE TABLE ADDRESS                           
         LA    RE,12(R3)           ADDRESS THE DATA                             
         CLI   5(RE),C'/'          / DIVIDES PROGRAM/STATION                    
         BE    VFIL44              IT'S THERE, OK                               
         CHI   RF,5                NOT THERE, IS THERE A STATION?               
         BH    BADFILT             YES, ERROR                                   
*                                                                               
VFIL44   CLI   0(RE),C'/'          SKIP THE DIVIDER                             
         BE    VFIL46                                                           
         MVC   0(1,R1),0(RE)       MOVE 1 BYTE                                  
         LA    R1,1(R1)            SHIFT THE TABLE                              
*                                                                               
VFIL46   LA    RE,1(RE)            GET NEXT BYTE                                
         BCT   RF,VFIL44           CONTINUE                                     
*                                                                               
         OC    0(9,R4),SPACES      FILL IN WITH BLANKS                          
         LA    R4,9(R4)            MAX ENTRY IS 9 BYTES                         
         MVI   0(R4),X'FF'         MARK END OF TABLE                            
*                                                                               
VFIL48   LA    R3,42(R3)           GET NEXT ENTRY IN BLOCK                      
         AI    FIELDERR,1                                                       
         BCT   R0,VFIL38                                                        
         MVI   0(R4),X'FF'                                                      
         B     VFILX                                                            
*                                                                               
VFIL50   L     R4,APDSUBPT         GET SUBPTYPE TABLE                           
         MVC   0(1,R3),1(R3)       SHIFT DATA FIRST TIME IN                     
         MVC   2(1,R3),3(R3)                                                    
         MVC   4(4,R3),8(R3)                                                    
         MVC   12(10,R3),22(R3)                                                 
*                                                                               
VFIL52   MVC   0(4,R4),12(R3)      2ND HALF IS VALID NUMERIC                    
         LA    R4,4(R4)            GET NEXT SPOT IN LIST                        
         MVI   0(R4),X'FF'         MARK IN CASE LAST                            
*                                                                               
         L     RF,APDSUBPT                                                      
         LA    RF,L'PDSUBPT(RF)    A(END OF SUB PROGRAM TABLE)                  
         CR    R4,RF                                                            
         BNL   BADFILT2            ERROR IF PAST IT                             
*                                                                               
         LA    R3,42(R3)           BUMP TO NEXT FILTER                          
         AI    FIELDERR,1                                                       
         BCT   R0,*+8              IS IT THE END                                
         B     VFILX               YES - EXIT                                   
         CLI   1(R3),0             IS THERE ANOTHER FIELD?                      
         BE    VFIL52              YES, STILL IN SUBPTYPE                       
         B     VFIL02              NO, A KEYWORD                                
*                                                                               
VFILX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL VALFILT ROUTINES                                  *         
***********************************************************************         
*                                                                               
BMPSTRG  DS    0H                                                               
         MVC   PDHISPNM(1),8(R3)   JUST SAVE STARTING DISPLACEMENT              
         LR    RF,R0               LINES LEFT                                   
         CLI   1(R3),0                                                          
         BZ    BMPSERR             HAS TO BE AN EXPRESSION                      
         ZIC   R1,8(R3)            COMPUTE LENGTH OF FIRST PART                 
         ZIC   R0,1(R3)                                                         
         B     BMPS04                                                           
*                                                                               
BMPS02   CLI   1(R3),0                                                          
         BNZ   BMPSX                                                            
         CLI   0(R3),0                                                          
         BZ    BMPSX                                                            
         ZIC   R1,4(R3)                                                         
         ZIC   R0,0(R3)                                                         
*                                                                               
BMPS04   AR    R1,R0                                                            
         STC   R1,PDHISPNM+1                                                    
         LA    R3,42(R3)                                                        
         BCT   RF,BMPS02                                                        
*                                                                               
BMPSX    LR    R0,RF               RESTORE AMOUNT LEFT                          
         SR    R1,R1                                                            
         B     *+8                                                              
*                                                                               
BMPSERR  LA    R1,1                                                             
         LTR   R1,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FILTERS                                                *         
***********************************************************************         
*                                                                               
VCHEFILT L     R4,0(R1)                                                         
         L     R2,4(R1)            FILTERS TABLE                                
         CLI   0(R2),X'FF'         ARE ANY SPECIFIED                            
         BE    CHEFYES             NO SO ITS OK                                 
         LA    R0,8                UP TO 8 MAY BE SPECIFIED                     
*                                                                               
CHEF02   CLI   0(R2),X'FF'         ARE ANY SPECIFIED                            
         BE    CHEFNO                                                           
         BAS   RE,CHEF06           GO CHECK                                     
         CLI   NEGSW,1             REVERSE THE FILTERS                          
         BE    CHEF04                                                           
         CLI   4(R1),0                                                          
         BE    CHEFYES             FOR ANY ONE TO BE SATISFIED                  
         LA    R2,6(R2)                                                         
         BCT   R0,CHEF02                                                        
         B     CHEFNO              NONE PASSED SO NO GOOD                       
*                                                                               
CHEF04   MVI   NEGSW,0             TRAP NEGATIVE FILTERS                        
         CLI   4(R1),0                                                          
         BE    CHEFNO              FOR ANY ONE TO BE SATISFIED                  
         LA    R2,6(R2)                                                         
         CLI   0(R2),X'FF'         END OF TABEL                                 
         BE    CHEFYES             IT'S FINE                                    
         BCT   R0,CHEF02                                                        
         B     CHEFYES             NONE PASSED SO ITS GOOD                      
*                                                                               
CHEF06   NTR1                                                                   
         LR    R3,R4                                                            
         LA    R0,4                                                             
         MVI   NEGSW,0                                                          
*                                                                               
CHEF08   MVC   BYTE,0(R2)          SAVE FOR NEGATIVE COMPARE                    
         OI    BYTE,X'40'          AND FORCE FOR COMPARE                        
         CLI   0(R2),C'*'          * IS WILD                                    
         BE    CHEF14                                                           
         CLI   0(R2),0             SO IS ZERO                                   
         BE    CHEF14                                                           
         CLI   0(R2),C'?'          QUESTION SIGN IS SPECIAL CHAR MATCH          
         BE    CHEF12                                                           
         TM    0(R2),X'40'         DO WE HAVE A NEGATE?                         
         BO    CHEF10              NO, GO ON CHECKING                           
         MVI   NEGSW,1             SET TO REVERSE                               
*                                                                               
CHEF10   CLC   BYTE,0(R3)          MUST MATCH                                   
         BNE   CHEFNO                                                           
         B     CHEF14                                                           
*                                                                               
CHEF12   CLI   0(R3),C' '          MATCH ON ANY SPECIAL CHARACTER               
         BH    CHEFNO              INCLUDING SPACE AND BINARY ZERO              
*                                                                               
CHEF14   LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,CHEF08                                                        
         B     CHEFYES                                                          
*                                                                               
CHEFNO   MVI   4(R1),X'FF'                                                      
         B     CHEFX                                                            
*                                                                               
CHEFYES  DS    0H                  PART 2, CHECK HISPANIC PNAME FILT            
         OC    PDHISPLN,PDHISPLN   IF LENGTH IS ZERO, SKIP IT                   
         BZ    CHEFYES2                                                         
         MVI   BYTE,X'FF'          PRESET TO FAILED                             
         GOTO1 HISPNAME,DMCB,(PDHISPLN,PDHISPNM),(25,PDLPRO),SCANNER            
         BNE   *+8                                                              
         MVI   BYTE,X'00'          SUCCESS                                      
         GOTO1 HISPNAME,DMCB,(PDHISPLN,PDHISPNM),(16,PDPROG),SCANNER            
         BNE   *+8                                                              
         MVI   BYTE,X'00'          SUCCESS                                      
         GOTO1 HISPNAME,DMCB,(PDHISPLN,PDHISPNM),(6,PDSPRO),SCANNER             
         BNE   *+8                                                              
         MVI   BYTE,X'00'          SUCCESS                                      
         MVC   4(1,R1),BYTE                                                     
         B     CHEFX                                                            
*                                                                               
CHEFYES2 MVI   4(R1),X'00'                                                      
*                                                                               
CHEFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE LEFT SIDE HEADING                                   *         
***********************************************************************         
*                                                                               
VVALLEFT XC    TOTWIDTH,TOTWIDTH   NOT CHECKING REPORT WIDTH YET                
         ZIC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,5                                                             
         LA    R4,4                START ON HEAD 4                              
         MVI   ANYROWSW,C'N'                                                    
         CLC   8(3,R2),=C'MED'     UNLESS FIRST HEAD NOT MEDIA                  
         BE    VLEF02                                                           
         LA    R4,5                                                             
*                                                                               
VLEF02   MVI   MYPOSO,C'H'                                                      
         STC   R4,MYPOSO+1                                                      
         MVI   MYPOSO+2,2          (COLUMN 2)                                   
         STC   R4,LASTHEAD                                                      
         BAS   RE,VALROW                                                        
         BAS   RE,BUMP                                                          
         CLI   5(R2),0                                                          
         BE    VLEFX                                                            
         ZIC   R4,LASTHEAD                                                      
         LA    R4,1(R4)                                                         
         BCT   R3,VLEF02                                                        
*                                                                               
VLEFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE RIGHT SIDE HEADING                                  *         
***********************************************************************         
*                                                                               
VVALRGHT ZIC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,3                MAX 3 FIELDS                                 
         LA    R4,5                (START ON HEAD 5)                            
*                                                                               
VRIG02   CLI   5(R2),0                                                          
         BE    VRIG04                                                           
         MVI   MYPOSO,C'H'                                                      
         STC   R4,MYPOSO+1                                                      
         MVI   MYPOSO+2,96         (COLUMN 96)                                  
         CLI   NARROPT,C'Y'        (NOT ALLOWED FOR NARROW)                     
         BE    BADROW                                                           
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         MVI   MYPOSO+2,129        (COLUMN 129 FOR WIDE)                        
         BAS   RE,VALROW                                                        
         LA    R4,1(R4)                                                         
*                                                                               
VRIG04   BAS   RE,BUMP                                                          
         BCT   R3,VRIG02                                                        
*                                                                               
         LA    R4,2(R4)                                                         
         CH    R4,=H'8'                                                         
         BH    *+8                                                              
         LA    R4,8                                                             
         IC    R3,MYFIRSTH                                                      
         CR    R4,R3                                                            
         BL    VRIGX                                                            
         STC   R4,MYFIRSTH                                                      
*                                                                               
VRIGX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE MID LINES                                           *         
***********************************************************************         
*                                                                               
VVALMID  MVI   MYPOSO,C'M'                                                      
         MVI   MYPOSO+1,1                                                       
         MVI   MYPOSO+2,1                                                       
         BAS   RE,VALROW                                                        
*                                                                               
VMIDX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ROWS                                                *         
***********************************************************************         
*                                                                               
VVALROWS MVI   TOTWIDTH+1,1        START CHECKING REPORT WIDTH NOW              
         ST    R2,ALASTCOL                                                      
         ZIC   R3,MAX                                                           
         LTR   R3,R3                                                            
         BNZ   *+8                                                              
         LA    R3,8                                                             
         LA    R0,8                                                             
         BAS   RE,DELINS                                                        
*                                                                               
         XC    MYPOSO,MYPOSO                                                    
         TM    PDCOLRNK,4          NO RANK IN COLUMNS                           
         BZ    VVALR02                                                          
         TM    PDCOLRNK,3          IF COLUMNS HAVE RANK                         
         BO    RANKERR                                                          
         XC    WORK,WORK                  MAKE A PHANTOM DETAIL                 
         MVC   WORK(8),0(R2)              SAVE THE CURRENT HEADER               
         MVI   WORK+5,4                   NEW LENGTH                            
         MVC   WORK+8(16),=CL16'RANK'     RANK WORKS BEST                       
         LA    R2,WORK                    SEND THE PHANTOM THROUGH              
         BAS   RE,VALROW                                                        
         L     R2,ALASTCOL                USE ORIGINAL DETAIL                   
*                                                                               
VVALR02  XC    MYPOSO,MYPOSO                                                    
         BAS   RE,VALROW                                                        
         ZIC   R0,TOTWIDTH+1                                                    
         BCTR  R0,0                                                             
         CLI   ROW1WIDE,0                                                       
         BNE   *+8                                                              
         STC   R0,ROW1WIDE                                                      
         BAS   RE,BUMP                                                          
         BCT   R3,VVALR02                                                       
*                                                                               
         CLC   =C'SQAD',PODBEXT    IS THIS A SQAD REQUEST?                      
         BNE   VVALR04             NO                                           
         CLI   PDSQTROW,C'Y'       YES, MUST PRINT SQUARTER                     
         BNE   BADROW                                                           
                                                                                
VVALR04  STC   R0,ROWWIDTH                                                      
         L     R2,ALASTCOL                                                      
         CLI   ANYROWSW,C'N'                                                    
         BE    BADNEED1                                                         
*                                                                               
         ZIC   R4,LASTHEAD                                                      
         LA    R4,3(R4)                                                         
         CH    R4,=H'9'                                                         
         BH    *+8                                                              
         LA    R4,9                                                             
         STC   R4,MYFIRSTH                                                      
*                                                                               
VVALRX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL ROUTINES FOR VALROWS                              *         
***********************************************************************         
*                                                                     *         
*        VALIDATE FOR KEYWORD                                         *         
*                                                                     *         
VALROW   NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    VROWX                                                            
         MVI   ANYROWSW,C'Y'                                                    
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADROW                                                           
         MVI   FIELDERR,1                                                       
         LA    R4,BLOCK                                                         
*                                                                               
         CLI   PDUSECAB,C'Y'       CABLE USED?                                  
         BNE   VROW02                                                           
         CLC   12(2,R4),=C'L '     TEST IF LENGTH USED                          
         BE    BADLEN                                                           
*                                                                               
VROW02   CLC   12(4,R4),=C'MGRP'   TEST ROW = LOWEST LEVEL MKTGRP               
         BNE   VROW04                                                           
         CLC   PDQMGR,SPACES                                                    
         BNH   BADROW                                                           
         MVI   15(R4),C'1'         YES - DETERMINE ITS LEVEL                    
         CLC   PDMGR1LN,PDMGR2LN                                                
         BE    VROW05                                                           
         MVI   15(R4),C'2'                                                      
         CLC   PDMGR2LN,PDMGR3LN                                                
         BE    VROW05                                                           
         MVI   15(R4),C'3'                                                      
         B     VROW05                                                           
*                                                                               
VROW04   CLC   12(4,R4),=C'PRGGRP' NEED TO KNOW IF PROGRAM GROUP                
         BNE   *+8                                                              
         MVI   PDPRGROW,C'Y'                                                    
*                                                                               
         CLC   12(6,R4),=C'SQUART' NEED TO KNOW IF SQAD QUARTER                 
         BNE   *+8                                                              
         MVI   PDSQTROW,C'Y'                                                    
*                                                                               
VROW05   GOTO1 VROWDRON            VALIDATE A ROW ENTRY                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         CLI   DRATTRIB,C'C'       COLUMN ONLY ENTRIES NOT ALLOWED              
         BE    BADROW                                                           
         CLI   DRATTRIB,C'D'       DETIAL ONLY ENTIES IN HEAD OR MID            
         BNE   *+12                NOT ALLOWED                                  
         CLI   MYPOSO,0                                                         
         BNE   BADROW                                                           
         CLI   DRATTRIB+1,C'P'     TEST PERIOD ROW                              
         BNE   VROW06                                                           
         CLI   PDQPER,0            YES-CHECK NO PERIOD ROW BEFORE               
         BNE   BADROW                                                           
         MVI   PDQPERLO,1                                                       
         MVI   PDQPERHI,X'FF'                                                   
         CLI   DRARGSI,C'Q'        TEST QUATERS                                 
         BNE   *+12                                                             
         OI    PDQPER,PDQPQT                                                    
         B     VROW06                                                           
         CLI   DRARGSI,C'M'        TEST MONTHS                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PDQPER,PDQPMN                                                    
*                                                                               
VROW06   CLC   12(4,R4),=C'RANK'                                                
         BNE   VROW10                                                           
         OC    PDSTACK,PDSTACK     CAN'T DO STACK DEMOS AND RANK                
         BNZ   BADSTROW                                                         
         CH    R0,=H'1'            IF ON ITS OWN, MAKE NO PRINT                 
         BE    VROW26                                                           
         LA    R4,32(R4)           RANK NEEDS A COMPUTE EXPRESSION              
         BCTR  R0,0                                                             
         AI    FIELDERR,1                                                       
         MVI   DRCMPMAX,C'P'                                                    
         CLC   44(2,R4),=CL2'NP'   SEE IF NOPRINT OPTION FOR RANK               
         BNE   *+24                                                             
         NI    DRFLAGO,X'7F'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VROW08                                                           
         GOTO1 GROWDRON                                                         
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         B     VROWX                                                            
*                                                                               
VROW08   GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADROW                                                           
         CLI   MYPOSO,0            IF WE ARE IN THE ROWS                        
         BNE   VROW10                                                           
         CH    R3,=H'1'            CHECK THIS IS NOT THE LAST ROW               
         BE    BADLRANK                                                         
         LR    R3,R2                                                            
         BAS   RE,BUMP                                                          
         CLI   5(R2),0             AND THERE IS INPUT IN NEXT                   
         LR    R2,R3                                                            
         BE    BADLRANK            NOT GOOD TO RANK ON LAST ROW                 
*                                                                               
VROW10   MVI   BYTE,C'R'           BUILD ENTRYTAB BEFORE DOING                  
         BAS   RE,BLDENTTB          SPECIAL FOR HEADS & MIDS                    
                                                                                
         CLI   MYPOSO,C'H'         SPECIAL FOR HEADS                            
         BNE   VROW12                                                           
         BAS   RE,HEADROW                                                       
         B     VROW36                                                           
*                                                                               
VROW12   CLI   MYPOSO,C'M'         SPECIAL FOR MID                              
         BNE   VROW36                                                           
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         MVI   DRLSPACE,1          WITH ONE SPACE                               
         B     VROW36                                                           
*                                                                               
VROW14   CLC   12(2,R4),=C'* '     TOTAL EXPRESSION                             
         BNE   VROW16                                                           
         OI    DRTOTAL,X'80'                                                    
         MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         B     VROW36                                                           
*                                                                               
VROW16   CLC   12(5,R4),=C'SKIP '  SKIP TO CHANNEL 1 AFTER BREAK                
         BNE   VROW18                                                           
         OI    DRFIRST,X'80'       GENERATE FIRST STATEMENT                     
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         B     VROW36                                                           
*                                                                               
VROW18   CLC   12(6,R4),=C'SPACE ' SPACE OPTION                                 
         BNE   VROW20                                                           
         OI    DRLAST,X'80'        GENERATE LAST STATEMENT                      
         MVI   DRLSPACE,1          WITH AT LEAST ONE SPACE                      
         CLI   1(R4),0             CHECK SECOND PARAMETER                       
         BE    VROW36                                                           
         MVC   DRLSPACE,11(R4)                                                  
         CLI   DRLSPACE,0          S/B 1-3 LINES                                
         BE    BADROW                                                           
         CLI   DRLSPACE,3                                                       
         BH    BADROW                                                           
         B     VROW36                                                           
*                                                                               
VROW20   CLC   12(4,R4),=C'DET '   DET=N OR D=N                                 
         BE    VROW22                                                           
         CLC   12(2,R4),=C'D '                                                  
         BNE   VROW24                                                           
*                                                                               
VROW22   OI    DRTOTAL,X'80'                                                    
         MVI   DRTSPACE,1          SPACE AFTER TOTALS                           
         MVC   DRTDET(1),11(R4)    NUMBER OF DETAILS                            
         CLI   DRTDET,0                                                         
         BE    BADROW                                                           
         B     VROW36                                                           
*                                                                               
VROW24   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VROW28                                                           
*                                                                               
VROW26   NI    DRFLAGO,X'7F'                                                    
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         B     VROW36                                                           
*                                                                               
VROW28   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VROW30                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VROW36                                                           
*                                                                               
VROW30   LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VROW32                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VROW32                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VROW32                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VROW32                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BNE   VROW34                                                           
*                                                                               
VROW32   CLI   MYPOSO,0            CHECK NOT HEADLINE OR MIDLINE                
         BNE   BADROW                                                           
         XC    0(26,R1),0(R1)      TURN OFF ROUTINE & ARGS                      
         CLI   1(R4),0                                                          
         BE    VROW36              HN= CAUSES REMOVAL                           
         USING DRHEADD,R1                                                       
         OI    DRHEAD,X'80'        OTHERWISE TURN IT BACK ON                    
         MVC   DRHLITL,1(R4)       PASS LITERAL LENGTH TO DRONE                 
         CLC   DRHLITL,DRLENO                                                   
         BH    HEADERR             CHECK LITERAL NOT WIDER THAN COLUMN          
*                                                                               
         ZIC   RE,DRHLITL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE IN THE HEADER LITERAL                   
         B     VROW36                                                           
         MVC   DRHLIT(0),22(R4)    ** EXECUTED                                  
         DROP  R1                                                               
*                                                                               
VROW34   CLC   12(2,R4),=C'U '                                                  
         BNE   BADROW                                                           
         BAS   RE,VUSRDRON                                                      
*                                                                               
VROW36   LA    R4,32(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VROW14                                                        
*                                                                               
         TM    DRTOTAL,X'80'       TEST SPACE AFTER TOTAL                       
         BZ    VROW38                                                           
         TM    DRLAST,X'80'        AND SPACE OPTION SET                         
         BZ    VROW38                                                           
         MVC   DRTSPACE,DRLSPACE   YES-MOVE SPACE OPTION TO SPACE               
         MVI   DRLAST,0                AFTER TOTAL AND FORGET LAST              
         MVI   DRLSPACE,0              SPACE                                    
*                                                                               
VROW38   OC    TOTWIDTH,TOTWIDTH   IF WE ARE CHECKING WIDTH                     
         BZ    VROW40                                                           
         TM    DRFLAGO,X'80'                                                    
         BZ    VROW40                                                           
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
*                                                                               
VROW40   CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   VROWX                                                            
         MVC   DRPOSO,MYPOSO                                                    
*                                                                               
         CLI   DRPOSO,C'H'         TEST HEADLINE                                
         BNE   VROW46                                                           
         TM    DRTOTAL,X'80'       AND TOTAL REQUESTED                          
         BZ    VROW46                                                           
         CLI   DRTDET,0            AND NOT DETAILED TOTAL                       
         BNE   VROW46                                                           
         CLI   DRRTNO,C' '         AND OUT ROUTINE SPECIFIED                    
         BNH   VROW46                                                           
         L     R1,ATWA             AND (THERE IS A MIDLINE                      
         USING T325FFD,R1                                                       
         CLI   SPLMIDH+5,0                                                      
         BNE   VROW44                                                           
         DROP  R1                                                               
         LR    RE,R2                    OR THIS IS NOT LAST HEADLINE)           
         SR    RF,RF                                                            
*                                                                               
VROW42   IC    RF,0(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    VROW46                                                           
         TM    1(RE),X'20'                                                      
         BO    VROW42              FIND AN UNPROTECTED FIELD                    
         CLI   5(RE),0                                                          
         BE    VROW46              WITH SOME DATA                               
*                                                                               
VROW44   MVC   DRTRTN,DRRTNO       YES-USE THIS FOR TOTAL AS WELL               
         MVC   DRTARGS,DRARGSO     AND PASS THROUGH THE ARGUMENTS               
         MVI   DRTNARGS,16                                                      
         MVI   DRTLITLN,0                                                       
*                                                                               
VROW46   GOTO1 GROWDRON                                                         
*                                                                               
VROWX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL ROUTINES FOR VALROW                               *         
***********************************************************************         
*                                                                               
HEADROW  DS    0H                                                               
         OI    DRFIRST,X'80'                                                    
         OI    DRFOPTS,DRFSKIP     WITH SKIP OPTION                             
         MVI   DRFSPACE,0                                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE COLUMNS                                             *         
***********************************************************************         
*                                                                               
VVALCOLS LA    R0,12                                                            
         BAS   RE,DELINS                                                        
         ZIC   R0,MAX                                                           
         LTR   R0,R0                                                            
         BZ    VVALCX                                                           
         CLI   5(R2),0             NEED AT LEAST ONE COLUMN                     
         BE    BADNEED1                                                         
         MVI   MYLABEL,C'A'                                                     
         ST    R2,ALASTCOL                                                      
         BAS   RE,SETMAX           SET LAST COLUMN FOR COMPUTE                  
         LA    R3,EDITLIST                                                      
*                                                                               
VVALC02  XC    MYPOSO,MYPOSO                                                    
         MVC   0(1,R3),MYLABEL     SET LABEL IN EDIT LIST                       
         BAS   RE,VALCOL                                                        
         BAS   RE,BUMP                                                          
         MVC   MYLABEL,8(R2)                                                    
         BAS   RE,BUMP                                                          
         LA    R3,4(R3)                                                         
         BCT   R0,VVALC02                                                       
*                                                                               
         CLC   TOTWIDTH,=H'80'     CHECK NOT TOO BIG NOW                        
         BNH   VVALCX                                                           
         CLI   NARROPT,C'Y'        ONLY 80 ALLOWED WITH NARROW OPT              
         BE    COLWIDE                                                          
         CLC   TOTWIDTH,=H'132'    CHECK NOT TOO BIG NOW                        
         BNH   VVALCX                                                           
         CLI   WIDEOPT,C'Y'                                                     
         BNE   COLWIDE                                                          
         CLC   TOTWIDTH,=H'165'                                                 
         BH    COLWIDE                                                          
*                                                                               
VVALCX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL ROUTINES FOR VALCOLS                              *         
***********************************************************************         
VALCOL   NTR1                                                                   
         L     R5,PDAFLBUF                                                      
         USING PDFLBUFF,R5                                                      
         XC    EXPLIST,EXPLIST                                                  
         DROP  R5                                                               
*                                                                               
VCOL02   CLI   5(R2),0                                                          
         BE    VCOLX                                                            
         ST    R2,ALASTCOL                                                      
         LA    R4,BLOCK+42                                                      
         MVI   FIELDERR,1                                                       
         CLI   COLXTEND,2          HANDLING EXTENSION HERE                      
         BNE   VCOL04                                                           
         LA    R4,BLOCK+42+42                                                   
         B     VCOL06                                                           
*                                                                               
VCOL04   MVI   COLXTEND,0                                                       
         XC    BLOCK(252),BLOCK                                                 
         ZIC   R1,5(R2)                                                         
         LA    R1,8-1(R1,R2)       (R1=A(LAST CHARACTER))                       
         CLI   0(R1),C','          IF THIS IS A COMMA                           
         BNE   VCOL06                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,5(R2)            REDUCE APPARENT LENGTH                       
         MVI   COLXTEND,1          AND NOTE THAT THERE IS AN EXTENSION          
*                                                                               
VCOL06   GOTO1 SCANNER,DMCB,(20,(R2)),(6,(R4)),0                                
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    BADCOL                                                           
         CLI   COLXTEND,2          TEST THIS IS AN EXTENSION                    
         BE    VCOL30                                                           
         GOTO1 VCOLDRON            VALIDATE A COLUMN ENTRY                      
         CLI   DRERROR,0                                                        
         BNE   VCOL26                                                           
         CLI   DRATTRIB,C'R'       ROW ONLY ENTRIES NOT ALLOWED                 
         BE    BADCOL                                                           
         CLI   DRATTRIB,C'D'       ROW DETAIL ONLY ENTRIES NOT ALLOWED          
         BE    BADCOL                                                           
         MVI   BYTE,C'C'           STORE COLUMN ENTRY INTO                      
         BAS   RE,BLDENTTB          ENTRYTAB FIRST                              
*                                                                               
* DEAL WITH PREC FOR NON-DEMO COLUMNS                                           
         CLC   =C'AIR',12(R4)      DEMO OVERLAP COLS WILL BE                    
         BE    VCOL76              TRASHED BY PREC RTN WHEN STACKING            
*                                                                               
         CLI   DRATTRIB+1,C'D'     TEST FOR DOLLAR COLUMN                       
         BNE   VCOL08                                                           
         TM    COLIND,COLIRND      YES - TEST FOR ROUNDING OPTION               
         BZ    VCOL76                                                           
         MVI   DRDECO,0            YES - NO DECIMAL PLACES                      
         B     VCOL76                                                           
*                                                                               
VCOL08   MVC   DRDECO,13(R4)                                                    
         NI    DRDECO,X'0F'        ACTUAL DEMO #                                
*                                                                               
* FIND THE DEMO FOR THIS COLUMN                                                 
*                                                                               
         L     RE,PDADEMTB                                                      
         ZIC   R1,DRDECO                                                        
         BCTR  R1,0                                                             
         LTR   R1,R1               FIRST DEMO?                                  
         BZ    VCOL14              YES, DON'T NEED TO SEARCH                    
*                                                                               
VCOL10   CLI   0(RE),X'FF'         END OF DEMOS?                                
         BNE   VCOL12                                                           
         MVI   DRDECO,0            NO SUCH DEMO, USE ZERO                       
         B     VCOL76                                                           
*                                                                               
VCOL12   AH    RE,LNDEMCOD                                                      
         BCT   R1,VCOL10                                                        
*                                                                               
* FOUND DEMO, NOW FIND # OF DECS FOR THAT DEMO                                  
VCOL14   MVC   DRDECO,1(RE)                                                     
         L     R1,=A(PRECTABL)                                                  
         A     R1,PGNR                                                          
*                                                                               
         USING PRECTDST,R1                                                      
VCOL16   CLI   PRECSRCE,X'FF'      END OF SOURCE PREC TABLE                     
         BE    VCOL24              USE DEFAULT, ZERO                            
         CLC   PRECSRCE,PODBFIL      LOOK UP SOURCE                             
         BNE   VCOL18                                                           
         L     R1,PRECADDR         GOT IT, LOAD PREC FOR SOURCE                 
         A     R1,PGNR                                                          
         B     VCOL20                                                           
*                                                                               
VCOL18   LA    R1,PRECTLEN(R1)                                                  
         B     VCOL16                                                           
*                                                                               
VCOL20   CLI   0(R1),X'FF'         END OF DEMOS?                                
         BE    VCOL24                                                           
         CLC   DRDECO,0(R1)        FOUND IT?                                    
         BNE   VCOL22                                                           
         MVC   DRDECO,3(R1)        YES, GRAB # OF DECS                          
         B     VCOL76                                                           
*                                                                               
VCOL22   LA    R1,PRECLEN(R1)                                                   
         B     VCOL20                                                           
*                                                                               
VCOL24   MVI   DRDECO,0                                                         
         B     VCOL76                                                           
*                                                                               
VCOL26   XC    BLOCK(42),BLOCK     MAY BE A COMPUTE EXPRESSION                  
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(7),=C'COMPUTE'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON            VALIDATE THE COMPUTE COLUMN                  
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
         LA    R4,BLOCK+42                                                      
         CLI   OFFLINE,C'Y'                                                     
         BE    VCOL28                                                           
         GOTO1 VCMPDRON            VALIDATE A COMPUTE EXPRESSION                
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
*                                                                               
VCOL28   BAS   RE,COMPEDIT         AUTO EDITS FOR COMPUTES                      
         B     VCOL76                                                           
*                                                                               
*                                                                               
* CHECK FOR SUBSIDIARY COLUMN EXPRESSIONS                                       
*                                                                               
VCOL30   TM    2(R4),X'80'         NUMERIC=OUTPUT WIDTH OVERRIDE                
         BNO   VCOL32                                                           
         L     R1,4(R4)            OUTPUT WIDTH OVERRIDE                        
         STC   R1,DRLENO           (NEW OUTPUT LENGTH)                          
         B     VCOL76                                                           
*                                                                               
VCOL32   DS    0H                                                               
         LA    R1,DRHEAD1          CHECK FOR HEADING OVERRIDES                  
         CLC   12(2,R4),=C'H '                                                  
         BE    VCOL34                                                           
         CLC   12(3,R4),=C'H1 '                                                 
         BE    VCOL34                                                           
         LA    R1,DRHEAD2                                                       
         CLC   12(3,R4),=C'H2 '                                                 
         BE    VCOL34                                                           
         LA    R1,DRHEAD3                                                       
         CLC   12(3,R4),=C'H3 '                                                 
         BE    VCOL34                                                           
         LA    R1,DRHEAD4                                                       
         CLC   12(3,R4),=C'H4 '                                                 
         BNE   VCOL36                                                           
*                                                                               
         USING DRHEADD,R1                                                       
VCOL34   XC    0(64,R1),0(R1)      TURN OFF ROUTINE & ARGS                      
         CLI   1(R4),2                                                          
         BL    VCOL76              HN= CAUSES REMOVAL                           
         MVC   DRH1LIT-DRH1ALL(L'DRH1LIT,R1),SPACES  CLEAR OLD HL               
         OI    DRHEAD,X'80'        OTHERWISE TURN IT BACK ON                    
         MVC   DRHLITL,1(R4)       PASS LITERAL LENGTH TO DRONE                 
         CLC   DRHLITL,DRLENO                                                   
         BH    HEADERR             CHECK LITERAL NOT WIDER THAN COLUMN          
*                                                                               
         ZIC   RE,DRLENO                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8              CLEAR THE HEADER LITERAL AREA                
         B     *+10                                                             
         MVC   DRHLIT(0),SPACES    ** EXECUTED                                  
         ZIC   RE,DRHLITL                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE IN THE HEADER LITERAL                   
         B     VCOL76                                                           
         MVC   DRHLIT(0),22(R4)    ** EXECUTED                                  
         DROP  R1                                                               
*                                                                               
VCOL36   CLC   12(3,R4),=C'NP '    OPTION NOT TO PRINT                          
         BNE   VCOL38                                                           
         MVI   MYPOSO,C'N'                                                      
         NI    DRHEAD1,X'7F'                                                    
         NI    DRHEAD2,X'7F'                                                    
         NI    DRHEAD3,X'7F'                                                    
         NI    DRHEAD4,X'7F'                                                    
         B     VCOL76                                                           
*                                                                               
VCOL38   CLC   12(3,R4),=C'NT '    OPTION NOT TO TOTAL THIS COLUMN              
         BNE   VCOL40                                                           
         OI    DRARGSI+11,X'80'    12TH ARGUMENT X'80'                          
         MVI   DRNARGSI,16                                                      
         B     VCOL76                                                           
*                                                                               
VCOL40   CLC   12(8,R4),=C'BRACKET '   (MINUS NUMBERS)                          
         BNE   VCOL42                                                           
         OI    DROPTSO,DRBKMINO                                                 
         B     VCOL76                                                           
*                                                                               
VCOL42   CLC   12(2,R4),=C'U '     USER RECORD                                  
         BNE   VCOL44                                                           
         BAS   RE,VUSRDRON                                                      
         B     VCOL76                                                           
*                                                                               
VCOL44   CLC   12(4,R4),=C'RANK'   (COLUMN) RANK                                
         BNE   VCOL46                                                           
         MVI   COLRANK,1                                                        
         CLI   16(R4),C'-'                                                      
         BE    VCOL76                                                           
         CLI   22(R4),C'-'                                                      
         BE    VCOL76                                                           
         MVI   COLRANK,2                                                        
         B     VCOL76                                                           
*                                                                               
VCOL46   CLC   12(3,R4),=C'DEC'    DECIMAL FOR COMPUTES                         
         BNE   VCOL48                                                           
         CLI   11(R4),0                                                         
         BL    BADCOL                                                           
         CLI   11(R4),5                                                         
         BH    BADCOL                                                           
         MVC   DRDECO,11(R4)                                                    
         B     VCOL76                                                           
*                                                                               
VCOL48   CLC   12(3,R4),=C'NET'    NETWORK FILTER BYTE 8                        
         BE    VCOL50                     - OR -                                
         CLC   12(3,R4),=C'STA'    STATION                                      
         BNE   VCOL54                                                           
*                                                                               
VCOL50   L     R1,=A(VCOLNTTB)                                                  
         A     R1,PGNR                                                          
*                                                                               
VCOL52   CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+7(1),0(R1)                                               
         CLC   22(2,R4),1(R1)      CHECK NET EXPRESSION                         
         BE    VCOL76                                                           
         CLC   22(2,R4),3(R1)      CHECK STATION EXPRESSION                     
         BE    VCOL76                                                           
         LA    R1,5(R1)                                                         
         B     VCOL52                                                           
*                                                                               
VCOL54   CLC   12(4,R4),=C'BOOK'   BOOK FILTER BYTE 13                          
         BNE   VCOL58                                                           
         L     R1,=A(VCOLBKTB)                                                  
         A     R1,PGNR                                                          
*                                                                               
VCOL56   CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+12(1),0(R1)                                              
         CLC   22(2,R4),1(R1)                                                   
         BE    VCOL76                                                           
         LA    R1,3(R1)                                                         
         B     VCOL56                                                           
*                                                                               
VCOL58   CLC   12(3,R4),=C'DAY'    DAY FILTER BYTE 9                            
         BNE   VCOL62                                                           
         L     R1,=A(VCOLDYTB)                                                  
         A     R1,PGNR                                                          
*                                                                               
VCOL60   CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+8(1),0(R1)                                               
         CLC   22(3,R4),1(R1)                                                   
         BE    VCOL76                                                           
         LA    R1,4(R1)                                                         
         B     VCOL60                                                           
*                                                                               
VCOL62   CLC   12(3,R4),=C'DPT'    DAYPART FILTER 10                            
         BNE   VCOL66                                                           
         L     R1,=A(VCOLDPTB)                                                  
         A     R1,PGNR                                                          
*                                                                               
VCOL64   CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+9(1),0(R1)                                               
         CLC   22(1,R4),0(R1)                                                   
         BE    VCOL76                                                           
         LA    R1,1(R1)                                                         
         B     VCOL64                                                           
*                                                                               
VCOL66   CLI   PRECOPT,C'Y'        PRECISION IS CABLE?                          
         BNE   *+8                 NO                                           
         MVI   DRDECO,2            YES, CHANGE DECIMALS                         
         BAS   RE,VCOLMXMN                                                      
         BE    VCOL76                                                           
         CLC   12(3,R4),=C'SRC'    SOURCE FILTER BYTE 11                        
         BE    VCOL68                                                           
         CLC   12(6,R4),=C'SOURCE'                                              
         BNE   VCOL74                                                           
                                                                                
VCOL68   L     R1,=A(VCOLSCTB)                                                  
         A     R1,PGNR                                                          
*                                                                               
VCOL70   CLI   0(R1),X'FF'         (EOL?)                                       
         BE    BADCOL                                                           
         MVC   DRARGSI+10(1),0(R1)                                              
         CLC   22(5,R4),1(R1)                                                   
         BE    VCOL72                                                           
         LA    R1,6(R1)                                                         
         B     VCOL70                                                           
*                                                                               
VCOL72   BAS   RE,SRC2HEAD         PUT THE SOURCE IN HEADLINE                   
         AH    R0,=H'1'                                                         
         B     VCOL76                                                           
*                                                                               
VCOL74   GOTO1 =A(OVFLRTN),DMCB,(4,DUB),(RC),RR=PGNR VPEREXPR                   
         L     R1,PDAFLBUF                                                      
         USING PDFLBUFF,R1                                                      
         CLI   EXPLIST,0                                                        
         BE    BADCOL                                                           
         CLI   EXPLIST,X'FF'                                                    
         BE    BADCOL                                                           
         MVC   DRARGSI+15(1),EXPLIST                                            
         BAS   RE,HEADFILT                                                      
         DROP  R1                                                               
*                                                                               
VCOL76   LA    R4,42(R4)                                                        
         AI    FIELDERR,1                                                       
         BCT   R0,VCOL30                                                        
*                                                                               
         CLI   COLXTEND,1          TEST THERE IS AN EXTENSION PENDING           
         BNE   VCOL78                                                           
         MVI   COLXTEND,2          YES-NOT TIME TO WRAP UP YET                  
         ZIC   R1,5(R2)                PUT BACK ACTUAL LENGTH                   
         LA    R1,1(R1)                                                         
         STC   R1,5(R2)                                                         
         B     VCOLX                                                            
*                                                                               
VCOL78   MVI   COLXTEND,0                                                       
         MVI   DRNARGSI,16         PASS ALL 16 ARGUMENTS                        
         MVI   DRNARGSO,16                                                      
         CLI   MYPOSO,C'N'         IF THERE IS ANY PRINTING                     
         BE    VCOL80                                                           
         LH    R1,TOTWIDTH         ADJUST CURRENT WIDTH                         
         ZIC   RF,DRLENO                                                        
         AR    R1,RF                                                            
         LA    R1,1(R1)                                                         
         STH   R1,TOTWIDTH                                                      
*                                                                               
VCOL80   CLI   OFFLINE,C'Y'        NOW GENERATE ELEMENTS                        
         BNE   VCOL82                                                           
         MVC   DRLABELI,MYLABEL                                                 
         MVC   1(1,R3),DRDECO      SAVE EDIT CHARACTERISTICS                    
         MVC   2(1,R3),DRDIVO                                                   
         MVC   DRPOSO,MYPOSO                                                    
         GOTO1 GCOLDRON                                                         
         CLC   BLOCK+12(7),=C'COMPUTE'                                          
         BNE   VCOL82                                                           
         LA    R4,BLOCK+42                                                      
         GOTO1 GCMPDRON                                                         
         CLI   DRERROR,0                                                        
         BNE   BADCOL                                                           
*                                                                               
VCOL82   BAS   RE,ANYMOCOL                                                      
         L     R1,PDAFLBUF                                                      
         USING PDFLBUFF,R1                                                      
         MVC   EXPLIST(15),EXPLIST+1                                            
         MVI   EXPLIST+15,0                                                     
         CLI   EXPLIST,0                                                        
         BE    VCOLX                                                            
         B     VCOL02                                                           
*                                                                               
VCOLX    B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL ROUTINES FOR VALCOLS                              *         
***********************************************************************         
*                                                                               
       ++INCLUDE DDVALMNMX                                                      
*                                                                     *         
*                                                                     *         
*                                                                     *         
ANYMOCOL NTR1                                                                   
         CLI   COLRANK,0           COLUMN RANKING                               
         BE    ANYMX                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   ANYMX                                                            
         XC    BLOCK(12),BLOCK     FOR DATE TOTAL/DETAIL EXPRESSIONS            
         MVI   BLOCK,7                                                          
         MVC   BLOCK+12(30),SPACES                                              
         MVC   BLOCK+12(7),=C'COLRANK'                                          
         LA    R4,BLOCK                                                         
         GOTO1 VCOLDRON                                                         
         MVC   DRARGSI(1),COLRANK       PASS ARGUMENT                           
         MVI   DRNARGSI,1                                                       
         MVI   COLRANK,0                                                        
         GOTO1 GCOLDRON                                                         
*                                                                               
ANYMX    B     XIT                                                              
*                                                                     *         
*                                                                     *         
*                                                                     *         
SETMAX   NTR1                                                                   
         MVI   BYTE,C'A'           FIND LAST INPUT COLUMN                       
*                                                                     *         
SETM02   CLI   5(R2),0                                                          
         BE    SETMX                                                            
         MVC   DRCMPMAX,BYTE                                                    
         BAS   RE,BUMP                                                          
         MVC   BYTE,8(R2)                                                       
         BAS   RE,BUMP                                                          
         BCT   R0,SETM02                                                        
*                                                                     *         
SETMX    B     XIT                                                              
**                                                                              
*              FIND SPOT FOR HEADING                                  *         
**                                                                              
SRC2HEAD NTR1                                                                   
         LR    RF,R4                                                            
*                                                                               
SRCH02   LA    R4,42(R4)                                                        
         CLI   0(R4),0                                                          
         BNZ   SRCH02                                                           
         MVC   0(42,R4),0(RF)      COPY BLOCK                                   
         MVI   0(R4),2             'H4'                                         
         MVI   2(R4),0                                                          
         MVC   12(3,R4),=C'H4 '                                                 
*                                                                               
SRCHX    B     XIT                                                              
*                                                                     *         
*              FIGURE OUT EDITS FOR COMPUTES                          *         
*                                                                     *         
COMPEDIT NTR1                                                                   
         ZIC   R1,0(R4)            PICK UP EXPRESSION LENGTH                    
         LA    R1,10(R1,R4)                                                     
         CLI   0(R1),C'%'          IS LAST OPERATOR PERCENT?                    
         BE    COMPPCT                                                          
         CLI   0(R1),C'I'          OR INDEX?                                    
         BE    COMPINX                                                          
         BCTR  R1,0                                                             
         CLI   0(R1),C'V'          OR VERTICAL PERCENT                          
         BNE   COMPDEF                                                          
*                                                                               
COMPPCT  MVI   DRDECO,2            PERCENTS HAVE 2 DEC                          
         MVI   DRTRAILO,C'%'       AND END WITH PERCENT SIGN                    
         B     XIT                                                              
*                                                                               
COMPINX  MVI   DRDECO,0            INDEXES HAVE 0 DEC                           
         B     XIT                                                              
*                                                                               
COMPDEF  MVI   DRDECO,0            DEFAULT HAVE 1 DEC                           
*                                                                               
* DEFAULT CALCULATIONS USES THE SAME # OF DEC AS THE FIRST OPERAND              
* SEARCH FOR THE FIRST COLUMN DATA                                              
*                                                                               
         LA    R1,EDITLIST                                                      
COMPDEF1 CLI   0(R1),0             END OF LIST?                                 
         BE    XIT                 YES, KEEP DEFAULT DEC                        
         CLC   0(1,R1),8(R2)       FOUND IT?                                    
         BNE   COMPDEF5            NO, NEXT ONE                                 
         MVC   DRDECO,1(R1)        COPY # OF DEC                                
         CLI   PRECOPT,C'Y'        EXTEND CABLE PRECISION                       
         BNE   XIT                                                              
         ZIC   R3,DRDECO           <----TEMP FIX                                
         LA    R3,1(R3)                 NEED REAL LOGIC FOR THIS                
         STC   R3,DRDECO                                                        
         B     XIT                                                              
COMPDEF5 LA    R1,4(R1)                                                         
         B     COMPDEF1                                                         
*                                                                               
CMPEDFND LA    R1,EDITLIST                                                      
CMPEDFN2 CLI   0(R1),0                                                          
         BNE   *+14                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,CMPEDFND                                                      
         DC    H'0'                NO VARIABLES IN INPUT STRING                 
         CLC   0(1,R1),0(RF)                                                    
         BER   RE                                                               
         LA    R1,4(R1)                                                         
         B     CMPEDFN2                                                         
*                                                                               
*              BUILD ENTRY TABLE                                      *         
*              R4 = SCANNER BLOCK CONTAINING THE ENTRY                *         
*              BYTE = C'C' OR C'R' FOR COLUMN OR ROW                  *         
*                                                                               
BLDENTTB NTR1                                                                   
         L     R3,APODINPL         POINT MYSELF TO TABLE AREA                   
         LA    R5,1(R3)            1ST BYTE IN TABLE = # OF ENTRIES             
         USING ENTRYTBD,R5                                                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,0(R3)          R1=# OF ENTRIES SO FAR                       
         LR    R0,R1                                                            
         BZ    *+12                                                             
         LA    R5,ENTRYTBQ(R5)     BUMP TO NEXT SLOT IN TABLE                   
         BCT   R1,*-8                                                           
*                                                                               
         MVC   ETENTRY,12(R4)      MOVE ENTRY INTO TABLE                        
         MVC   ETCOLROW,BYTE       MOVE COL/ROW INDICATOR                       
         MVI   ENTRYTBQ(R5),0      CLEAR THE NEXT SLOT                          
*                                                                               
         LR    R1,R0                                                            
         LA    R1,1(R1)            UPDATE # OF ENTRIES                          
         STC   R1,0(R3)                                                         
*                                                                               
BLDEX    B     XIT                                                              
         DROP  R5                                                               
*                                                                     *         
*              FORMAT HEADINGS FOR FILTERS                            *         
*                                                                     *         
HEADFILT NTR1                                                                   
         TM    DRFLAGO,X'80'       NOT NEEDED IF NOT PRINTING                   
         BNO   HEADX                                                            
*                                                                               
         LA    R1,DRHEAD1          FIND AN EMPTY HEADING                        
         CLC   1(4,R1),=C'HDEM'    DONT SET FOR DEMO HEADER                     
         BE    HEAD06                                                           
         BAS   RE,HEAD02                                                        
         LA    R1,DRHEAD2                                                       
         BAS   RE,HEAD02                                                        
         LA    R1,DRHEAD3                                                       
         BAS   RE,HEAD02                                                        
         LA    R1,DRHEAD4                                                       
         BAS   RE,HEAD02                                                        
*                                                                               
HEADX    B     XIT                 NO ROOM!                                     
*                                                                               
HEAD02   TM    0(R1),X'80'         LOOK FOR EMPTY SLOT                          
         BNO   HEAD04                                                           
         CLC   1(8,R1),=C'HFILTER '     OR A PREVIOUS VISIT                     
         BNER  RE                                                               
*                                                                               
HEAD04   OI    0(R1),X'80'         FOUND A SPACE - SO TURN ON                   
         MVC   1(8,R1),=CL8'HFILTER '                                           
*                                                                               
HEAD06   MVC   9(16,R1),DRARGSI    PASS ALL THE ARGUMENTS                       
         MVI   25(R1),16           N'ARGUMENTS IS 16                            
         B     HEADX                                                            
*                                                                     *         
*              VALIDATE USER RECORD                                   *         
*                                                                     *         
VUSRDRON NTR1                                                                   
         MVI   DRACTION,DRUSER                                                  
         MVC   DRUSRKEY(2),AGENCY           KEY IS AGENCY                       
         MVC   DRUSRKEY+2(8),22(R4)                AND USER CODE                
         GOTO1 DRONE,DMCB,DRGEN                                                 
         CLI   DRERROR,0                                                        
         BNE   USERER                                                           
*                                                                     *         
VUSRX    B     XIT                                                              
*                                                                     *         
*              INSERT/DELETE UNPROTECTED FIELDS                       *         
*              INPUT   R2 = A(FIRST UNPROTECTED FIELD)                *         
*                      R0 = NUMBER OF INPUT FIELDS                    *         
*                                                                     *         
DELINS   NTR1                                                                   
         CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         CLI   PFAID,3             WAS PF3 OR PF4 HIT                           
         BE    DI2                                                              
         CLI   PFAID,15                (15/16 EQUIVALENT)                       
         BE    DI2                                                              
         CLI   PFAID,4                                                          
         BE    DI2                                                              
         CLI   PFAID,16                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
DI2      L     R4,SYSPARMS                                                      
         L     R4,0(R4)                                                         
         USING TIOBD,R4                                                         
         LH    R4,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         A     R4,ATWA             INTO TWA                                     
         SPACE 1                                                                
DI4      CR    R2,R4                                                            
         BE    DI6                                                              
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,DI4                                                           
         B     XIT                 (NOT IN THIS PART OF THE SCREEN)             
         SPACE 1                                                                
DI6      CLI   PFAID,3                                                          
         BE    DEL2                                                             
         CLI   PFAID,15                                                         
         BE    DEL2                                                             
         XC    BLOCK(80),BLOCK                                                  
         SPACE 1                                                                
INS2     MVC   BLOCK+80(80),8(R2)  SAVE THIS FIELD                              
         ZIC   R1,0(R2)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BLOCK       MOVE IN PREVIOUS (OR CLEAR)                  
         OI    6(R2),X'80'                                                      
         MVC   BLOCK(80),BLOCK+80                                               
         BAS   RE,BUMPTOUN                                                      
         BCT   R0,INS2                                                          
         B     INSFOUND                                                         
*                                                                               
         USING T325FFD,R4                                                       
INSFOUND L     R4,ATWA                                                          
         MVC   CONHEAD(L'INSMESS),INSMESS                                       
         DROP  R4                                                               
         USING TIOBD,R4                                                         
         L     R4,SYSPARMS                                                      
         L     R4,0(R4)                                                         
         LH    R2,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         A     R2,ATWA             INTO TWA                                     
         B     VEXIT                                                            
         SPACE 1                                                                
DEL2     LR    R3,R2                                                            
         BAS   RE,BUMPTOUN                                                      
         CLI   5(R2),0                                                          
         BE    DEL4                                                             
         ZIC   R1,0(R3)            GET L'FIELD-1 INTO R1                        
         SH    R1,=H'9'                                                         
         TM    1(R3),X'02'                                                      
         BNO   *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE NEXT INTO THIS                          
         OI    6(R3),X'80'                                                      
         BCT   R0,DEL2                                                          
         SPACE 1                                                                
DEL4     EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR LAST ONE                               
         OI    6(R3),X'80'                                                      
         LR    R2,R3                                                            
         USING T325FFD,R4                                                       
         L     R4,ATWA                                                          
         MVC   CONHEAD(L'DELMESS),DELMESS                                       
         B     VEXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PERIOD EXPRESSIONS                                  *         
*              INPUTS : P1 = A(INPUT EXPRESSION)                      *         
*                       P2 = A(PERIOD S/E)                            *         
***********************************************************************         
*                                                                               
VVALPEX  LM    R2,R3,0(R1)                                                      
         XC    0(4,R3),0(R3)                                                    
         CLI   PDQPERHI,X'FF'      CHECK FOR PERIOD IN ROWS                     
         BE    VPERXNE             YES                                          
         CLI   0(R2),C'Q'          QUARTERS 1-5                                 
         BNE   VPER02                                                           
         CLI   2(R2),C' '                                                       
         BNE   VPERXNE                                                          
         CLI   1(R2),C'1'                                                       
         BL    VPERXEQ                                                          
         CLI   1(R2),C'5'                                                       
         BH    VPERXEQ                                                          
         TM    PDQPER,PDQPMN+PDQPWK  DON'T MIX PERIODS                          
         BNZ   VPERXEQ                                                          
         MVC   BYTE,1(R2)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   PDQPERLO,0                                                       
         BE    *+14                                                             
         CLC   BYTE,PDQPERLO                                                    
         BNL   *+10                                                             
         MVC   PDQPERLO,BYTE                                                    
         CLC   BYTE,PDQPERHI                                                    
         BNH   *+10                                                             
         MVC   PDQPERHI,BYTE                                                    
         ZIC   RF,BYTE                                                          
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         L     RE,AQTRS                                                         
         LA    RE,0(RE,RF)                                                      
         ST    RE,0(R3)                                                         
         OI    PDQPER,PDQPQT                                                    
         B     VPERXEQ                                                          
*                                                                               
VPER02   CLI   0(R2),C'M'          MONTHS 1-15                                  
         BNE   VPER04                                                           
         TM    PDQPER,PDQPQT+PDQPWK  DON'T MIX PERIODS                          
         BNZ   VPERXEQ                                                          
         L     RF,AMONTHS                                                       
         MVI   BYTE,C'5'                                                        
         BAS   RE,CHKPER                                                        
         OI    PDQPER,PDQPMN                                                    
         B     VPERXEQ                                                          
*                                                                               
VPER04   CLI   0(R2),C'W'          WEEKS 1-13                                   
         BNE   VPERXNE                                                          
         TM    PDQPER,PDQPQT+PDQPMN  DON'T MIX PERIODS                          
         BNZ   VPERXEQ                                                          
         L     RF,AWEEKS                                                        
         MVI   BYTE,C'3'                                                        
         BAS   RE,CHKPER                                                        
         OI    PDQPER,PDQPWK                                                    
*                                                                               
VPERXEQ  CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
VPERXNE  LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL VALPEX ROUTINES                                   *         
***********************************************************************         
*                                                                               
CHKPER   LR    R0,RE                                                            
         CLI   1(R2),C'1'                                                       
         BL    VPERXNE                                                          
         CLI   1(R2),C'9'                                                       
         BH    VPERXNE                                                          
         CLI   3(R2),C' '                                                       
         BNE   VPERXEQ                                                          
         LA    RE,X'70'                                                         
         CLI   2(R2),C' '                                                       
         BNH   CHKP02                                                           
         CLI   1(R2),C'1'                                                       
         BNE   VPERXEQ                                                          
         CLI   2(R2),C'0'                                                       
         BL    VPERXEQ                                                          
         CLC   2(1,R2),BYTE                                                     
         BH    VPERXEQ                                                          
         LA    RE,1(RE)                                                         
*                                                                               
CHKP02   EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  WORK(0),1(0,R2)                                                  
         CVB   R1,WORK                                                          
         CLI   PDQPERLO,0                                                       
         BE    *+12                                                             
         CLM   R1,1,PDQPERLO                                                    
         BNL   *+8                                                              
         STC   R1,PDQPERLO                                                      
         CLM   R1,1,PDQPERHI                                                    
         BNH   *+8                                                              
         STC   R1,PDQPERHI                                                      
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LR    RE,RF                                                            
         LA    RE,0(RE,R1)                                                      
         ST    RE,0(R3)                                                         
*                                                                               
CHKPX    LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        DRONE UTILITIES                                              *         
***********************************************************************         
*                                                                               
VINTDRON MVI   DRWHO,DRNETWHO      INITIALIZATION                               
         MVI   DRACTION,DRINIT                                                  
         MVC   DRDICT,=CL8'PDWRI'                                               
         MVC   DRALTDIC,=CL8'DRIVER'                                            
         MVC   DRCOMFAC,ACOMFACS                                                
         MVC   DRMAXWID,=H'999'    FORCE BIG - I CHECK WIDTH                    
         MVI   DRCMPMAX,C'N'       MAX COL FOR COMPS                            
         GOTO1 DRONE,DMCB,DRGEN                                                 
*                                                                               
VINTX    B     XIT                                                              
*                                                                               
VVRWDRON MVI   DRACTION,DRROW      VALIDATE A ROW                               
         B     ALLVAL                                                           
*                                                                               
VGRWDRON MVI   DRACTION,DRGENROW   GENERATE A ROW                               
         B     ALLDRONE                                                         
*                                                                               
VVCLDRON MVI   DRACTION,DRCOL      VALIDATE A COLUMN                            
         B     ALLVAL                                                           
*                                                                               
VGCLDRON MVI   DRACTION,DRGENCOL   GENERATE A COLUMN                            
         B     ALLDRONE                                                         
*                                                                               
VVCMDRON MVI   DRACTION,DRCMP      VALIDATE A COMP                              
         B     ALLVAL                                                           
*                                                                               
VGCMDRON MVI   DRACTION,DRGENCMP   GENERATE A COMP                              
         B     ALLVAL              (LOOKS LIKE A VALIDATION)                    
*                                                                               
VGUSDRON MVI   DRACTION,DRUSER     GENERATE USER ELEMENTS                       
         B     ALLDRONE                                                         
*                                                                               
VWRPDRON MVI   DRACTION,DRWRAPUP   WRAP UP                                      
         GOTO1 DRONE,DMCB,DRGEN                                                 
         BAS   RE,TRACDRON         (OPTIONAL TRACE)                             
*                                                                               
VWRPX    B     XIT                                                              
*                                                                               
*                                                                               
ALLVAL   XC    WORK,WORK           GENERATE A PSEUDO TWA HEADER                 
         MVC   WORK+5(1),0(R4)     (PASS THROUGH THE LENGTH)                    
         MVC   WORK+8(30),12(R4)                                                
         LA    R1,WORK                                                          
         ST    R1,DRACCFLD                                                      
         OI    DRFLAGS,DREXPDIC    TELL DRONE TO EXPLODE DICT.                  
         GOTO1 DRONE,DMCB,DRGEN                                                 
*                                                                               
ALLVX    B     XIT                                                              
*                                                                               
*                                                                               
ALLDRONE GOTO1 DRONE,DMCB,DRGEN                                                 
*                                                                               
ALLDX    B     XIT                 USER NEEDS TO TEST DRERROR                   
         EJECT                                                                  
***********************************************************************         
*        TRACE OPTION                                                 *         
***********************************************************************         
*                                                                               
TRACDRON NTR1                                                                   
         CLI   TRACEOPT,C'Y'       DRONE TRACING OPTION                         
         BNE   TRACX                                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   TRACX                                                            
         L     R3,ADPGPROG                                                      
*                                                                               
TRAC02   CLI   0(R3),0                                                          
         BE    TRACX                                                            
         ZIC   R4,1(R3)                                                         
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
         LA    R4,1(R4)                                                         
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         GOTO1 HEXOUT,DMCB,(R3),BLOCK,(R4),=C'SEP'                              
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),BLOCK                                                       
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         LA    R5,BLOCK+1(R4)                                                   
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R5)                                                       
         BASR  RE,RF                                                            
         MVC   P,SPACES                                                         
         BASR  RE,RF                                                            
         LA    R3,1(R3,R4)                                                      
         B     TRAC02                                                           
*                                                                               
TRACX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE DRIVER                                            *         
***********************************************************************         
*                                                                               
VINTDRIV GOTO1 =A(INTDRIV),DMCB,(R9),(RC),RR=PGNR                               
         L     RE,APODOBSQ                                                      
         XC    0(L'PODOBSQ,RE),0(RE)                                            
*                                                                               
VINTDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        GENERAL HEADLINE HOOK ROUTINES                               *         
***********************************************************************         
*                                                                               
VGENHEAD L     R3,AH4              SET TO HEADLINE 4                            
         MVC   1(60,R3),0(R2)      MOVE BOOK FIELD TO HEADLINE                  
*                                                                               
         L     R2,AH1              DEAL WITH MAIN TITLE                         
         LA    R2,32(R2)                                                        
         CLI   WIDEOPT,C'Y'                                                     
         BNE   *+8                                                              
         LA    R2,16(R2)                                                        
         MVC   0(64,R2),TITLE      (TITLES ARE ALREADY CENTERED)                
         A     R2,PWIDTH                                                        
         GOTO1 UNDERLIN,DMCB,(64,TITLE),(X'BF',(R2))                            
*                                                                               
         L     R2,AH5              LINE UP THE LEFT SIDE                        
         LA    R2,1(R2)                                                         
         LA    R3,15                                                            
         L     R1,AGLOBAL                                                       
         USING GLOBALD,R1                                                       
         ZIC   R4,GLFHEADL                                                      
         DROP  R1                                                               
         SH    R4,=H'7'                                                         
         BAS   RE,GETLONG                                                       
         LA    R3,16(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
         LA    R3,12                                                            
         BAS   RE,GETLONG                                                       
         LA    R3,13(R2)                                                        
         A     R2,FULL                                                          
         LA    R2,2(R2)                                                         
         BAS   RE,SHUFFLE                                                       
*                                                                               
VGENX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL GENHEAD ROUTINES                                  *         
***********************************************************************         
**                                                                              
*        GET LONGEST FIELD                                            *         
*              INPUT : R2 = A(FIELD ON FIRST LINE)                    *         
*                      R3 = MAX WIDTH                                 *         
*                      R4 = NUMBER OF LINES                           *         
*              OUTPUT: FULL = WIDEST FOUND                            *         
**                                                                              
GETLONG  NTR1                                                                   
GETL02   ST    R3,FULL                                                          
         LTR   R3,R3                                                            
         BZ    GETLX                                                            
         LA    R1,0(R2,R3)                                                      
         BCTR  R1,0                R1=END OF PRESENT FIELD                      
         LR    R0,R4                                                            
*                                                                               
GETL04   CLI   0(R1),C' '          SEE IF ANYTHING SIGNIFICANT                  
         BH    GETLX                                                            
         A     R1,PWIDTH           ON EACH OF THE LINES                         
         BCT   R0,GETL04                                                        
         BCTR  R3,0                                                             
         B     GETL02                                                           
*                                                                               
GETLX    B     XIT                                                              
         EJECT                                                        *         
* INPUT  : R2=A(START DATA ON FIRST LINE)                                       
*          R3=A(FROM DATA)                                                      
*          R4=NUMBER OF LINES                                                   
*                                                                     *         
*                                                                     *         
*                                                                     *         
SHUFFLE  NTR1                                                                   
*                                                                               
SHUF02   MVC   0(50,R2),0(R3)                                                   
         A     R2,PWIDTH                                                        
         A     R3,PWIDTH                                                        
         BCT   R4,SHUF02                                                        
*                                                                               
SHUFX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        GENERATE EDICT CONTROL CARDS FOR TRANSMIT REPORT             *         
***********************************************************************         
*                                                                               
VGENEDCT L     R4,ATWA                                                          
         USING T325FFD,R4                                                       
         LA    R2,KEY              READ DESTINATION ID RECORD                   
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         L     R1,TWAMASTC                                                      
         MVC   CTIKNUM,MCDESTID-MASTD(R1)                                       
         MVC   FILENAME,=CL8'CTFILE'                                            
         IC    R0,USEIO                                                         
         MVI   USEIO,C'Y'                                                       
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         STC   R0,USEIO                                                         
         XC    FILENAME,FILENAME                                                
         CLC   KEY(L'CTIKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         SR    R0,R0               FIND DESCRIPTION ELEMENT                     
         LA    R1,CTIDATA                                                       
*                                                                               
GENED2   CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),CTDSCELQ                                                   
         BNE   GENED4                                                           
         CLI   1(R1),12                                                         
         BNE   GENED4                                                           
         MVC   WORK(10),CTDSC-CTDSCD(R1)  EXTRACT DESTINATION ID NAME           
         B     GENED6                                                           
*                                                                               
GENED4   IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     GENED2                                                           
*                                                                               
GENED6   MVC   P,SPACES            *HDR* CARD                                   
         LA    R1,P                                                             
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(6,R1),=C'EDICT='                                               
         MVC   15(10,R1),WORK      DESTINATION                                  
         MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVC   38(10,R1),WORK      FORMATTED DESTINATION NAME                   
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
         MVC   P,SPACES            ++DDS TRN CARD                               
         MVC   P(14),=CL14'++DDS NEPODTRN'                                      
         MVC   P+6(2),RCPROG       CHANGE SYSTEM TO NE, RE OR SP                
         MVC   P+9(2),SPLNAM                                                    
         LA    R1,P+15                                                          
         USING SPEDICTD,R1                                                      
         MVI   SPWRTYPE,SPWRDATQ                                                
         MVC   SPWRNAME,SPLNAM                                                  
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
GENEDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        OTHER DATA HANDLING ROUTINES                                 *         
***********************************************************************         
*                                                                               
VNUMERIC TM    4(R2),X'08'                                                      
         BO    VPACK                                                            
         MVI   ERROR,NOTNUM                                                     
         B     VEXIT                                                            
*                                                                               
VPACK    SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         SR    R1,R1                                                            
         ZAP   DUB,=P'0'                                                        
         LTR   R3,R3                                                            
         BZ    VPACKX                                                           
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
VPACKX   XIT1  REGS=(R1)                                                        
         EJECT                                                                  
***********************************************************************         
*        POSITION CURSOR TO CORRECT FIELD FOR ERRORS                  *         
*              INPUT : R2 = A(SCREEN HEADER)                          *         
*                      FIELDERR = NUMBER OF FIELD IN ERROR            *         
***********************************************************************         
*                                                                               
VCURSERR CLI   FIELDERR,0          APPLICATION MUST SET FIELD NUMBER            
         BE    VERRXIT                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    VERRXIT                                                          
         L     R4,ATIOB                                                         
         USING TIOBD,R4                                                         
         OI    6(R2),X'80'         TRANSMIT ERROR FIELD HEADER                  
         OI    TIOBINDS,TIOBSETC   INSTRUCT CURSOR SETTING                      
         LR    RF,R2                                                            
         S     RF,ATWA                                                          
         STCM  RF,3,TIOBCURD       DISPLACEMENT FROM START OF TWA               
         LA    RE,8(R2)                                                         
         SR    R1,R1               COMPUTE FIELD DISPLACEMENT INTO R1           
         SR    R0,R0                                                            
         ICM   R0,1,5(R2)          R0 HAS FIELD LENGTH                          
         BZ    CURS05                                                           
         ZIC   RF,FIELDERR                                                      
         BCT   RF,CURS02           CHECK IF ERROR IS IN FIELD 1                 
         B     CURS04                                                           
*                                                                               
CURS02   CLI   0(RE),C','          SCAN FOR THE COMMAS                          
         BNE   CURS04                                                           
         BCT   RF,CURS04                                                        
         LA    R1,1(R1)            FOUND ENOUGH - SPACE PAST LAST               
         B     CURS06                                                           
*                                                                               
CURS04   LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,CURS02                                                        
*                                                                               
CURS05   SR    R1,R1               ERROR - DIDN'T FIND ENOUGH COMMAS            
*                                                                               
CURS06   STC   R1,TIOBCURI         SET CURSOR DISPLACEMENT WITHIN FIELD         
         B     VEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE INTERFACES TO GENCON'S ERREX ROUTINE AND SETS SPECIAL  *         
* PARAMETERS TO MAKE IT CALL GETTXT INSTEAD OF GETMSG. SINCE WUNDERMAN*         
* USES MESSAGE NUMBERS GREATER THAN 255, GETTXT MUST BE USED BECAUSE  *         
* GETMSG IS NOT DESIGNED TO HANDLE THESE NUMBERS.                     *         
***********************************************************************         
*                                                                               
VPDSERR  OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         MVC   GTMSGNO,PERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,PMSGTYPE     MESSAGE TYPE                                 
*                                                                               
         CLC   PERROR,=H'60'       IF MESSAGE NUMBER <= 60                      
         BH    *+8                                                              
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
*                                                                               
         CLC   PERROR,=H'300'      IF MESSAGE NUMBER >= 300                     
         BL    *+8                                                              
         MVI   GTMSYS,24           USE SYSTEM 24 MESSAGES                       
*                                                                               
         GOTO1 CURSERR                                                          
         EJECT                                                                  
BUMP     ZIC   RF,0(R2)            GET TO NEXT SCREEN FIELD                     
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            GET TO NEXT UNPROTECTED FIELD                
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         DROP  R4                                                               
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
         EJECT                                                                  
***********************************************************************         
*        ERROR ROUTINES                                               *         
***********************************************************************         
*                                                                               
BADFILT  MVC   PERROR,=AL2(INVFEX)                                              
         B     FILTERR                                                          
*                                                                               
BADFILT2 MVC   PERROR,=AL2(TOOMANY)                                             
         B     FILTERR                                                          
*                                                                               
BADPREF  MVC   PERROR,=AL2(INVFPR)                                              
         B     FILTERR                                                          
*                                                                               
BADFMX8  MVC   PERROR,=AL2(MAX8FIL)                                             
         B     FILTERR                                                          
*                                                                               
FILTERR  L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLFILTH                                                      
         B     PODERR1                                                          
         DROP  R1                                                               
*                                                                               
*                                                                               
BADNET   MVC   PERROR,=AL2(INVNET)                                              
         CLI   PDOVSYS,3           NETWORK OR STATION                           
         BE    *+10                                                             
         MVC   PERROR,=AL2(INVSTA)                                              
         B     NETERR                                                           
*                                                                               
BADNET2  MVC   PERROR,=AL2(TOOMKT) TOO MANY MARKETS                             
         B     NETERR                                                           
*                                                                               
BADNTBK  L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLNETH                                                       
         MVC   CONHEAD(L'INVNTBK),INVNTBK   INVALID NETWORK/BOOK                
         CLI   PDOVSYS,3           NETWORK OR STATION                           
         BE    *+10                                                             
         MVC   CONHEAD+21(7),=C'STATION'                                        
         MVC   CONHEAD+41(5),0(R4)                                              
         MVI   CONHEAD+46,C'-'                                                  
         MVC   CONHEAD+47(1),5(R4)                                              
         B     MYEND                                                            
         DROP  R1                                                               
*                                                                               
NETERR   L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLNETH                                                       
         B     PODERR1                                                          
         DROP  R1                                                               
*                                                                               
*                                                                               
OPTCON   MVC   PERROR,=AL2(MRKCON)                                              
         B     PODERR1                                                          
*                                                                               
OPTCON2  MVC   PERROR,=AL2(MRGCON)                                              
         B     PODERR1                                                          
*                                                                               
FILCON   MVC   PERROR,=AL2(NTICON)                                              
         B     PODERR1                                                          
*                                                                               
BADDEM   MVC   PERROR,=AL2(INVDEM)                                              
         B     PODERR1                                                          
*                                                                               
BADDEM2  MVC   PERROR,=AL2(IMPRTG)                                              
         B     PODERR1                                                          
*                                                                               
INVPROG  MVC   PERROR,=AL2(INVFORM)                                             
         B     PODERR1                                                          
*                                                                               
HEADERR  MVC   PERROR,=AL2(WIDOVR)                                              
         B     PODERR1                                                          
*                                                                               
BADROW   MVC   PERROR,=AL2(INVROW)                                              
         B     PODERR1                                                          
*                                                                               
BADSTROW MVC   PERROR,=AL2(NOSTAK)                                              
         B     PODERR1                                                          
*                                                                               
BADLRANK MVC   PERROR,=AL2(NOLAST)                                              
         B     PODERR1                                                          
*                                                                               
BADLEN   MVC   PERROR,=AL2(NOCLEN)                                              
         B     PODERR1                                                          
*                                                                               
BADCOL   MVC   PERROR,=AL2(INVCOL)                                              
         B     PODERR1                                                          
*                                                                               
RANKERR  MVC   PERROR,=AL2(INVRNK)                                              
         B     PODERR1                                                          
*                                                                               
BADNEED1 MVC   PERROR,=AL2(NEED1)                                               
         B     PODERR1                                                          
*                                                                               
USERER   MVC   PERROR,=AL2(NOUSER)                                              
         B     PODERR1                                                          
*                                                                               
PERERR2  MVC   PERROR,=AL2(MISSPER)                                             
         B     PODERR1                                                          
*                                                                               
BADDAY   MVI   ERROR,INVDAY        INVALID DAY                                  
         B     VEXIT                                                            
*                                                                               
BADTIME  MVI   ERROR,INVTIME                                                    
         L     R2,SAVER2           RESTORE R2 FOR CURSOR                        
         B     VEXIT                                                            
*                                                                               
BADTIME2 MVC   PERROR,=AL2(TALLERR)                                             
         B     PODERR1                                                          
*                                                                               
BADMED   MVI   ERROR,INVMED                                                     
         B     VEXIT                                                            
*                                                                               
BADDRONE L     R1,ATWA             ERROR - SO SHOW WHAT DRONE PASSED            
         USING T325FFD,R1                                                       
         MVC   CONHEAD,DRERRMSG                                                 
         MVI   ERROR,X'FE'                                                      
         GOTO1 CURSERR                                                          
         DROP  R1                                                               
*                                                                               
DTOVER   L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         MVC   CONHEAD(L'INVDTOV),INVDTOV    ERROR                              
         B     MYEND                                                            
*                                                                               
COLWIDE  L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         MVC   CONHEAD(36),=C'REPORT HAS NNN CHARACTERS - TOO WIDE'             
         LA    R3,CONHEAD+11                                                    
         EDIT  (2,TOTWIDTH),(3,(R3))                                            
         L     R2,ALASTCOL                                                      
         B     MYEND                                                            
*                                                                               
PODERR1  MVI   PMSGTYPE,C'E'                                                    
         GOTO1 VPDSERR                                                          
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        COMMON EXIT ROUTINES                                         *         
***********************************************************************         
*                                                                               
MYEND    MVI   ERROR,X'FE'                                                      
*                                                                               
VEXIT    OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
VERRXIT  CLI   ERROR,X'FE'                                                      
         BE    VEXIT2                                                           
         GOTO1 ERREX               PUT OUT SYSTEM MESSAGE                       
*                                                                               
VEXIT2   GOTO1 ERREX2              PUT OUT CUSTOM MESSAGE                       
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS, TABLES, ETC.                                      *         
***********************************************************************         
*                                                                               
INVNTBK  DC    C'EX/0118 INVALID BOOK NETWORK COMBINATION'                      
INVDTOV  DC    C'EX/0119 INVALID DAY AND TIME OVERLAP'                          
INSMESS  DC    C'EX/0204 NEW FIELD INSERTED ON SCREEN'                          
DELMESS  DC    C'EX/0205 FIELD DELETED ON SCREEN'                               
*                                                                               
*RELO     DS    A                                                               
*                                                                               
RELOC    DC    A(*)                                                             
*                                                                               
NADUSE   DC    C'N'                                                             
*                                                                               
COLRANK  DC    X'00'                                                            
*                                                                               
SETNFF   DC    C'NETW',A(0),C'YYYY',A(0)                                        
*                                                                               
EDITLIST DC    XL64'00'            USED OFFLINE ONLY                            
*                                                                               
NEGSW    DS    C                                                                
*                                                                               
MYBLOCK  DS    CL800               USE MYBLOCK FOR VALFILT/VALOPT               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE BOOK                                                *         
***********************************************************************         
*                                                                               
VALBK    NMOD1 0,**VALBK**                                                      
         L     R2,0(R1)                                                         
         L     R9,4(R1)                                                         
         L     RC,8(R1)                                                         
*                                                                               
         CLI   5(R2),0             NO SOURCE, BOOK?                             
         BNE   VALB02                                                           
         MVI   PERROR+1,MISSING    * SPECIAL CASE *                             
         MVI   PMSGTYPE,C'E'       LET ERROR ROUTINE HANDLE IT                  
         GOTO1 VPDSERR                                                          
*                                                                               
VALB02   L     RE,APODBKL                                                       
         LA    RF,451                                                           
         XCEFL                                                                  
*                                                                               
         L     RE,APDREQDT         CLEAR REQUESTED DATE TABLE (EXACT)           
         LA    RF,L'PDREQDT                                                     
         XCEFL                                                                  
*                                                                               
         XC    PDAESTBK,PDAESTBK        RESET EST BOOK PTR                      
         GOTO1 PODSCAN,DMCB,(20,(R2)),(12,BLOCK),C',=,-'                        
         CLI   DMCB+4,2            MUST BE 2 PARTS FOR RANGE                    
         BL    BADBOOK                                                          
*                                                                               
         ZIC   R2,DMCB+4                                                        
         LA    R3,BLOCK            SET UP DBLOCK                                
         L     R4,APODBKL                                                       
*                                                                               
VALB04   L     R5,=A(BOOKTAB)                                                   
         A     R5,PGNR                                                          
                                                                                
         CLC   =C'INV',12(R3)                                                   
         BNE   *+10                                                             
         MVC   12(3,R3),=C'IUN'                                                 
                                                                                
         CLI   1(R3),0             NO SECOND FIELD                              
         BE    VALB06                                                           
         MVI   15(R3),C'-'                                                      
         MVC   16(1,R3),24(R3)                                                  
         LA    RE,5                ALWAYS 5 AFTER THIS                          
         STCM  RE,1,0(R3)                                                       
*                                                                               
VALB06   CLC   0(5,R5),12(R3)                                                   
         BE    VALB08                                                           
         LA    R5,12(R5)                                                        
         CLI   0(R5),X'FF'                                                      
         BNE   VALB06                                                           
         B     BADSRCE                                                          
*                                                                               
VALB08   CLC   7(3,R5),=CL3'NAD'   NAD AND NHI                                  
         BNE   VALB10                                                           
         MVI   PODNADBK,X'FF'                                                   
         B     VALB14                                                           
*                                                                               
VALB10   CLC   0(3,R5),=CL3'MPA'                                                
         BE    VALB12                                                           
         CLC   7(2,R5),=CL3'TP '   THIS COVERS TP, T4 ,DPT, SRC, BBM            
         BE    VALB12                          CSI                              
         CLC   7(3,R5),=CL3'PAV'                                                
         BE    VALB12                          CSI                              
*        BNE   VALB14                                                           
         CLC   7(3,R5),=CL3'IUN'                                                
         BNE   VALB14                                                           
VALB12   MVI   PODNADBK,X'F0'      SPOT BOOK WAS REQUESTED                      
*                                                                               
VALB14   CLC   0(3,R5),=CL3'PIV'                                                
         BNE   *+14                                                             
         OC    PODPSTRT(4),PODPSTRT   SEE IF DATES REQUESTED                    
         BZ    BADPIVDT                                                         
*                                                                               
         L     RE,APDREQDT                                                      
         STCM  RE,15,REQDPTR       SAVE A(REQUESTED DATE TABLE)                 
*                                                                               
         ZIC   RF,11(R5)                                                        
         SLL   RF,2                CK FOR NO INPUT                              
         B     VALB16(RF)                                                       
*                                                                               
VALB16   B     VALB100             ROUTINE DOES BOOKVAL                         
         B     VALB300             ROUTINE FOR EMI/NMI                          
         EJECT                                                                  
VALB100  DS    0H                                                               
         BAS   RE,BOOKBUMP                                                      
         BZ    BADBOOK                                                          
*                                                                               
VALB102  BAS   RE,BOOKNUM                                                       
         BNZ   BADBOOK                                                          
*                                                                               
         BAS   RE,SAVEREQ          SAVE REQUESTED DATE                          
         BNE   BADBOOK                                                          
*                                                                               
         L     RE,APODBKL                                                       
         MVC   PODBD(PODBLNQ),0(RE)                                             
         GOTO1 =A(OVFLRTN),DMCB,(6,DUB),(RC),RR=PGNR VSFTBOOK                   
         BE    VALB112                                                          
*                                                                               
VALB104  XC    DMCB(12),DMCB                                                    
         ZIC   RF,0(R3)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 ONEBOOK,DMCB,12(R3),,FULL,(R5)                                   
         BNE   BADBOOK                                                          
         MVC   1(11,R4),0(R5)      INFO FROM BOOKTAB                            
         MVC   12(2,R4),FULL+1     MOVE BOOK TO TABLE                           
         MVC   14(1,R4),DMCB+20    MOVE IN BOOK TYPE                            
         CLI   DMCB+8,C'L'         WAS THIS SET FOR LABEL (SQAD)?               
         BNE   VALB105             NO                                           
*                                                                               
         MVI   14(R4),0            CLEAR BOOK TYPE                              
         CLC   DMCB+20(5),SPACES   DID WE GET A QUARTER?                        
         BNH   BADBOOK             NO                                           
         LA    RF,DMCB+21          YES, POINT TO QUARTER PARAMETER              
         PACK  DUB,0(1,RF)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,1                MUST BE BETWEEN 1 AND 4                      
         BL    BADBOOK                                                          
         CHI   R1,4                                                             
         BH    BADBOOK                                                          
         STC   R1,17(R4)           STORE QUARTER AS BINARY                      
         LA    RF,1(0,RF)          BUMP TO YEAR OR /                            
*                                                                               
         CLI   0(RF),C'/'          SKIP OVER / IF THERE                         
         BNE   *+8                                                              
         LA    RF,1(0,RF)                                                       
         PACK  DUB,0(2,RF)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'27'                                                        
         BH    *+8                                                              
         LA    R1,100(R1)                                                       
         STC   R1,16(R4)           STORE YEAR AS BINARY                         
*                                                                               
VALB105  CLC   =C'IUN',0(R5)       DECODE REP INV KEY SOURCE                    
         BNE   VALB108                                                          
         LA    RF,SVCLST                                                        
         MVI   15(R4),C'N'                                                      
*                                                                               
VALB106  CLI   0(RF),X'FF'         DEFAULT - JUST LEAVE AS C'N'                 
         BE    VALB110                                                          
         CLC   FULL(1),3(RF)       FOUND A MATCH (SOURCE/BOOK)                  
         BE    *+12                                                             
         LA    RF,L'SVCLST(RF)                                                  
         B     VALB106                                                          
         CLC   DMCB+20(1),4(RF)       FOUND A MATCH (BOOKTYPE)                  
         BE    *+12                                                             
         LA    RF,L'SVCLST(RF)                                                  
         B     VALB106                                                          
         MVC   15(1,R4),2(RF)      SET INV KEY SOURCE                           
         B     VALB110                                                          
*                                                                               
VALB108  TM    FULL,X'20'          DO WE USE ESTIMATED BOOK?                    
         BZ    *+12                                                             
         OI    13(R4),X'80'        SET HOB ON MONTH/WEEK                        
         STCM  R4,15,PDAESTBK                                                   
*                                                                               
VALB110  CLI   1(R3),0                                                          
         BE    VALB112                                                          
         OI    0(R4),X'01'         SET RANGE INDICATOR                          
         LA    R4,PODBLNQ(R4)                                                   
*                                                                               
         ZIC   RF,1(R3)            SET 2ND FLD LEN                              
         ST    RF,DMCB+4                                                        
         GOTO1 ONEBOOK,DMCB,24(R3),,FULL,(R5)                                   
         BNE   BADBOOK                                                          
         MVC   1(11,R4),0(R5)      INFO FROM BOOKTAB                            
         MVC   12(2,R4),FULL+1     MOVE BOOK TO TABLE                           
         MVC   14(1,R4),DMCB+20                                                 
*                                                                               
         CLC   0(3,R5),=CL3'PIV'   IF PROGRAM INVENTORY                         
         BNE   *+14                                                             
         OC    PODPSTRT,PODPSTRT   THEN PROGRAM DATE RANGE MUST EXIST           
         BZ    BADBOOK                                                          
*                                                                               
         LR    RE,R4                                                            
         LA    RF,PODBLNQ                                                       
         SR    RE,RF               CHECK THE YEARS AT LEAST                     
         CLC   12(1,RE),PODBLNQ+PODBBKS-PODBD(RE)                               
         BH    BADBOOK                                                          
*--SET PODBOOK AREA                                                             
*                                                                               
VALB112  LA    R4,PODBLNQ(R4)                                                   
         CLI   1(R4),0                                                          
         BNE   VALB112                                                          
         ZIC   RF,PODBKNUM                                                      
         LA    RF,1(RF)                                                         
         STC   RF,PODBKNUM                                                      
         BAS   RE,BOOKBUMP                                                      
         BE    VALBX                                                            
         BAS   RE,BOOKNUM                                                       
         BNZ   VALB04                                                           
         B     VALB102                                                          
*                                                                               
VALBX    MVI   0(R4),X'FF'         SET END OF TABLE MARKER                      
         L     RE,APODBKL                                                       
         MVC   PODBD(PODBLNQ),0(RE)                                             
         ICM   RE,15,REQDPTR       SET END OF REQUEST DATES TABLE               
         MVI   0(RE),X'FF'                                                      
         GOTO1 =A(OVFLRTN),DMCB,(3,DUB),(RC),RR=PGNR VFLOWTAB                   
         XMOD1                                                                  
         EJECT                                                                  
*----------------------------------------------------------*                    
* THIS ROUTINE IS FOR VALIDATING EMI/NMI SOURCES AND BOOKS *                    
*----------------------------------------------------------*                    
VALB300  DS    0H                                                               
         LR    R0,R5               SAVE THE BOOK TABLE ADDRESS                  
         CLC   0(3,R5),=C'EMI'     SEE IF DATES REQUESTED                       
         BNE   *+14                                                             
         OC    PODPSTRT(4),PODPSTRT                                             
         BZ    BADPIVDT                                                         
*                                                                               
* FIRST BOOK IN EXPRESSION                                                      
*                                                                               
         BAS   RE,BOOKBUMP                                                      
         BZ    BADBOOK                                                          
VALB302  BAS   RE,BOOKNUM                                                       
         BNZ   BADBOOK                                                          
*                                                                               
         BAS   RE,SAVEREQ          SAVE REQUESTED DATE                          
         BNE   BADBOOK                                                          
*                                                                               
         GOTO1 =A(OVFLRTN),DMCB,(6,DUB),(RC),RR=PGNR VSFTBOOK                   
         BE    VALB304                                                          
*                                                                               
         XC    DMCB(12),DMCB                                                    
         ZIC   RF,0(R3)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 ONEBOOK,DMCB,12(R3),,FULL,(R5)                                   
         BNE   BADBOOK                                                          
         MVC   1(11,R4),0(R5)      INFO FROM BOOKTAB                            
         MVC   12(2,R4),FULL+1     MOVE BOOK TO TABLE                           
         MVC   14(1,R4),DMCB+20                                                 
*                                                                               
         CLI   1(R3),0                                                          
         BE    VALB304                                                          
         OI    0(R4),X'01'         SET RANGE INDICATOR                          
         LA    R4,PODBLNQ(R4)                                                   
*                                                                               
         ZIC   RF,0(R3)                                                         
         ST    RF,DMCB+4                                                        
         GOTO1 ONEBOOK,DMCB,24(R3),,FULL,(R5)                                   
         BNE   BADBOOK                                                          
         MVC   1(11,R4),0(R5)      INFO FROM BOOKTAB                            
         MVC   12(2,R4),FULL+1     MOVE BOOK TO TABLE                           
         MVC   14(1,R4),DMCB+20                                                 
*                                                                               
         CLC   0(3,R5),=CL3'EMI'   IF PROGRAM INVENTORY                         
         BNE   *+14                                                             
         OC    PODPSTRT,PODPSTRT   THEN PROGRAM DATE RANGE MUST EXIST           
         BZ    BADBOOK                                                          
*                                                                               
         LR    RE,R4                                                            
         LA    RF,PODBLNQ                                                       
         SR    RE,RF                                                            
         CLC   12(1,RE),PODBLNQ+PODBBKS-PODBD(RE) CHECK THE YEARS               
         BH    BADBOOK                                                          
*--SET PODBOOK AREA                                                             
*                                                                               
VALB304  DS    0H                                                               
         LA    R4,PODBLNQ(R4)                                                   
         CLI   1(R4),0                                                          
         BNE   VALB304                                                          
         ZIC   RF,PODBKNUM                                                      
         LA    RF,1(RF)                                                         
         STC   RF,PODBKNUM                                                      
         CR    R0,R5                                                            
         BNE   VALB306             FINISHED THE 2 PARTS, NTI & MPA              
         BAS   RE,BOOKBUMP                                                      
         BZ    BADBOOK2            NEED MPA BOOK                                
*                                                                               
*----------------------------------------------------------------               
* FIRST BOOK (NTI) FINISHED GRAB SECOND BOOK (MPA)                              
*----------------------------------------------------------------               
         BAS   RE,BOOKNUM                                                       
         BNZ   VALB04                                                           
*                                  NEED MPA INFO                                
         L     R5,=A(BOOKMPA)                                                   
         A     R5,PGNR                                                          
         B     VALB302                                                          
*                                                                               
VALB306  BAS   RE,BOOKBUMP                                                      
         BZ    VALBX                                                            
         BAS   RE,BOOKNUM                                                       
         BNZ   VALB04                                                           
         LR    R5,R0               RESTORE ORIGINAL SOURCE                      
         B     VALB302                                                          
         EJECT                                                                  
***********************************************************************         
*        OPTIONALLY SAVE REQUESTED DATES.                             *         
*        VALID ENTRIES ARE MMM/DD/YY AND MMM/YY                       *         
***********************************************************************         
*                                                                               
SAVEREQ  NTR1                                                                   
         CLI   EXACTOPT,C'Y'       ARE WE RUNNING WITH EXACT DATES?             
         BNE   SAVEOK              NO, DON'T NEED THIS THEN                     
         ICM   R5,15,REQDPTR       GET NEXT SPACE IN TABLE                      
*                                                                               
SAVE01   LA    R4,12(R3)           POINT TO FIRST DATE                          
*                                                                               
SAVE02   GOTO1 DATVAL,DMCB,(R4),WORK                                            
         OC    DMCB(4),DMCB        MMDDYY?                                      
         BZ    SAVENG              NO, NOT A VALID DATE                         
*                                                                               
SAVE04   GOTO1 DATCON,DMCB,WORK,WORK+6                                          
         ORG   *-2                                                              
         CLC   WORK+4(2),=C'00'                                                 
         BNE   SAVE06                                                           
         MVC   WORK+4(2),=C'01'                                                 
         MVI   0(R1),X'30'                                                      
         LA    RE,WORK                                                          
         ST    RE,8(R1)                                                         
         MVI   8(R1),X'01'                                                      
*                                                                               
SAVE06   BASR  RE,RF                                                            
         OC    0(6,R5),0(R5)       DO WE ALREADY HAVE START DATE?               
         BNZ   SAVE08              YES, THIS IS THE SEND PASS THEN              
         MVC   0(6,R5),WORK        NO, SAVE THE START DATE                      
         CLC   WORK(6),WORK+6      ARE START AND END THE SAME?                  
         BE    *+10                YES                                          
         MVC   6(6,R5),WORK+6      NO, MOVE IN END OF MONTH                     
         LA    R4,24(R3)           BUMP TO NEXT FIELD IN CASE NEEDED            
         CLI   1(R3),0             IS THERE A DATE RANGE?                       
         BNE   SAVE02              YES, PROCESS SECOND HALF                     
         MVC   6(6,R5),WORK+6      NO, START/END THE SAME                       
         B     SAVE10              DONE                                         
*                                                                               
SAVE08   MVC   6(6,R5),WORK        SAVE SECOND DATE                             
*                                                                               
SAVE10   LA    R5,12(R5)           GET NEXT SPOT                                
         STCM  R5,15,REQDPTR       SAVE THE ADDRESS FOR NEXT TIME               
         LA    R3,42(R3)           GET NEXT BOOK/DATE                           
         BCT   R2,SAVE01           IF ANY, GO BACK                              
*                                                                               
SAVEOK   SR    R1,R1               EQUAL IF OK                                  
         B     *+8                                                              
*                                                                               
SAVENG   LA    R1,1                NOT EQUAL IF INVALID                         
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DEAL WITH ONE BOOK EXPRESSION                     *         
*              INPUT          (R2) P1=A(INPUT EXPRESSION)             *         
*                             (R3) P2=LENGTH OF EXPRESSION            *         
*                             (R4) P3=A(BOOK OUTPUT)                  *         
*                             (R5) P4=A(BOOKTAB)                      *         
***********************************************************************         
*                                                                               
ONEBOOK  NTR1                                                                   
         LM    R2,R5,0(R1)                                                      
         MVI   DMCB+20,X'00'       RESET POTENTIAL BOOKTYPE                     
         MVI   SVBBMWK,0           CLEAR BBM WEEKLY REQUEST INDICATOR           
         MVI   BYTE,0                                                           
*                                                                               
         GOTO1 DATVAL,PARAS,(R2),WORK                                           
         OC    PARAS(4),PARAS                                                   
         BNZ   ONEB02                                                           
*                                                                               
         GOTO1 DATVAL,PARAS,(2,(R2)),WORK                                       
         OC    PARAS(4),PARAS                                                   
         BZ    ONEB04                                                           
*                                                                               
ONEB02   CLC   WORK(4),=CL4'8709'  TEST BOOK LESS THEN SEP/87                   
         BNL   *+8                                                              
         OI    PODBKOPT,X'80'                                                   
*                                                                               
         CLC   WORK(4),=CL4'8710'  TEST BOOK GREATER THEN OCT/87                
         BL    *+8                                                              
         OI    PODBKOPT,X'40'                                                   
*                                                                               
         CLC   WORK(4),=CL4'8609'  TEST BOOK LESS THEN SEP/86                   
         BNL   ONEB04                                                           
         OI    PODBKOPT,X'20'                                                   
*                                  FIRST SEE IF BOOKVAL CAN HANDLE              
ONEB04   MVC   HALF(2),WORK        SAVE THE YEAR FOR CSI                        
         XC    WORK,WORK                                                        
         STC   R3,WORK+5                                                        
         MVC   WORK+8(12),0(R2)                                                 
         MVC   DMCB+8(4),SCANNER                                                
         MVI   DMCB+8,C'N'         NET  VALIDATES FOR WEEK  BOOK                
         LA    RF,ONEB06                                                        
         BAS   RE,ONEMCHK                                                       
         B     *+8                 NOT EQUAL DON'T SET FOR SPOT                 
*                                  FIRST SEE IF BOOKVAL CAN HANDLE              
ONEB06   MVI   DMCB+8,C'S'                                                      
         CLI   DMCB+8,C'S'         IF SPOT                                      
         BNE   ONEB08                                                           
         LA    RE,DMCB+20          SETUP FOR BOOKTYPE                           
         ST    RE,DMCB+12                                                       
         MVI   DMCB+8,C'B'                                                      
         CLC   0(4,R5),=C'SQAD'    EXCEPT IF SQAD                               
         BNE   ONEB08                                                           
         MVI   DMCB+8,C'L'         THEN LOOK FOR LABEL                          
*                                  FIRST SEE IF BOOKVAL CAN HANDLE              
ONEB08   SR    RF,RF                                                            
         IC    RF,=C'N'            DEFAULT SOURCE                               
         CLC   =C'IUN',0(R5)       NEED SOURCE FOR INVENTORY FILE               
         BNE   *+8                                                              
         CLI   3(R5),C'-'                                                       
         BNE   *+8                                                              
         IC    RF,4(R5)                                                         
         GOTO1 BOOKVAL,DMCB,((RF),WORK),(1,(R4))                                
         CLI   4(R1),1                                                          
         BE    ONEOK                                                            
*                                  NOW TRY FOR A DATE/NETWEEK APPROACH          
         LA    RF,ONENG                                                         
         BAS   RE,ONEMCHK                                                       
         GOTO1 DATVAL,PARAS,(R2),WORK                                           
         OC    PARAS(4),PARAS                                                   
         BZ    ONENG                                                            
         CLC   7(3,R5),=C'WTP'     WTP, CSI NAW AND BBM USE NSIWEEK             
         BE    ONEB10                                                           
         CLC   0(3,R5),=C'CSI'                                                  
         BE    ONEB10                                                           
         CLC   0(3,R5),=C'BBM'                                                  
         BNE   ONEB12                                                           
         MVI   SVBBMWK,C'Y'        BBM MUST BE MM/DD/YY(W) TO GET HERE          
         MVI   BYTE,1              SET NSIWEEK FOR MONDAY START                 
*                                                                               
ONEB10   DS    0H                                                               
         GOTO1 NSIWEEK,PARAS,WORK,(BYTE,GETDAY),ADDAY,DATCON                    
         MVI   0(R4),X'50'                                                      
         MVC   1(1,R4),PARAS+4     YEAR NO.                                     
         MVC   2(1,R4),PARAS       WEEK NO.                                     
         B     ONEOK                                                            
*                                                                               
ONEB12   GOTO1 NETWEEK,PARAS,WORK,GETDAY,ADDAY                                  
         MVI   0(R4),X'50'                                                      
         MVC   1(1,R4),PARAS+4     YEAR NO.                                     
         MVC   2(1,R4),PARAS+12    WEEK NO.                                     
         B     ONEOK                                                            
*                                                                               
ONEOK    SR    R1,R1                                                            
         B     *+8                                                              
*                                                                               
ONENG    LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     XIT2                                                             
*                                                                               
ONEMCHK  CLC   0(3,R5),=C'NAW'     WEEKLY OK FOR NAW                            
         BER   RE                                                               
         CLC   7(3,R5),=C'NAD'     MONTHLY BOOK CHECK                           
         BER   RF                                                               
         CLC   0(3,R5),=C'MPA'                                                  
         BER   RF                                                               
         CLC   0(3,R5),=C'BBM'                                                  
         BE    ONEMC04                                                          
*                                                                               
         CLC   0(3,R5),=C'CSI'     CSI WEEKLY AFTER 96                          
         BNE   ONEMC02                                                          
         CLC   HALF(2),=C'96'                                                   
         BLR   RF                                                               
         BR    RE                                                               
*                                                                               
ONEMC02  CLC   7(3,R5),=C'TP '                                                  
         BER   RF                                                               
         CLC   0(3,R5),=C'PAV'                                                  
         BER   RF                                                               
         CLC   0(3,R5),=C'IUN'                                                  
         BER   RF                                                               
         CLC   0(3,R5),=C'SRC'                                                  
         BER   RF                                                               
         BR    RE                                                               
*                                                                               
ONEMC04  ST    RE,SAVERE           SAVE RETURN ADDRESS                          
         ST    RF,SAVERF                                                        
         LA    RE,ONEB06           SEE IF FIRST TIME IN                         
         CR    RF,RE               IS THIS THE FIRST TIME IN?                   
         BER   RF                  YES, MONTHLY OK FOR BBM                      
*                                                                               
         XC    WORK,WORK           IF WE GET HERE A SECOND TIME,                
         LR    RF,R3                 IT FAILED BOOKVAL AND WE HAVE TO           
         BCTR  RF,0                   FIND BOOKTYPE OURSELVES                   
         STC   R3,WORK+45                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+48(0),0(R2)                                                 
         GOTO1 SCANNER,PARAS,WORK+40,(10,WORK),C',=,('                          
         L     RE,SAVERE                                                        
         L     RF,SAVERF                                                        
         LA    R1,WORK                                                          
         MVC   DMCB+20(1),22(R1)                                                
         CLI   DMCB+20,C'W'        IS IT WEEKLY BOOKTYPE?                       
         BNER  RF                  NO, ERROR                                    
         BR    RE                  OK, CONTINUE                                 
         EJECT                                                                  
BOOKBUMP LA    R3,42(R3)                                                        
         BCTR  R2,0                                                             
         C     R2,=F'0'                                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
         BR    RE                                                               
*                                                                               
BOOKNUM  NTR1                                                                   
         ZIC   RF,0(R3)            LENGTH OF FIELD                              
         BCTR  RF,0                                                             
         LA    RE,12(R3)           START OF FIELD                               
         CLC   0(2,RE),=C'T4'      IS IT 4 WEEK FILE                            
         BE    BKNTNUM                                                          
         AR    RE,RF               POINT TO LAST CHARACTER                      
*--CHECK FOR NUMERIC                                                            
         CLI   0(RE),C')'          POSSIBLE BOOKTYPE                            
         BE    BOOKNUM2                                                         
         CLI   0(RE),X'F0'                                                      
         BL    BKNTNUM                                                          
         CLI   0(RE),X'F9'                                                      
         BH    BKNTNUM                                                          
*                                                                               
BOOKNUM2 SR    RF,RF                                                            
         LTR   RF,RF                                                            
XIT2     XIT1                                                                   
*                                                                               
BKNTNUM  LTR   RE,RE                                                            
         B     XIT2                                                             
*                                                                               
BOOKHD   MVC   WORK+8(0),12(R3)                                                 
VVDXIT   B     XIT2                                                             
         EJECT                                                                  
***********************************************************************         
*        ERROR ROUTINES                                               *         
***********************************************************************         
*                                                                               
BADBOOK  MVC   PERROR,=AL2(INVBOOK)                                             
         B     BOOKERR                                                          
*                                                                               
BADBOOK2 MVC   PERROR,=AL2(MPABOOK)                                             
         B     BOOKERR                                                          
*                                                                               
BADSRCE  MVC   PERROR,=AL2(INVSRC)                                              
         B     BOOKERR                                                          
*                                                                               
BADPIVDT MVC   PERROR,=AL2(NOPIVD)                                              
         B     BOOKERR                                                          
*                                                                               
BOOKERR  L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLSBKH                                                       
         B     PODERR1                                                          
         DROP  R1                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RESVCTABN                                                      
         EJECT                                                                  
***********************************************************************         
*       VALIDATE OPTIONS                                              *         
***********************************************************************         
*                                                                               
VALOPXNS NMOD1 0,**VALOPXNS**,R8                                                
         L     R2,4(R1)                                                         
         L     R9,8(R1)                                                         
         L     RC,12(R1)                                                        
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BE    VALO04              ALSO SET 2ND TIME FLAG ON                    
         MVI   BOXOPT,C'Y'                                                      
         MVI   LEFTOPT,C'N'                                                     
         MVI   SPACOPT,1                                                        
         MVI   DOWNOPT,0                                                        
         MVI   DOWNOPT2,0                                                       
         MVI   DOWNCHAR,0                                                       
         MVI   PAYOPT,0                                                         
         MVI   WIDEOPT,C'N'                                                     
         MVI   TRACEOPT,C'N'                                                    
         MVI   NARROPT,C'N'                                                     
         MVI   PEROPT,C'M'                                                      
         MVI   SCNDTIME,C'N'                                                    
         MVI   TYPEOPT,0                                                        
         MVI   SCHEMOPT,0                                                       
         MVI   HUT52OPT,0                                                       
         MVI   DPTOPT,0                                                         
         MVI   WKINDS2,0                                                        
         MVI   PDDETOPT,0          DEFAULT VARIES BY REQ. DATA                  
         MVI   PDHUTTYP,0                                                       
         MVI   EXACTOPT,0                                                       
*                                                                               
         XC    DAYPOPT(15),DAYPOPT                                              
         ZAP   MAXOPT,=PL8'999999999999999'                                     
         ZAP   MINOPT,=PL1'0'                                                   
         XC    UNIVOPT,UNIVOPT                                                  
         XC    HOMEOPT,HOMEOPT                                                  
         XC    PDRNKMAX,PDRNKMAX                                                
         XC    PDMGRP,PDMGRP                                                    
         MVI   PDBSTOPT,0                                                       
         MVI   PDDUROPT,0                                                       
*                                                                               
         L     RE,APDNTIFT         CLEAR NTI/PROGRAM CODE TABLE                 
         LA    RF,L'PDNTIFT                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,APODMKTL         CLEAR MARKET LIST TABLE                      
         LA    RF,L'PODMKTL                                                     
         XCEFL                                                                  
         L     RE,APODMKTL                                                      
         MVI   0(RE),X'FF'         SET END MARKER                               
*                                                                               
         L     RE,APDAFFLT         CLEAR AFFILIATE LIST TABLE                   
         XC    0(L'PDAFFLT,RE),0(RE)                                            
         MVI   0(RE),X'FF'         SET END MARKER                               
*                                                                               
         L     RE,APDUFFLT         CLEAR UAFFILIATE LIST TABLE                  
         XC    0(L'PDUFFLT,RE),0(RE)                                            
         MVI   0(RE),X'FF'         SET END MARKER                               
*                                                                               
         L     RE,APODPRGL         CLEAR PROGRAM GROUP TABLE                    
         XC    0(L'PODPRGL,RE),0(RE)                                            
         MVI   0(RE),X'FF'         SET END MARKER                               
*                                                                               
*                                                                               
         CLC   AGENCY,=C'OM'                                                    
         BNE   *+8                                                              
         MVI   PDDUROPT,C'Y'                                                    
*                                                                               
         XC    PDSDPER,PDSDPER                                                  
         MVI   PDNUMQH,0           DEFAULT                                      
         MVI   PDWKSOPT,0          DEFAULT                                      
         MVI   PDINSEQ,0           DEFAULT                                      
         MVI   PDDAYLY,C'N'        DEFAULT                                      
         MVI   PDBASE,C'R'         DEFAULT                                      
         MVI   PDTRNKST,C'N'                                                    
         MVI   PDTRNKET,C'N'                                                    
         L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         CLC   CONWHEN(3),=C'OV,'  STOP EOD FUCKUP                              
         BNE   VALO02                                                           
         CLI   CONWHEN+5,C' '                                                   
         BH    VALO02                                                           
         MVI   CONWHEN+5,C'.'                                                   
         MVI   CONWHENH+5,6                                                     
         FOUT  CONWHENH                                                         
         DROP  R1                                                               
*                                                                               
VALO02   MVI   PDQSPILL,C'N'       SET NO SPILL                                 
         MVI   PDIUNOPT,C'Y'       DEFAULT, USE IUN                             
         CLI   PDOVSYS,3           UNLESS NET SYSTEM                            
         BNE   *+8                                                              
         MVI   PDIUNOPT,C'N'                                                    
         B     *+8                                                              
*                                                                               
VALO04   MVI   SCNDTIME,C'Y'                                                    
         CLI   5(R2),0                                                          
         BE    OPTX                                                             
         MVI   FIELDERR,1                                                       
*                                                                               
         XCEFL MYBLOCK,800                                                      
*                                                                               
         GOTO1 SCANNER,DMCB,(20,(R2)),(X'8F',MYBLOCK),0 15 LINES MAX            
         CLI   4(R1),6             USE LONG SCAN IF BUFFER CAN HANDLE           
         BH    VALO06               OTHERWISE LEAVE WHAT WE GOT                 
         MVI   OPTSCNLN,87                                                      
         GOTO1 SCANNER,DMCB,(65,(R2)),(7,MYBLOCK),0                             
         B     VALO08                                                           
*                                                                               
VALO06   MVI   OPTSCNLN,42                                                      
*                                                                               
VALO08   ZIC   R0,4(R1)                                                         
         LA    R4,MYBLOCK                                                       
         LTR   R0,R0                                                            
         BZ    BADOPT                                                           
         STC   R0,NUMLINES                                                      
*                                                                               
VALO10   CLC   12(3,R4),=C'BOX'    BOX OPTION                                   
         BNE   VALO12                                                           
         MVC   BOXOPT,22(R4)                                                    
         B     OPTEND                                                           
*                                                                               
VALO12   CLC   12(3,R4),=C'PAY'    PAY OPTION                                   
         BNE   VALO14                                                           
         MVC   PAYOPT,22(R4)                                                    
         B     OPTEND                                                           
*                                                                               
VALO14   CLC   12(4,R4),=C'LEFT'   LEFT OPTION                                  
         BNE   VALO16                                                           
         MVI   LEFTOPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
VALO16   CLC   12(2,R4),=C'S   '   SPACING OPTION                               
         BNE   VALO18                                                           
         MVC   SPACOPT,11(R4)                                                   
         CLI   SPACOPT,1                                                        
         BL    BADOPT                                                           
         CLI   SPACOPT,5                                                        
         BH    BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO18   CLC   12(4,R4),=C'DOWN'   DOWNLOADING OPTION                           
         BNE   VALO22                                                           
         OI    DOWNOPT,DOWNON                                                   
         OI    REQRTYP,REQTDOWN                                                 
         CLC   16(4,R4),=C'HEAD'   DOWNLOAD HEADLINES OPTION                    
         BNE   *+12                                                             
         OI    DOWNOPT,GLDLHEAD                                                 
         B     VALO20                                                           
         CLC   16(3,R4),=C'TOT'    DOWNLOAD TOTALS OPTION                       
         BNE   VALO20                                                           
         OI    DOWNOPT,GLDLTOTS                                                 
         MVI   DOWNCHAR,0                                                       
         NI    DOWNOPT2,255-GLDLTTXT                                            
         CLI   19(R4),C'T'                                                      
         BNE   *+8                                                              
         OI    DOWNOPT2,GLDLTTXT   DOWNLOAD TOTAL ROUTINE TEXT                  
         CLI   1(R4),0                                                          
         BNH   *+10                                                             
         MVC   DOWNCHAR,22(R4)     CHARACTER FOR DOWNLOADING TOTALS             
*                                                                               
VALO20   L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         CLI   CONOUT,C' '                                                      
         BH    OPTEND                                                           
         MVC   CONOUT(8),=CL8'DOWN'                                             
         MVI   CONOUTH+5,4                                                      
         MVC   TWAOUT,CONOUT                                                    
         FOUT  CONOUTH                                                          
         DROP  R1                                                               
         B     OPTEND                                                           
*                                                                               
VALO22   CLC   12(4,R4),=C'WIDE'   WIDE PRINTING (165)                          
         BNE   VALO24                                                           
         MVI   WIDEOPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
VALO24   CLC   12(5,R4),=C'TRACE'  TRACE OPTION                                 
         BNE   VALO26                                                           
         MVI   TRACEOPT,C'Y'                                                    
         MVI   PDQTRACE,C'Y'       +++ ALSO SET SPOTIO TRACE                    
         B     OPTEND                                                           
*                                                                               
VALO26   CLC   12(6,R4),=C'NARROW'  NARROW OPTION                               
         BNE   VALO28                                                           
         MVI   NARROPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
VALO28   CLC   12(5,R4),=C'EXACT'  EXACT DATES OPTION                           
         BNE   VALO30                                                           
         MVI   EXACTOPT,C'Y'                                                    
         B     OPTEND                                                           
*                                                                               
VALO30   CLC   12(2,R4),=C'BK'     BOOK                                         
         BNE   VALO34                                                           
         CLI   PDOVSYS,3                                                        
         BNE   VALO186             USED FOR UPGRADES                            
         CLI   15(R4),C' '                                                      
         BNE   BADOPT                                                           
         LA    R3,PDQBOOK                                                       
         CLI   14(R4),C' '                                                      
         BE    VALO32                                                           
         CLI   14(R4),C'1'                                                      
         BE    VALO32                                                           
         LA    R3,PDQBKS                                                        
         CLI   14(R4),C'2'                                                      
         BE    VALO32                                                           
         LA    R3,PDQBKS+4                                                      
         CLI   14(R4),C'3'                                                      
         BNE   BADOPT                                                           
*                                                                               
VALO32   MVC   0(4,R3),=C'ACT '                                                 
         CLC   22(4,R4),=C'ACT '                                                
         BE    OPTEND                                                           
         GOTO1 DATVAL,DMCB,(2,22(R4)),DUB                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    BADOPT                                                           
         MVC   0(4,R3),DUB                                                      
         B     OPTEND                                                           
*                                                                               
VALO34   CLC   12(5,R4),=C'MENU '  DEMO MENU                                    
         BNE   VALO36                                                           
         MVC   PDQDEMOS,22(R4)                                                  
         B     OPTEND                                                           
*                                                                               
VALO36   CLC   12(5,R4),=C'DEMO '  DEMO OPTION                                  
         BNE   VALO38                                                           
         CLC   22(3,R4),=C'TGT'                                                 
         BNE   *+12                                                             
         MVI   DEMOPT,DEMOTGT                                                   
         B     OPTEND                                                           
         CLC   22(3,R4),=C'SEC'                                                 
         BNE   *+12                                                             
         MVI   DEMOPT,DEMOSEC                                                   
         B     OPTEND                                                           
*                                                                               
         MVC   PDQDEMOS,22(R4)     *** FOR THE TIME BEING                       
         B     OPTEND              ***                                          
*                                                                               
VALO38   CLC   12(6,R4),=C'ROUND ' ROUND                                        
         BNE   VALO40                                                           
         OI    COLIND,COLIRND                                                   
         B     OPTEND                                                           
*                                                                               
VALO40   CLC   12(6,R4),=C'SPILL ' SPILL                                        
         BNE   VALO46                                                           
         CLI   1(R4),1                                                          
         BNE   BADOPT                                                           
         MVC   PDQSPILL,22(R4)                                                  
         CLI   22(R4),C'N'         NO SPILL                                     
         BE    VALO44                                                           
         CLI   22(R4),C'C'         COMBINED                                     
         BE    VALO44                                                           
         CLI   22(R4),C'S'         SEPARATE                                     
         BE    VALO44                                                           
         B     BADOPT                                                           
*                                                                               
VALO44   L     R1,APODMKTL                                                      
         CLI   0(R1),X'FF'         DO WE HAVE A MARKET LIST?                    
         BE    OPTEND              NO                                           
         XC    PODNET,PODNET       YES, CLEAR NET - REBUILD IN 01               
         B     OPTEND                                                           
*                                                                               
VALO46   CLC   12(9,R4),=C'DEMOPGRP '                                           
         BNE   VALO48                                                           
         MVI   DEMGRP,C'P'                                                      
         B     OPTEND                                                           
*                                                                               
VALO48   CLC   12(9,R4),=C'DEMROUND '   DEMO ROUNDING                           
         BNE   VALO50                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         OI    COLIND,COLINDR                                                   
         CLI   22(R4),C'N'                                                      
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO50   CLC   12(7,R4),=C'NOHEAD '     NO HEADINGS FOR DOWNLOAD                
         BNE   VALO52                                                           
         OI    DOWNOPT,DOWNNOH                                                  
         B     OPTEND                                                           
*                                                                               
VALO52   CLC   12(6,R4),=C'SCHEME'                                              
         BNE   VALO54                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BZ    BADOPT                                                           
         MVC   SCHEMOPT,22(R4)                                                  
         B     OPTEND                                                           
*                                                                               
VALO54   CLC   12(2,R4),=C'52'                                                  
         BNE   VALO56                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BNZ   *+12                                                             
         CLI   22(R3),C'Y'                                                      
         BE    BADOPT                                                           
         MVC   HUT52OPT,22(R4)                                                  
         B     OPTEND                                                           
*                                                                               
VALO56   CLC   12(4,R3),=C'TYPE'   HUT TYPE (D, A, I OR C)                      
         BNE   VALO58                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BZ    BADOPT                                                           
         MVC   TYPEOPT,22(R4)                                                   
         CLI   22(R4),C'D'         DIARY                                        
         BE    OPTEND                                                           
         CLI   22(R4),C'A'         ASCRIBED                                     
         BE    OPTEND                                                           
         CLI   22(R4),C'I'         INTEGRATED                                   
         BE    OPTEND                                                           
         CLI   22(R4),C'C'         CONFORMED                                    
         BE    OPTEND                                                           
         CLI   22(R4),C'Z'         Z-BOOK (TEST)                                
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO58   CLC   12(3,R4),=C'DPT'    DAYPART OPTION                               
         BNE   VALO60                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BZ    BADOPT                                                           
         MVC   DPTOPT,22(R4)                                                    
         B     OPTEND                                                           
*                                                                               
VALO60   CLC   12(4,R4),=C'UNIV'   UNIVERSE OPTION                              
         BNE   VALO62                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BZ    BADOPT                                                           
         OC    8(4,R4),8(R4)                                                    
         BZ    BADOPT                                                           
         MVC   UNIVOPT,8(R4)                                                    
         B     OPTEND                                                           
*                                                                               
VALO62   CLC   12(4,R4),=C'HOMES'  HOMES OPTION                                 
         BNE   VALO64                                                           
         OC    PODPSTRT,PODPSTRT   OPTION FOR PROGRAM AVG. ONLY                 
         BZ    BADOPT                                                           
         OC    8(4,R4),8(R4)                                                    
         BZ    BADOPT                                                           
         MVC   HOMEOPT,8(R4)                                                    
         B     OPTEND                                                           
*                                                                               
VALO64   CLC   12(3,R4),=C'HUT'    HUT=123.45 PERCENT ADJUST                    
         BNE   VALO66                                                           
         ZIC   R1,1(R4)            L'SECOND HALF                                
         ST    R1,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,22(R4)                                              
         CLI   DMCB,X'FF'                                                       
         BE    BADOPT                                                           
         OC    DMCB+4(2),DMCB+4                                                 
         BNZ   BADOPT                                                           
         MVC   HUTOVER,DMCB+6                                                   
         B     OPTEND                                                           
*                                                                               
VALO66   CLC   12(4,R4),=C'WEEK'   WEEK BREAKOUT                                
         BNE   VALO70                                                           
         CLI   1(R4),0                                                          
         BZ    VALO68                                                           
         CLI   16(R4),C'S'         HAS TO BE WEEKS=ALL                          
         BNE   VALO70                                                           
         CLC   =C'ALL',22(R4)                                                   
         BNE   OPTEND                                                           
         MVI   PDWKSOPT,X'FF'                                                   
         B     OPTEND                                                           
*                                                                               
VALO68   MVI   PEROPT,C'W'                                                      
         B     OPTEND                                                           
*                                                                               
VALO70   CLC   12(5,R4),=C'MONTH'  MONTH BREAKOUT                               
         BNE   *+12                                                             
         MVI   PEROPT,C'M'                                                      
         B     OPTEND                                                           
         CLC   12(7,R4),=C'QUARTER'  QUARTER BREAKOUT                           
         BNE   VALO72                                                           
         MVI   PEROPT,C'Q'                                                      
         B     OPTEND                                                           
*                                                                               
VALO72   CLC   12(7,R4),=C'DAYPART'  DAYPART OPTION                             
         BNE   VALO74                                                           
         MVI   DAYPOPT,C'Y'                                                     
         B     OPTEND                                                           
*                                                                               
VALO74   CLC   12(2,R4),=C'15'       15 MINUTE OPTION                           
         BNE   VALO76                                                           
         CLI   22(R4),C'Y'                                                      
         BNE   *+12                                                             
         MVI   FTNYOPT,C'Y'        INCLUDE 15 MINUTE PROGRAMS                   
         B     OPTEND                                                           
         CLI   22(R4),C'O'                                                      
         BNE   BADOPT                                                           
         MVI   FTNYOPT,C'O'        ONLY 15 MINUTE PROGRAMS                      
         B     OPTEND                                                           
*                                                                               
VALO76   CLC   12(4,R4),=C'SOLO'     15 MINUTE OPTION                           
         BNE   VALO78                                                           
         MVC   SOLOOPT,22(R4)                                                   
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO78   CLC   12(3,R4),=C'SEP'      SHOULD SEPTEMBER PRINT                     
         BNE   VALO80                                                           
         MVC   NSEPOPT,22(R4)                                                   
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO80   CLC   12(3,R4),=C'MAX'      MAXIMUM DEMO AMOUNT                        
         BNE   VALO82                                                           
         ZIC   R1,1(R4)            L'SECOND HALF                                
         ST    R1,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(1,22(R4))                                          
         ORG   *-2                                                              
         CLI   PRECOPT,C'Y'        USING CABLE PRECISION?                       
         BNE   *+8                 NO                                           
         MVI   0(R1),2             YES, CHANGE DECIMALS                         
         BASR  RE,RF                                                            
         CLI   DMCB,X'FF'                                                       
         BE    BADOPT                                                           
         ICM   R1,15,DMCB+4                                                     
         CVD   R1,MAXOPT                                                        
         B     OPTEND                                                           
*                                                                               
VALO82   CLC   12(3,R4),=C'MIN'      MINIMUM DEMO AMOUNT                        
         BNE   VALO84                                                           
         ZIC   R1,1(R4)            L'SECOND HALF                                
         ST    R1,DMCB+4                                                        
         GOTO1 CASHVAL,DMCB,(1,22(R4))                                          
         ORG   *-2                                                              
         CLI   PRECOPT,C'Y'        USING CABLE PRECISION?                       
         BNE   *+8                 NO                                           
         MVI   0(R1),2             YES, CHANGE DECIMALS                         
         BASR  RE,RF                                                            
         CLI   DMCB,X'FF'                                                       
         BE    BADOPT                                                           
         ICM   R1,15,DMCB+4                                                     
         CVD   R1,MINOPT                                                        
         B     OPTEND                                                           
*                                                                               
VALO84   CLC   12(3,R4),=C'TOP'      PRINT ONLY TOP X SHOWS                     
         BNE   VALO86                                                           
         TM    3(R4),X'80'                                                      
         BZ    BADOPT                                                           
         MVC   PDRNKMAX,8(R4)                                                   
         B     OPTEND                                                           
*                                                                               
VALO86   CLC   12(5,R4),=C'STACK'    STACK DEFINITION                           
         BNE   VALO88                                                           
         CLI   PDGSTACK,X'FF'      USING GENDER STACK?                          
         BE    BADOPT5             YES, ERROR                                   
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),RR=PGNR VALSTACK                   
         BNE   BADOPT                                                           
         MVI   PDSTACK,X'FF'                                                    
         GOTO1 =A(OVFLRTN),DMCB,(1,DUB),(RC),RR=PGNR VALSTDEM                   
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO88   CLC   12(4,R4),=C'PRGS'     FILTER ON NTI CODES                        
         BNE   VALO90                                                           
         GOTO1 =A(OVFLRTN),DMCB,(5,DUB),(RC),RR=PGNR VALNTINM                   
         BNE   BADPRGS                                                          
         B     OPTEND                                                           
*                                                                               
VALO90   CLC   12(4,R4),=C'XBOX'     EXTRA BOX AFTER ROWS                       
         BNE   VALO92                                                           
         OI    WKINDS2,X'40'                                                    
         B     OPTEND                                                           
*                                                                               
VALO92   CLC   12(4,R4),=C'BOOK'       BOOK OPTION                              
         BE    VALO184                                                          
         CLC   12(2,R4),=C'BA'     OTHER OPTIONS START WITH B                   
         BE    VALO94                                                           
         CLC   12(2,R4),=C'BO'         BREAKOUT OPTION                          
         BE    *+10                                                             
         CLC   12(1,R4),=C'B'          BREAKOUT OPTION                          
         BNE   VALO94                                                           
         MVC   BRKOOPT,22(R4)                                                   
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO94   CLC   12(3,R4),=C'PRE'      CABLE PRECISSION                           
         BNE   VALO98                                                           
         CLC   12(4,R4),=C'PREM'     PREMIERE OPTIONS                           
         BE    VALO96                                                           
         MVI   PRECOPT,C'Y'                                                     
         CLC   22(3,R4),=CL3'CAB'  CABLE PRECISION?                             
         BNE   VALO95              NO                                           
         CP    MINOPT,=P'0'        YES, ERROR IF MIN/MAX ALREADY SET            
         BNE   MINERR                                                           
         CP    MAXOPT,=PL8'999999999999999'                                     
         BNE   MINERR                                                           
         B     OPTEND                                                           
*                                                                               
VALO95   CLC   =C'NO',22(R4)                                                    
         BNE   BADOPT                                                           
         MVI   PRECOPT,C'N'                                                     
         B     OPTEND                                                           
*                                                                               
VALO96   MVI   PDPRMOPT,C'Y'                                                    
         CLC   12(5,R4),=C'PREM+'                                               
         BNE   *+8                                                              
         MVI   PDPRMOPT,C'+'                                                    
         B     OPTEND                                                           
*                                                                               
VALO98   CLC   12(3,R4),=C'VAR'      VARIOUS DAYS OPTION                        
         BNE   VALO100                                                          
         MVI   VAROPT,C'Y'                                                      
         B     OPTEND                                                           
*                                                                               
VALO100  CLC   12(3,R4),=C'DAY'      BREAKOUT OF M-F, AND M-S DAYS              
         BNE   VALO102                                                          
         MVC   DAYSOPT(1),22(R4)                                                
         CLI   22(R4),C'I'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO102  CLC   12(5,R4),=C'HHHUT'    BREAKOUT HUTS BY HALF HOUR                 
         BNE   VALO104                                                          
         MVI   HHHUT,C'Y'                                                       
         B     OPTEND                                                           
*                                                                               
VALO104  CLC   =C'MRKT',12(R4)       LOOK FOR MARKET                            
         BNE   VALO122                                                          
         CLI   1(R4),0                                                          
         BE    BADOPT                                                           
         L     R5,APODMKTL         ADDRESS MARKET LIST                          
         MVC   0(1,R4),1(R4)       SHIFT DATA FIRST TIME IN                     
         MVC   2(1,R4),3(R4)                                                    
         MVC   4(4,R4),8(R4)                                                    
         MVC   12(10,R4),22(R4)                                                 
*                                                                               
VALO106  TM    2(R4),X'80'         2ND HALF IS VALID NUMERIC                    
         BO    VALO108             GET MARKET NUMBER                            
         TM    2(R4),X'40'         2ND HALF IS VALID ALPHA                      
         BO    VALO114             GO LOOK FOR ALPHA MARKET                     
         B     BADOPT              BAD MARKET                                   
*                                                                               
VALO108  ICM   R1,15,4(R4)         BINARY VALUE OF MARKET=???                   
         BZ    BADOPT                                                           
         STCM  R1,3,0(R5)                                                       
         CLI   PODBEXT+4,C'A'      EMI-A/NMI-A USES ARB MPA                     
         BE    VALO110                                                          
         CLI   PODBSRC,C'N'                                                     
         BNE   VALO110                                                          
         AH    R1,=H'400'                                                       
         STCM  R1,3,0(R5)                                                       
*                                                                               
VALO110  LA    R5,2(R5)            GET NEXT SPOT IN LIST                        
         MVI   0(R5),X'FF'         MARK IN CASE LAST                            
*                                                                               
         L     RF,APODMKTL                                                      
         LA    RF,L'PODMKTL(RF)    A(END OF MARKET LIST)                        
         CR    R5,RF                                                            
         BNL   BADOPT3             ERROR IF PAST END                            
*                                                                               
         ZIC   RE,OPTSCNLN                                                      
         AR    R4,RE                                                            
         AI    FIELDERR,1                                                       
         ZIC   RE,NUMLINES                                                      
         BCTR  RE,0                                                             
         STC   RE,NUMLINES                                                      
         LTR   RE,RE               ANY FIELDS LEFT?                             
         BZ    VALO112             NO, BACK IT UP                               
         CLI   1(R4),0             IS THERE A SECOND FIELD?                     
         BE    VALO106             NO, WE'RE STILL IN MRKT                      
*                                                                               
VALO112  ZIC   RE,NUMLINES                                                      
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
         ZIC   RE,OPTSCNLN                                                      
         SR    R4,RE                                                            
         B     OPTEND                                                           
*                                                                               
*--------------------------------------------------------------                 
* CHECK IF THE ALPHA MARKET EXIST IN THE CONTROL FILE                           
*--------------------------------------------------------------                 
*                                                                               
VALO114  XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CTDMREC,R1                                                       
         MVI   CTDMKTYP,CTDMKTEQ   SET UP KEY                                   
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVI   CTDMKMED,C'T'       MEDIA                                        
         CLI   PODBMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   CTDMKMED,C'C'       MEDIA                                        
         MVC   CTDMKSRC,PODBSRC    SOURCE (A/N)                                 
         CLI   PODBEXT+4,C'A'      EMI-A/NMI-A USES ARBITRON MPA                
         BNE   *+8                                                              
         MVI   CTDMKSRC,C'A'                                                    
         MVC   CTDMKMKT,12(R4)     MARKET NAME                                  
         MVC   CTDMKBKT,PODBBTY                                                 
         CLI   CTDMKBKT,0                                                       
         BNE   *+10                                                             
         MVC   CTDMKBKT,PODNET+5   BOOK TYPE                                    
         CLI   CTDMKBKT,C'A'       DEFAULT FOR NON-SPOT                         
         BE    VALO116                                                          
         CLI   CTDMKBKT,C'S'       DEFAULT FOR SQAD                             
         BE    VALO116                                                          
         CLI   CTDMKBKT,0          DEFAULT FOR SPOT                             
         BNE   *+8                                                              
*                                                                               
VALO116  MVI   CTDMKBKT,X'FF'      DEFAULT BOOK TYPE                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI  '),=C'CTFILE  ',KEY,KEY,0             
         CLC   KEY+1(22),KEYSAVE+1 CAN'T FIND IT? 1ST BYTE DESTROYED            
         BNE   BADOPT              BAD ONE                                      
         LA    R1,KEY              RELOAD R1                                    
         MVC   0(2,R5),CTDMKNUM    GOT IT, SAVE MARKET NUMBER                   
         CLI   PODBEXT+4,C'A'      EMI-A/NMI-A USES ARB MPA                     
         BE    VALO110                                                          
         CLI   PODBSRC,C'N'        NSI MARKET NEEDS ADJUSTMENT                  
         BNE   VALO110                                                          
         SR    R1,R1                                                            
         ICM   R1,3,0(R5)                                                       
         AH    R1,=H'400'                                                       
         STCM  R1,3,0(R5)                                                       
         B     VALO110                                                          
         DROP  R1                                                               
*                                                                               
VALO122  CLC   =C'MPA',12(R4)      LOOK FOR INDEX                               
         BNE   VALO146                                                          
         CLI   1(R4),0                                                          
         BE    OPTEND                                                           
         TM    3(R4),X'80'         NUMERIC INPUT?                               
         BO    VALO142             YES, SAVE THE INDEX                          
         TM    3(R4),X'40'         IF NOT ALPHA THEN                            
         BNO   BADPRGS             IT IS A BAD PROGRAM                          
         L     R1,=F'9600'                                                      
         LR    R3,R4               SAVE BLOCK ADDR                              
         CLI   22(R4),C'C'         IS IT A CLEARED PROGRAM?                     
         BNE   VALO124                                                          
         LA    R3,1(R4)            GET RID OF THE 'C'                           
         B     VALO126                                                          
*                                                                               
VALO124  LA    R1,100(R1)          NOT, HUNDREDS PLACE IS 7                     
*                                                                               
VALO126  CLC   =C'PRIME',22(R3)                                                 
         BNE   VALO128                                                          
         LA    R1,1(R1)                                                         
         B     VALO132                                                          
*                                                                               
VALO128  CLC   =C'DAY',22(R3)                                                   
         BNE   VALO130                                                          
         LA    R1,2(R1)                                                         
         B     VALO132                                                          
*                                                                               
VALO130  CLC   =C'KIDS',22(R3)                                                  
         BNE   BADPRGS             BAD PROGRAM                                  
         LA    R1,3(R1)                                                         
*                                                                               
VALO132  CLC   =C'ABC',PODNET                                                   
         BNE   VALO134                                                          
         LA    R1,10(R1)                                                        
         B     VALO144                                                          
*                                                                               
VALO134  CLC   =C'CBS',PODNET                                                   
         BNE   VALO136                                                          
         LA    R1,20(R1)                                                        
         B     VALO144                                                          
*                                                                               
VALO136  CLC   =C'NBC',PODNET                                                   
         BNE   VALO138                                                          
         LA    R1,30(R1)                                                        
         B     VALO144                                                          
*                                                                               
VALO138  CLC   =C'FOX',PODNET                                                   
         BNE   VALO140                                                          
         LA    R1,40(R1)                                                        
         B     VALO144                                                          
*                                                                               
VALO140  CLC   =C'ZZZ',PODNET                                                   
         BNE   BADPRGS                                                          
         LA    R1,50(R1)                                                        
         B     VALO144                                                          
*                                                                               
VALO142  ICM   R1,15,8(R4)         BINARY VALUE OF INDEX=???                    
*                                                                               
VALO144  STCM  R1,3,MPAINDEX                                                    
         B     OPTEND                                                           
*                                                                               
VALO146  CLC   =C'PAV',12(R4)      PAV=ALL, BEST, LAYOUT                        
         BNE   VALO150                                                          
         CLI   22(R4),C'A'                                                      
         BE    VALO148                                                          
         CLI   22(R4),C'B'                                                      
         BE    VALO148                                                          
         CLI   22(R4),C'L'                                                      
         BNE   BADOPT                                                           
*                                                                               
VALO148  MVC   PDBSTOPT,22(R4)     SET OPTION FOR DBBEST                        
         B     OPTEND                                                           
*                                                                               
VALO150  CLC   =C'DUR',12(R4)      DURATION = PROGRAM/TIME                      
         BNE   VALO158                                                          
         CLI   22(R4),C'-'                                                      
         BE    VALO152                                                          
         CLI   23(R4),C'-'                                                      
         BE    VALO154                                                          
         CLI   22(R4),C'P'         PROGRAM = Y                                  
         BE    VALO156                                                          
         CLI   22(R4),C'T'         TIME    = X'00'                              
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO152  MVI   PDTRNKST,C'Y'                                                    
         CLI   24(R4),C'-'                                                      
         BNE   VALO156                                                          
*                                                                               
VALO154  MVI   PDTRNKET,C'Y'                                                    
*                                                                               
VALO156  MVI   PDDUROPT,C'Y'       SET OPTION FOR PROGRAM DURATION              
         B     OPTEND                                                           
*                                                                               
VALO158  CLC   =C'QH',12(R4)       QH=(1/2)                                     
         BNE   VALO160                                                          
         TM    3(R4),X'80'         HAS TO BE A NUMBER                           
         BO    *+8                                                              
         B     BADOPT                                                           
         MVC   PDNUMQH,11(R4)                                                   
         B     OPTEND                                                           
*                                                                               
VALO160  CLC   =C'SID',12(R4)      NEW SID, SCHEME                              
         BNE   VALO168                                                          
         CLI   PDOVSYS,3         NET SYSTEM CAN'T USE SIDS                      
         BE    BADSYS                                                           
         CLI   PDSIDOPT,C'X'                                                    
         BE    BADSYS                                                           
         CLI   PDOVSYS,8                                                        
         BNE   VALO164                                                          
         CLI   OFFLINE,C'Y'                                                     
         BNE   VALO162                                                          
         ICM   RF,15,PDAUTL                                                     
         MVC   4(1,RF),PDSSENUM    OFFLINE SWITCH                               
         B     VALO164                                                          
*                                                                               
VALO162  GOTO1 SWITCH,DMCB,(0,=C'SPT'),0    SPOT SYS HAS SID RECS               
         CLI   4(R1),0                      HAVE TO BE ABLE TO SWITCH           
         BZ    VALO164                                                          
         DC    H'0'                                                             
*                                                                               
VALO164  MVC   PDSDSCHM,=C'ALL'                                                 
         CLI   1(R4),0                                                          
         BE    BADSID                                                           
         CLI   1(R4),2                                                          
         BL    BADSID                                                           
         CLI   1(R4),3                                                          
         BH    BADSID                                                           
         MVC   PDSDSCHM,22(R4)                                                  
* MAKE SURE SCHEME EXISTS                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),PDBAGYMD                                                
         CLC   =C'ALL',PDSDSCHM                                                 
         BE    VALO166                                                          
         GOTO1 CLPACK,DMCB,PDSDSCHM,KEY+2                                       
*                                                                               
VALO166  GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADSID                                                           
         MVC   PDSDDSKA,KEY+14     SAVE DISK ADDRESS                            
         B     OPTEND                                                           
*                                                                               
* VALIDATE SID PERIOD                                                           
VALO168  CLC   =C'PER',12(R4)                                                   
         BNE   VALO184                                                          
         OC    PDSDDSKA,PDSDDSKA   GOT TO HAVE SID                              
         BZ    BADSID                                                           
*                                                                               
         LA    R0,5                                                             
         LA    R1,22(R4)           POINT TO PERIOD                              
*                                                                               
VALO170  CLI   0(R1),C' '                                                       
         BE    VALO172                                                          
         CLI   0(R1),C'/'                                                       
         BE    VALO172                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VALO170                                                       
         B     PERERR                                                           
*                                                                               
VALO172  LA    RE,5                                                             
         SR    RE,R0               GIVES NUM CHARS PRESENT FOR PERIOD           
         BZ    PERERR                                                           
         ST    RE,FULL                                                          
         CH    RE,=H'4'                                                         
         BH    PERERR                                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PDSDPER(0),22(R4)                                                
         OC    PDSDPER,=CL4'    '                                               
*                                                                               
         CLI   0(R1),C'/'          TEST YEAR PRESENT                            
         BNE   VALO176                                                          
*                                                                               
         ZIC   R0,1(R4)                                                         
         S     R0,FULL                                                          
         BCTR  R0,0                ADJUST FOR /                                 
         LA    R1,1(R1)            POINT TO FIRST DIGIT OF YEAR                 
         ST    R1,FULL             SAVE ADDRESS                                 
         CH    R0,=H'2'                                                         
         BNE   PERERR                                                           
*                                                                               
VALO174  CLI   0(R1),C'0'                                                       
         BL    PERERR                                                           
         CLI   0(R1),C'9'                                                       
         BH    PERERR                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VALO174                                                       
*                                                                               
         L     RE,FULL                                                          
         PACK  DUB,0(2,RE)                                                      
         CVB   R0,DUB                                                           
         STC   R0,PDSDYEAR                                                      
*                                                                               
* VALIDATE PERIOD NAME *                                                        
*                                                                               
VALO176  XC    KEY,KEY                                                          
         MVC   KEY+14(4),PDSDDSKA                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R5,AIO                                                           
         MVI   ELCODE,EPNCODEQ     FIND PERIOD NAME ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   PERERR                                                           
*                                                                               
         USING EPNELEM,R5                                                       
         ZIC   R0,EPNLEN                                                        
         SH    R0,=H'2'                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LR    R0,R1                                                            
         LA    R1,EPNDATA                                                       
*                                                                               
VALO178  CLC   PDSDPER,1(R1)                                                    
         BE    VALO180                                                          
         LA    R1,5(R1)                                                         
         BCT   R0,VALO178                                                       
         B     PERERR                                                           
*                                                                               
VALO180  MVI   PDSIDOPT,C'Y'                                                    
         CLI   PDOVSYS,8                                                        
         BNE   OPTEND                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   VALO182                                                          
         ICM   RF,15,PDAUTL                                                     
         MVC   4(1,RF),PDSYSTEM                                                 
         B     OPTEND                                                           
*                                                                               
VALO182  GOTO1 SWITCH,DMCB,(PDSYSTEM,0),0                                       
         CLI   4(R1),0                                                          
         BZ    OPTEND                                                           
         DC    H'0'                                                             
*                                                                               
VALO184  CLC   12(4,R4),=C'BOOK'   BOOK FOR UPGRADE, WE TAKE 'BK' TOO           
         BNE   VALO188                                                          
*                                                                               
VALO186  XC    WORK,WORK                                                        
         MVC   WORK+5(1),1(R4)                                                  
         MVC   WORK+8(10),22(R4)                                                
         LA    R5,PDBKOPT                                                       
         GOTO1 BOOKVAL,DMCB,(PODBSRC,WORK),(1,(R5)),(C'S',SCANNER)              
         CLI   4(R1),1                                                          
         BNE   BADOPT                                                           
         B     OPTEND                                                           
*                                                                               
VALO188  CLC   12(3,R4),=C'PUT'    PUT FOR UPGRADE                              
         BE    VALO190                                                          
         CLC   12(3,R4),=C'RTG'    RTG FOR UPGRADE                              
         BE    VALO190                                                          
         CLC   12(3,R4),=C'SHR'    RTG FOR UPGRADE                              
         BNE   VALO192                                                          
*                                                                               
VALO190  MVI   PDPUTOPT,C'N'                                                    
         CLC   8(4,R4),=F'1'                                                    
         BE    OPTEND                                                           
         MVI   PDPUTOPT,C'Y'                                                    
         CLC   8(4,R4),=F'2'                                                    
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO192  CLC   12(2,R4),=C'UP..'   UPGRADE                                      
         BNE   VALO198                                                          
         L     RE,APODBKL                                                       
*                                                                               
VALO193  CLC   1(3,RE),=C'PAV'     UPGRADE INVALID FOR PAV                      
         BE    BADOPT7                                                          
         LA    RE,PODBLNQ(RE)                                                   
         CLI   0(RE),X'FF'                                                      
         BNE   VALO193                                                          
*                                                                               
         MVI   PDDPOPT,C'Y'        FORCE ON DAYPART OPTION                      
         MVI   PDUPFILE,C'T'       DEFAULT IS TP                                
         CLI   14(R4),C' '                                                      
         BE    *+10                                                             
         MVC   PDUPFILE,14(R4)                                                  
         MVC   PDUPOPT(3),22(R4)                                                
         CLC   PDUPOPT(3),=C'SID'  OPTION TO COME FROM SID                      
         BNE   VALO194                                                          
         MVI   PDSUPOPT,C'Y'                                                    
         MVI   PDSIDOPT,C'Y'                                                    
         B     VALO196                                                          
*                                                                               
VALO194  XC    WORK,WORK                                                        
         MVC   WORK+5(1),1(R4)                                                  
         MVC   WORK+8(20),22(R4)                                                
         GOTO1 UPVAL,DMCB,WORK,PDUPOPT,(C'/',ACOMFACS)                          
         CLI   0(R1),0                                                          
         BE    BADOPT                                                           
*                                                                               
VALO196  OC    PDAESTBK,PDAESTBK     MUST BE ESTIMATED BOOK                     
         BNZ   OPTEND                                                           
         B     BADEBK                                                           
*                                                                               
VALO198  CLC   =C'NOIUN',12(R4)    TURN IUN CALCULATION OFF                     
         BNE   VALO200                                                          
         MVI   PDIUNOPT,C'N'                                                    
         B     OPTEND                                                           
*                                                                               
VALO200  CLC   12(4,R4),=C'DETAIL'   SUPPRESS/ACTIVATE DETAIL LINES             
         BNE   VALO202                FOR SPOT/REP LOOKUPS                      
         MVC   PDDETOPT,22(R4)                                                  
         CLI   22(R4),C'N'                                                      
         BE    OPTEND                                                           
         CLI   22(R4),C'Y'                                                      
         BE    OPTEND                                                           
         B     BADOPT                                                           
*                                                                               
VALO202  CLC   12(3,R4),=C'NHTI'      OPTIONAL HISPANIC HUT SURVEY              
         BNE   VALO204                FOR GET HUT(NET ONLY)                     
         MVI   PDHUTTYP,C'H'                                                    
         B     OPTEND                                                           
*                                                                               
VALO204  CLC   12(5,R4),=C'-SPOR'                                               
         BNE   VALO206                                                          
         MVI   PDSPTOPT,C'N'                                                    
         B     OPTEND                                                           
*                                                                               
VALO206  CLC   12(5,R4),=C'SPORT'                                               
         BNE   VALO208                                                          
         MVI   PDSPTOPT,C'Y'                                                    
         B     OPTEND                                                           
*                                                                               
VALO208  CLC   =C'TELECAST',12(R4)  CABLE TELECAST OPTION                       
         BE    *+10                                                             
         CLC   =C'TCAST',12(R4)                                                 
         BNE   VALO212                                                          
         CLI   22(R4),C'Y'                                                      
         BE    VALO210                                                          
         B     BADOPT                                                           
*                                                                               
VALO210  MVI   PDBSTOPT,C'A'       SET OPTION FOR DBBEST                        
         B     OPTEND                                                           
*                                                                               
VALO212  CLC   =C'DMA',12(R4)      DMA RPS CALC OPTION                          
         BNE   VALO216                                                          
         CLI   22(R4),C'R'                                                      
         BE    VALO214                                                          
         CLI   22(R4),C'I'                                                      
         BE    VALO214                                                          
         B     BADOPT                                                           
*                                                                               
VALO214  MVC   DMAOPT,22(R4)       SET OPTION FOR RPS CALC                      
         B     OPTEND                                                           
*                                                                               
VALO216  CLC   =C'INSEQ',12(R4)                                                 
         BNE   VALO222                                                          
         CLI   22(R4),C'S'                                                      
         BE    VALO218                                                          
         CLI   22(R4),C'B'                                                      
         BE    VALO220                                                          
         B     BADOPT                                                           
*                                                                               
VALO218  OI    PDINSEQ,PDSEQSTA    SET SEQUENCING OPTIONS                       
         B     OPTEND                                                           
*                                                                               
VALO220  OI    PDINSEQ,PDSEQBK     SET SEQUENCING OPTIONS                       
         B     OPTEND                                                           
*                                                                               
VALO222  CLC   12(3,R4),=C'CALENDAR' SPECIAL CALENDAR ROUTINES                  
         BNE   VALO226                                                          
         CLC   22(3,R4),=C'NSI'                                                 
         BNE   VALO224                                                          
         MVI   PDCALOPT,PDCALNSI                                                
         B     OPTEND                                                           
*                                                                               
VALO224  CLC   22(3,R4),=C'NTI'                                                 
         BNE   BADOPT                                                           
         MVI   PDCALOPT,PDCALNTI                                                
         B     OPTEND                                                           
*                                    AVERAGE BASE RTG/IMP/BOOK                  
VALO226  CLC   12(4,R4),=C'BASE'                                                
         BNE   VALO228                                                          
         L     RE,APODBKL                                                       
         CLC   1(3,RE),=C'NTI'     ONLY FOR NTI                                 
         BNE   BADOPT                                                           
         CLC   22(3,R4),=C'BOO'                                                 
         BNE   *+12                                                             
         MVI   PDBASE,C'B'                                                      
         B     OPTEND                                                           
         CLC   22(3,R4),=C'IMP'                                                 
         BNE   BADOPT                                                           
         MVI   PDBASE,C'I'                                                      
         B     OPTEND                                                           
*                                                                               
VALO228  CLC   12(4,R4),=C'MASTER' MASTER STATION FOR DAYS/TIMES                
         BNE   VALO230                                                          
         MVC   PDINVSTA,22(R4)                                                  
         OC    PDINVSTA(4),=C'    '                                             
         B     OPTEND                                                           
*                                                                               
VALO230  CLC   =C'MRKGRP',12(R4)   IS IT NON-SID MARKET GROUP?                  
         BNE   VALO236             NO, ERROR                                    
         OC    PDMGRP,PDMGRP       YES, ERROR IF ALREADY BEEN HERE              
         BNZ   BADOPT2                                                          
         TM    23(R4),C'0'         2ND CHARACTER MUST BE NUMERIC                
         BNO   BADOPT                                                           
         CLI   22(R4),C'G'         1ST MUST BE G-Z                              
         BL    BADOPT                                                           
         CLI   22(R4),C'Z'                                                      
         BH    BADOPT                                                           
         LA    RE,23(R4)           GET FIRST NUMBER                             
         SR    R1,R1                                                            
         IC    R1,1(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    VALO234                                                          
*                                                                               
VALO232  TM    0(RE),C'0'          MUST BE NUMERIC                              
         BNO   BADOPT                                                           
         LA    RE,1(RE)                                                         
         BCT   R1,VALO232                                                       
*                                                                               
VALO234  MVC   PDMGRP,22(R4)                                                    
         B     OPTEND                                                           
*                                                                               
VALO236  CLC   12(5,R4),=C'AFFILIATE'   LOOK FOR AFFILATE                       
         BNE   VALO238                                                          
         CLI   1(R4),0                                                          
         BE    BADOPT                                                           
*                                                                               
         L     R5,APDUFFLT         CHECK UAFFILIATE                             
         CLI   0(R5),X'FF'                                                      
         BNE   BADOPT4             CAN'T HAVE BOTH                              
*                                                                               
         L     R5,APDAFFLT         ADDRESS AFFILATE LIST                        
         LR    RF,R5                                                            
         LA    RF,L'PDAFFLT(RF)    SAVE END ADDRESS                             
         B     VALO240                                                          
*                                                                               
VALO238  CLC   12(6,R4),=C'UAFFILIATE'                                          
         BNE   VALO246                                                          
         CLI   1(R4),0                                                          
         BE    BADOPT                                                           
*                                                                               
         L     R5,APDAFFLT         CHECK AFFILIATE                              
         CLI   0(R5),X'FF'                                                      
         BNE   BADOPT4             CAN'T HAVE BOTH                              
*                                                                               
         L     R5,APDUFFLT         ADDRESS UAFFILIATE LIST                      
         LR    RF,R5                                                            
         LA    RF,L'PDUFFLT(RF)    SAVE END ADDRESS                             
*                                                                               
VALO240  MVC   0(1,R4),1(R4)       SHIFT DATA FIRST TIME IN                     
         MVC   2(1,R4),3(R4)                                                    
         MVC   4(4,R4),8(R4)                                                    
         MVC   12(10,R4),22(R4)                                                 
*                                                                               
VALO242  MVC   0(5,R5),12(R4)      MOVE ENTRY TO TABLE                          
         LA    R5,5(R5)            GET NEXT SPOT IN LIST                        
         MVI   0(R5),X'FF'         MARK IN CASE LAST                            
*                                                                               
         CR    R5,RF                                                            
         BNL   BADOPT3             ERROR IF PAST END                            
*                                                                               
         ZIC   RE,OPTSCNLN                                                      
         AR    R4,RE                                                            
         AI    FIELDERR,1                                                       
         ZIC   RE,NUMLINES                                                      
         BCTR  RE,0                                                             
         STC   RE,NUMLINES                                                      
         LTR   RE,RE               ANY FIELDS LEFT?                             
         BZ    VALO244             NO, BACK IT UP                               
         CLI   1(R4),0             IS THERE A SECOND FIELD?                     
         BE    VALO242             NO, WE'RE STILL IN AFFL                      
*                                                                               
VALO244  ZIC   RE,NUMLINES                                                      
         LA    RE,1(RE)                                                         
         STC   RE,NUMLINES                                                      
         ZIC   RE,OPTSCNLN                                                      
         SR    R4,RE                                                            
         B     OPTEND                                                           
*                                                                               
VALO246  CLC   12(6,R4),=C'GSTACK'   GENDER STACKING OPTION                     
         BNE   VALO248                                                          
         CLI   PDSTACK,X'FF'       USING DEMO STACK?                            
         BE    BADOPT5             YES, ERROR                                   
         GOTO1 =A(OVFLRTN),DMCB,(7,DUB),(RC),RR=PGNR VALGSTCK                   
         BNE   BADOPT                                                           
         MVI   PDGSTACK,X'FF'      INDICATE TYPE OF STACKING                    
         BAS   RE,ADDGEN                                                        
         BE    OPTEND              OK                                           
         L     R1,ATWA                                                          
         USING T325FFD,R1                                                       
         LA    R2,SPLDEMH          ERROR, POINT TO DEMO FIELD                   
         B     BADDEM                                                           
*                                                                               
VALO248  CLC   12(6,R4),=C'PRGGRP'   PROGRAM GROUP OPTION                       
         BNE   BADOPT                                                           
         CLI   1(R4),0                                                          
         BE    BADOPT                                                           
         L     RE,APDNTIFT                                                      
         CLI   0(RE),0             PROGS OPTION IS CONFLICTING                  
         BNE   BADOPT6                                                          
         GOTO1 =A(OVFLRTN),DMCB,(8,DUB),(RC),RR=PGNR VALPRGRP                   
         BNE   BADOPT                                                           
*                                                                               
OPTEND   ZIC   RE,OPTSCNLN                                                      
         AR    R4,RE                                                            
         AI    FIELDERR,1                                                       
         ZIC   RE,NUMLINES                                                      
         BCTR  RE,0                                                             
         STC   RE,NUMLINES                                                      
         LTR   RE,RE                                                            
         BNZ   VALO10                                                           
*                                                                               
OPTEND5  CLI   DEMOPT,DEMOTGT      TEST DEMO OPTION SET TO TGT/SEC              
         BE    *+12                                                             
         CLI   DEMOPT,DEMOSEC                                                   
         BNE   OPTX                                                             
         OC    PDQDEMOS,PDQDEMOS   YES - TEST DEMO MENU                         
         BZ    BADMEN                                                           
*                                                                               
* MUST HAVE A STATION BY NOW                                                    
OPTX     CLI   PODNET,0            DO WE HAVE A STATION?                        
         BNE   OPTX2               YES                                          
         L     RE,APODMKTL                                                      
         CLI   0(RE),X'FF'         NO, DO WE HAVE A MARKET LIST?                
         BNE   OPTX2               YES                                          
         OC    PDMGRP,PDMGRP       NO, DO WE HAVE A MARKET GROUP?               
         BNZ   OPTX2               YES                                          
         CLI   SCNDTIME,C'Y'       NO, IS THIS 2ND PASS?                        
         BE    BADSTA              YES, ERROR                                   
*                                                                               
OPTX2    OC    PDAESTBK,PDAESTBK   ESTIMATE BOOK USED?                          
         BZ    OPTX4                                                            
         CLI   SCNDTIME,C'Y'       LET US VALIDATE BOTH OPTIONS                 
         BNE   OPTX4                                                            
         OC    PDUPOPT,PDUPOPT     EST BOOKS NEED UPGRADE                       
         BZ    BADUPG                                                           
*                                                                               
OPTX4    L     R5,APODMKTL                                                      
         CLI   0(R5),X'FF'         USING MARKET LIST?                           
         BE    OPTXIT              NO                                           
         OC    PDMGRP,PDMGRP       YES, USING MARKET GROUP ALSO?                
         BNZ   OPTCON2             YES, ERROR                                   
*                                                                               
OPTXIT   XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADDITIONAL VALDEMO ROUTINE                                   *         
***********************************************************************         
*                                                                               
*        CREATE NEW LIST OF DEMOS WITH STACKED GENDER                           
*                                                                               
ADDGEN   NTR1                                                                   
         XCEFL BLOCK,480                                                        
*                                                                               
         L     R5,PDADEMTB                                                      
         USING PDDMBUFF,R5                                                      
         LA    RF,DBLOCKA                                                       
         ST    RF,DMCB+8                                                        
         USING DBLOCKD,RF                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'PAV'                                                   
         MVC   DBSELMED,PODBMED                                                 
         MVI   DBSELMED,C'N'       DEFAULT NETWORK                              
         CLI   PDOVSYS,3           NET USES 4 BYTE DEMOS                        
         BNE   *+8                                                              
         MVI   DBDEMTYP,C'4'                                                    
         CLI   PDOVSYS,2                                                        
         BNE   ADDG02                                                           
         MVI   DBSELMED,C'T'                                                    
         MVI   DMCB+8,C'S'                                                      
         CLI   PODBMED,C'C'                                                     
         BNE   ADDG02                                                           
         MVI   DBSELMED,C'C'                                                    
         MVC   DBFILE,=C'TP '                                                   
*                                                                               
ADDG02   GOTO1 DEMOCON,DMCB,(ACTUAL,PODDEMWK),(5,BLOCK),,0                      
         DROP  R5,RF                                                            
*                                                                               
         L     RE,APDSTSCN         CLEAR SCREEN BUILD AREA                      
         LA    RF,L'PDSTSCN                                                     
         XCEFL                                                                  
*                                                                               
         L     RE,APDSTDEM         CLEAR DEMOS SAVE AREA                        
         LA    RF,L'PDSTDEM                                                     
         XCEFL                                                                  
*                                                                               
         ZIC   R0,ACTUAL           NUMBER OF ENTRIES                            
*                                                                               
         L     RE,APDSTDEM         SAVE ADDRESS OF DEMO OUTPUT                  
         ST    RE,STDMPTR                                                       
*                                                                               
         L     R3,APDSTSCN         BUILD NEW SCREEN HERE                        
         LA    R3,8(R3)                                                         
*                                                                               
         LA    R4,BLOCK            OUTPUT FROM DEMCON                           
         MVI   FIELDERR,0                                                       
         SR    RF,RF               CLEAR DEMO COUNTER                           
*                                                                               
ADDG04   LA    R2,PDSTADEF         GET STACK OF GENDERS                         
         AI    FIELDERR,1                                                       
*                                                                               
ADDG06   CLI   0(R2),X'FF'         END OF LIST?                                 
         BE    ADDG08              YES, GET NEXT BLOCK ENTRY                    
         LA    R5,MODTAB           GET MODIFIER TABLE                           
*                                                                               
ADDG07   CLI   0(R5),X'FF'         END OF MODIFIER TABLE                        
         BE    ADDG07A                                                          
         CLC   0(5,R5),5(R4)       FIND ENTRY IN TABLE                          
         BE    *+12                                                             
         LA    R5,1(R5)                                                         
         B     ADDG07                                                           
         MVC   0(1,R3),5(R5)       FOUND IT, MOVE IN CODE                       
         CLI   0(R3),C' '          DID WE MOVE ANYTHING?                        
         BNH   *+8                 NO                                           
         LA    R3,1(R3)            BUMP                                         
*                                                                               
ADDG07A  MVC   0(5,R3),0(R4)       MOVE FROM BLOCK TO PDSTSCN                   
         MVC   0(1,R3),1(R2)       REPLACE GENDER                               
*                                                                               
         LA    R1,5-1(R3)          GET TO LAST BYTE                             
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         SR    R1,R3               GET LENGTH OF DATA                           
         LA    R1,1(R1)                                                         
         LA    R3,0(R1,R3)         BUMP TO END                                  
         MVI   0(R3),C','          PUT IN A COMMA                               
         LA    R3,1(R3)            NEXT IN PDSTSCN                              
         LA    R2,2(R2)            NEXT IN PDSTADEF                             
         LA    RF,1(RF)            INCREASE DEMO COUNT                          
*                                                                               
         CHI   RF,24               24 IS MAX DEMOVAL CAN HANDLE                 
         BNE   ADDG06                                                           
*                                                                               
         ST    R2,SAVER2           SAVE PLACE IN PDSTADEF                       
         ST    R4,SAVER4           SAVE PLACE IN BLOCK                          
         ST    R0,SAVER0           SAVE REMAINING DEMOS                         
         B     ADDG09                                                           
*                                                                               
ADDG08   LA    R4,10(R4)                                                        
         BCTR  R0,0                                                             
         LTR   R0,R0               ANY LINES LEFT?                              
         BNZ   ADDG04              YES                                          
*                                                                               
ADDG09   BCTR  R3,0                NO, BACK UP PDSTSCN                          
         MVI   0(R3),X'00'         REMOVE THE LAST COMMA                        
*                                                                               
         L     R2,APDSTSCN                                                      
         LA    R2,8(R2)                                                         
         SR    R3,R2               LENGTH OF STRING                             
*                                                                               
         AHI   R2,-8               BACK UP TO START                             
         STC   R3,5(R2)            FUDGE LENGTH OF DATA                         
         AHI   R3,8                                                             
         STC   R3,0(R2)            AND LENGTH OF ENTRY                          
*                                                                               
         STC   RF,NFLD#                                                         
*                                                                               
         L     R5,PDADEMTB                                                      
         USING PDDMBUFF,R5         DEMO BUFFER                                  
         LA    RF,DBLOCKA                                                       
         USING DBLOCKD,RF                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         CLI   PODBMED,C'C'                                                     
         BNE   *+8                                                              
         MVI   DBSELMED,C'C'                                                    
         CLI   PDOVSYS,3           FOR NET                                      
         BNE   *+8                                                              
         MVI   DBSELMED,C'N'       SET SELMED TO N                              
         MVC   DBFILE,PODBFIL                                                   
*                                                                               
         ST    RF,DMCB+8                                                        
         CLI   PDOVSYS,2                                                        
         BNE   *+8                                                              
         MVI   DMCB+8,C'S'        SPOTPAK CALL                                  
         CLI   PDOVSYS,3           ONLY NET CAN USE 4 BYTE DEMOS                
         BNE   *+8                                                              
         MVI   DBDEMTYP,C'4'                                                    
*                                                                               
         GOTO1 DEMOVAL,DMCB,(NFLDS,(R2)),STDMPTR,,0                             
         CLI   4(R1),0                                                          
         BE    ADDGNEX                                                          
*                                                                               
         CLI   NFLD#,24            DID WE HAVE 24 DEMOS?                        
         BNE   ADDG09A             NO, DONE                                     
*                                                                               
         ZIC   RF,4(R1)            SAVE NUMBER OF DEMOS                         
         LA    R1,4                                                             
         CLI   PDOVSYS,3           IS THIS NET?                                 
         BE    *+8                 YES                                          
         LA    R1,3                NO                                           
         MR    RE,R1               NUMBER OF DEMOS X LENGTH                     
*                                                                               
         L     RE,STDMPTR          GET CURRENT OUT ADDRESS                      
         LA    RE,0(RE,RF)         BUMP TO END                                  
         ST    RE,STDMPTR          SAVE FOR NEXT TIME                           
*                                                                               
         L     R2,SAVER2           RESTORE PLACE IN PDSTADEF                    
         L     R4,SAVER4           RESTORE PLACE IN BLOCK                       
         L     R0,SAVER0           RESTORE DEMO COUNT                           
*                                                                               
         L     RE,APDSTSCN         CLEAR SCREEN BUILD AREA AGAIN                
         LA    RF,L'PDSTSCN                                                     
         XCEFL                                                                  
*                                                                               
         L     R3,APDSTSCN                                                      
         LA    R3,8(R3)                                                         
*                                                                               
         SR    RF,RF               CLEAR COUNTER                                
         B     ADDG06                                                           
*                                                                               
ADDG09A  L     R5,PDADEMTB                                                      
         USING PDDMBUFF,R5                                                      
         LA    R4,PODDEMWK         DEMO WORK AREA                               
         LA    R3,PODDEMO          DEMO HOLD AREA                               
         LA    R1,PDSTADEF         STACK DEFINITION                             
         L     RE,APDSTDEM         STACK DEMOS                                  
         SR    R2,R2                                                            
         SR    RF,RF                                                            
*                                                                               
ADDG10   CLI   0(R4),X'FF'         ANY MORE DEMOS?                              
         BE    ADDGEQX             NO                                           
         CLI   PDOVSYS,3           IS IT NET?                                   
         BNE   ADDG12              NO                                           
         MVC   0(4,R3),0(R4)       YES, MOVE 4 BYTES TO PODDEMO                 
         LA    R3,4(R3)                                                         
         B     ADDG14                                                           
*                                                                               
ADDG12   MVC   0(3,R3),0(R4)       NOT NET, MOVE 3 BYTES                        
         LA    R3,3(R3)                                                         
*                                                                               
ADDG14   LA    R2,1(R2)            ADD 1 TO COUNTER                             
         LA    RF,1(RF)                                                         
*                                                                               
ADDG16   CLI   1(R1),0             END OF STACK DEFINITIONS                     
         BNE   ADDG18              NO                                           
         AH    R4,LNDEMCOD         YES, GET TO NEXT DEMO                        
         LA    R1,PDSTADEF         GET TO TOP OF STACK                          
         STC   RF,PODDMENT         SET LENGTH OF DEMO SOURCES                   
         SR    RF,RF                                                            
         B     ADDG10                                                           
*                                                                               
ADDG18   MVC   0(3,R3),0(RE)                                                    
         CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+10                                                             
         MVC   0(4,R3),0(RE)       USE 4 BYTE DEMOS                             
*                                                                               
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   0(R3),X'01'                                                      
*                                                                               
         LA    R3,3(R3)            PODDEMO                                      
         LA    RE,3(RE)            PDSTSCN                                      
*                                                                               
         CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+12                                                             
         LA    R3,1(R3)            PODDEMO                                      
         LA    RE,1(RE)            PDSTSCN                                      
*                                                                               
         LA    R1,2(R1)            PDSTADEF                                     
         LA    R2,1(R2)            COUNTER                                      
         LA    RF,1(RF)                                                         
         B     ADDG16                                                           
*                                                                               
ADDGEQX  MVI   0(R3),X'FF'         END OF DEMO MARK                             
         STC   R2,PODDMNUM                                                      
         CR    RB,RB                                                            
         B     ADDGX                                                            
*                                                                               
ADDGNEX  LTR   RB,RB                                                            
         B     ADDGX                                                            
*                                                                               
ADDGX    XIT1                                                                   
NFLD#    DS    X                                                                
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
BADOPT   MVC   PERROR,=AL2(INVOPT)                                              
         B     OPTERR                                                           
*                                                                               
BADOPT2  MVC   PERROR,=AL2(DUPOPT)                                              
         B     OPTERR                                                           
*                                                                               
BADOPT3  MVC   PERROR,=AL2(TOOMANY)                                             
         B     OPTERR                                                           
*                                                                               
BADOPT4  MVC   PERROR,=AL2(AFFCON)                                              
         B     OPTERR                                                           
*                                                                               
BADOPT5  MVC   PERROR,=AL2(STKCON)                                              
         B     OPTERR                                                           
*                                                                               
BADOPT6  MVC   PERROR,=AL2(PRGCON)                                              
         B     OPTERR                                                           
*                                                                               
BADOPT7  MVC   PERROR,=AL2(PAVUPG)                                              
         B     OPTERR                                                           
*                                                                               
MINERR   MVC   PERROR,=AL2(PREMIN)                                              
         B     OPTERR                                                           
*                                                                               
BADPRGS  MVC   PERROR,=AL2(INVPRGS)                                             
         B     OPTERR                                                           
*                                                                               
BADSID   MVC   PERROR,=AL2(INVSID)                                              
         B     OPTERR                                                           
*                                                                               
PERERR   MVC   PERROR,=AL2(INVPER)                                              
         B     OPTERR                                                           
*                                                                               
BADEBK   MVC   PERROR,=AL2(INVEST)                                              
         B     OPTERR                                                           
*                                                                               
BADSYS   MVC   PERROR,=AL2(NOSIDS)                                              
         B     OPTERR                                                           
*                                                                               
BADUPG   MVC   PERROR,=AL2(NEEDUP)                                              
         B     OPTERR                                                           
*                                                                               
BADMEN   MVC   PERROR,=AL2(MENUOPT)                                             
         B     OPTERR                                                           
*                                                                               
BADSTA   MVC   PERROR,=AL2(MISSTA)                                              
         B     OPTERR                                                           
*                                                                               
OPTERR   MVI   PMSGTYPE,C'E'                                                    
         GOTO1 VPDSERR                                                          
         EJECT                                                                  
***********************************************************************         
*              LIST OF VALID MODIFIERS                                *         
***********************************************************************         
*                                                                               
MODTAB   DS    0CL6                                                             
         DC    C'*VWR*',C'A'                                                    
         DC    C'*PUT)',C'B'                                                    
         DC    C'HIMPS',C'C'                                                    
         DC    C'*N/A*',C'D'                                                    
         DC    C'*HUT)',C'H'                                                    
         DC    C'(IMP)',C' '                                                    
         DC    C'(PUT)',C'P'                                                    
         DC    C'(TOT)',C'Q'                                                    
         DC    C'(RTG)',C'R'                                                    
         DC    C'(SHR)',C'S'                                                    
         DC    C'(TSH)',C'T'                                                    
         DC    C'(UNV)',C'U'                                                    
         DC    C'*000)',C'X'                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*         INITIALIZE DRIVER                                           *         
***********************************************************************         
*                                                                               
INTDRIV  NMOD1 0,**INTDRV**                                                     
         L     R9,0(R1)                                                         
         L     RC,4(R1)                                                         
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   DRIX                                                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         GOTO1 CALLOV,DMCB,X'B1000000',0,0  LOAD T325B1(GLOBAL STORAGE)         
         L     R4,DMCB                     FOR DRIVER                           
         ST    R4,AGLOBAL                                                       
*                                  THIS ALSO CONTAINS BUFFERS                   
         LA    R2,16(R4)                                                        
         L     R1,0(R2)                                                         
         LA    R2,4+8(R1,R2)                                                    
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',90000,90000    GET OFFLINE BUFFER             
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,PDAOFFBF         A(OFFLINE BUFFER)                            
         MVC   0(4,RE),8(R1)       L'BUFFER                                     
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',300000,300000  GET NAME POOL BUFFER           
         ICM   RE,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,8(R1)                                                         
         ST    RE,ANAMPOOL         A(OFFLINE BUFFER)                            
         ST    RF,LNAMPOOL                                                      
         LA    R1,8(RE)                                                         
         ST    R1,0(RE)            A(FIRST AVAILABLE ENTRY)                     
         ST    RF,4(RE)                                                         
*                                                                               
         USING GLOBALD,R4                                                       
         GOTO1 CALLOV,DMCB,0,X'D9000A3A'   LOAD T00A3A (DRIVER)                 
         L     R2,DMCB                                                          
         ST    R2,DRIVER                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A53'   LOAD T00A53 (RESEARCH DRIVE)         
         L     R2,DMCB                                                          
         ST    R2,GLASYSDR                                                      
         ST    RC,GLAWORKD                                                      
         MVC   GLAPROG,ADPGPROG                                                 
         MVI   GLTWORKD,GLTSPOOL                                                
         MVC   GLFHEADL,MYFIRSTH                                                
         MVC   GLSPACE,SPACOPT      PASS THRU SPACING OPT                       
         MVC   GLBOXOPT,BOXOPT                BOX OPTION                        
         MVC   GLLFTOPT,LEFTOPT           AND LEFT OPTION                       
*                                                                               
         TM    DOWNOPT,DOWNON       OPTION TO DOWNLOAD                          
         BZ    *+14                                                             
         OC    GLDOWNLD,DOWNOPT                                                 
         OI    GLDOWNLD,GLDLACTV                                                
*                                                                               
         TM    REQIND,REQITRN      TEST RECORD=TRANSMIT                         
         BZ    *+8                                                              
         OI    GLDOWNLD,GLDLACTV+GLDLNOTR+GLDLALPH+GLDLNOHD+GLDLCUT             
*                                                                               
         OC    GLDWNLD2(1),DOWNOPT2  OTHER DOWNLOAD OPTIONS                     
         CLI   DOWNCHAR,0                                                       
         BE    *+10                                                             
         MVC   GLDLCHAR,DOWNCHAR                                                
*                                                                               
         MVC   GLINDS2,WKINDS2                                                  
         CLI   TRACEOPT,C'Y'        OPTION TO TRACE                             
         BNE   *+8                                                              
         MVI   GLTRACE,C'Y'                                                     
*                                                                               
         XC    GLOPTS,GLOPTS       SET GLOBAL USER OPTIONS                      
*                                                                               
DRI4     B     DRI6                                                             
         EJECT                                                                  
* INITIALIZATION OF PRINT RELATED FIELDS                                        
*                                                                               
DRI6     L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         CLI   WIDEOPT,C'Y'                                                     
         BE    DRIWIDE                                                          
         MVC   BOXWIDTH,=F'132'                                                 
         MVI   BOXFONT,0                                                        
         LA    R1,H1               PRINT ADDRESSES FOR STANDARD                 
         ST    R1,AH1                                                           
         LA    R1,H4                                                            
         ST    R1,AH4                                                           
         LA    R1,H5                                                            
         ST    R1,AH5                                                           
         LA    R1,P                                                             
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'132'                                                   
         LA    R1,REGSPECS                                                      
         ST    R1,SPECS                                                         
         CLI   NARROPT,C'Y'                                                     
         BNE   DRIX                                                             
         LA    R1,NARSPECS                                                      
         ST    R1,SPECS                                                         
         B     DRIX                                                             
*                                                                               
DRIWIDE  MVC   BOXWIDTH,=F'165'                                                 
         MVI   BOXFONT,1                                                        
         L     R4,BOXAWIDE                                                      
         USING WIDED,R4                                                         
         LA    R1,XHEAD1                                                        
         ST    R1,AH1                                                           
         LA    R1,XHEAD4                                                        
         ST    R1,AH4                                                           
         LA    R1,XHEAD5                                                        
         ST    R1,AH5                                                           
         LA    R1,XP                                                            
         ST    R1,AP1                                                           
         MVC   PWIDTH,=F'198'                                                   
         LA    R1,WIDSPECS                                                      
         ST    R1,SPECS                                                         
         B     DRIX                                                             
*                                                                               
DRIX     XMOD1                                                                  
         EJECT                                                                  
REGSPECS DS    0C                                                               
         SSPEC H1,2,RUN            SPECS FOR REGULAR PRINTING                   
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,REPORT                                                     
         SSPEC H4,110,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
NARSPECS DS    0C                                                               
         SSPEC H1,60,RUN           SPECS FOR NARROW PRINTING                    
         SSPEC H2,60,REQUESTOR                                                  
         SSPEC H4,60,REPORT                                                     
         SSPEC H4,73,PAGE                                                       
         DC    X'00'                                                            
*                                                                               
WIDSPECS DS    0C                                                               
         WSPEC H1,2,RUN            SPECS FOR WIDE PRINTING                      
         WSPEC H2,2,REQUESTOR                                                   
         WSPEC H1,129,AGYNAME                                                   
         WSPEC H2,129,AGYADD                                                    
         WSPEC H4,129,REPORT                                                    
         WSPEC H4,142,PAGE                                                      
         DC    X'00'                                                            
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*--OVERFLOW ROUTINES                                                            
*                                                                               
         DS    0F                                                               
         DROP  R6,R7,RA,RB                                                      
OVFLRTN  NMOD1 0,**A54OV*                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING OVFLRTN+4096,RA                                                  
         L     RC,4(R1)                                                         
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    RE,RELO2C                                                        
         S     RE,RELO2C                                                        
         ST    RE,PGNR2                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRANCH(RF)                                                     
*                                                                               
OVBRANCH B     VALSTACK                                                         
         B     VALSTDEM                                                         
*        B     VGENERIC                                                         
         B     *+2                                                              
         B     VFLOWTAB                                                         
         B     VPEREXPR                                                         
         B     VALNTINM                                                         
         B     VSFTBOOK                                                         
         B     VALGSTCK                                                         
         B     VALPRGRP                                                         
         EJECT                                                                  
*              VALIDATE A STACK EXPRESSION                                      
*                                                                               
*              INPUT               R4=A(SCANNER BLOCK)                          
*                                                                               
VALSTACK ZIC   R1,1(R4)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         BZ    OVNEXIT                                                          
         LA    R1,22(R1,R4)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R4,22(R4)           (BUMP TO EXPRESSION)                         
         LA    R3,PDSTADEF                                                      
         LA    R0,8                (MAX 8 TERMS)                                
*                                                                               
VSTK2    CLI   0(R4),C'T'          PREFIX OF T=TOTAL ONLY                       
         BNE   VSTK2A                                                           
         OI    0(R3),X'40'                                                      
*                                                                               
VSTK2A   L     RF,=A(STKTABLE)                                                  
         A     RF,PGNR2                                                         
         CLC   PODBEXT(3),=C'BBM'                                               
         BNE   VSTK2B                                                           
         L     RF,=A(STKTABC)                                                   
         A     RF,PGNR2                                                         
*                                                                               
VSTK2B   LR    R2,RF               R2-->STKTABLE                                
         LA    R8,7                7 BYTE EXPRESSIONS                           
*                                                                               
VSTK4    L     R6,0(R2)            GET ADDRESS FROM TABLE                       
         A     R6,PGNR2                                                         
         LR    R5,R8                                                            
         BCTR  R5,0                                                             
*                                                                               
VSTK4A   DS    0H                                                               
         CLI   0(R6),X'FF'         END OF TABLE?                                
         BE    VSTK5                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),1(R6)                                                    
         BE    VSTKFND                                                          
         AR    R6,R8               BUMP TO NEXT TABLE ELEMENT                   
         LA    R6,1(R6)                                                         
         B     VSTK4A                                                           
*                                                                               
VSTK5    BCTR  R8,0                                                             
         LTR   R8,R8                                                            
         BZ    OVNEXIT             CAN'T FIND IT, ERROR                         
         LA    R2,4(R2)            NEXT LENGTH IN STACK TABLE                   
         B     VSTK4                                                            
*                                                                               
VSTKFND  MVC   1(1,R3),0(R6)       SAVE EXPRESSION NUMBER                       
*                                                                               
VSTKFND2 CLI   0(R6),250                                                        
         BNE   VSTKAVLX                                                         
         MVI   1(R3),240           DEMO                                         
         MVI   3(R3),241           SHARE                                        
         MVI   5(R3),242           HPT                                          
         LA    R3,4(R3)                                                         
VSTKAVLX LA    R3,2(R3)                                                         
         AR    R4,R8                                                            
         CLI   0(R4),C'/'          NEED A /                                     
         BNE   OVNEXIT                                                          
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    OVEQXIT                                                          
         BCT   R0,VSTK2                                                         
         B     OVNEXIT                                                          
         EJECT                                                                  
*-CHECK IF EXTRA DEMO'S CATEGORIES NEEDED TO                                    
*-COVER THE STACKS DEMO NEEDS.                                                  
*                                                                               
VALSTDEM L     R5,PDADEMTB                                                      
         USING PDDMBUFF,R5                                                      
         LA    R4,PODDEMWK         DEMO WORK AREA                               
         LA    R3,PODDEMO          DEMO HOLD AREA                               
         LA    R1,PDSTADEF         STACK DEFINITION                             
         SR    R2,R2                                                            
         SR    RF,RF                                                            
*                                                                               
VSTD20   CLI   0(R4),X'FF'                                                      
         BE    VSTD80                                                           
         CLI   PDOVSYS,3                                                        
         BNE   VSTD25                                                           
         MVC   0(4,R3),0(R4)                                                    
         LA    R3,4(R3)                                                         
         B     VSTD27                                                           
VSTD25   MVC   0(3,R3),0(R4)                                                    
         LA    R3,3(R3)                                                         
VSTD27   LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
*                                                                               
VSTD30   CLI   1(R1),0             END OF STACK DEFINITIONS                     
         BNE   VSTD40                                                           
         AH    R4,LNDEMCOD         BUMP TO NEXT DEMO                            
         LA    R1,PDSTADEF         RE-START STACK LIST                          
         STC   RF,PODDMENT         SET LENGTH OF DEMO SOURCES                   
         SR    RF,RF                                                            
         B     VSTD20                                                           
VSTD40   CLI   1(R1),C'A'                                                       
         BNL   VSTD50                                                           
         B     VSTD60                                                           
*                                                                               
VSTD50   MVC   0(3,R3),0(R4)                                                    
*                                                                               
         STM   RE,RF,DMCB                                                       
         CLI   1(R1),240           ABOVE 240 ARE SPECIAL                        
         BL    *+12                                                             
         BAS   RE,MODSTK           MODIFY THE STACK MODIFIERS                   
         B     *+10                AND BYPASS CONSTANT TYPE SET                 
*                                                                               
         MVC   1(1,R3),1(R1)       MOVE NEW TYPE IN                             
         LM    RE,RF,DMCB                                                       
VSTD52   CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+8                                                              
         BAS   RE,HNDLNET          USE 4 BYTE DEMOS                             
         LA    R3,3(R3)                                                         
         LA    R1,2(R1)                                                         
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     VSTD30                                                           
*                                                                               
VSTD60   MVC   0(3,R3),0(R4)                                                    
         MVC   0(1,R3),1(R1)       MOVE NAD CATEGORY IN                         
         CLI   PDOVSYS,3           NET SYSTEM?                                  
         BNE   *+8                                                              
         BAS   RE,HNDLNAD          USE 4 BYTE DEMOS                             
         LA    R3,3(R3)                                                         
         LA    R1,2(R1)                                                         
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     VSTD30                                                           
*                                                                               
HNDLNET  MVC   0(4,R3),0(R4)       USE 4 BYTES FOR DEMOS                        
         MVC   1(1,R3),1(R1)                                                    
         LA    R3,1(R3)                                                         
         BR    RE                                                               
*                                                                               
HNDLNAD  MVC   0(4,R3),0(R4)       USE 4 BYTES FOR DEMOS                        
         CLI   1(R1),C'A'                                                       
         BL    *+14                                                             
         MVC   1(1,R3),1(R1)                                                    
         B     HNDLNAD2                                                         
         MVC   0(1,R3),1(R1)                                                    
HNDLNAD2 LA    R3,1(R3)                                                         
         BR    RE                                                               
*                                                                               
VSTD80   MVI   0(R3),X'FF'         END OF DEMO MARK                             
         STC   R2,PODDMNUM                                                      
         B     OVEQXIT                                                          
*                                                                               
MODSTK   NTR1                                                                   
         CLI   1(R1),240           DEMO KEYWORK GETS NO MODIFICATION            
         BE    MODSTKX                                                          
         SR    RF,RF                                                            
         CLI   1(R1),241           SHARES                                       
         BNE   *+8                                                              
         LA    RF,KSHREQ                                                        
         CLI   1(R1),242           HPT                                          
         BNE   *+8                                                              
         LA    RF,KHPTEQ                                                        
         LTR   RF,RF               ERROR - JUST GET OUT                         
         BZ    MODSTKX                                                          
MODSTK2  CLI   0(RF),X'FF'         SCAN FOR MODIFIER EQUATES                    
         BE    MODSTKX                                                          
         CLC   1(1,R3),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     MODSTK2                                                          
         MVC   1(1,R3),1(RF)       SET NEW MODIFIER                             
MODSTKX  XIT1                                                                   
*                                                                               
KSHREQ   DC    C'RS',C' X',C'IX',C'TX',X'FF'                                    
KHPTEQ   DC    C'RP',C' Q',C'IQ',C'PP',X'FF'                                    
         DS    0H                                                               
         EJECT                                                                  
*              BUILD THE FLOWCHART DATE TABLE                                   
         SPACE 3                                                                
*              INPUT               R4=A(SCANNER BLOCK)                          
         SPACE 1                                                                
VFLOWTAB L     R2,PDAFLBUF         LENGTH OF EXPRESSIONS                        
         XCEFL 0(R2),300                                                        
         USING PDFLBUFF,R2                                                      
*                                                                               
*--SET WEEK TAB                                                                 
         LA    RF,VFLT100                                                       
         BAS   RE,FLTMCHK                                                       
         LA    RE,104                                                           
         MVC   WEEKLIST(2),PODBBKS                                              
         LA    R3,WEEKLIST                                                      
*                                                                               
VFLT20   MVC   2(2,R3),0(R3)                                                    
         ZIC   RF,3(R3)            BUMP THE WEEK                                
         LA    RF,1(RF)                                                         
         STC   RF,3(R3)                                                         
         CH    RF,=H'53'           CHECK FOR YEAR BREAK (SOME HAVE 53)          
         BNH   VFLT40                                                           
         ZIC   RF,2(R3)                                                         
         LA    RF,1(RF)            BUMP THE YEAR                                
         STC   RF,2(R3)                                                         
         MVI   3(R3),1             SET WEEK TO 1                                
VFLT40   LA    R3,2(R3)                                                         
         BCT   RE,VFLT20                                                        
*                                                                               
*--SET MONTH TAB                                                                
VFLT100  LA    R3,MNTHLIST                                                      
         MVC   0(2,R3),PODBBKS                                                  
         LA    RF,VFLT120                                                       
         BAS   RE,FLTMCHK                                                       
*--CONVERT WEEK FORMAT TO MONTH FORMAT                                          
         GOTO1 NETUNBK,DMCB,(C'M',0(R3)),DUB,GETDAY,ADDAY,GETBROAD              
         PACK  DUB(8),DUB+2(2)                                                  
         CVB   R1,DUB                                                           
         STC   R1,1(R3)                                                         
*                                                                               
VFLT120  LA    RE,24                                                            
*                                                                               
VFLT140  MVC   2(2,R3),0(R3)                                                    
         ZIC   RF,3(R3)            BUMP THE MONTH                               
         LA    RF,1(RF)                                                         
         STC   RF,3(R3)                                                         
         CH    RF,=H'12'           CHECK FOR YEAR BREAK                         
         BNH   VFLT160                                                          
         ZIC   RF,2(R3)                                                         
         LA    RF,1(RF)            BUMP THE YEAR                                
         STC   RF,2(R3)                                                         
         MVI   3(R3),1             SET MONTH TO ONE                             
VFLT160  LA    R3,2(R3)                                                         
         BCT   RE,VFLT140                                                       
*                                                                               
*--SET QUARTER TAB                                                              
         LA    R3,QURTLIST                                                      
         MVC   0(2,R3),PODBBKS                                                  
         LA    RF,VFLT200                                                       
         BAS   RE,FLTMCHK                                                       
*--CONVERT WEEK FORMAT TO QUARTER FORMAT (NTI,PIV)                              
         GOTO1 NETUNBK,DMCB,(C'Q',0(R3)),DUB,GETDAY,ADDAY,GETBROAD              
         MVC   1(1,R3),DUB+1       TAKE THE QUARTER NUMBER                      
         NI    1(R3),X'0F'         REMOVE THE ZONE                              
         B     VFLT220                                                          
*--CONVERT WEEK FORMAT TO QUARTER FORMAT (NAD, MPA)                             
VFLT200  L     R4,=A(FLWNAD)                                                    
         A     R4,PGNR2                                                         
         BAS   RE,GETMNQT          CONV FROM WEEK TO MONTH/QUARTER              
*                                                                               
VFLT220  LA    RE,7                                                             
*                                                                               
VFLT240  MVC   2(2,R3),0(R3)                                                    
         ZIC   RF,3(R3)            BUMP THE QUARTER                             
         LA    RF,1(RF)                                                         
         STC   RF,3(R3)                                                         
         CH    RF,=H'4'            CHECK FOR YEAR BREAK                         
         BNH   VFLT260                                                          
         ZIC   RF,2(R3)                                                         
         LA    RF,1(RF)            BUMP THE YEAR                                
         STC   RF,2(R3)                                                         
         MVI   3(R3),1             SET QUARTER TO ONE                           
VFLT260  LA    R3,2(R3)                                                         
         BCT   RE,VFLT240                                                       
*                                                                               
*--SET YEAR TAB                                                                 
         LA    RE,3                                                             
         MVC   YEARLIST(1),PODBBKS                                              
         LA    R3,YEARLIST                                                      
*                                                                               
VFLT300  MVC   2(2,R3),0(R3)                                                    
         ZIC   RF,2(R3)            BUMP THE YEAR                                
         LA    RF,1(RF)                                                         
         STC   RF,2(R3)                                                         
         LA    R3,2(R3)                                                         
         BCT   RE,VFLT300                                                       
*                                                                               
         B     OVXIT                                                            
         DROP  R2                                                               
*                                                                               
FLTMCHK  CLC   PODBEXT(3),=CL3'NAW'       NAW IS WEEKLY                         
         BER   RE                                                               
         CLC   PODBFIL,=CL3'NAD'          COVERS NAD AND NHI                    
         BER   RF                                                               
         CLC   PODBEXT(3),=CL3'MPA'                                             
         BER   RF                                                               
         CLC   PODBEXT(3),=CL3'CSI'        CSI IS WEEKLY SOMETIMES              
         BE    FLTM02                                                           
         CLC   PODBEXT(3),=CL3'WTP'        NSI WTP OVERRIDE                     
         BE    FLTM02                                                           
         CLC   PODBEXT(3),=CL3'BBM'        BBM IS WEEKLY SOMETIMES              
         BE    FLTM06                                                           
         CLC   PODBFIL(3),=CL3'TP'         COVERS TP, T4 AND DPT                
         BER   RF                                                               
*                                                                               
FLTM02   CLC   PODBEXT(3),=CL3'PAV'                                             
         BER   RF                                                               
         CLC   PODBEXT(3),=CL3'IUN'                                             
         BER   RF                                                               
         CLI   PODBBKS,96                                                       
         BNL   FLTM04                                                           
         CLC   PODBEXT(3),=CL3'CSI'                                             
         BER   RF                                                               
*                                                                               
FLTM04   CLC   PODBEXT(3),=CL3'SRC'                                             
         BER   RF                                                               
         BR    RE                                                               
*                                                                               
FLTM06   CLI   SVBBMWK,C'Y'        YES, IS THIS WEEKLY?                         
         BER   RE                  YES                                          
         BR    RF                  NO                                           
         EJECT                                                                  
*--CONVERT WEEK TO MONTH OR QUARTER                                             
*--R3=BOOK DATE                                                                 
*--R4=CONVERSION TABLE                                                          
GETMNQT  NTR1                                                                   
GETMQ20  ZIC   RF,0(R4)                                                         
         LA    R4,1(R4)                                                         
         TM    1(R3),X'80'         ESTIMATED BOOK?                              
         BNO   GETMQ40                                                          
         NI    1(R3),X'7F'         EXTRACT MONTH                                
         SR    RE,RE                                                            
GETMQ40  CLC   1(1,R3),0(R4)                                                    
         BE    GETMQ100                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   GETMQ40                                                          
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     GETMQ20                                                          
*                                                                               
GETMQ100 STC   RF,1(R3)                                                         
         LTR   RE,RE               IF ZERO, IT WAS ESTIMATED BOOK               
         BNZ   OVXIT                                                            
         OI    1(R3),X'80'                                                      
         B     OVXIT                                                            
         EJECT                                                                  
*              ROUTINE TO VALIDATE PERIOD EXPRESSIONS                           
*                                                                               
*              INPUT               R4=A(SCANNER ENTRY)                          
*              OUTPUT              RETURN IN EXPLIST                            
*                                  X'00' NOT VALID                              
*                                  X'FF' VALID BUT DATE MISSED                  
*                                  X'NN00' PERIOD NUMBER                        
*                                  OR LIST OF PERIOD NUMBERS                    
*                                                                               
*              VALID               W1 THRU W105   SINGLE WEEK                   
*                                  M1 THRU M25           MONTH                  
*                                  Q1 THRU Q8            QUARTER                
*                                  Y1 THRU Y3            YEARS                  
*                                  WN-N           FLOW   WEEK                   
*                                  MN-N                  MONTH                  
*                                  QN-N                  QUARTER                
*                                  YN-N                  YEARS                  
*                                                                               
*                                                                               
VPEREXPR L     R5,PDAFLBUF                                                      
         USING PDFLBUFF,R5                                                      
         CLI   EXPLIST,0           IF THERE IS ONE THERE ALREADY                
         BNE   OVXIT                  WE ARE THROUGH                            
         SR    R2,R2                                                            
         LA    R3,105                                                           
         CLI   12(R4),C'W'         EXPRESSIONS MUST START W/M/Q/Y               
         BE    PEX2                                                             
         LA    R2,105                                                           
         LA    R3,25                                                            
         CLI   12(R4),C'M'                                                      
         BE    PEX2                                                             
         LA    R2,130                                                           
         LA    R3,8                                                             
         CLI   12(R4),C'Q'                                                      
         BE    PEX2                                                             
         LA    R2,138                                                           
         LA    R3,3                                                             
         CLI   12(R4),C'Y'                                                      
         BE    PEX2                                                             
         B     OVXIT                                                            
         SPACE 1                                                                
PEX2     LA    R4,13(R4)           R4=A(FIRST POTENTIAL NUMBER)                 
         BAS   RE,PEXNUM                                                        
         LTR   R0,R0               DID WE GET A VALID NUMBER                    
         BZ    OVXIT                                                            
         CR    R0,R3               THAT WASN'T TOO BIG?                         
         BH    OVXIT                                                            
         AR    R0,R2                                                            
         STC   R0,EXPLIST          THEN SAVE THAT NUMBER                        
         CLI   0(R4),C'-'          IS THERE A RANGE SPECIFIED?                  
         BNE   PEX6                                                             
         LA    R4,1(R4)                                                         
         BAS   RE,PEXNUM           WAS THE SECOND NUMBER VALID?                 
         LTR   R0,R0                                                            
         BZ    PEXNO                                                            
         CR    R0,R3                                                            
         BH    PEXNO                                                            
         AR    R0,R2                                                            
         ZIC   R1,EXPLIST                                                       
         CR    R0,R1                                                            
         BNH   PEXNO               SECOND MUST BE HIGHER THAN FIRST             
         SR    R1,R0                                                            
         LCR   R1,R1                                                            
         CH    R1,=H'15'           AND NOT MORE THAT 15 MORE                    
         BH    PEXNO                                                            
         ZIC   R1,EXPLIST                                                       
         LA    R1,1(R1)                                                         
         LA    R2,EXPLIST+1                                                     
         SPACE 1                                                                
PEX4     STC   R1,0(R2)            GENERATE A LIST OF NUMBERS                   
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         CR    R1,R0                                                            
         BNH   PEX4                                                             
         SPACE 1                                                                
PEX6     LA    R2,EXPLIST          NOW HAVE A LIST OF NUMBERS                   
         LA    R3,16               IN EXPLIST                                   
         SPACE 1                                                                
PEX8     CLI   0(R2),0                                                          
         BE    OVXIT                                                            
         ZIC   R1,0(R2)            CHECK THAT EACH NUMBER                       
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         AR    R1,R5                                                            
         OC    0(2,R1),0(R1)       PRODUCES A VALID DATE RANGE                  
         BZ    PEXMISS                                                          
         LA    R2,1(R2)                                                         
         BCT   R3,PEX8                                                          
         B     OVXIT                                                            
         SPACE 1                                                                
PEXMISS  MVI   EXPLIST,X'FF'       PASS BACK X'FF'                              
         B     OVXIT                                                            
         SPACE 1                                                                
PEXNO    XC    EXPLIST,EXPLIST                                                  
         B     OVXIT                                                            
         DROP  R5                                                               
         SPACE 1                                                                
PEXNUM   SR    R0,R0               VALIDATE FOR NUMERIC                         
*                                  R4=A(POTENTIAL EXPRESSION)                   
*                                  PASS BACK NUMBER IN R0                       
         SPACE 1                                                                
PEXNUM2  CLI   0(R4),C' '          DELIMITED BY SPACE OR DASH                   
         BER   RE                                                               
         CLI   0(R4),C'-'                                                       
         BER   RE                                                               
         CLI   0(R4),C'0'          CHECK FOR NUMERIC DIGIT                      
         BL    PEXNUMNO                                                         
         CLI   0(R4),C'9'                                                       
         BH    PEXNUMNO                                                         
         MH    R0,=H'10'           MULTIPLY PREVIOUS BY 10                      
         ZIC   R1,0(R4)            AND ADD THIS DIGIT                           
         SLL   R1,28               (STRIP TOP BITS)                             
         SRL   R1,28                                                            
         AR    R0,R1                                                            
         LA    R4,1(R4)                                                         
         B     PEXNUM2                                                          
         SPACE 1                                                                
PEXNUMNO SR    R0,R0               NO GOOD - RETURN ZERO                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*-SET NTI FILTER TABLE                                                          
*                                                                               
VALNTINM L     RE,APDNTIFT                                                      
         CLI   0(RE),0                                                          
         BNE   OVNEXIT             ALREADY INPUTTED ERROR                       
         CLI   22(R4),X'40'                                                     
         BNH   OVNEXIT             NO INPUT ERROR                               
         LA    R1,22(R4)                                                        
         L     R5,APDNTIFT                                                      
         LA    RF,10                                                            
*                                                                               
VNTI02   MVC   0(5,R5),0(R1)       NTI HOLD AREA                                
         LA    R1,5(R1)                                                         
         CLI   0(R1),X'40'                                                      
         BNH   OVEQXIT             NO MORE EXIT                                 
         CLI   0(R1),C'/'                                                       
         BNE   OVNEXIT                                                          
         LA    R1,1(R1)                                                         
         LA    R5,21(R5)                                                        
         BCT   RF,VNTI02                                                        
         BCTR  R1,0                                                             
         CLI   0(R1),C'/'                                                       
         BE    OVNEXIT             TOO MANY ENTRIES                             
         B     OVEQXIT             NO MORE EXIT                                 
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK FOR SOFT HUT EXPRESSIONS                    *         
*        ALLOWABLE           Q1/85      Q4/85                         *         
*                            Q1/85,86   Q4/85,86                      *         
*                            M1/85      M12/85                        *         
*                            M1/85,86   M12/85,86                     *         
***********************************************************************         
*                                                                               
VSFTBOOK DS    0H                                                               
         CLC   7(3,R5),=C'NAD'     NOT APPLICABLE FOR NAD AND NHI               
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'MPA'     NOT APPLICABLE FOR MPA                       
         BE    SOFTNO                                                           
         CLC   PODBEXT(3),=CL3'WTP'   NSI WTP OVERRIDE                          
         BE    *+14                                                             
         CLC   7(3,R5),=C'TP '     NOT APPLICABLE FOR TP, T4 AND DPT            
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'PAV'     NOT APPLICABLE FOR PAV                       
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'IUN'     NOT APPLICABLE FOR PAV                       
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'CSI'     NOT APPLICABLE FOR CSI                       
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'BBM'     NOT APPLICABLE FOR BBM                       
         BE    SOFTNO                                                           
         CLC   0(3,R5),=C'SRC'     NOT APPLICABLE FOR SRC                       
         BE    SOFTNO                                                           
         CLC   0(4,R5),=C'SQAD'    NOT APPLICABLE FOR SQAD                      
         BE    SOFTNO                                                           
         CLI   MAX,1               MUST BE REQUESTING MULTIPLE BOOKS            
         BE    SOFTNO                                                           
                                                                                
*                                  CHECK FOR A MATCH ON PHRASE                  
         L     R1,=A(HUTTABLE)                                                  
         A     R1,PGNR2                                                         
         LA    R2,12(R3)           POINT TO DATE                                
*                                                                               
VSB10    ZIC   RE,0(R1)            LENGTH OF PHRASE                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),1(R1)                                                    
         BE    VSB20                                                            
         LA    R1,13(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BE    SOFTNO                                                           
         B     VSB10                                                            
*                                                                               
VSB20    MVI   0(R4),X'01'         SOFT EXPRESSION IS A RANGE                   
         MVC   1(11,R4),0(R5)                                                   
         MVC   PODBLNQ+PODBEXT-PODBD(11,R4),0(R5)                               
         CLI   1(R1),C'Q'          CHECK FOR QUARTERS                           
         BE    VSB40                                                            
*                                                                               
*--MONTH HANDLING                                                               
         XC    WORK,WORK                                                        
         MVC   DUB+2(4),5(R1)                                                   
         LA    R2,1(RE,R2)         SPACE TO YEAR                                
         CLI   0(R2),C'/'          GET PAST /                                   
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   0(R2),C'0'          MUST BE NUMERIC                              
         BL    SOFTNO                                                           
         CLI   1(R2),C'0'                                                       
         BL    SOFTNO                                                           
         CLI   2(R2),C' '          AND EXPRESSION MUST END THERE                
         BH    SOFTNO                                                           
         MVC   DUB(2),0(R2)                                                     
*                                                                               
         GOTO1 GETBROAD,PARAS,(1,DUB),WORK,GETDAY,ADDAY                         
*                                                                               
         CLI   PDCALOPT,PDCALNTI                                                
         BNE   VSB23                                                            
         GOTO1 DATCON,PARAS,(0,DUB),(3,FULL)                                    
         LA    RE,NADMON                                                        
         CLC   FULL(2),6(RE)                                                    
         BNH   *+12                                                             
         LA    RE,8(RE)                                                         
         B     *+14                                                             
         CLC   FULL(2),6(RE)                                                    
         BL    VSB23                                                            
         ST    RE,PARAS+16                                                      
         GOTO1 DATCON,PARAS,(3,(RE)),(0,WORK)                                   
         L     RE,PARAS+16                                                      
         GOTO1 DATCON,PARAS,(3,3(RE)),(0,WORK+6)                                
*                                                                               
*GET START RANGE                                                                
VSB23    GOTO1 NETWEEK,PARAS,WORK,GETDAY,ADDAY                                  
         MVC   12(1,R4),PARAS+4                                                 
         MVC   13(1,R4),PARAS+12                                                
*                                                                               
VSB25    CLI   1(R3),0             IS THERE A SECOND EXPRESSION                 
         BE    VSB30                                                            
         ZIC   R0,11(R3)           YES - IT MUST BE ANOTHER YEAR                
         LTR   R0,R0                                                            
         BZ    SOFTNO                                                           
         CLC   22(2,R3),DUB        CHECK YEARS IN SEQ                           
         BL    SOFTNO                                                           
         MVC   DUB(2),22(R3)                                                    
*                                                                               
         GOTO1 GETBROAD,PARAS,(1,DUB),WORK,GETDAY,ADDAY                         
*                                                                               
         CLI   PDCALOPT,PDCALNTI                                                
         BNE   VSB30                                                            
         GOTO1 DATCON,PARAS,(0,DUB),(3,FULL)                                    
         LA    RE,NADMON                                                        
         CLC   FULL(2),6(RE)                                                    
         BNH   *+12                                                             
         LA    RE,8(RE)                                                         
         B     *+14                                                             
         CLC   FULL(2),6(RE)                                                    
         BL    VSB30                                                            
         GOTO1 DATCON,PARAS,(3,3(RE)),(0,WORK+6)                                
*                                                                               
*GET END RANGE                                                                  
VSB30    GOTO1 NETWEEK,PARAS,WORK+6,GETDAY,ADDAY                                
         MVC   PODBLNQ+PODBBKS-PODBD(1,R4),PARAS+4                              
         MVC   PODBLNQ+PODBBKS+1-PODBD(1,R4),PARAS+12                           
         B     SOFTYES                                                          
*--QUARTER HANDLING                                                             
VSB40    XC    WORK,WORK                                                        
         MVC   DUB+2(4),5(R1)                                                   
         MVC   WORK+30(4),9(R1)    END DATE FOR QUARTER                         
         ST    R2,FULL                                                          
         LA    R2,1(RE,R2)         SPACE TO YEAR                                
         CLI   0(R2),C'/'          GET PAST /                                   
         BNE   *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   0(R2),C'0'          MUST BE NUMERIC                              
         BL    SOFTNO                                                           
         CLI   1(R2),C'0'                                                       
         BL    SOFTNO                                                           
         CLI   2(R2),C' '          AND EXPRESSION MUST END THERE                
         BH    SOFTNO                                                           
         MVC   DUB(2),0(R2)                                                     
         CLI   PDCALOPT,PDCALNTI   SPECIAL NTI CALENDAR REQUESTED               
         BNE   VSB41                                                            
         LA    RE,NTIQRT                                                        
         L     RF,FULL                                                          
*                                                                               
VSBNTI   CLI   0(RE),X'FF'                                                      
         BE    VSB41                                                            
         CLC   DUB(2),0(RE)                                                     
         BNE   *+10                                                             
         CLC   0(2,RF),2(RE)                                                    
         BE    *+12                                                             
         LA    RE,16(RE)                                                        
         B     VSBNTI                                                           
*                                                                               
         MVC   WORK(6),4(RE)                                                    
         MVC   WORK+30(4),10(RE)                                                
         MVC   WORK+6(6),10(RE)                                                 
         GOTO1 NETWEEK,PARAS,WORK,GETDAY,ADDAY                                  
         MVC   12(1,R4),PARAS+4                                                 
         MVC   13(1,R4),PARAS+12                                                
         GOTO1 NETWEEK,PARAS,WORK+6,GETDAY,ADDAY                                
         MVC   PODBLNQ+PODBBKS-PODBD(1,R4),PARAS+4                              
         MVC   PODBLNQ+PODBBKS+1-PODBD(1,R4),PARAS+12                           
         B     SOFTYES                                                          
*                                                                               
VSB41    CLI   13(R3),C'4'         CHECK 4TH QTR                                
         BNE   VSB50                                                            
         GOTO1 NETWEEK,PARAS,DUB,GETDAY,ADDAY                                   
         MVC   12(1,R4),PARAS+4                                                 
         ZIC   RF,PARAS+12                                                      
         LA    RF,1(RF)                                                         
         STC   RF,13(R4)                                                        
         B     VSB60                                                            
*                                                                               
VSB50    GOTO1 GETBROAD,PARAS,(1,DUB),WORK,GETDAY,ADDAY                         
*GET START RANGE                                                                
         GOTO1 NETWEEK,PARAS,WORK,GETDAY,ADDAY                                  
         MVC   12(1,R4),PARAS+4                                                 
         MVC   13(1,R4),PARAS+12                                                
*                                                                               
VSB60    MVC   DUB+2(4),WORK+30    GET END RANGE FOR QUARTER                    
         CLI   1(R3),0             IS THERE A SECOND EXPRESSION                 
         BE    VSB65                                                            
         ZIC   R0,11(R3)           YES - IT MUST BE ANOTHER YEAR                
         LTR   R0,R0                                                            
         BZ    SOFTNO                                                           
         CLC   22(2,R3),DUB        CHECK YEARS IN SEQ                           
         BL    SOFTNO                                                           
         MVC   DUB(2),22(R3)                                                    
*                                                                               
VSB65    CLI   13(R3),C'3'         CHECK 3RD QTR                                
         BNE   VSB70                                                            
         GOTO1 NETWEEK,PARAS,DUB,GETDAY,ADDAY                                   
         MVC   PODBLNQ+PODBBKS-PODBD(1,R4),PARAS+4                              
         MVC   PODBLNQ+PODBBKS+1-PODBD(1,R4),PARAS+12                           
         B     SOFTYES                                                          
*                                                                               
VSB70    GOTO1 GETBROAD,PARAS,(1,DUB),WORK,GETDAY,ADDAY                         
*GET END RANGE                                                                  
         GOTO1 NETWEEK,PARAS,WORK+6,GETDAY,ADDAY                                
         MVC   PODBLNQ+PODBBKS-PODBD(1,R4),PARAS+4                              
         MVC   PODBLNQ+PODBBKS+1-PODBD(1,R4),PARAS+12                           
         B     SOFTYES                                                          
*                                                                               
SOFTYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     OVXIT                                                            
         SPACE 1                                                                
*--IF ERROR CLEAR BOOK TABLE OF ALL ENTRIES CREATED IN THIS PASS                
SOFTNO   XC    0(PODBLNQ,R4),0(R4)                                              
         LA    R4,PODBLNQ(R4)                                                   
         CLI   1(R4),0                                                          
         BNE   SOFTNO                                                           
*                                                                               
         LA    R1,1                                                             
         LTR   R1,R1                                                            
         B     OVXIT                                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE A GENDER STACK EXPRESSION                     *         
*                    INPUT  R4=A(SCANNER BLOCK)                       *         
***********************************************************************         
*                                                                               
VALGSTCK ZIC   R1,1(R4)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         BZ    VALGNEX             NOTHING INPUT, ERROR                         
         LA    R1,22(R1,R4)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R4,22(R4)           (BUMP TO EXPRESSION)                         
*                                                                               
         LA    R3,PDSTADEF                                                      
         LA    R0,8                (MAX 8 TERMS)                                
*                                                                               
VALG00   LA    RF,GENTAB           GET GEN TABLE                                
*                                                                               
VALG02   CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    VALGNEX                                                          
         CLC   0(1,R4),0(RF)                                                    
         BE    VALG04                                                           
         LA    RF,1(RF)                                                         
         B     VALG02                                                           
*                                                                               
VALG04   MVC   1(1,R3),0(RF)       SAVE IN LIST                                 
         LA    R3,2(R3)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),C'/'          NEED A /                                     
         BNE   VALGNEX                                                          
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '          ANYTHING LEFT?                               
         BE    VALGEQX             NO, DONE                                     
         BCT   R0,VALG00                                                        
*                                                                               
VALGNEX  LTR   RB,RB                                                            
         B     OVXIT                                                            
*                                                                               
VALGEQX  MVI   0(R3),X'FF'                                                      
         CR    RB,RB                                                            
         B     OVXIT                                                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE A PROGRAM GROUP OR LIST OF PROGRAM GROUPS     *         
*              STORE LIST OF PROGRAMS IN PRODNTIFT                    *         
*                    INPUT  R4=A(SCANNER BLOCK)                       *         
***********************************************************************         
*                                                                               
VALPRGRP ZIC   R1,1(R4)            LENGTH OF EXPRESSIONS                        
         LTR   R1,R1                                                            
         BZ    VALPNEX             NOTHING INPUT, ERROR                         
*                                                                               
         LA    R1,22(R1,R4)                                                     
         MVI   0(R1),C'/'          DELIMIT LAST WITH A SLASH                    
         LA    R4,22(R4)           (BUMP TO EXPRESSION)                         
*                                                                               
         L     R3,APODPRGL                                                      
         LA    R0,10               MAXIMUM NUMBER OF GROUPS ALLOWED             
*                                                                               
VALP02   LR    R1,R4               GET LENGTH OF THIS EXPRESSION                
*                                                                               
VALP04   CLI   0(R1),C'/'                                                       
         BE    VALP06                                                           
         LA    R1,1(R1)                                                         
         B     VALP04                                                           
*                                                                               
VALP06   SR    R1,R4                                                            
         BZ    VALPNEX                                                          
         CHI   R1,8                                                             
         BH    VALPNEX                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)                                                    
         OC    0(8,R3),SPACES                                                   
         LA    R4,2(R4,R1)         GET TO NEXT PROGRAM GROUP                    
*                                                                               
         LA    R5,KEY              BUILD KEY                                    
         USING NPRGRECD,R5                                                      
         XC    NPRGKEY,NPRGKEY                                                  
         MVC   NPRGKTYP,=X'0D3C'                                                
         MVC   NPRGKAGM,PDBAGYMD                                                
         MVC   NPRGKCOD,0(R3)      GET PROGRAM GROUP CODE                       
*                                                                               
         MVC   DATADISP,=H'24'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     MAKE SURE WE FOUND THE GROUP                 
         BNE   VALPNEX                                                          
*                                                                               
         LA    R3,8(R3)            ALLOW FOR MAX LENGTH                         
         MVI   0(R3),X'FF'         MARK END OF TABLE                            
         CLI   0(R4),C' '          ANY MORE?                                    
         BNH   VALPEQX             NO                                           
         BCT   R0,VALP02                                                        
*                                                                               
VALPEQX  B     OVEQXIT                                                          
*                                                                               
VALPNEX  B     OVNEXIT                                                          
         EJECT                                                                  
OVNEXIT  LTR   RB,RB                                                            
         B     OVXIT                                                            
*                                                                               
OVEQXIT  CR    RB,RB                                                            
         B     OVXIT                                                            
*                                                                               
OVXIT    XMOD1 1                                                                
*                                                                               
*RELO2    DS    A                                                               
RELO2C   DC    A(*)                                                             
         DROP  R8,R9,RA,RB,RC                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
VCOLNTTB DS    0H                                                               
         DC    X'01',C'N1',C'S1'                                                
         DC    X'02',C'N2',C'S2'                                                
         DC    X'03',C'N3',C'S3'                                                
         DC    X'04',C'N4',C'S4'                                                
         DC    X'05',C'N5',C'S5'                                                
         DC    X'06',C'N6',C'S6'                                                
         DC    X'07',C'N7',C'S7'                                                
         DC    X'08',C'N8',C'S8'                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
VCOLBKTB DS    0H                                                               
         DC    X'01',C'B1'                                                      
         DC    X'02',C'B2'                                                      
         DC    X'03',C'B3'                                                      
         DC    X'04',C'B4'                                                      
         DC    X'05',C'B5'                                                      
         DC    X'06',C'B6'                                                      
         DC    X'07',C'B7'                                                      
         DC    X'08',C'B8'                                                      
         DC    X'FF'                                                            
         SPACE 2                                                                
VCOLDYTB DS    0H                                                               
         DC    X'00',C'M-F'                                                     
         DC    X'01',C'MON'                                                     
         DC    X'02',C'TUE'                                                     
         DC    X'03',C'WED'                                                     
         DC    X'04',C'THU'                                                     
         DC    X'05',C'FRI'                                                     
         DC    X'06',C'SAT'                                                     
         DC    X'07',C'SUN'                                                     
         DC    X'08',C'M-S'                                                     
         DC    X'FF'                                                            
         SPACE 2                                                                
VCOLDPTB DS    0H                                                               
         DC    C'E'                EARLY                                        
         DC    C'D'                DAY                                          
         DC    C'P'                PRIME                                        
         DC    C'L'                LATE                                         
         DC    C'S'                SATURDAY AM                                  
         DC    C'W'                WEEKEND                                      
         DC    C'N'                NEWS                                         
         DC    X'FF'                                                            
         SPACE 2                                                                
VCOLSCTB DS    0H                                                               
         DC    X'01',C'NAD  '                                                   
         DC    X'02',C'NAD-T'                                                   
         DC    X'03',C'NAD-D'                                                   
         DC    X'04',C'NTI  '                                                   
         DC    X'04',C'NTI-N'                                                   
         DC    X'05',C'NTI-T'                                                   
         DC    X'06',C'PIV  '                                                   
         DC    X'07',C'NSI-W'                                                   
         DC    X'08',C'MPA  '                                                   
         DC    X'08',C'MPA-N'                                                   
         DC    X'09',C'EMI  '                                                   
         DC    X'0A',C'NMI  '                                                   
         DC    X'0B',C'TP   '                                                   
         DC    X'0B',C'TP-N '                                                   
         DC    X'0C',C'PAV  '                                                   
         DC    X'0C',C'PAV-N'                                                   
         DC    X'0D',C'DPT  '                                                   
         DC    X'0D',C'DPT-N'                                                   
         DC    X'0E',C'MPA-A'                                                   
         DC    X'0F',C'TP-A '                                                   
         DC    X'10',C'PAV-A'                                                   
         DC    X'11',C'DPT-A'                                                   
         DC    X'12',C'CSI  '                                                   
         DC    X'13',C'BBM  '                                                   
         DC    X'14',C'T4   '                                                   
         DC    X'14',C'T4-N '                                                   
         DC    X'15',C'T4-A '                                                   
         DC    X'16',C'SRC  '                                                   
         DC    X'17',C'NHI-T'                                                   
         DC    X'17',C'NHT-H'                                                   
         DC    X'18',C'NHT  '                                                   
         DC    X'19',C'NHT-D'                                                   
         DC    X'1A',C'NHT-T'                                                   
         DC    X'1B',C'NSI  '                                                   
         DC    X'1C',C'ARB  '                                                   
         DC    X'1D',C'NHW-H'                                                   
         DC    X'1E',C'NHW  '                                                   
         DC    X'1F',C'T3   '                                                   
         DC    X'1F',C'T3-C '                                                   
         DC    X'20',C'T3-B '                                                   
         DC    X'21',C'IUN  '                                                   
         DC    X'22',C'TP-M '                                                   
         DC    X'22',C'MFX  '                                                   
         DC    X'23',C'PAV-M'                                                   
         DC    X'24',C'T4 -M'                                                   
         DC    X'25',C'IUN-S'                                                   
         DC    X'26',C'IUN-M'                                                   
         DC    X'27',C'NAW  '                                                   
         DC    X'28',C'NAW-D'                                                   
         DC    X'29',C'NAW-T'                                                   
         DC    X'2A',C'NHW-D'                                                   
         DC    X'2B',C'HWH-T'                                                   
         DC    X'2C',C'SQAD '                                                   
         DC    X'2D',C'TCAR '                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
FLIST    DS    0F                                                               
         DC    CL8'USPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    CL8'NDEMDIRA'                                                    
         DC    CL8'NDEMFILA'                                                    
         DC    CL8'NDEMDIRN'                                                    
         DC    CL8'NDEMFILN'                                                    
         DC    CL8'NDEMDIRO'                                                    
         DC    CL8'NDEMFILO'                                                    
         DC    CL8'X'                                                           
         SPACE 2                                                                
FLWNAD   DC    X'01',AL1(1,2,3,0)       NAD QUARTERS                            
         DC    X'02',AL1(4,5,6,0)                                               
         DC    X'03',AL1(7,8,9,0)                                               
         DC    X'04',AL1(10,11,12,0)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
*   CONSTANTS FOR DAY EDIT                                                      
*   BE CAREFUL SEQUENCE IS IMPORTANT BECAUSE WE ALLOW INPUT                     
*   TRUNCATION.  M=MONDAY NOT MTWTF                                             
*                                                                               
DAYLIST  DC    C'YYYYYYYYY',X'00'       DUMMY ENTRY                             
         DC    C'MONDAY   ',X'40'         DALL FOR WHOLE WEEK                   
         DC    C'MTWTF    ',X'FC'       INDIVIDUAL WEEKDAYS, USE                
         DC    C'TUESDAY  ',X'20'                                               
         DC    C'WEDNESDAY',X'10'                                               
         DC    C'THURSDAY ',X'08'                                               
         DC    C'FRIDAY   ',X'04'                                               
         DC    C'SATURDAY ',X'02'                                               
         DC    C'SUNDAY   ',X'01'                                               
         DC    C'YYYYYYYYY',X'00'       DUMMY ENTRY                             
         DC    C'VARIABLE ',X'FF'                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
DYTMMAC  DC    C'EARLY ',X'7C02BC03E80000000000',X'00'  M-F,7-10A               
         DC    C'DAY   ',X'7C03E8065E0000000000',X'00'  M-F,10A-430P            
         DC    C'PRIME ',X'FF07D008FC01076C07D0',X'05'  ALL,8-11P               
         DC    C'LATE  ',X'7C091A0ABC0000000000',X'00'  M-F,1130P-3A            
         DC    C'SATAM ',X'02032005140000000000',X'00'  SAT,8A-1P               
         DC    C'WKEND ',X'0202BC076C0102BC076C',X'05'  SAT,7A-7P               
*                                                       SUN,7A-7P               
         DC    C'NEWS  ',X'FF0708076C0000000000',X'00'  ALL,6-7P                
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        BOOKTAB                                                      *         
*                  BYTE 0-4 = EXTERNAL SOURCE CODE                    *         
*                  BYTE 5   = INTERNAL SOURCE                         *         
*                             N=NIELSEN, ARBITRON, MEDIAFAX           *         
*                  BYTE 6   = INTERNAL MEDIA                          *         
*                  BYTE 7-9 = FILE                                    *         
*                  BYTE 10  = MEDIA TYPE                              *         
*                  BYTE 11  = ROUTINE DISPLACEMENT                    *         
***********************************************************************         
*                                                                               
BOOKTAB  DC    CL5'NSI  ',CL1'N',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'ARB  ',CL1'A',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'MFX  ',CL1'M',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'CSI  ',CL1'N',CL1'C',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'BBM  ',CL1'A',CL1'C',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T3   ',CL1'N',CL1'C',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T3 -C',CL1'N',CL1'C',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T3 -B',CL1'A',CL1'C',CL3'TP ',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'SRC  ',CL1'S',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'NSI-W',CL1'N',CL1'W',CL3'WTP',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'NAD  ',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NAD-T',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NAD-D',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
*                                                                               
         DC    CL5'NAW  ',CL1'N',CL1'W',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NAW-T',CL1'N',CL1'W',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NAW-D',CL1'N',CL1'W',CL3'NAD',CL1'N',XL1'00'                 
*                                                                               
         DC    CL5'TCAR ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NTI  ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NTI-N',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NTI-T',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'PIV  ',CL1'N',CL1'V',CL3'EVN',CL1'N',XL1'00'                 
         DC    CL5'SQAD ',CL1'N',CL1'T',CL3'TP ',CL1'N',XL1'00'                 
*                                                                               
BOOKMPA  DC    CL5'MPA  ',CL1'N',CL1'P',CL3'MPA',CL1'T',XL1'00'                 
         DC    CL5'MPA-N',CL1'N',CL1'P',CL3'MPA',CL1'T',XL1'00'                 
         DC    CL5'MPA-A',CL1'A',CL1'P',CL3'MPA',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'EMI  ',CL1'N',CL1'V',CL3'EVN',CL1'N',XL1'01'                 
         DC    CL5'NMI  ',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'01'                 
         DC    CL5'EMI-A',CL1'N',CL1'V',CL3'EVN',CL1'N',XL1'01'                 
         DC    CL5'NMI-A',CL1'N',CL1'N',CL3'NTI',CL1'T',XL1'01'                 
*                                                                               
         DC    CL5'TP   ',CL1'N',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'TP -N',CL1'N',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'TP -A',CL1'A',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'TP -M',CL1'M',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'T4   ',CL1'N',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T4 -N',CL1'N',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T4 -A',CL1'A',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'T4 -M',CL1'M',CL1'T',CL3'TP ',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'INV  ',CL1'I',CL1'U',CL3'IUN',CL1'T',XL1'00'                 
         DC    CL5'PAV  ',CL1'N',CL1'T',CL3'PAV',CL1'T',XL1'00'                 
         DC    CL5'PAV-N',CL1'N',CL1'T',CL3'PAV',CL1'T',XL1'00'                 
         DC    CL5'PAV-A',CL1'A',CL1'T',CL3'PAV',CL1'T',XL1'00'                 
         DC    CL5'PAV-M',CL1'M',CL1'T',CL3'PAV',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'DPT  ',CL1'N',CL1'D',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'DPT-N',CL1'N',CL1'D',CL3'TP ',CL1'T',XL1'00'                 
         DC    CL5'DPT-A',CL1'A',CL1'D',CL3'TP ',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'NHI-T',CL1'N',CL1'N',CL3'NAD',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'NHT  ',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NHT-D',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
         DC    CL5'NHT-H',CL1'N',CL1'N',CL3'NAD',CL1'T',XL1'00'                 
         DC    CL5'NHT-T',CL1'N',CL1'N',CL3'NAD',CL1'N',XL1'00'                 
*                                                                               
         DC    CL5'NHW  ',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NHW-D',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NHW-H',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
         DC    CL5'NHW-T',CL1'N',CL1'W',CL3'NTI',CL1'T',XL1'00'                 
*                                                                               
         DC    CL5'IUN  ',CL1'N',CL1'U',CL3'IUN',CL1'T',XL1'00'                 
         DC    CL5'IUN-S',CL1'N',CL1'U',CL3'IUN',CL1'T',XL1'00'                 
         DC    CL5'IUN-M',CL1'N',CL1'U',CL3'IUN',CL1'T',XL1'00'                 
* NOTE: NHI-T VALIDATES AS NAD BUT READ NTI                                     
         DC    X'FF'                                                            
         EJECT                                                                  
* STACK TABLES AND VALUES                                                       
STKTABC  DC    A(STKTAB7),A(STKTAB6),A(STKTAB5),A(STKTAB4)                      
         DC    A(STKTAB3),A(STKTAB2),A(STKTABC1)                                
*                                                                               
STKTABLE DC    A(STKTAB7),A(STKTAB6),A(STKTAB5),A(STKTAB4)                      
         DC    A(STKTAB3),A(STKTAB2),A(STKTAB1)                                 
*                                                                               
STKTAB7  DC    AL1(15),C'PACIFIC',AL1(31),C'ANY-CBL',AL1(32),C'PAY-CBL'         
         DC    AL1(33),C'BAS-CBL',AL1(55),C'ANY6-11',AL1(56),C'ANY1217'         
         DC    AL1(71),C'$30+-NA',AL1(72),C'$30+POM',AL1(73),C'$30+COL'         
         DC    AL1(74),C'$40+-NA',AL1(75),C'$40+POM',AL1(76),C'$40+COL'         
         DC    AL1(05),C'SPANISH',AL1(06),C'ENGLISH',AL1(35),C'PAYCBL+'         
         DC    AL1(36),C'BASCBL+',AL1(38),C'ANYCBL+'                            
         DC    AL1(86),C'$50+/NA',AL1(87),C'$50+POM'                            
         DC    AL1(88),C'$50+COL',AL1(78),C'$50DUAL'                            
         DC    X'FF'                                                            
STKTAB6  DC    AL1(11),C'N-EAST',AL1(12),C'E-CENT',AL1(13),C'W-CENT'            
         DC    AL1(123),C'CS-C+D',AL1(34),C'NO-CBL',AL1(43),C'HHS-3+'           
         DC    AL1(44),C'HHS-4+',AL1(51),C'ANY18-',AL1(52),C'ANY12-'            
         DC    AL1(07),C'BILING',AL1(37),C'BRDCST',AL1(84),C'$60-75'            
         DC    X'FF'                                                            
STKTAB5  DC    AL1(234),C'SPACE',AL1(235),C'INDEX',AL1(14),C'SOUTH'             
         DC    AL1(41),C'HHS-1',AL1(42),C'HHS-2',AL1(53),C'ANY6-'               
         DC    AL1(54),C'ANY3-',AL1(61),C'$15M+',AL1(62),C'$20M+'               
         DC    AL1(63),C'$30M+',AL1(64),C'$40M+',AL1(65),C'$50M+'               
         DC    AL1(66),C'$60M+',AL1(67),C'$30M-',AL1(85),C'$75M+'               
         DC    AL1(241),C'SHARE',AL1(250),C'AVAIL',C'E',C'E-RTG'                
         DC    C'C',C'C-RTG'                                                    
         DC    X'FF'                                                            
STKTAB4  DC    AL1(236),C'DIFF',AL1(3),C'DIF/',AL1(21),C'CS-A'                  
         DC    AL1(22),C'CS-B',AL1(240),C'DEMO'                                 
         DC    X'FF'                                                            
STKTAB3  DC    AL1(1),C'USA',C'T',C'IMP',C'P',C'PUT'                            
         DC    C'R',C'RTG',C'S',C'SHR',C'V',C'VPH',C'E',C'ERT'                  
         DC    AL1(101),C'101',AL1(102),C'102',AL1(103),C'103'                  
         DC    AL1(104),C'104',AL1(105),C'105',AL1(106),C'106'                  
         DC    AL1(111),C'111',AL1(112),C'112',AL1(113),C'113'                  
         DC    AL1(114),C'114',AL1(115),C'115',AL1(121),C'121'                  
         DC    AL1(122),C'122',AL1(123),C'123',AL1(124),C'124'                  
         DC    AL1(125),C'125',AL1(131),C'131',AL1(132),C'132'                  
         DC    AL1(133),C'133',AL1(141),C'141',AL1(142),C'142'                  
         DC    AL1(151),C'151',AL1(152),C'152',AL1(153),C'153'                  
         DC    AL1(154),C'154',AL1(156),C'156',AL1(157),C'157'                  
         DC    AL1(158),C'158',AL1(159),C'159',AL1(161),C'161'                  
         DC    AL1(162),C'162',AL1(163),C'163',AL1(164),C'164'                  
         DC    AL1(165),C'165',AL1(166),C'166',AL1(242),C'HPT'                  
         DC    X'FF'                                                            
STKTAB2  DC    AL1(11),C'11',AL1(12),C'12',AL1(13),C'13'                        
         DC    AL1(14),C'14',AL1(15),C'15',AL1(21),C'21'                        
         DC    AL1(22),C'22',AL1(23),C'23',AL1(31),C'31'                        
         DC    AL1(32),C'32',AL1(33),C'33',AL1(34),C'34'                        
         DC    AL1(35),C'35',AL1(36),C'36',AL1(37),C'37'                        
         DC    AL1(38),C'38'                                                    
         DC    AL1(41),C'41',AL1(42),C'42',AL1(43),C'43'                        
         DC    AL1(44),C'44',AL1(51),C'51',AL1(52),C'52'                        
         DC    AL1(53),C'53',AL1(54),C'54',AL1(55),C'55'                        
         DC    AL1(56),C'56',AL1(61),C'61',AL1(62),C'62'                        
         DC    AL1(63),C'63',AL1(64),C'64',AL1(65),C'65'                        
         DC    AL1(66),C'66',AL1(67),C'67',AL1(71),C'71'                        
         DC    AL1(72),C'72',AL1(73),C'73',AL1(74),C'74'                        
         DC    AL1(75),C'75',AL1(76),C'76',AL1(77),C'77'                        
         DC    AL1(78),C'78'                                                    
         DC    AL1(80),C'80',AL1(81),C'81',AL1(82),C'82'                        
         DC    AL1(83),C'83',AL1(85),C'85',AL1(87),C'87'                        
         DC    AL1(88),C'88'                                                    
         DC    AL1(91),C'91',AL1(92),C'92',AL1(93),C'93'                        
*                                                                               
         DC    AL1(16),C'16',AL1(17),C'17',AL1(45),C'45'                        
         DC    X'FF'                                                            
STKTAB1  DC    C'TI',C'PP',C'RR',C'SS',C'VV',C'DD',C'UU',C'CC'                  
         DC    C'QQ',C'OO',C'LL',C'MM',C'NN',C'AA',C'XT',C'EE'                  
         DC    X'01',C'1',X'05',C'5',X'06',C'6',X'07',C'7'                      
         DC    X'FF'                                                            
*        SPECIAL TABLE FOR CANADA                                               
STKTABC1 DC    C'II',C'PP',C'RR',C'SS',C'VV',C'DD',C'UU'                        
         DC    C'QQ',C'OO',C'LL',C'MM',C'NN',C'AA',C'TT',C'EE'                  
         DC    X'01',C'1',X'05',C'5',X'06',C'6',X'07',C'7'                      
         DC    X'FF'                                                            
         EJECT                                                                  
HUTTABLE DC    AL1(3),C'Q1/ ',C'01150315'        QUARTERS                       
         DC    AL1(3),C'Q2/ ',C'04150615'                                       
         DC    AL1(3),C'Q3/ ',C'07150911'                                       
         DC    AL1(3),C'Q4/ ',C'09111215'                                       
*                                                                               
         DC    AL1(3),C'M1/ ',C'0115    '        MONTHS                         
         DC    AL1(3),C'M2/ ',C'0215    '                                       
         DC    AL1(3),C'M3/ ',C'0315    '                                       
         DC    AL1(3),C'M4/ ',C'0415    '                                       
         DC    AL1(3),C'M5/ ',C'0515    '                                       
         DC    AL1(3),C'M6/ ',C'0615    '                                       
         DC    AL1(3),C'M7/ ',C'0715    '                                       
         DC    AL1(3),C'M8/ ',C'0815    '                                       
         DC    AL1(3),C'M9/ ',C'0915    '                                       
         DC    AL1(4),C'M10/',C'1015    '                                       
         DC    AL1(4),C'M11/',C'1115    '                                       
         DC    AL1(4),C'M12/',C'1215    '                                       
*                                                                               
         DC    AL1(3),C'JAN ',C'0115    '        MONTHS                         
         DC    AL1(3),C'FEB ',C'0215    '                                       
         DC    AL1(3),C'MAR ',C'0315    '                                       
         DC    AL1(3),C'APR ',C'0415    '                                       
         DC    AL1(3),C'MAY ',C'0515    '                                       
         DC    AL1(3),C'JUN ',C'0615    '                                       
         DC    AL1(3),C'JUL ',C'0715    '                                       
         DC    AL1(3),C'AUG ',C'0815    '                                       
         DC    AL1(3),C'SEP ',C'0915    '                                       
         DC    AL1(3),C'OCT ',C'1015    '                                       
         DC    AL1(3),C'NOV ',C'1115    '                                       
         DC    AL1(3),C'DEC ',C'1215    '                                       
*                                                                               
         DC    AL1(3),C'01/ ',C'0115    '        MONTHS                         
         DC    AL1(3),C'02/ ',C'0215    '                                       
         DC    AL1(3),C'03/ ',C'0315    '                                       
         DC    AL1(3),C'04/ ',C'0415    '                                       
         DC    AL1(3),C'05/ ',C'0515    '                                       
         DC    AL1(3),C'06/ ',C'0615    '                                       
         DC    AL1(3),C'07/ ',C'0715    '                                       
         DC    AL1(3),C'08/ ',C'0815    '                                       
         DC    AL1(3),C'09/ ',C'0915    '                                       
         DC    AL1(3),C'10/ ',C'1015    '                                       
         DC    AL1(3),C'11/ ',C'1115    '                                       
         DC    AL1(3),C'12/ ',C'1215    '                                       
*                                                                               
         DC    AL1(2),C'1/  ',C'0115    '        MONTHS                         
         DC    AL1(2),C'2/  ',C'0215    '                                       
         DC    AL1(2),C'3/  ',C'0315    '                                       
         DC    AL1(2),C'4/  ',C'0415    '                                       
         DC    AL1(2),C'5/  ',C'0515    '                                       
         DC    AL1(2),C'6/  ',C'0615    '                                       
         DC    AL1(2),C'7/  ',C'0715    '                                       
         DC    AL1(2),C'8/  ',C'0815    '                                       
         DC    AL1(2),C'9/  ',C'0915    '                                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              LIST OF VALID EXPRESSIONS FOR GENDER STACK             *         
***********************************************************************         
*                                                                               
GENTAB   DS    0CL1                                                             
         DC    C'L'                LISTEN                                       
         DC    C'G'                GIRLS                                        
         DC    C'F'                FEMALE                                       
         DC    C'W'                WOMEN                                        
         DC    C'B'                BOYS                                         
         DC    C'M'                MEN                                          
         DC    C'V'                VIEWER                                       
         DC    C'A'                ADULTS                                       
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DENADMON                                                       
         EJECT                                                                  
       ++INCLUDE NEPODPREC                                                      
         EJECT                                                                  
       ++INCLUDE DENTIBKEQU                                                     
         EJECT                                                                  
       ++INCLUDE DENTIQRT                                                       
         EJECT                                                                  
NEWTAB   DS    0H                  PUT NEW TABLES HERE                          
DBFUNTAB DC    CL5'NAD  '                                                       
         DC    CL5'NAD-D'                                                       
         DC    CL5'NAD-T'                                                       
*                                                                               
         DC    CL5'NAW  '                                                       
         DC    CL5'NAW-D'                                                       
         DC    CL5'NAW-T'                                                       
*                                                                               
         DC    CL5'TCAR '                                                       
         DC    CL5'NTI  '                                                       
         DC    CL5'NTI-N'                                                       
         DC    CL5'NTI-T'                                                       
*                                                                               
         DC    CL5'MPA  '                                                       
*                                                                               
         DC    CL5'NMI  '                                                       
*                                                                               
         DC    CL5'NHI-T'                                                       
*                                                                               
         DC    CL5'NHT  '                                                       
         DC    CL5'NHT-D'                                                       
         DC    CL5'NHT-H'                                                       
         DC    CL5'NHT-T'                                                       
*                                                                               
         DC    CL5'NHW  '                                                       
         DC    CL5'NHW-D'                                                       
         DC    CL5'NHW-H'                                                       
         DC    CL5'NHW-T'                                                       
*                                                                               
         DC    X'FF'                                                            
         SPACE 2                                                                
DBNADTAB DC    CL7'NAD  PN'                                                     
         DC    CL7'NAD-DDN'                                                     
         DC    CL7'NAD-TTN'                                                     
*                                                                               
         DC    CL7'NAW  PN'                                                     
         DC    CL7'NAW-DDN'                                                     
         DC    CL7'NAW-TTN'                                                     
*                                                                               
         DC    CL7'NHT  PN'                                                     
         DC    CL7'NHT-DDN'                                                     
         DC    CL7'NHT-TTN'                                                     
*                                                                               
         DC    CL7'NHW  PN'                                                     
         DC    CL7'NHW-DDN'                                                     
         DC    CL7'NHW-TTN'                                                     
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE NEPODWORK                                                      
         EJECT                                                                  
       ++INCLUDE NEPODBOOK                                                      
         EJECT                                                                  
*DDCOREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE NEPODFFD                                                       
         ORG CONTAGH                                                            
       ++INCLUDE NEPODE0D                                                       
         EJECT                                                                  
         PRINT OFF                                                              
*DDGENTWA                                                                       
       ++INCLUDE DDGENTWA                                                       
RSETD    DSECT                                                                  
       ++INCLUDE REGENSET                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENPRG                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENMKG                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
SPEDICTD DSECT                                                                  
       ++INCLUDE SPEDICT                                                        
*CTGENFILE                                                                      
       ++INCLUDE CTGENFILE                                                      
*FAFACTS                                                                        
       ++INCLUDE FAFACTS                                                        
*FATIOB                                                                         
       ++INCLUDE FATIOB                                                         
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
*DRGLOBAL                                                                       
       ++INCLUDE DRGLOBAL                                                       
*DDBIGBOX                                                                       
       ++INCLUDE DDBIGBOX                                                       
*DDWIDED                                                                        
       ++INCLUDE DDWIDED                                                        
*DDOFFICED                                                                      
       ++INCLUDE DDOFFICED                                                      
*DDMASTD                                                                        
       ++INCLUDE DDMASTD                                                        
*DRONEBLKHD                                                                     
       ++INCLUDE DRONEBLKHD                                                     
       ++INCLUDE NEGETHUTD                                                      
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
         PRINT ON                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE NEGENPRG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         EJECT                                                                  
PRECTDST DSECT                                                                  
PRECSRCE DS    CL3                                                              
         DS    CL1                 FOR ALIGNMENT                                
PRECADDR DS    F                                                                
         EJECT                                                                  
ENTRYTBD DSECT                     DSECT IS LOCAL TO NEPODGEN                   
ETENTRY  DS    CL(L'DRENTRYI)                                                   
ETCOLROW DS    CL1                 COL/ROW INDICATOR                            
ETSPARE  DS    XL1                 SPARE                                        
ENTRYTBQ EQU   *-ENTRYTBD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010NEPODGENA 05/01/02'                                      
         END                                                                    
