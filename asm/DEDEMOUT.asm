*          DATA SET DEDEMOUT   AT LEVEL 209 AS OF 05/26/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE T00ADFC                                                                  
         TITLE 'DEMOUT - EXTRACT VALUES FROM A DEMO RECORD'                     
************************UPDATE LOG*************************************         
* 01/03/05   163   ADD DEMO ENGINE OPTION BASED ON NEW FORMULA TABLES           
* 11/14/00   081   PASS DATA BACK AS 2 DECIMAL PLACE                            
* 06/16/98   045   SET UP FOR TVQ PROCESSING                                    
*                  (ZERO VALUES INHIBIT CALCULATIONS)                           
*                                                                               
* 09/06/96   001   RESET LEVEL NUMBERS                                          
*                  OVERRIDES FOR IMP BASE REP SYSTEM                            
*                                                                               
* 06/09/94   037   SPEED UP DEMO CALCULATIONS FOR CABLE AND NHTI                
*                  (SEE CODE IN DEMGET -> FSTDEM)                               
*                                                                               
* 04/20/94   097   HANDLE NHTI UNIVERSE RECORDS                                 
*                  (SEE CODE A DEMGET3)                                         
*                                                                               
*                                                                               
* 02/03/94   084   DOUBLE CHECK THAT DMASTER TABLE HAS BEEN BUILT               
*                  (SEE CODE AT DEMO3)                                          
*                                                                               
* 06/03/93   043   CHANGE ZERO VPH SUPPRESSION TO WORK ONLY ABOVE               
*                  LEVEL 1 DEMOS.                                               
*                                                                               
* 11/25/92   041   SUPPORT OPTIONAL ZERO VPH SUPPRESSION                        
*                                                                               
* 11/02/92         SUPPORT PACKED AND 4CHAR DEMO LISTS                          
*                                                                               
* 10/02/92         SOFT CODE ARB CONDENSED MARKET LOGIC                         
*                  (CONDENSED MARKET IND. NOW ON FILE).                         
*                                                                               
* 09/27/92         MAJOR REVISION - LEVEL NUMBERS RESET                         
*                  EXPAND MASTER DEMO CODE TO 3 BYTES                           
*                  EXPAND FORMULA DISPLACEMENTS TO 3 BYTES                      
*                  ADD 1 ADDITIONAL BYTE FOR CONTROL INFO.                      
*                                                                               
* 01/24/92         NAD VPH FOR NTI/NAD POSTING                                  
*                    ALLOWS MERGING OF NTI WEEKLY HOMES WITH NAD VPH            
*                                                                               
* 08/08/91         NAD DEFAULT CHECKING FOR NTI/NAD POSTING                     
*                      (NTI DEMOS AND NAD OVERRIDES)                            
*                                                                               
* 08/07/91         MERGE NTI DEMO OVERRIDES WITH NETWORK ACTUALS                
*                                                                               
* 03/12/91         SUPPORT NEW STYLE OVERRIDE ELEMENT                           
*                    SUPPORTS SYSTEM SPECIFIC FLAGS AND VARIABLE                
*                    PRECISION                                                  
*                                                                               
* 02/22/91         DAYPART ADJUSTMENT FACTORS                                   
*                                                                               
* 01/25/91         NETWORK OVERRIDE PRECISION ADJUSTMENT                        
*                                                                               
* 09/25/90         CALL SPECIFIC PRECISION ADJUSTMENT                           
*                                                                               
* 07/26/90         SUPPORT FOR RADIO TSL CALCULATION                            
*                    SEE DEMCALC DEMO T255=DBFACTOR                             
*                                                                               
* 07/20/90         SUPPORT FOR EXTENSIONS                                       
*                    *RADI*                                                     
*                                                                               
* 03/27/89         EQUATE RATING=PUT FOR NTI HUT STATION                        
*                                                                               
* 01/06/89         SUPPORT REACH FIGURES FOR NTI (FALSE DEMO)                   
*                                                                               
* 10/31/88         SUPPORT GAA VALUES FOR NTI                                   
*                                                                               
* 05/17/88         CALCULATE BASE CELLS FOR ESTIMATED RECS.                     
*                                                                               
* 10/07/87         SUPPORT FOR NETWORK ESTIMATED TO ACTUAL RECS.                
*                                                                               
* 09/18/87         AUTHORIZATION FOR NTI PUT/SHARE DATA                         
*                                                                               
**************************************************************                  
         EJECT                                                                  
         MACRO                                                                  
.* TO CATCH THE PROBLEM WHERE FIELD DEMOUTRD IS PRESUMABLY BEING                
.* BLOWN AWAY                                                                   
         TRAPIT                                                                 
         CLI   DEMOUTRD+1,0                                                     
         JNE   *+8                                                              
         NOPR  R0                                                               
         DC    H'0'                                                             
*********CLI   DEMMODC,C' '                                                     
*********JH    *+8                                                              
*********NOPR  R0                                                               
*********DC    H'0'                                                             
         MEND                                                                   
         SPACE 2                                                                
DEMOUT   RSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 DEMOUTL,**DEMOUT,RA,R8,RR=RE,CLEAR=YES                           
         LR    R9,RC                                                            
         USING DEMOUTD,R9          R9=A(DEMOUT LOCAL W/S)                       
         ST    RE,RELO                                                          
         ST    RD,DEMOUTRD         BUILD DEMCALC INTERFACE BLOCK                
         TRAPIT                                                                 
         ST    R1,APARM                                                         
         MVC   LISTTYPE,0(R1)                                                   
         LM    R2,R4,0(R1)         R2=A(CMD),R3=A(DBLOCK),R4=A(OUTPUT)          
         LA    R2,0(R2)                                                         
         USING DBLOCKD,R3          SET & EXTRACT DBLOCK VALUES                  
*                                                                               
         BRAS  RE,INIT             PERFORM INITIALISATION                       
*                                                                               
         CLI   LISTTYPE,LISTDEMO   TEST IF SINGLE DEMO PASSED                   
         JNE   DEMO02                                                           
         MVI   LISTTYPE,LISTMULT                                                
         MVC   DUMMYLST,0(R2)      BUILD SINGLE ENTRY LIST AT DUMMYLST          
         MVI   DUMMYLST+4,EOT      3 OR 4 BYTE LENGTH                           
         CLI   DBDEMTYP,C'4'                                                    
         JE    *+8                                                              
         MVI   DUMMYLST+3,EOT                                                   
         LA    R2,DUMMYLST                                                      
*                                                                               
DEMO02   MVI   OVERELEM,OVELQ                                                   
         MVI   BOOKELEM,BKELQ                                                   
         MVI   QTHRELEM,QHCODEQ                                                 
         OC    DBACTBK,DBACTBK     PRESET BOOK FOR TABLES                       
         JNZ   *+10                                                             
         MVC   DBACTBK,DBSELBK                                                  
         CLC   DBFILE,=AL3(EVNFILE)                                             
         JNE   *+12                                                             
         MVI   OVERELEM,X'DD'                                                   
         MVI   BOOKELEM,X'5D'                                                   
*                                                                               
         MVI   NEIDELEM,0                                                       
         MVI   FCOVELEM,0                                                       
         L     RE,AQTHREL          PROCESS SPECIAL RECORD ELEMENTS              
         XR    R0,R0                                                            
DEMO04   CLI   0(RE),0             TEST E-O-R                                   
         JE    DEMO22                                                           
         CLI   1(RE),0             PROTECT AGAINST BAD INPUT                    
         JNE   *+12                                                             
         MVI   0(RE),0             IT'S BAD SO TRY TO FIX IT                    
         J     DEMO22                                                           
*                                                                               
         CLI   DBSELMED,C'C'       CANADIAN?                                    
         JNE   DEMV05                                                           
         CLI   0(RE),X'35'         BBM FULL COVERAGE ELEMENT                    
         JNE   DEMV05                                                           
         MVI   FCOVELEM,X'35'                                                   
*                                                                               
DEMV05   CLI   0(RE),X'41'         NETWORK PAV ID ELEMENT                       
         JNE   DEMV06                                                           
         ST    RE,ANEIDELM                                                      
         MVI   NEIDELEM,X'41'                                                   
****     OC    3(12,RE),3(RE)                                                   
****     JNZ   *+8                                                              
****     MVI   NEIDELEM,0                                                       
*                                                                               
DEMV06   CLC   0(1,RE),OVERELEM    HANDLE OVERRIDE ELEMENTS                     
         JNE   DEMO10                                                           
         CLI   2(RE),NDXDEMO       TEST THIS IS AN INDEX ELEMENT                
         JE    DEMV08                                                           
         TM    DEMOFLAG,OVERPRES   NO - TEST FIRST OVERRIDE ELEMENT             
         JNZ   DEMO18                                                           
         ST    RE,AOVEREL          SAVE A(FIRST OVERRIDE ELEMENT)               
         OI    DEMOFLAG,OVERPRES                                                
*                                                                               
         STM   RE,RF,DUB           SAVE THE REGISTERS                           
         BRAS  RE,FIXNADO          FUDGE NAD HOMES                              
         LM    RE,RF,DUB           RESTORE THE REGISTERS                        
         J     DEMO18                                                           
*                                                                               
DEMV08   XR    RF,RF                                                            
         ICM   RF,3,4(RE)          GET INDEX VALUE                              
         CLI   1(RE),6             TEST FOR 6 BYTE ELEMENT                      
         JE    *+8                 YES-INDEX IS HALFWORD                        
         ICM   RF,7,4(RE)          NO-7 BYTE ELEM/3 BYTE INDEX                  
         LTR   RF,RF               TEST FOR ZERO INDEX                          
         JZ    DEMO18              YES-IGNORE IT                                
         TM    DEMOFLAG,INDXPRES   TEST FIRST INDEX ELEMENT                     
         JNZ   DEMO18                                                           
         ST    RE,AINDXEL          SAVE A(FIRST INDEX ELEMENT)                  
         OI    DEMOFLAG,INDXPRES                                                
         J     DEMO18                                                           
*                                                                               
DEMO10   CLI   0(RE),UPGEL         NEW UPGRADE ELEMENT?                         
         JNE   DEMO12                                                           
         CLI   4(RE),UPGMIN        RAVLNTYP=X'07'                               
         JNE   DEMO18                                                           
         OC    6(2,RE),6(RE)       IS THERE A VALUE?                            
         JZ    DEMO18                                                           
         TM    DEMOFLAG,MINVPRES                                                
         JO    DEMO18                                                           
         OI    DEMOFLAG,MINVPRES   MIN VALUE ON UPG ELEM FND                    
         ST    RE,AUPGREL          SAVE A(1ST UPGRADE ELEM)                     
         CLC   DBFILE(3),=C'INV'                                                
         JNE   *+8                                                              
         MVI   MININV,C'Y'         MIN VALUE AND INV RECD                       
         J     DEMO18                                                           
*                                                                               
DEMO12   CLC   0(1,RE),BOOKELEM    HANDLE BOOK ELEMENTS                         
         JNE   DEMO18                                                           
         TM    DEMOFLAG,BOOKPRES                                                
         JNZ   DEMO18                                                           
         OI    DEMOFLAG,BOOKPRES                                                
         LR    R1,RE                                                            
         CLI   1(RE),4             TEST IF COMPRESSED BOOK ELEMENT              
         JNE   DEMO14                                                           
         XR    R1,R1               YES - POINT TO UNCOMPRESSED ELEMENT          
         ICM   R1,3,2(RE)                                                       
         SLL   R1,17                                                            
         SRL   R1,17                                                            
         A     R1,ARECORD                                                       
*                                                                               
DEMO14   MVC   DBINTFIL(2),2(R1)   SET INTERNAL VALUES FROM ELEMENT             
         MVC   DBACTSRC,4(R1)                                                   
         CLI   DBSELMED,C'W'                                                    
         JE    DEMO16                                                           
         CLC   DBSELBK,=AL1(YR_1994,WEEK_37)  NTI FIX FOR 95 UNIV               
         JNE   *+10                                                             
         MVC   5(2,R1),=AL1(YR_1994,WEEK_37)                                    
         CLC   DBSELBK,=AL1(YR_1994,WEEK_38)  NTI FIX FOR 95 UNIV               
         JNE   *+10                                                             
         MVC   5(2,R1),=AL1(YR_1994,WEEK_38)                                    
         CLC   DBSELBK,=AL1(YR_1989,WEEK_53)  NTI FIX FOR 89 UNIV               
         JNE   *+10                                                             
         MVC   5(2,R1),=AL1(YR_1989,WEEK_33)                                    
         CLC   DBSELBK,=AL1(YR_1991,WEEK_51)  NTI FIX FOR 91 UNIV               
         JNE   *+10                                                             
         MVC   5(2,R1),=AL1(YR_1991,WEEK_33)                                    
         CLI   DBINTFIL,C'C'                                                    
         JNE   DEMO16                                                           
         CLC   DBSELBK,=AL1(YR_1993,WEEK_49)                                    
         JL    DEMO16                                                           
         CLC   DBSELBK,=AL1(YR_1993,WEEK_51)                                    
         JH    DEMO16                                                           
         MVC   5(2,R1),=AL1(YR_1993,WEEK_36)                                    
*                                                                               
DEMO16   MVC   DBACTBK,5(R1)                                                    
         CLC   2(3,R1),=C'PNN'     NETWORK POCKETPIECE FILE                     
         BNE   DEMO18                                                           
         CLC   DBACTBK,=AL1(YR_1987,WEEK_33)  NOW IMP BASED                     
         BH    DEMO18                WAS VPH BASED                              
         ICM   RF,15,ANEIDELM                                                   
         BZ    DEMO18                                                           
         OC    3(12,RF),3(RF)      FUDGE FORMULAS FOR NO VPH CONDITION          
         BNZ   *+8                                                              
         MVI   NEIDELEM,0                                                       
*                                                                               
DEMO18   IC    R0,1(RE)            BUMP TO NEXT ELEMENT                         
         AR    RE,R0                                                            
         J     DEMO04                                                           
*                                                                               
*                                                                               
* MISSING BOOK ELEMENT: FIGURE OUT DBINTFIL FROM OTHER DBLOCK FIELDS            
*                                                                               
DEMO22   DS    0H                  SET FLAG IF SPT-NET LK UP                    
         MVI   SPNETFLG,0                                                       
         XC    SPNETMST,SPNETMST                                                
         CLI   DBSELMED,C'N'                                                    
         JNE   DEMO25                                                           
         CLC   DBFILE,=C'TP '                                                   
         JE    *+10                                                             
         CLC   DBFILE,=C'PAV'                                                   
         JNE   DEMO25                                                           
         LAY   RE,SPNETBL          DETERMINE WHICH FILE LK UP IS FOR            
DEMO23   CLI   0(RE),EOT                                                        
         JE    DEMO25              NOT A SPT/NET LK UP                          
         CLC   DBSELSRC,0(RE)                                                   
         JE    *+12                                                             
         LA    RE,L'SPNETBL(RE)                                                 
         J     DEMO23                                                           
         MVC   SPNETFLG,1(RE)                                                   
         MVC   SPNETMST,2(RE)                                                   
         OC    DBINTFIL,DBINTFIL   ALREADY SET                                  
         BNZ   DEMO30                                                           
         MVC   DBINTFIL,5(RE)                                                   
         MVC   DUB(3),SPNETMST                                                  
         B     DEMO32                                                           
*                                                                               
DEMO25   DS    0H                  NOT A SPT-NET FUDGE                          
         OC    DBINTFIL(2),DBINTFIL SET INTERNAL VALUES FROM DBLOCK             
         JNZ   DEMO30                                                           
         MVC   DUB(3),DBFILE       SET INTERNAL FILE & MEDIA CODES              
         MVC   DUB+3(1),DBACTMED                                                
         CLI   DBACTMED,0                                                       
         JNE   *+10                                                             
         MVC   DUB+3(1),DBSELMED                                                
*                                                                               
         L     RE,=A(DEMTABCL)     A(LIST OF TABLES IN PROGRAM)                 
         A     RE,RELO                                                          
         L     RE,TABFORM-DEMTABCL(RE)     RE=A(FILE/MEDIA TABLE)               
         LAM   ARE,ARE,ALET                                                     
         SAC   512                                                              
*                                                                               
DEMO26   CLI   0(RE),EOT           TEST E-O-L                                   
         JNE   DEMO28                                                           
         MVI   DBERROR,INVFM                                                    
         SAC   0                                                                
         LAM   ARE,ARE,=F'0'                                                    
         B     DEMOX                                                            
*                                                                               
DEMO28   CLC   DUB(4),0(RE)                                                     
         JE    *+12                                                             
         AHI   RE,6                                                             
         J     DEMO26                                                           
*                                                                               
         MVC   DBINTFIL(2),4(RE)   SET INTERNAL CODES                           
         SAC   0                                                                
         LAM   ARE,ARE,=F'0'                                                    
*                                                                               
DEMO30   CLC   DBFILE,=C'NTI'      NTI      FORCE 'NN' FOR CNAD                 
         JNE   *+10                                                             
         CLC   =C'PC',DBINTFIL     CABLE                                        
         JNE   *+8                                                              
         CLI   DBBTYPE,C'N'        NAD                                          
         JNE   *+10                                                             
         MVC   DBINTFIL(2),=C'NN'                                               
*                                                                               
         MVC   DUB+0(2),DBINTFIL   SET FILE/SOURCE/BOOK FROM DBLOCK             
         MVI   DUB+2,0                                                          
         OC    DUB+2(1),DBACTSRC                                                
         JNZ   *+10                                                             
         OC    DUB+2(1),DBSELSRC                                                
         JNZ   *+8                                                              
         MVI   DUB+2,C'N'                                                       
*                                                                               
         CLI   DBSELMED,C'N'       NETWORK IUN RECD?                            
         JNE   DEMO32                                                           
         CLC   DUB(3),=C'IUN'      IUN FILE?                                    
         JE    DEMO32                                                           
         CLC   DUB(3),=C'IUH'      IUN BUILT FROM NHT FILE?                     
         JE    *+10                                                             
         CLC   DUB(3),=C'IUD'      IUN BUILT FROM NAD FILE?                     
         JE    *+10                                                             
         CLC   DUB(3),=C'IUK'      IUN BUILT FROM NTI FILE?                     
         JE    *+10                                                             
         CLC   DUB(3),=C'IUC'      IUN BUILT FROM CABLE FILE?                   
         JNE   *+12                                                             
         MVI   DUB+2,C'N'          SET TO IUN                                   
         J     DEMO32                                                           
*                                                                               
         CLI   SPNETFLG,0          SPOT TO NETWORK LK UP?                       
         JE    *+10                                                             
         MVC   DUB(3),SPNETMST     OVVERIDE DUB WITH CORRECT FILE               
*                                                                               
DEMO32   OC    BOOK,DBACTBK                                                     
         JNZ   *+10                                                             
         OC    BOOK,DBSELBK                                                     
         JZ    *+10                                                             
         XC    BOOK,=X'FFFF'                                                    
*                                         SET UP PRECISION OPTIONS              
         L     RE,DBEXTEND                                                      
         USING DBEXTRAD,RE                                                      
         LR    R0,RE                                                            
         XR    RF,RF                                                            
         ST    RF,PRECLIST                                                      
*                                                                               
DEMO34   LTR   RE,RE                                                            
         JZ    DEMO48                                                           
         CLC   DBINTFIL(2),=AL3(IUNFILE)  REP                                   
         JNE   DEMO36                                                           
         CLC   DBRID,=C'SPOT'                                                   
         JNE   DEMO40                                                           
         USING DBXTTID,RE                                                       
         MVC   RTGADJ,DBXTSCTL                                                  
         CLI   RTGADJ,C'0'                                                      
         BE    DEMO340                                                          
         OC    DBXTTRP(3),DBXTTRP                                               
         JZ    DEMO39                                                           
         LAY   RF,SPOTADJ                                                       
         J     DEMO39                                                           
*                                                                               
DEMO340  MVC   DBXTTRP(3),=X'808043'                                            
         LAY   RF,SPOTADJ2                                                      
         J     DEMO39                                                           
*                                                                               
DEMO36   DS    0C                                                               
*                                  CHECK FOR NON CABLE NETFILES                 
         CLC   DBINTFIL(2),=AL2(NTIFILE)  NETWORK                               
         JE    *+10                                                             
         CLC   DBINTFIL(2),=AL3(EINFILE)  ESTIMATED IMPS NETWORK                
         JE    *+10                                                             
         CLC   DBINTFIL(2),=AL2(NHIFILE)  NTI HISPANIC NETWORK                  
         JE    *+10                                                             
         CLC   DBINTFIL(2),=AL2(NADFILE)  NAD                                   
         JE    *+10                                                             
         CLC   DBINTFIL(2),=AL2(MVGFILE)  MOVIE GOER                            
         JE    *+10                                                             
         CLC   DBINTFIL(2),=AL3(EVNFILE)  ESTIMATED NETWORK                     
         JNE   DEMO38                                                           
*                                                                               
         USING DBEXTRAD,RE                                                      
*                                                                               
         CLC   DBRID,=C'UFIL'      NETWORK USER FILE                            
         JE    DEMO42                                                           
*                                                                               
         CLC   DBRID,=C'NETW'                                                   
         JNE   DEMO40                                                           
         LR    R1,RE                                                            
         USING DBXNTID,R1                                                       
*                                                                               
         CLC   DBINTFIL(2),=AL3(EVNFILE)  ESTIMATED NETWORK                     
         JNE   *+10                                                             
         MVC   ZAPZERO,DBX0EVPH    ZAP 0 CELL CALCULATION OPTION                
*                                                                               
         MVC   NETWPREC(4),=X'81814381'   DEFAULT NET R/P/I/S                   
         CLC   DBINTFIL(2),=AL2(NADFILE)  NAD FORMS RETURN 10000                
*----->  JNE   *+10      <----------------DISABLE FOR NOW                       
         J     *+10                                                             
         LAY   RF,NTIADJ                  ALWAYS FORCE ADJUSTMENT               
         LA    RE,NETWPREC                                                      
         CLI   DBXNHOPT,C'Y'                                                    
         JNE   *+14                                                             
         LAY   RF,NTIADJ                                                        
         MVI   NETWPREC+2,X'42'                                                 
         CLI   DBXNHOPT,C'H'         NETVALUE SPECIAL                           
         JNE   *+14                                                             
         LAY   RF,NTIADJ                                                        
         MVI   NETWPREC+2,X'42'                                                 
*                                                                               
         CLI   DBXNNR2,C'Y'                                                     
         JNE   DEMO39                                                           
         LAY   RF,NTIADJ                                                        
         MVC   NETWPREC(2),=X'8282'  2 DECIMAL RATING/HUT/PUT                   
         MVI   NETWPREC+3,X'81'      1 DECIMAL SHARE                            
         CLI   DBXNNP1,C'Y'          1 DECIMAL HUT/PUT                          
         BNE   *+8                                                              
         MVI   NETWPREC+1,X'81'                                                 
         CLI   DBXNNS1,C'Y'          1 DECIMAL SHARE                            
         BNE   *+8                                                              
         MVI   NETWPREC+3,X'81'                                                 
         CLI   DBXNHOPT,C'Y'                                                    
         BNE   *+8                                                              
         MVI   NETWPREC+3,X'82'      2 DECIMAL SHARE                            
*                                                                               
         J     DEMO39                                                           
*                                                                               
         USING DBEXTRAD,RE                                                      
DEMO38   CLC   DBINTFIL(2),=AL2(CABFILE)  CABLE                                 
         JE    *+14                                                             
         CLC   DBINTFIL(2),=AL2(CHNFILE)  CABLE NHTI FILE                       
         JNE   DEMO48                                                           
         CLC   DBRID,=C'UFIL'             NETWORK USER FILE                     
         JE    DEMO42                                                           
         CLC   DBRID,=C'NETW'                                                   
         JNE   DEMO40                                                           
         LR    R1,RE                                                            
         USING DBXNTID,R1                                                       
         MVC   NETWPREC(4),=X'81814381'   DEFAULT CAB R/P/I/S                   
         LA    RE,NETWPREC                                                      
         CLI   DBXNHOPT,C'Y'                                                    
         JNE   *+14                                                             
         LAY   RF,CABADJ                                                        
         MVI   NETWPREC+2,X'42'                                                 
         CLI   DBXNHOPT,C'H'                                                    
         JNE   *+14                                                             
         LAY   RF,CABADJ                                                        
         MVI   NETWPREC+2,X'42'      NETVALUE SPECIAL                           
         CLI   DBXNCR2,C'Y'                                                     
         JNE   DEMO39                                                           
         LAY   RF,CABADJ                                                        
         MVC   NETWPREC(2),=X'8282'       DEFAULT CAB R/P/I                     
         MVI   NETWPREC+3,X'82'      2 DEC SHARE                                
         CLI   DBXNNP1,C'Y'          1 DECIMAL HUT/PUT                          
         BNE   *+8                                                              
         MVI   NETWPREC+1,X'81'                                                 
         CLI   DBXNNS1,C'Y'          1 DECIMAL SHARE                            
         BNE   *+8                                                              
         MVI   NETWPREC+3,X'81'                                                 
         J     DEMO39                                                           
*                                                                               
DEMO39   ST    RF,PRECLIST                                                      
         ST    RE,SYSOPT                                                        
*                                                                               
         USING DBEXTRAD,RE                                                      
DEMO40   LR    RE,R0                                                            
         L     RE,DBRNEXT                                                       
         LR    R0,RE                                                            
         J     DEMO34                                                           
         DROP  RE                                                               
*                                                                               
DEMO42   XC    TVQADDR,TVQADDR                                                  
         XC    SVDBQRT,SVDBQRT     AND INIT                                     
         MVI   SAVETVQ,0                                                        
         ICM   R1,15,8(RE)         A(USER FILE IO)                              
         BZ    DEMO40                                                           
         LA    R1,PMDATA-PMKEY(R1)                                              
*                                                                               
DEMO44   CLI   0(R1),X'23'         FIND QH ELEMENT                              
         BNL   DEMO46              NOT THERE BYPASS                             
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     DEMO44                                                           
*                                                                               
DEMO46   MVC   TVQID,=C'TVQ'       SETUP FOR TVQFILE                            
         CLI   2(R1),172           OPI FILE                                     
         BNE   *+10                                                             
         MVC   TVQID,=C'OPI'                                                    
*                                                                               
         CLI   2(R1),NADIGOQ       FOR IAG DATA ORGINAL                         
         BE    *+12                                                             
         CLI   2(R1),NADIGRQ                AND REPEAT                          
         BNE   *+14                                                             
         MVC   TVQID,=C'IAG'                                                    
         B     DEMO47              START AT 1ST '23' ELEM (1ST MKT BK)          
*                                                                               
         ZIC   RF,1(R1)                                                         
         AR    R1,RF                                                            
DEMO47   ST    R1,TVQADDR          SAVE QH ELEMENT ADDRESS                      
         DROP  R1                                                               
         XC    SVDBQRT,SVDBQRT     AND INIT                                     
         MVI   SAVETVQ,0                                                        
         SR    RF,RF               RESET RF=0 ZERO FOR A(PRECLIST)              
         B     DEMO40                                                           
*                                                                               
DEMO48   DS    0H                                                               
         MVI   CALC2TRU,0          CALC TO 2 PLACES & TRUNCATE OPTION           
         LA    RF,DBEXTEND-4                                                    
*                                                                               
DEMOC2TG DS    0H                                                               
         ICM   RF,15,4(RF)          LOOK FOR CORRECT LINK                       
         BZ    DEMOC2TX              IF END REACHED, GET OUT                    
         CLC   0(4,RF),=C'SPOT'     LOOK FOR OPTION IN THIS LINK                
         BNE   DEMOC2TG                                                         
*                                                                               
         USING DBXTTID,RF                                                       
         MVC   RTGADJ,DBXTSCTL      SET VARIABLE ADJUSTMENT                     
         CLI   RTGADJ,0              USE THAT IF SET                            
         BNE   DEMOC2TG                                                         
         CLI   DBXTRC2T,C'Y'        CALC RTG TO 2 PLACES & TRUNCATE?            
         BNE   *+8                                                              
         OI    CALC2TRU,C2TRTG       YEP                                        
                                                                                
         CLI   DBXTSC2T,C'Y'        CALC SHR TO 2 PLACES & TRUNCATE?            
         BNE   *+8                                                              
         OI    CALC2TRU,C2TSHR       YEP                                        
                                                                                
         CLI   DBXTPC2T,C'Y'        CALC PUT TO 2 PLACES & TRUNCATE?            
         BNE   *+8                                                              
         OI    CALC2TRU,C2TPUT       YEP                                        
         DROP  RF                                                               
DEMOC2TX EQU   *                                                                
         EJECT                                                                  
         MVC   SVFMS,DUB           SAVE FILE/MEDIA/SOURCE COMBO                 
         L     R1,=A(DEMTABCL)     A(LIST OF TABLES IN PROGRAM)                 
         A     R1,RELO                                                          
         L     R1,TABMAST-DEMTABCL(R1)  GET MASTER DEMO TABLE HEADERS           
         LAM   AR1,AR1,ALET                                                     
         SAC   512                                                              
         USING MSTHDRD,R1                                                       
*                                                                               
DEMO50   CLC   MSTFILE(2),=H'0'    TEST E-O-T                                   
         JNE   DEMO52                                                           
         SAC   0                                                                
         LAM   AR1,AR1,=F'0'                                                    
         MVI   DBERROR,NOMAST                                                   
         B     DEMOX                                                            
*                                                                               
DEMO52   CLC   MSTFMS,DUB          MATCH ON FILE/SUB-FILE/SOURCE                
         JE    *+16                                                             
         ICM   R1,7,MSTAET                                                      
         AHI   R1,1                                                             
         J     DEMO50                                                           
*                                                                               
         STCM  R1,15,AMSTHDR       SET TABLE START ADDRESS                      
         XR    R0,R0               CONVERT BOOK TO BOOK NUMBER                  
         ICM   R0,1,MSTBKNUM                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,MSTBKNDX                                                      
         CPYA  ARF,AR1                                                          
         LA    RE,1                                                             
*                                                                               
DEMO54   CLC   BOOK,0(RF)          MATCH BOOK                                   
         JNH   DEMO56                                                           
         LA    RF,3(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,DEMO54                                                        
*                                                                               
         SAC   0                                                                
         LAM   ARF,AR1,=3F'0'                                                   
         MVI   DBERROR,NOMAST      SET INVALID BOOK & EXIT                      
         B     DEMOX                                                            
*                                                                               
DEMO56   STCM  RE,1,BKNO                                                        
         MVC   BKEF,2(RF)                                                       
*                                                                               
         SAC   0                                                                
         LAM   ARF,AR1,=3F'0'                                                   
         DROP  R1                                                               
*                                                                               
* SET UP TVQ DISPLACEMENT TABLE IF REQUIRED                                     
         OC    TVQADDR,TVQADDR                                                  
         BZ    DEMO68                                                           
         L     R1,TVQADDR                                                       
TVQBKEL  CLI   0(R1),0                                                          
         BE    DEMO68                                                           
         CLI   0(R1),X'5E'                                                      
         BE    TVQBKEL1                                                         
         ZIC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     TVQBKEL                                                          
TVQBKEL1 MVC   TVQBOOK,5(R1)                                                    
         MVC   TVQID,2(R1)         TVQ, OPI, OR IAG                             
         XC    TVQBOOK,=X'FFFF'                                                 
*                                                                               
         L     R1,=A(DEMTABCL)     A(LIST OF TABLES IN PROGRAM)                 
         A     R1,RELO                                                          
         L     R1,TABMAST-DEMTABCL(R1)  GET MASTER DEMO TABLE HEADERS           
         LAM   AR1,AR1,ALET                                                     
         SAC   512                                                              
         USING MSTHDRD,R1                                                       
DEMO58   CLC   MSTFILE(2),=H'0'    TEST E-O-T                                   
         BNE   DEMO60                                                           
         SAC   0                                                                
         LAM   ARF,AR1,=3F'0'                                                   
         MVI   DBERROR,NOMAST                                                   
         B     DEMOX                                                            
*                                                                               
DEMO60   CLC   MSTFMS,TVQID        MATCH ON FILE/SUB-FILE/SOURCE                
         BE    *+16                                                             
         ICM   R1,7,MSTAET                                                      
         LA    R1,1(R1)                                                         
         B     DEMO58                                                           
         ST    R1,AMSTTVQ          SET TABLE START ADDRESS                      
         SR    R0,R0               CONVERT BOOK TO BOOK NUMBER                  
         ICM   R0,1,MSTBKNUM                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RF,MSTBKNDX                                                      
         CPYA  ARF,AR1                                                          
         LA    RE,1                                                             
DEMO62   CLC   TVQBOOK,0(RF)                                                    
         BNH   DEMO64                                                           
         LA    RF,3(RF)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,DEMO62                                                        
         SAC   0                                                                
         LAM   ARF,AR1,=3F'0'                                                   
         MVI   DBERROR,NOMAST      SET INVALID BOOK & EXIT                      
         B     DEMOX                                                            
*                                                                               
DEMO64   STC   RE,BKNOTVQ                                                       
         MVC   BKEFTVQ,2(RF)                                                    
         XC    TVQBOOK,=X'FFFF'                                                 
*                                                                               
         SAC   0                                                                
         LAM   ARF,AR1,=3F'0'                                                   
         DROP  R1                                                               
*                                                                               
DEMO68   CLI   DBINTMED,C'R'       TEST RADIO FILE                              
         JE    DEMRAD                                                           
*        THESE FILES HAVE FAST DEMO CALC SUPPORT                                
         CLC   DBINTFIL(2),=AL2(CABFILE)  CABLE/HISP CBL/NAD/NTI/NHT            
         BE    FSTDEMST                                                         
         CLC   DBINTFIL(2),=AL2(CHNFILE)                                        
         BE    FSTDEMST                                                         
         CLC   DBINTFIL(2),=AL2(NHIFILE)                                        
         BE    FSTDEMST                                                         
         CLC   DBINTFIL(2),=AL2(NADFILE)                                        
         BE    FSTDEMST                                                         
         CLC   DBINTFIL(2),=AL2(NTIFILE)                                        
         BE    FSTDEMST                                                         
         B     *+8                                                              
FSTDEMST OI    DEMOFLAG,FASTPRES   FLAG FOR FAST DEMO LOOKUPS                   
*                                                                               
*                                   ALLOW EVEN ELEMS FOR NAD/NHT/CHN            
         CLC   DBINTFIL(2),=AL2(NHIFILE)                                        
         BE    EVENOK                                                           
         CLC   DBINTFIL(2),=AL2(NADFILE)                                        
         BE    EVENOK                                                           
         CLC   DBINTFIL(2),=AL2(MVGFILE)                                        
         BE    EVENOK                                                           
         CLC   DBINTFIL(2),=AL2(CHNFILE)                                        
         BNE   EVENNG                                                           
*                                  AFTER 9405 EVEN AND ODD ELEMS OKAY           
         CLC   DBACTBK,=AL1(YR_1994,WEEK_05)                                    
         BL    EVENNG                                                           
*                                                                               
EVENOK   OI    DEMOFLAG,EVENPRES   FILE HAS DEMOS IN EVEN ELEMS                 
EVENNG   DS    0C                                                               
*                                                                               
         CLC   =AL2(NADFILE),DBINTFIL TEST NAD FILE                             
         JE    NADUTCK                                                          
         CLC   =AL2(MVGFILE),DBINTFIL TEST MOVIE GOER FILE                      
         JE    NADUTCK                                                          
         CLC   =AL2(IAGFILE),DBINTFIL TEST IAG FILE                             
         JE    DEMNADS                                                          
         CLC   =AL2(NHIFILE),DBINTFIL TEST NTI HISPANIC FILE                    
         JE    DEMNADS                                                          
         CLI   LISTTYPE,LISTMAST   TEST IF MASTER DISP. LIST PASSED             
         JE    DEMMAST                                                          
         CLI   LISTTYPE,LISTORIG   TEST IF DEMO ORIGINS REQUESTED               
         JE    DEMORIG                                                          
         CLI   LISTTYPE,LISTOPTI   TEST IF DEMO ORIGINS PASSED                  
         JE    DEMOPTI                                                          
         CLI   LISTTYPE,LISTMULT   TEST MULTI-DEMO LIST PASSED                  
         JE    DEMLIST                                                          
         CLI   LISTTYPE,LISTMULP   TEST MULTI-DEMO LIST PASSED (W/ PLD)         
         JE    DEMLIST                                                          
         CLI   LISTTYPE,LISTPREC   TEST PRECISION LIST PASSED                   
         JE    DEMLIST                                                          
*                                                                               
         MVI   DBERROR,INVCMND                                                  
         B     DEMOX                                                            
         EJECT                                                                  
***********************************************************************         
* MASTER DISPLACEMENT LIST PASSED                                     *         
***********************************************************************         
         SPACE 1                                                                
DEMMAST  L     R1,APARM                                                         
         L     R0,12(R1)           R0=A(END OF MASTER DISP. LIST)               
*                                                                               
DEMST02  CR    R2,R0               TEST E-O-L                                   
         BNL   DEMOX                                                            
         USING DSPDTAD,R2                                                       
*                                                                               
         CLI   DSPELCD,HIELCD      BYPASS CALC ONLY ELEMENTS                    
         BH    DEMOX                                                            
         CLI   DSPMOD,0                                                         
         BNE   *+14                                                             
         XC    0(4,R4),0(R4)       CLEAR OUTPUT AREA                            
         J     DEMST04                                                          
*                                                                               
         MVC   BASEMOD,DSPMOD                                                   
         GOTO1 DEMCALC,DMCB,(DSPPREC,DSPMOD),(0,(R4)),0                         
*                                                                               
DEMST04  AHI   R2,DSPDTALN                                                      
         LA    R4,4(R4)                                                         
         J     DEMST02                                                          
         EJECT                                                                  
***********************************************************************         
* REGULAR (3 BYTE) DEMO LIST PASSED                                   *         
***********************************************************************         
         SPACE 1                                                                
DEMLIST  DS    0H                                                               
         CLI   LISTTYPE,LISTMULP   MULTI-DEMO LIST PASSED W/ PLD?               
         BNE   DEMLST02                                                         
*                                                                               
         LR    RF,R2               A(DEMO LIST)                                 
         ST    RF,ADEMOLST                                                      
         SR    R5,R5               DEMO COUNTER                                 
DEMLST00 DS    0H                                                               
         CLI   0(RF),EOT           ANY MORE DEMOS?                              
         BE    DEMLST01                                                         
         AHI   R5,1                YES: BUMP COUNTER                            
         LA    RF,3(RF)            BUMP TO NEXT DEMO                            
         CLI   DBDEMTYP,C'4'                                                    
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         B     DEMLST00                                                         
*                                                                               
DEMLST01 DS    0H                                                               
*                                  ENCODE THE DEMO LISTS FOR PLD                
         ST    R5,NUMDEMOS                                                      
         L     R1,APARM            A(PARALLEL PERSONAL LANGUAGE ARRAY)          
         MVC   DMCB+12(4),12(R1)                                                
         GOTO1 ADEMOCON,DMCB,((R5),(R2)),('DEMOCON_17',(R2)),(R3)               
*                                                                               
DEMLST02 CLI   0(R2),EOT           TEST E-O-L                                   
         JE    DEMLST60            YES                                          
*                                                                               
         ICM   R5,3,1(R2)          SAVE ORIG                                    
         CLI   DBDEMTYP,C'4'                                                    
         JNE   *+8                                                              
         ICM   R5,7,1(R2)                                                       
         MVI   GAASW,0             RESET FOR NEXT DEMO                          
         CLC   =AL2(NTIFILE),DBINTFIL NETWORK FILE                              
         JNE   DEMLST14                                                         
*                                                                               
         CLI   0(R2),0             TCAR GAA TYPE                                
         JE    DEMLST03                                                         
         CLI   DBBTYPE,C'T'        TCAR LK UP (INCL VCR)                        
         JE    DEMLTCR1                                                         
         CLI   DBBTYPE,C'V'        WB LK UP (INCL VCR)                          
         JE    DEMLTCR1                                                         
         CLI   DBBTYPE,C'W'        WB LK UP (EXL VCR)                           
         JNE   DEMLST03                                                         
DEMLTCR1 NI    DEMOFLAG,255-DEMOPRES                                            
         ICM   RE,15,DBAQUART                                                   
         XR    R0,R0                                                            
DEMLTCR2 CLI   0(RE),0                                                          
         JE    DEMLTCRX            MKT BRK NOT FOUND                            
         CLI   0(RE),X'23'         FIND SECTION LEAD                            
         JNE   *+14                                                             
         CLC   2(1,RE),0(R2)       SAME MKT BRK?                                
         JE    DEMLTCRX                                                         
         IC    R0,1(RE)            BUMP TO NEXT ELEM                            
         AR    RE,R0                                                            
****     CLI   0(RE),X'0F'         NEXT PROGRAM SEGEMENT AVERAGE                
****     BE    DEMLTCRX             MKT BREAK NOT FOUND                         
         J     DEMLTCR2                                                         
DEMLTCRX ST    RE,AQTHREL                                                       
         J     DEMLST04                                                         
*                                                                               
DEMLST03 DS    0H                                                               
         LLC   R1,1(R2)            ENCODED MODIFIER                             
         A     R1,APLMODTB         A(DECODED MODIFIER)                          
         MVC   BYTE,0(R1)          SAVE DECODED MODIFIER                        
         LLC   R1,1(R2)            ENCODED MODIFIER                             
         A     R1,APLPLDTB         A(DECODED PLD CHARACTER)                     
         MVC   BYTE2,0(R1)         SAVE DECODED PLD CHARACTER                   
*                                                                               
         CLC   DBSELSTA(3),=C'HUT' HUT STATION                                  
         JNE   DEMLST04                                                         
         CLI   BYTE,DEMO_MODIFIER_P EQUATE PUTS...                              
         JE    *+12                                                             
         CLI   BYTE,DEMO_MODIFIER_O AND TP PUTS...                              
         JNE   DEMLST04                                                         
         MVI   1(R2),DEMO_MODIFIER_R ...TO RATINGS                              
         B     DEMLST_CONV                                                      
*                                                                               
DEMLST04 CLI   BYTE,DEMO_MODIFIER_L GAA RATING                                  
         JNE   *+16                                                             
         MVI   GAASW,1                                                          
         MVI   1(R2),DEMO_MODIFIER_R TO RATINGS                                 
         B     DEMLST_CONV                                                      
*                                                                               
         CLI   BYTE,DEMO_MODIFIER_N GAA IMPS                                    
         JNE   *+8                                                              
         MVI   GAASW,1                                                          
*                                                                               
         CLI   BYTE,DEMO_MODIFIER_M GAA VPVH                                    
         JNE   *+16                                                             
         MVI   GAASW,1                                                          
         MVI   1(R2),DEMO_MODIFIER_V                                            
         B     DEMLST_CONV                                                      
*                                                                               
         CLI   BYTE,DEMO_MODIFIER_B GAA EXPANDED                                
         JNE   DEMLST05                                                         
         MVI   GAASW,1                                                          
         MVI   1(R2),DEMO_MODIFIER_Y                                            
*                                                                               
DEMLST_CONV DS 0H                                                               
         GOTO1 ADEMOCON,DMCB,(1,(R2)),('DEMOCON_17',(R2)),(R3),BYTE2            
*                                                                               
DEMLST05 DS    0H                                                               
         CLI   DBSELSTA+4,C'S'                                                  
         JNE   DEMLST06                                                         
*                                                                               
         LLC   R1,1(R2)            ENCODED MODIFIER                             
         A     R1,APLMODTB         A(DECODED MODIFIER)                          
         CLI   0(R1),DEMO_MODIFIER_T                                            
         JNE   DEMLST06                                                         
*                                                                               
         CLI   DBDEMTYP,C'4'       SETUP FOR FORMAT                             
         JNE   *+18                                                             
         CLC   2(2,R2),=H'7'       MAGE DOESN'T GET 'N'                         
         JE    DEMLST06                                                         
         J     *+12                                                             
*                                                                               
         CLI   2(R2),7                                                          
         JE    DEMLST06                                                         
*                                                                               
         MVI   1(R2),DEMO_MODIFIER_N                                            
         GOTO1 ADEMOCON,DMCB,(1,(R2)),('DEMOCON_17',(R2)),(R3),BYTE2            
*                                                                               
DEMLST06 CLI   GAASW,0              ANY GAA REQUESTED                           
         JE    DEMLST12             NO - IT'S GOT TO BE OK                      
         L     RE,AQTHREL           YES - MAKE SURE WE HAVE SOME GAA            
         XR    R0,R0                                                            
*                                                                               
DEMLST08 CLI   0(RE),0              EOR - RESET GAA                             
         JE    DEMLST10                                                         
         CLI   0(RE),X'55'          GAA OK                                      
         JE    DEMLST12                                                         
         CLI   0(RE),X'56'          FOR NAD-GAA                                 
         JE    DEMLST12                                                         
         JH    DEMLST10             NO GAA ELEMENT - RESET GAA                  
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         J     DEMLST08                                                         
*                                                                               
DEMLST10 MVI   GAASW,0                                                          
*                                                                               
DEMLST12 CLI   DBSATLIT,C'Y'       AUTHORIZED FOR PEOPLE SHARES/PUTS            
         JE    DEMLST14            YES - DON'T CHECK                            
*                                                                               
         CLI   DBDEMTYP,C'4'                                                    
         JNE   *+18                                                             
         CLC   2(2,R2),=H'20'                                                   
         JL    DEMLST14                                                         
         J     *+12                                                             
         CLI   2(R2),20            ALLOW ALL HOMES                              
         JL    DEMLST14                                                         
*                                                                               
         XC    0(4,R4),0(R4)       CLEAR OUTPUT                                 
         LLC   R1,1(R2)            ENCODED MODIFIER                             
         A     R1,APLMODTB         A(DECODED MODIFIER)                          
         CLI   0(R1),DEMO_MODIFIER_P PUT                                        
         JE    DEMLST34                                                         
         CLI   0(R1),DEMO_MODIFIER_S SHARE                                      
         JE    DEMLST34            BYPASS DEMO                                  
*                                                                               
DEMLST14 ICM   R0,1,0(R2)                                                       
         CLI   LISTTYPE,LISTPREC   TEST IF PRECISION LIST PASSED                
         JE    DEMLST24                                                         
         CLC   DBINTFIL(2),=AL3(IUNFILE) REP INVENTORY FILE                     
         JE    *+10                 AVAIL PRINT SCREWS THIS FIELD               
         MVC   LASTNAD,0(R2)                                                    
*                                                                               
         CLI   LASTNAD,240          NAD CAT 240-243 ARE NETWORK                 
         BL    DEMLS14A              USER INDEXES                               
         CLI   LASTNAD,243                                                      
         BH    DEMLS14A                                                         
         MVI   LASTNAD,0            FORCE A TOTAL US LOOKUP                     
*                                    APPLICATION WILL ADJUST                    
*                                                                               
* CHECK FOR TVQ ACTIVE                                                          
DEMLS14A CLI   SAVETVQ,0                                                        
         BNE   DEMLS14B                                                         
         CLI   LASTNAD,NADOPIQ     USER CATEGORIES                              
         BE    DEMLS14B                                                         
         CLI   LASTNAD,NADIGOQ                                                  
         BE    DEMLS14B                                                         
         CLI   LASTNAD,NADIGRQ                                                  
         BE    DEMLS14B                                                         
         CLI   LASTNAD,NADTVQQ                                                  
         BNE   TVQX                                                             
DEMLS14B OC    TVQADDR,TVQADDR     TVQ AVAILABLE                                
         BZ    TVQ3                 NO                                          
         CLI   SAVETVQ,0           FIRST PASS                                   
         BNE   TVQ2                 NO - DO THE TVQ FACTOR                      
         CLI   LASTNAD,NADOPIQ     OPI DEMO                                     
         BE    DEMLS14C                                                         
         CLI   LASTNAD,NADIGOQ     IAG ORIGINAL DEMO                            
         BE    DEMLS14C                                                         
         CLI   LASTNAD,NADIGRQ     IAG REPEAT DEMO                              
         BE    DEMLS14C                                                         
         CLI   LASTNAD,NADTVQQ     IS IT A TVQ DEMO                             
         BNE   TVQX                 NO                                          
DEMLS14C MVC   SAVETVQ,LASTNAD     SET UP FOR PASS 2                            
         SR    R0,R0                                                            
         MVI   0(R2),0                                                          
         MVI   LASTNAD,0           DO THE REQ LOOKUP                            
         B     TVQX                                                             
TVQ2     MVC   SVDBQRT,DBAQUART    SAVE THE NORMAL DATA ADDRESS                 
         MVC   DBAQUART,TVQADDR    POINT TO TVQ DATA                            
*                                                                               
TVQIAG   CLC   TVQID,=C'IAG'       FOR IAG,                                     
         BNE   TVQIAGX             NEED TO MAKE SURE WE'RE ON THE               
         L     RE,TVQADDR          CORRECT MARKET BREAK, SINCE                  
TVQIAG2  CLI   0(RE),0                                                          
         BE    TVQIAGX                                                          
         CLI   0(RE),X'23'         ORIGINAL AND REPEATS ARE ON DIFFRENT         
         BNE   TVQIAG6             MKT BREAKS                                   
         CLC   SAVETVQ,2(RE)       MATCH ON MARKET BREAK?                       
         BE    TVQIAG8                                                          
TVQIAG6  ZIC   R0,1(RE)            NO. TRY NEXT MARKET BREAK                    
         AR    RE,R0                                                            
         B     TVQIAG2                                                          
TVQIAG8  ZIC   R0,1(RE)            YES.                                         
         AR    RE,R0                                                            
         STCM  RE,15,DBAQUART      STORE BEGINNING OF DEMO DETA                 
TVQIAGX  DS    0H                                                               
*                                                                               
         MVC   AQTHREL,DBAQUART                                                 
         MVI   AQTHREL,0                                                        
         XC    ADEMOEL(ADEMOELL),ADEMOEL                                        
         MVI   LASTNAD,NADTVQQ     SET FOR TVQ DEMO                             
         CLC   TVQID,=C'OPI'                                                    
         BNE   *+8                                                              
         MVI   LASTNAD,NADOPIQ     OPI                                          
         CLC   TVQID,=C'IAG'                                                    
         BNE   *+10                                                             
         MVC   LASTNAD,SAVETVQ     IAG ORIGINAL OR REPEAT                       
         MVC   SVDBFIL,DBFILE                                                   
         MVC   SVDBABK,DBACTBK                                                  
         MVC   SVDBSBK,DBSELBK                                                  
         MVC   SVDBINTF,DBINTFIL                                                
         MVC   DBFILE,=C'NAD'                                                   
         MVC   DBINTFIL(2),=C'TV'                                               
         MVC   SVMODIFY,1(R2)                                                   
         MVC   SVGAA(1),GAASW                                                   
         MVI   GAASW,0                                                          
         MVI   1(R2),DEMO_MODIFIER_I IQ                                         
         GOTO1 ADEMOCON,DMCB,(1,(R2)),('DEMOCON_17',(R2)),(R3),BYTE2            
*                                                                               
         CLI   DBSELSTA+4,C'N'     IQ FOR NETS- TVQ FOR OTHERS                  
         BE    TVQ2C                                                            
         CLI   DBSELSTA+4,C'T'     IQ FOR NETS- TVQ FOR OTHERS                  
         BE    TVQ2C                                                            
         MVI   1(R2),DEMO_MODIFIER_X TVQ                                        
         GOTO1 ADEMOCON,DMCB,(1,(R2)),('DEMOCON_17',(R2)),(R3),BYTE2            
*                                                                               
TVQ2C    DS    0H                                                               
         MVI   0(R2),NADTVQQ                                                    
         CLC   TVQID,=C'OPI'                                                    
         BNE   TVQ2D                                                            
         MVC   DBFILE,=C'OPI'                                                   
         MVC   DBINTFIL(2),=C'OP'                                               
         MVI   1(R2),DEMO_MODIFIER_A                                            
         GOTO1 ADEMOCON,DMCB,(1,(R2)),('DEMOCON_17',(R2)),(R3),BYTE2            
         MVI   0(R2),NADOPIQ                                                    
*                                                                               
TVQ2D    CLC   TVQID,=C'IAG'                                                    
         BNE   TVQ2E                                                            
         MVC   DBFILE,=C'IAG'                                                   
         MVC   DBINTFIL(2),=C'IA'                                               
         MVI   1(R2),DEMO_MODIFIER_A                                            
         GOTO1 ADEMOCON,DMCB,(1,(R2)),('DEMOCON_17',(R2)),(R3),BYTE2            
         MVC   0(1,R2),SAVETVQ                                                  
*                                                                               
TVQ2E    IC    R0,0(R2)                                                         
         MVC   SVMSTHDR,AMSTHDR                                                 
         MVC   SVBKEF,BKEF                                                      
         MVC   SVBKNO,BKNO                                                      
         MVC   AMSTHDR,AMSTTVQ                                                  
         MVC   BKEF,BKEFTVQ                                                     
         MVC   BKNO,BKNOTVQ                                                     
         B     TVQX                                                             
TVQ3     CLI   LASTNAD,NADIGOQ     FOR IAG, IF NO FACTOR AVAILABLE              
         BE    TVQX                RETURN ZERO                                  
         CLI   LASTNAD,NADIGRQ                                                  
         BE    TVQX                                                             
         MVI   LASTNAD,0           FOR EVERYTHING ELSE, RETURN BASE             
         SR    R0,R0                                                            
TVQX     DS    0C                                                               
*                                                                               
         CLI   LASTNAD,NAD170Q                                                  
         JNE   *+8                                                              
         MVI   LASTNAD,0           TREAD NAD170 AS USA ON LK UP                 
         XR    R0,R0                                                            
*                                                                               
*  SET SPECIFIC DEFAULT PRECISIONS                                              
*                                                                               
         LLC   R1,1(R2)            ENCODED MODIFIER                             
         A     R1,APLMODTB         A(DECODED MODIFIER)                          
*                                                                               
         CLC   DBINTFIL(2),=C'EI'  ESTIMATED IMP                                
         JE    DEMLST17                                                         
         CLC   DBINTFIL(2),=C'EV'  ESTIMATED VPH                                
         JE    DEMLST17                                                         
         CLC   DBINTFIL(2),=C'IU'  INVENTORY                                    
         JE    DEMLST17                                                         
         CLC   DBINTFIL(2),=AL2(CABFILE) CABLE                                  
         JE    DEMLST17                                                         
         CLC   DBINTFIL(2),=AL2(CHNFILE) C'CH' = HISPANIC CABLE                 
         JNE   DEMLST18                                                         
*                                                                               
DEMLST17 DS    0H                                                               
         CLI   0(R1),DEMO_MODIFIER_R                                            
         JNE   DEMLST18                                                         
         LA    R0,129                                                           
*                                                                               
DEMLST18 DS    0H                                                               
         CLC   DBINTFIL(2),=AL3(EINFILE)                                        
         JNE   DEMLST19                                                         
         CLI   0(R1),DEMO_MODIFIER_T                                            
         JNE   DEMLST19                                                         
         LA    R0,67                                                            
*                                                                               
DEMLST19 DS    0H                                                               
         ICM   RE,15,PRECLIST                                                   
         JZ    DEMLST24                                                         
*                                                                               
DEMLST20 CLI   0(RE),X'FF'                                                      
         JE    DEMLST24                                                         
         LLC   RF,1(R2)            ENCODED MODIFIER                             
         A     RF,APLMODTB         A(DECODED MODIFIER)                          
         CLC   0(1,RF),0(RE)       PRECISION CATEGORY                           
         JNE   DEMLST22                                                         
*                                                                               
         L     RF,SYSOPT                                                        
         XR    R0,R0                                                            
         IC    R0,2(RE)                                                         
         AR    RF,R0                                                            
         IC    R0,0(RF)                                                         
         LR    RF,R0                                                            
         IC    R0,1(RE)            SET OVERRIDE PRECISION                       
         CLI   2(RE),255           USE LIST FOR SCALE DIRECTION                 
         JE    *+6                                                              
         OR    R0,RF                                                            
         J     DEMLST24                                                         
*                                                                               
DEMLST22 LA    RE,3(RE)            TRY NEXT ONE IN PRECISION LIST               
         J     DEMLST20                                                         
*                                                                               
DEMLST24 CLC   =AL2(NTIFILE),DBINTFIL NTI                                       
         JE    DEMLST25                                                         
         CLC   =AL2(NHIFILE),DBINTFIL NHI                                       
         JE    DEMLST25                                                         
         CLC   DBFILE,=AL3(EVNFILE) EVN                                         
         JNE   DEMLST28                                                         
*                                                                               
DEMLST25 DS    0H                                                               
         CLI   DBDEMTYP,C'4'       4 CHAR LIST INPUT                            
         JE    DEMLST26                                                         
         CLI   2(R2),1             HOMES                                        
         JNE   DEMLST28                                                         
         CLI   0(R2),NADTVQQ                                                    
         JE    DEMLST28                                                         
         CLI   0(R2),NADOPIQ                                                    
         JE    DEMLST28                                                         
         CLI   0(R2),NADIGOQ                                                    
         JE    DEMLST28                                                         
         CLI   0(R2),NADIGRQ                                                    
         JE    DEMLST28                                                         
         CLI   0(R2),NAD170Q                                                    
         JE    DEMLST28                                                         
         CLI   0(R2),TCAR181       BYPASS FOR TCAR RANGE 181-185                
         BL    *+12                                                             
         CLI   0(R2),TCAR189                                                    
         BNH   DEMLST28                                                         
         CLI   0(R2),2             AND NAD CAT                                  
         JL    DEMLST28                                                         
         MVI   2(R2),249           FUDGE THE REQUEST                            
         J     DEMLST28                                                         
*                                                                               
DEMLST26 CLC   2(2,R2),=H'1'       HOMES 4 CHAR                                 
         JNE   DEMLST28                                                         
         CLI   0(R2),NADOPIQ                                                    
         JE    DEMLST28                                                         
         CLI   0(R2),NADIGOQ                                                    
         JE    DEMLST28                                                         
         CLI   0(R2),NADIGRQ                                                    
         JE    DEMLST28                                                         
         CLI   0(R2),NADTVQQ                                                    
         JE    DEMLST28                                                         
         CLI   0(R2),NAD170Q                                                    
         JE    DEMLST28                                                         
         CLI   0(R2),TCAR181       BYPASS FOR TCAR RANGE 181-185                
         BL    *+12                                                             
         CLI   0(R2),TCAR189                                                    
         BNH   DEMLST28                                                         
         CLI   0(R2),2             AND NAD CAT                                  
         JL    DEMLST28                                                         
         MVC   2(2,R2),=H'249'     FUDGE THE REQUEST                            
*                                                                               
DEMLST28 DS    0H                                                               
*&&DO                                                                           
         CLC   DBSELAGY,=C'MC'     MC CANN                                      
         BE    *+10                                                             
         CLC   DBSELAGY,=C'H9'     MEDIA EDGE                                   
         BNE   SQH9OK                                                           
         CLI   DBACTSRC,C'S'       SQAD                                         
         BNE   SQH9OK                                                           
         CLC   DBINTFIL(2),=C'TT'                                               
         BNE   SQH9OK                                                           
         LA    RE,SQH9LST           ONLY ALLOW THESE DEMOS                      
         CLC   DBSELAGY,=C'MC'     MC CANN                                      
         BNE   *+8                                                              
         LA    RE,SQMCLST                                                       
SQH92    CLI   0(RE),X'FF'                                                      
         BE    DEMLST40                                                         
         CLC   2(1,R2),0(RE)                                                    
         BE    SQH9OK                                                           
         LA    RE,1(RE)                                                         
         B     SQH92                                                            
SQH9LST  DC    AL1(1,42,48,92,98,141,142,148,129,153)                           
         DC    X'FF'                                                            
SQMCLST  DC    AL1(41,42,47,48,91,92,97,98,125,122,54,57,104,107,141)           
         DC    AL1(142,147,148,157,40,90,128,129,143,53,103,153)                
         DC    X'FF'                                                            
*&&                                                                             
SQH9OK   DS    0H                                                               
*                                                                               
         BRAS  RE,TESTC2T          TEST FOR CALC TO 2 DEC & TRUNCATE            
         TM    CALC2TRU,C2TYES                                                  
         BNO   DMLST28G                                                         
*                                                                               
         BRAS  RE,ADJDREL                                                       
DMLST28G EQU   *                                                                
                                                                                
         MVI   ZEROVAL,0           RESET ZERO VALUE IND.                        
         MVC   BASEMOD,1(R2)       VARIABLE PRECISION BASE MODIFIER             
         GOTO1 DEMCALC,DMCB,((R0),1(R2)),(DBDEMTYP,(R4)),0                      
*                                                                               
* RESTORE ELEMENTS                                                              
         LA    RE,SVELEMTB                                                      
DMLST28H CLC   =X'00000000',0(RE)                                               
         BE    DMLST28O                                                         
         L     RF,0(RE)                                                         
         MVC   2(1,RF),4(RE)                                                    
         LA    RE,5(RE)                                                         
         B     DMLST28H                                                         
*                                                                               
DMLST28O DS    0H                                                               
*                                                                               
                                                                                
         NI    CALC2TRU,X'FF'-C2TDONE                                           
         TM    CALC2TRU,C2TYES+C2T2DEC  AFTER CALCULATING TO 2 DEC,             
         BNO   DMLST28M                                                         
         SR    RE,RE                                                            
****     ICM   RF,15,0(R4)        *TAKE OUT THIS WE ARE PASSING 2 DEC           
****     D     RE,=F'10'                 TRUNCATE                               
****     STCM  RF,15,0(R4)                                                      
         OI    CALC2TRU,C2TDONE                                                 
                                                                                
*                                       RESTORE ADDRESSES                       
         MVC   ARECORD,DBAREC                                                   
         MVI   ARECORD,0                                                        
         MVC   AQTHREL,DBAQUART                                                 
         MVI   AQTHREL,0                                                        
DMLST28M EQU   *                                                                
*                                                                               
* THERE ARE SOME CORE-RESIDENT PROGRAMS WITH HARD-CODED DEMO LISTS.             
* THE PROGRAMS ARE PASSING THE ADDRESS OF THE LIST TO DEMOUT, AND AS A          
* RESULT, THE STCM INSTRUCTION AT LABEL TESTOK WANTS TO OVERWRITE CORE.         
* HOPEFULLY, IN EVERY SUCH CASE, THE DATA ISN'T ACTUALLY CHANGING,              
* WHICH EXPLAINS HOW WE'VE BEEN GETTING AWAY WITH THIS. THE CODE BELOW          
* CONFIRMS THAT THIS IS TRUE, AND THAT WE DON'T HAVE A BUG.                     
*                                                                               
         ICM   RE,15,ASSB          DO WE HAVE A(SSB)?                           
         BZ    TESTOK              NO: WE'RE OFFLINE                            
         ICM   RE,15,SSBTKADR-SSBD(RE)   A(CURRENT TASK AREA)                   
         BZ    DMLST28P            NO TASK: CHECK THE DEMO                      
*                                                                               
* THIS CODE WAS TAKEN FROM GETFACT (SEE F#CKITSK)                               
*                                                                               
         USING TCBD,RE                                                          
         LR    RF,R2               A(DEMO)                                      
         LA    RF,0(,RF)           CLEAR HOB                                    
         C     RF,TCBWRKA          ADDRESS IS BEFORE START OF TASK?             
         BL    DMLST28P            YES: CHECK THE DEMO                          
         C     RF,TCBPGMX          ADDRESS IS AFTER END OF TASK?                
         BNH   TESTOK              NO: WE'RE OKAY                               
         DROP  RE                                                               
*                                                                               
DMLST28P DS    0H                                                               
         LHI   RF,3                ASSUME 3-BYTE DEMO LIST ENTRY                
         CLI   DBDEMTYP,C'4'                                                    
         JNE   *+8                                                              
         LHI   RF,7                IT'S A 4-BYTE DEMO LIST ENTRY                
         EX    RF,*+8                                                           
         B     *+8                                                              
         CLM   R5,0,1(R2)          ARE WE ABOUT TO CHANGE THE DATA?             
         BE    DEMLST32            NO                                           
         DC    H'0'                YES!?! WHAT'S GOING ON?                      
*                                                                               
TESTOK   STCM  R5,3,1(R2)          RESTORE ORIG                                 
         CLI   DBDEMTYP,C'4'                                                    
         JNE   *+8                                                              
         STCM  R5,7,1(R2)                                                       
*&&DO                        OUT 11/13/03                                       
*                                                                               
*  SPECIAL KGF CRAP *********************************                           
*                                                                               
         CLI   DBDAYPT,0                                                        
         JE    DEMLST32                                                         
         OC    DBMMBOOK,DBMMBOOK   NOT A METERED MARKET                         
         JZ    *+14                 APPLY FACTOR                                
         CLC   DBACTBK,DBMMBOOK    NOT METERED YET                              
         JNL   DEMLST32             APPLY FACTOR                                
         LA    RE,ADJFTABC                                                      
*                                                                               
DEMLST30 CLC   DBSELAGY,0(RE)      FIND AGENCY TABLE                            
         JNE   *+14                                                             
         CLC   DBDAYPT,2(RE)       AND THE DAYPART                              
         JE    *+20                                                             
         LA    RE,7(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         JNE   DEMLST30                                                         
         J     DEMLST32                                                         
*                                                                               
         XR    RF,RF               GET THE FACTOR                               
         ICM   RF,3,3(RE)                                                       
         L     RE,0(R4)            AND THE DEMO VALUE                           
         MR    RE,RE               AND ADJUST                                   
         XR    RE,RE                                                            
         AHI   RF,50                                                            
         D     RE,=F'100'                                                       
         ST    RF,0(R4)            REPLACE ORIGINAL WITH ADJUSTED               
*&&                                                                             
*                                                                               
DEMLST32 CLC   DBINTFIL,=C'IU'     FIX UP IUN PRECISION                         
         JNE   DEMLST34            PRECISION IS MESSED UP IN FORMS              
         CLC   DBACTBK,=AL2(OCT_83)  FOR LATEST BOOKS                           
         JNH   DEMLST34                                                         
*                                                                               
         LLC   R1,1(R2)            ENCODED MODIFIER                             
         A     R1,APLMODTB         A(DECODED MODIFIER)                          
         CLI   0(R1),DEMO_MODIFIER_A                                            
         JNE   DEMLST33                                                         
         L     RF,0(R4)                                                         
         XR    RE,RE                                                            
         AHI   RF,500                                                           
         D     RE,=F'1000'                                                      
         ST    RF,0(R4)                                                         
*                                                                               
*                                  PRECISION IS MESSED UP IN FORMULAS           
DEMLST33 CLI   0(R1),DEMO_MODIFIER_D                                            
         JNE   DEMLST34        SO ADJUST IT HERE                                
         L     RF,0(R4)                                                         
         XR    RE,RE                                                            
         AHI   RF,50                                                            
         D     RE,=F'100'                                                       
         ST    RF,0(R4)                                                         
*                                                                               
DEMLST34 CLI   MININV,C'Y'         IF INV RECD & MINVAL PRESENT                 
         JNE   DEMLST36                                                         
         CLI   0(R1),DEMO_MODIFIER_R MINVAL FOR RTGS ONLY                       
         JNE   DEMLST36                                                         
*                                                                               
         L     R1,0(R4)                                                         
         BRAS  RE,SETMIN           YES - APPLY MINIMUM VALUE                    
         ST    R1,0(R4)                                                         
         J     DEMLST40                                                         
*                                                                               
DEMLST36 CLI   0(R2),NAD170Q       NAD 170 USES DD OVERIDE ELEMS                
         JNE   DEMLST38                                                         
         BRAS  RE,SETNUM           GET A 2 BYTE DEMO# TO CMP TO                 
         ICM   R1,15,0(R4)         R1 = DEMO VALUE                              
         BRAS  RE,NAD170                                                        
         STCM  R1,15,0(R4)            SAVE NEW NAD VALUE                        
         J     DEMLST40                                                         
*                                                                               
DEMLST38 CLI   0(R2),NADOPIQ                                                    
         JE    DEMLST39                                                         
         CLI   0(R2),NADIGOQ                                                    
         JE    DEMLST39                                                         
         CLI   0(R2),NADIGRQ                                                    
         JE    DEMLST39                                                         
         CLI   0(R2),NADTVQQ                                                    
         JNE   DEMLST40                                                         
*                                                                               
DEMLST39 DS    0H                                                               
****     CLI   ZEROVAL,0           ZERO IF MISSING BASIC VALUES                 
****     BE    *+10                                                             
****     XC    0(4,R4),0(R4)       ZAP NORMAL VALUES                            
         OC    TVQADDR,TVQADDR     DOING ADJUSTMENT                             
         JZ    DEMLST40                                                         
*                                                                               
         CLC   TVQID,=C'IAG'       DON'T FORCE INDEX FOR IAG                    
         JE    *+20                                                             
         CLC   0(4,R4),=F'0'       HAVE VALUE - ITS OK                          
         JNE   *+10                                                             
         MVC   0(4,R4),=F'100'     FORCE INDEX TO 100                           
*                                                                               
         LLC   R1,SVMODIFY         ENCODED MODIFIER                             
         A     R1,APLMODTB         A(DECODED MODIFIER)                          
         CLI   0(R1),DEMO_MODIFIER_U    NO ADJ FOR UNIVERSE                     
         JNE   *+10                                                             
         MVC   0(4,R4),=F'100'                                                  
****     OC    TVQADDR,TVQADDR     DOING ADJUSTMENT                             
****     JZ    DEMLST40                                                         
         L     RE,0(R4)            GET TVQ FACTOR                               
         L     RF,SVRAWV           GET DEMO VALUE                               
         MR    RE,RE               ADJUST IT                                    
         SR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'100'                                                       
         AHI   RF,1                                                             
         SRA   RF,1                                                             
         ST    RF,0(R4)            REPLACE VALUE                                
         MVC   0(1,R2),SAVETVQ                                                  
         MVI   SAVETVQ,0           RESET PASS                                   
         MVC   DBAQUART,SVDBQRT    RESTORE BASE RECORD                          
         MVC   AQTHREL,DBAQUART                                                 
         MVI   AQTHREL,0                                                        
         XC    ADEMOEL(ADEMOELL),ADEMOEL                                        
         MVC   1(1,R2),SVMODIFY                                                 
         MVC   GAASW,SVGAA                                                      
         MVC   DBFILE,SVDBFIL                                                   
         MVC   DBACTBK,SVDBABK                                                  
         MVC   DBSELBK,SVDBSBK                                                  
         MVC   DBINTFIL(2),SVDBINTF                                             
         MVC   AMSTHDR,SVMSTHDR                                                 
         MVC   BKEF,SVBKEF                                                      
         MVC   BKNO,SVBKNO                                                      
*                                                                               
DEMLST40 LA    R2,3(R2)            BUMP TO NEXT LIST ENTRY                      
         CLI   ZAPZERO,C'Y'        KILL VALUE IF ANY BASIC ZEROS                
         JNE   DEMLST42                                                         
         CLI   ZEROVAL,0           NON ZERO IF ZERO BASIC VALUES FOUND          
         JE    DEMLST42                                                         
         XC    0(4,R4),0(R4)       SUPPRESS THE OUTPUT                          
*                                                                               
DEMLST42 CLI   SAVETVQ,0           TVQ PASS                                     
         JE    DEMLST44            NO - PROCESS NORMAL                          
         SHI   R2,3                REVERSE PRELIM INCREMENT                     
         MVC   SVRAWV,0(R4)        YES - SAVE THE RAW DEMO VALUE                
         XC    0(4,R4),0(R4)       CLEAR THE RAW VALUE                          
         J     DEMLST46                                                         
*                                                                               
DEMLST44 CLI   DBDEMTYP,C'4'                                                    
         JNE   *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
*                                                                               
DEMLST46 DS    0H                                                               
         J     DEMLST02                                                         
*                                                                               
DEMLST60 DS    0H                                                               
         BRAS  RE,FIXNADI                                                       
*                                                                               
         CLI   LISTTYPE,LISTMULP   MULTI-DEMO LIST PASSED W/ PLD?               
         BNE   DEMLSTX                                                          
*                                  DECODE THE DEMO LIST BEFORE EXITING          
         L     R0,NUMDEMOS                                                      
         L     R1,APARM            A(PARALLEL PERSONAL LANGUAGE ARRAY)          
         MVC   DMCB+12(4),12(R1)                                                
         GOTO1 ADEMOCON,DMCB,((R0),ADEMOLST),('DEMOCON_16',ADEMOLST),  +        
               (R3)                                                             
*                                                                               
DEMLSTX  DS    0H                                                               
         B     DEMOX                                                            
         EJECT                                                                  
***********************************************************************         
* MASTER DISPLACEMENT LIST PASSED - RETURN DEMO ORIGINS               *         
***********************************************************************         
         SPACE 1                                                                
DEMORIG  L     R1,APARM                                                         
         L     R0,12(R1)           R0=A(END OF MASTER DISP. LIST)               
DEMORIG2 CR    R2,R0               TEST E-O-L                                   
         BNL   DEMOX                                                            
         USING DSPDTAD,R2                                                       
         MVC   BASEMOD,DSPMOD      VARIABLE PRECISION BASE MODIFIER             
         GOTO1 DEMCALC,DMCB,DSPMOD,(0,(R4)),0                                   
         LA    R2,DSPDTALN(R2)                                                  
         LA    R4,DSPDTALN(R4)                                                  
         B     DEMORIG2                                                         
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* MASTER DISPLACEMENT LIST & DEMO ORIGIN MAP PASSED                   *         
***********************************************************************         
         SPACE 1                                                                
DEMOPTI  L     R1,APARM                                                         
         L     R0,12(R1)           R0=A(END OF MASTER DISP. LIST)               
         L     R5,16(R1)           R5=A(DEMO ORIGIN MAP)                        
DEMOPTI2 CR    R2,R0               TEST E-O-L                                   
         BNL   DEMOX                                                            
         USING DSPDTAD,R2                                                       
         CLI   DSPMOD,0                                                         
         BNE   *+14                                                             
         XC    0(4,R4),0(R4)                                                    
         B     DEMOPTI4                                                         
         MVC   BASEMOD,DSPMOD      VARIABLE PRECISION BASE MODIFIER             
         GOTO1 DEMCALC,DMCB,(DSPPREC,DSPMOD),(0,(R4)),(R5)                      
DEMOPTI4 LA    R2,DSPDTALN(R2)                                                  
         LA    R5,DSPDTALN(R5)                                                  
         LA    R4,4(R4)                                                         
         B     DEMOPTI2                                                         
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* NAD FILE                                                            *         
***********************************************************************         
         SPACE 1                                                                
NADUTCK  BRAS  RE,NADUCHK          CHECK FOR TABLE BUILT                        
         J     DEMNADS                                                          
         SPACE 2                                                                
***********************************************************************         
* NAD FILE DEMO LOOK-UPS (REGULAR DEMO LISTS ONLY)                    *         
***********************************************************************         
*                                                                               
DEMNADS  DS    0H                                                               
         CLI   LISTTYPE,LISTMULP   MULTI-DEMO LIST PASSED W/ PLD?               
         BNE   DEMNAD01                                                         
*                                                                               
         LR    RF,R2               A(DEMO LIST)                                 
         ST    RF,ADEMOLST                                                      
         SR    R5,R5               DEMO COUNTER                                 
DEMNAD0I DS    0H                                                               
         CLI   0(RF),EOT           ANY MORE DEMOS?                              
         BE    DEMNAD0M                                                         
         AHI   R5,1                YES: BUMP COUNTER                            
         LA    RF,3(RF)            BUMP TO NEXT DEMO                            
         CLI   DBDEMTYP,C'4'                                                    
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         B     DEMNAD0I                                                         
*                                                                               
DEMNAD0M DS    0H                                                               
*                                  ENCODE THE DEMO LISTS FOR PLD                
         ST    R5,NUMDEMOS                                                      
         L     R1,APARM            A(PARALLEL PERSONAL LANGUAGE ARRAY)          
         MVC   DMCB+12(4),12(R1)                                                
         GOTO1 ADEMOCON,DMCB,((R5),(R2)),('DEMOCON_17',(R2)),(R3)               
*                                                                               
DEMNAD01 DS    0H                                                               
         CLI   0(R2),EOT           TEST E-O-L                                   
         BE    DEMNAD30                                                         
*                                                                               
         CLI   0(R2),0             ALWAYS RESET FOR USA                         
         JL    *+14                                                             
         CLC   LASTNAD,0(R2)       TEST SAME CATEGORY AS PREVIOUS               
         JE    DEMNAD06            YES - ELEMENT ALREADY FOUND                  
*                                                                               
         XC    ADEMOEL(ADEMOELL),ADEMOEL                                        
         NI    DEMOFLAG,255-DEMOPRES                                            
         MVC   LASTNAD,0(R2)                                                    
         L     R6,DBAQUART         R6=A(FIRST RECORD ELEMENT)                   
         ST    R6,AQTHREL                                                       
         XR    R0,R0                                                            
         CLI   LASTNAD,0        IF USA REQUEST - WON'T MATCH NAD X'01'          
         JE    DEMNAD08         SO JUST LEAVE ALONE                             
         USING SLELEM,R6                                                        
*                                                                               
DEMNAD02 CLI   SLCODE,0            TEST E-O-R                                   
         JNE   *+14                                                             
         XC    AQTHREL,AQTHREL     YES - SET ELEMENT NOT FOUND                  
         J     DEMNAD06                                                         
*                                                                               
         CLI   SLCODE,SLCODEQ      TEST SECTION CODE ELEMENT                    
         JNE   DEMNAD04                                                         
         CLC   SLSECT,0(R2)        YES - MATCH TO DEMO                          
         JNE   DEMNAD04                                                         
         ST    R6,AQTHREL                                                       
         IC    R0,SLLEN                                                         
         LR    RE,R6                                                            
         AR    RE,R0                                                            
****     CLI   0(RE),X'30'                                                      
         CLI   0(RE),X'24'                                                      
         BL    DEMNAD03A                                                        
         CLI   0(RE),X'5C'                                                      
         BH    DEMNAD03A                                                        
         J     DEMNAD06                                                         
DEMNAD03A XC   AQTHREL,AQTHREL                                                  
         J     DEMNAD06                                                         
*                                                                               
DEMNAD04 IC    R0,SLLEN            BUMP TO NEXT ELEMENT                         
         AR    R6,R0                                                            
         CLI   SLCODE,X'0F'         NEXT PROGRAM SEGEMENT AVERAGE               
         JNE   DEMNAD02                                                         
         XC    AQTHREL,AQTHREL     SET ELEMENT NOT FOUND                        
*                                                                               
DEMNAD06 OC    AQTHREL,AQTHREL                                                  
         JNZ   DEMNAD08                                                         
*                                                                               
         XC    0(4,R4),0(R4)                                                    
****     TM    DEMOFLAG,OVERPRES   TEST IF POTENTIAL OVERRIDE DEMO              
****     JZ    DEMNAD20             NO DEMOS IN RECD - NEXT DEMO                
         CLI   0(R6),0                                                          
         JNE   DEMNAD20                                                         
         MVC   0(7,R6),=X'2305000000000000' SET DUMMY SECTION LEAD              
         MVC   2(1,R6),0(R2)                AND EOR                             
         ST    R6,AQTHREL                                                       
*                                                                               
DEMNAD08 MVI   QTHRELEM,SLCODEQ                                                 
*                                                                               
         XR    R0,R0                                                            
         ICM   RE,15,PRECLIST                                                   
         JZ    DEMNAD14                                                         
*                                                                               
DEMNAD10 CLI   0(RE),X'FF'                                                      
         JE    DEMNAD14                                                         
         LLC   RF,1(R2)            ENCODED MODIFIER                             
         A     RF,APLMODTB         A(DECODED MODIFIER)                          
         CLC   0(1,RF),0(RE)       PRECISION CATEGORY                           
         JNE   DEMNAD12                                                         
*                                                                               
         L     RF,SYSOPT                                                        
         XR    R0,R0                                                            
         IC    R0,2(RE)                                                         
         AR    RF,R0                                                            
         IC    R0,0(RF)                                                         
         LR    RF,R0                                                            
         IC    R0,1(RE)            SET OVERRIDE PRECISION                       
         CLI   2(RE),255           USE LIST FOR SCALE DIRECTION                 
         JE    *+6                                                              
         OR    R0,RF                                                            
         J     DEMNAD14                                                         
*                                                                               
DEMNAD12 LA    RE,3(RE)                                                         
         J     DEMNAD10                                                         
*                                                                               
DEMNAD14 XR    R5,R5                                                            
         ICM   R5,7,1(R2)          SAVE THE ORIGINAL DEMO                       
*                                                                               
         BRAS  RE,DEMNADG1                                                      
*                                                                               
         CLI   LASTNAD,2           NAD OVERRIDES FOR HOMES ARE HIDDEN           
         JL    DEMNAD18            CHANGE DEMO NUMBER TO GET OVERRIDES          
         CLI   DBDEMTYP,C'4'       SUPPORT 4 BYTE DEMOS                         
         JE    DEMNAD16                                                         
         CLI   2(R2),1             SUPPORT 3 BYTE DEMOS                         
         JNE   DEMNAD18                                                         
         MVI   2(R2),249                                                        
         J     DEMNAD18                                                         
*                                                                               
DEMNAD16 CLC   2(2,R2),=H'1'                                                    
         JNE   DEMNAD18                                                         
         MVC   2(2,R2),=H'249'                                                  
*                                                                               
DEMNAD18 DS    0H                                                               
         GOTO1 DEMCALC,DMCB,((R0),1(R2)),(DBDEMTYP,(R4)),0                      
*                                                                               
         STCM  R5,7,1(R2)          RESTORE THE ORIGINAL DEMO                    
         L     RF,AQTHREL                                                       
         CLC   0(2,RF),=X'2305'                                                 
         JNE   DEMNAD20                                                         
         XC    AQTHREL,AQTHREL                                                  
         XC    0(3,RF),0(RF)                                                    
*                                                                               
DEMNAD20 LA    R2,3(R2)            BUMP TO NEXT DEMO                            
         CLI   DBDEMTYP,C'4'                                                    
         JNE   *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
*                                                                               
         J     DEMNAD01                                                         
*                                                                               
DEMNAD30 DS    0H                                                               
         CLI   LISTTYPE,LISTMULP   MULTI-DEMO LIST PASSED W/ PLD?               
         BNE   DEMNADX                                                          
*                                  DECODE THE DEMO LIST BEFORE EXITING          
         L     R0,NUMDEMOS                                                      
         L     R1,APARM            A(PARALLEL PERSONAL LANGUAGE ARRAY)          
         MVC   DMCB+12(4),12(R1)                                                
         GOTO1 ADEMOCON,DMCB,((R0),ADEMOLST),('DEMOCON_16',ADEMOLST),  +        
               (R3)                                                             
*                                                                               
DEMNADX  DS    0H                                                               
         B     DEMOX                                                            
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* GAA WHATEVER THAT IS                                                *         
***********************************************************************         
*                                                                               
DEMNADG1 NTR1                                                                   
*                                                                               
         MVI   GAASW,0             RESET FOR NEXT DEMO                          
*                                                                               
         LLC   R1,1(R2)            ENCODED MODIFIER                             
         A     R1,APLMODTB         A(DECODED MODIFIER)                          
*                                                                               
         CLC   DBSELSTA(3),=C'HUT' HUT STATION                                  
         JNE   DEMNADG2                                                         
         CLI   0(R1),DEMO_MODIFIER_P EQUATE PUTS...                             
         JE    *+12                                                             
         CLI   0(R1),DEMO_MODIFIER_O AND TP PUTS...                             
         JNE   DEMNADG2                                                         
*                                                                               
*                                  ...TO RATINGS                                
         LLC   RF,1(R2)            ENCODED MODIFIER                             
         A     RF,APLPLDTB         A(DECODED PLD CHARACTER)                     
         MVI   1(R2),DEMO_MODIFIER_R TO RATINGS                                 
         GOTO1 ADEMOCON,DMCB,(1,(R2)),('DEMOCON_17',(R2)),(R3),0(RF)            
         B     DEMNADG3                                                         
*                                                                               
DEMNADG2 CLI   0(R1),DEMO_MODIFIER_L GAA RATING                                 
         JNE   *+8                                                              
         MVI   GAASW,1                                                          
         CLI   0(R1),DEMO_MODIFIER_N GAA IMPS                                   
         JNE   *+8                                                              
         MVI   GAASW,1                                                          
         CLI   0(R1),DEMO_MODIFIER_M GAA VPVH                                   
         JNE   *+8                                                              
         MVI   GAASW,1                                                          
         CLI   0(R1),DEMO_MODIFIER_F GAA RAW IMPRESSIONS                        
         JNE   *+8                                                              
         MVI   GAASW,1                                                          
*                                                                               
DEMNADG3 DS    0H                                                               
         CLI   GAASW,0             ANY GAA REQUESTED                            
         BE    DEMNADGX            NO - IT'S GOT TO BE OK                       
         L     RE,AQTHREL          YES - MAKE SURE WE HAVE SOME GAA             
         XR    R1,R1                                                            
*                                                                               
DEMNADG4 CLI   0(RE),0             EOR - RESET GAA                              
         JE    DEMNADG6                                                         
         CLI   0(RE),X'55'         GAA OK                                       
         BE    DEMNADGX                                                         
         CLI   0(RE),X'56'         FOR NAD-GAA                                  
         BE    DEMNADGX                                                         
         JH    DEMNADG6            NO GAA ELEMENT - RESET GAA                   
*                                                                               
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         J     DEMNADG4                                                         
*                                                                               
DEMNADG6 MVI   GAASW,0                                                          
*                                                                               
DEMNADGX DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RADIO FILE DEMO LOOKUPS (REGULAR DEMO LISTS ONLY)                   *         
***********************************************************************         
         SPACE 1                                                                
DEMRAD   CLI   0(R2),EOT           TEST E-O-L                                   
         BE    DEMOX                                                            
         CLI   DBACTSRC,C'N'       FOR BIRCH                                    
         JNE   *+8                                                              
         MVI   DBACTSRC,C'A'       USE ARB FORMULAS ALWAYS                      
         NI    DEMOFLAG,255-DEMOPRES                                            
         CLI   0(R2),0             NO CATEGORY GIVEN                            
         JE    *+14                                                             
         CLC   LASTNAD,0(R2)       TEST SAME CATEGORY AS PREVIOUS               
         JE    DEMRAD10            YES - ELEMENT ALREADY FOUND                  
         XC    ADEMOEL(ADEMOELL),ADEMOEL                                        
         MVC   LASTNAD,0(R2)                                                    
         ICM   R1,15,DBAQUART      R1=A(FIRST RECORD ELEMENT)                   
         JZ    DEMRAD04                                                         
         XR    R0,R0                                                            
         USING SLELEM,R1                                                        
* ALWAYS CHECK THE GEO INDICATOR                                                
******   CLI   0(R2),0             DEFAULT TO METRO DEMOS                       
******   JE    DEMRAD06                                                         
*                                                                               
DEMRAD02 CLI   SLCODE,0            TEST E-O-R                                   
         JNE   *+14                                                             
DEMRAD04 XC    AQTHREL,AQTHREL     YES - SET ELEMENT NOT FOUND                  
         J     DEMRAD10                                                         
*                                                                               
         CLI   SLCODE,SLCODEQ      TEST SECTION CODE ELEMENT                    
         JNE   DEMRAD08                                                         
*                                                                               
         CLI   0(R2),0             IF NO GEO ASKED FOR                          
         BNE   DEMRAD05            THEN WE HAVE TO DEFAULT TO METRO             
         CLI   SLSECT,X'01'        MAKE SURE THE SLSECT ELEMENT IS              
         BNE   DEMRAD04            X'01' FOR METRO                              
         BE    DEMRAD06                                                         
*                                                                               
DEMRAD05 CLC   SLSECT,0(R2)        YES - MATCH TO DEMO                          
         JNE   DEMRAD08                                                         
*                                                                               
DEMRAD06 ST    R1,AQTHREL          SAVE THE CURRENT SECTION ADDRESS             
         J     DEMRAD10                                                         
*                                                                               
DEMRAD08 IC    R0,SLLEN            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         J     DEMRAD02                                                         
*                                                                               
DEMRAD10 OC    AQTHREL,AQTHREL     TEST SECTION CODE ELEMENT FOUND              
         JNZ   *+14                                                             
         XC    0(4,R4),0(R4)       NO - CLEAR OUTPUT VALUE                      
         J     DEMRAD16                                                         
         MVI   QTHRELEM,SLCODEQ                                                 
         GOTO1 DEMCALC,DMCB,1(R2),(DBDEMTYP,(R4)),0                             
         BRAS  RE,SPCLARB                                                       
*                                                                               
*-->NEW ARB FMT FILE PREC IS ACTUALLY IN UNITS (EVEN THOUGH DMDSP=00'S)         
*-->WE ADJUST THE OUTPUT PRECN HERE SO THEY ACTUALLY GET 00'S                   
*                                                                               
         CLC   DBINTFIL,=C'TR'     ARB/TRITON RADIO FILE?                       
         JNE   DEMRAD16            NEW FORMAT FROM FEB/96 ON                    
         CLI   DBACTSRC,C'T'       TRITON                                       
         JE    *+12                                                             
         CLI   DBACTSRC,C'A'       ARBITRON                                     
         JNE   DEMRAD16                                                         
         CLC   DBACTBK,=AL2(FEB_96)  AFTER FEB/96 FILE ACTUALLY IS IN           
         JL    DEMRAD16            UNITS (EVEN THOUGH DEMDISP=HUNDREDS)         
         CLI   1(R2),DEMO_MODIFIER_R DOES NOT APPLY FOR RATINGS                 
         JE    DEMRAD16                                                         
*                                                                               
         LA    RF,RADTAB           LOOK FOR ADJUSTMENT REQUIREMENTS             
         CLI   DBACTSRC,C'R'                                                    
         BNE   *+8                 RADAR                                        
         LA    RF,RADTABR                                                       
DEMRAD12 CLI   0(RF),255                                                        
         JE    DEMRAD16                                                         
         CLC   0(1,RF),1(R2)                                                    
         JE    DEMRAD14                                                         
         LA    RF,1(RF)                                                         
         J     DEMRAD12                                                         
*                                                                               
RADTAB   DC    C'IUCDEJKMQ'                                                     
RADTABR  DC    C'IUDEJKMQ'         RADAR TABLE                                  
         DC    AL1(255)                                                         
*                                                                               
DEMRAD14 L     RE,0(R4)            ROUND AND DIVIDE BY 100                      
         SRDL  RE,32                                                            
         AHI   RF,50                                                            
         D     RE,=F'100'                                                       
         ST    RF,0(R4)                                                         
*                                                                               
DEMRAD16 LA    R2,3(R2)            BUMP TO NEXT DEMO                            
         CLI   DBDEMTYP,C'4'                                                    
         JNE   *+8                                                              
         LA    R2,1(R2)                                                         
         LA    R4,4(R4)                                                         
         J     DEMRAD                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALCULATE A DEMO VALUE FROM A DEMO RECORD                *         
*                                                                     *         
* THIS ROUTINE CAN CALL ITSELF TO CALCULATE DEMOS WHICH ARE FORMULA   *         
* BASED. UNCOUNTM IS THE MAXIMUM RECURSION LEVEL ALLOWED.             *         
*                                                                     *         
* NTRY -  R1 POINTS TO A PARAMETER LIST AS FOLLOWS -                  *         
*                                                                     *         
* P1 BYTE 0   = OUTPUT FIELD PRECISION (ZERO=USE DEFAULT PRECISION)   *         
*         1-3 = A(2 BYTE DEMO EXPRESSION)                             *         
*                                                                     *         
* P2 BYTE 0   = DEMO FORMAT                                                     
*         1-3 = A(FULLWORD OUTPUT AREA)                                         
*                                                                     *         
* P3 BYTE 1-3 = A(ELEMENT CODE, FIELD NUMBER & PRECISION) OR ZERO     *         
***********************************************************************         
         SPACE 1                                                                
DEMCALC  NTR1  WORK=(RC,DEMCALCL)                                               
         STAR  CLEAR=Y,ARS=OFF                                                  
         USING DEMCALCD,RC         RC=A(LOCAL W/S)                              
*                                                                               
         XC    DEMCALCD(DEMCALCL),DEMCALCD   CLEAR W/S (ASSUMES < 257)          
*                                                                               
         L     RE,0(R1)            SAVE PARAMETER VALUES IN W/S                 
         MVC   OVREXP,0(RE)                                                     
         MVC   DEMOEXP(1),0(RE)    SAVE DEMO EXPRESSION                         
         MVI   DEMOEXP+1,0         CONVERT 2 CHAR EXPR TO 3 FOR NOW             
         MVC   DEMOEXP+2(1),1(RE)                                               
*                                                                               
         CLI   4(R1),0             UNPACKED 2 CHARACTER DEMO EXPRESSION         
         JE    DC2CHAR                                                          
         CLI   4(R1),C'P'          PACKED 3 CHARACTER DEMO EXPRESSION           
         JNE   DC3CHAR                                                          
*                                                                               
         LLC   R0,0(RE)            SET UP THE MODIFIER                          
         SRL   R0,3                                                             
         BCTR  R0,0                                                             
         LAY   RF,CHARTAB                                                       
         AR    RF,R0                                                            
         MVC   DEMOEXP(1),0(RF)                                                 
         XR    RF,RF                                                            
         ICM   RF,3,0(RE)          SET UP THE SEX CODE                          
         SLL   RF,21                                                            
         SRL   RF,28                                                            
         STC   RF,DEMOEXP+1        SET UP AGE CODE                              
         IC    RF,1(RE)                                                         
         SLL   RF,25                                                            
         SRL   RF,25                                                            
         STC   RF,DEMOEXP+2                                                     
         CLI   DEMOEXP+1,1         0-1 ARE OLD SEX/AGE CODES PACKED             
         JH    DC2CHAR                                                          
         ZIC   RF,DEMOEXP+1        RUN NUMBER FROM 1-256                        
         SLL   RF,7                 (THIS ALLOWS USE OF OLD FORMULAS)           
         STC   RF,DEMOEXP+1                                                     
         OC    DEMOEXP+2(1),DEMOEXP+1                                           
         MVI   DEMOEXP+1,0         ZAP THE SEX CODE                             
         J     DC2CHAR                                                          
*                                                                               
DC3CHAR  CLI   4(R1),3                                                          
         BNE   DC4CHAR                                                          
*                                                                               
         MVC   DEMOEXP,0(RE)                                                    
         XC    OVREXP,OVREXP                                                    
         CLI   1(RE),0                                                          
         BNE   DC2CHAR                                                          
         MVC   OVREXP(1),0(RE)                                                  
         MVC   OVREXP+1(1),2(RE)                                                
*                                                                               
DC4CHAR  CLI   4(R1),C'4'          4 CHARACTER CODE                             
         BNE   DC2CHAR                                                          
*                                                                               
         MVC   DEMOEXP,0(RE)                                                    
         XC    OVREXP,OVREXP                                                    
         CLI   DEMOEXP+1,1         NEED 4 CHAR - NO OVERRIDE SUPPORT            
         BH    DC2CHAR                                                          
         CLI   DENGINE,DENGONQ     DON'T ADJUST FOR DEMO ENGINE OPTION          
         BE    *+20                                                             
         CLI   DEMOEXP+1,1         0 AND 1 RUN 1-250                            
         BNE   *+12                FOR OLD NUMBERS                              
         OI    DEMOEXP+2,X'80'                                                  
         MVI   DEMOEXP+1,0                                                      
         MVC   OVREXP(1),DEMOEXP                                                
         MVC   OVREXP+1(1),DEMOEXP+2                                            
*                                                                               
DC2CHAR  L     RE,4(R1)                                                         
         LA    RE,0(RE)                                                         
         ST    RE,DEMAOUT          SAVE A(OUTPUT VALUE)                         
         XC    0(4,RE),0(RE)       CLEAR OUTPUT VALUE                           
                                                                                
         CLI   FCOVELEM,X'35'      ANY FULL COVERAGE?                           
         JNE   CHKENG                                                           
         CLI   BASEMOD,DEMO_MODIFIER_I                                          
         JNE   CHKENG                                                           
         CLI   DEMOEXP,DEMO_MODIFIER_I IMPRESSION = FULL COVERAGE               
         JNE   CHKENG                                                           
         MVI   DEMOEXP,DEMO_MODIFIER_F                                          
*                                                                               
CHKENG   DS    0H                                                               
         TRAPIT                                                                 
         CLI   DENGINE,DENGONQ                                                  
         BNE   DCNOGAA                                                          
         MVC   ENGEXP,DEMOEXP      DEMO EXPRESSION FOR DEMO ENGINE              
         CLI   GAASW,0             IF GAA DATA IS ON RECD, USE IT               
         BE    DCNOGAA                                                          
         CLI   ENGMODC,DEMO_MODIFIER_R GAA RATING                               
         BNE   *+8                                                              
         MVI   ENGMODC,DEMO_MODIFIER_L                                          
         CLI   ENGMODC,DEMO_MODIFIER_V GAA VPVH                                 
         BNE   *+8                                                              
         MVI   ENGMODC,DEMO_MODIFIER_M                                          
         CLI   ENGMODC,DEMO_MODIFIER_I GAA IMPRESSIONS                          
         BNE   *+8                                                              
         MVI   ENGMODC,DEMO_MODIFIER_N                                          
         CLI   ENGMODC,DEMO_MODIFIER_Y GAA RAW IMPRESSIONS                      
         BNE   DCNOGAA                                                          
         MVI   ENGMODC,DEMO_MODIFIER_B                                          
         CLC   DBINTFIL(2),=AL2(NADFILE)                                        
         BNE   *+8                                                              
         MVI   ENGMODC,DEMO_MODIFIER_F                                          
*                                                                               
DCNOGAA  DS    0H                                                               
         CLC   DEMOEXP,=AL1(DEMO_MODIFIER_U,0,15) U15 = REACH                   
         JNE   *+10                                                             
         MVC   DEMOEXP,=AL1(DEMO_MODIFIER_U,0,1) USE THE HOMES UNIVERSE         
*                                                                               
* EXTRACT THE INDEX FOR UPGRADES                                                
         CLC   DEMOEXP,=AL1(DEMO_MODIFIER_Z,0,254)  Z254=INDEX VALUE            
         BNE   NOIDXF                                                           
         LA    R1,1000             FORCE TO BASE 1000                           
         BRAS  RE,SETNDX                                                        
*        THIS IS DONE BEFORE WE LOOK FOR OVERRIDES                              
*        AND TELLS ME WHETHER THE PREVIOUS DEMO VALUE IS AN OVERRIDE            
*        IF IT IS I DONT WANT TO INDEX IT                                       
*                                                                               
         CLI   PREVOVR,C'Y'        PREVIOUS VALUE AN OVERRIDE                   
         BNE   *+8                                                              
         LA    R1,1000             YES - SET TO NO INDEX                        
         MVI   PREVOVR,C'N'        RESET OVERRIDE INDICATOR                     
         B     DEMCP26                                                          
*                                                                               
* OGILVY SUPERSTATION IMP ADJUST FACTORS                                        
*                                                                               
NOIDXF   B     DEMCP04                                                          
*&&DO                                                                           
*                                  C255=SUPERSTATION ADJUST                     
NOIDXF   CLC   DEMOEXP,=AL1(DEMO_MODIFIER_C,0,255)                              
         BNE   DEMCP02                                                          
         LA    R1,0                SET DEFAULT TO SUPPRESS                      
         CLI   DBSELSRC,C'N'       ONLY DO FOR NSI                              
         BNE   DEMCP26                                                          
         CLC   DBACTSTA,=C'WGN T'  THEN CHECK STATION OVERRIDES                 
         BNE   *+8                 AND SEED THIER FACTORS                       
         LA    R1,1200                                                          
         CLC   DBACTSTA,=C'WOR T'                                               
         BE    *+10                                                             
         CLC   DBACTSTA,=C'WPIXT'                                               
         BNE   *+8                                                              
         LA    R1,1150                                                          
         B     DEMCP26             RETURN THE FACTOR                            
*                                                                               
*                                    C254=INCLUDE IMPRESSIONS SWITCH            
DEMCP02  CLC   DEMOEXP,=AL1(DEMO_MODIFIER_C,0,254)                              
         JNE   DEMCP04                                                          
         LA    R1,1000             SET DEFAULT TO INCLUDE IMPS                  
         CLI   DBSELSRC,C'N'       ONLY DO FOR NSI                              
         JNE   DEMCP26                                                          
         CLC   DBACTSTA,=C'WGN T'  CHECK STATION OVERRIDES                      
         JE    *+10                                                             
         CLC   DBACTSTA,=C'WOR T'                                               
         JE    *+10                                                             
         CLC   DBACTSTA,=C'WPIXT'                                               
         JNE   DEMCP26                                                          
         XR    R1,R1               EXCLUDE REGULAR IMPS.                        
         J     DEMCP26             RETURN THE SWITCH                            
*&&                                                                             
*                                                                               
DEMCP04  CLI   DBSELMED,C'R'       FOR RADIO                                    
         JNE   DEMCP06                                                          
*                                  THIS IS NUMBER OF QH FOR TSL CALC            
         CLC   DEMOEXP,=AL1(DEMO_MODIFIER_T,0,255)                              
         JNE   DEMCP06                                                          
         LH    R1,DBFACTOR                                                      
         J     DEMCP26                                                          
*                                                                               
DEMCP06  MVC   DEMPREC,0(R1)       SAVE OUTPUT FIELD PRECISION                  
         ICM   RE,15,8(R1)         TEST DEMO ORIGIN PASSED                      
         JZ    DEMCP28             NO                                           
         CLI   0(RE),0             TEST ELEMENT CODE DEFINED                    
         JE    DEMCP28             NO                                           
*                                                                               
         CLI   DENGINE,DENGONQ     TRY COMPUTING DEMO THRU DEMO ENGINE          
         BNE   DEMCP07                                                          
         MVI   ROUNDIND,0                                                       
         BRAS  RE,DEMGETE                                                       
         B     DEMCP26             EXIT WITH VALUE OR ZERO                      
*                                                                               
DEMCP07  LA    R5,DEMDTAW                                                       
         USING MSTDTAD,R5                                                       
         MVC   MSTFLDN(2),0(RE)    SET ELEMENT CODE & FIELD NUMBER              
         MVC   DEMOPRC,2(RE)                                                    
         TM    DEMOFLAG,OVERPRES   TEST IF POTENTIAL OVERRIDE DEMO              
         JZ    DEMCP10             DEAL WITH NO OVERRIDES PRESENT               
         GOTO1 =A(DEMGETO),RR=RELO YES - GET OVERRIDE VALUE                     
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         JZ    DEMCP12                                                          
*                                                                               
*                                                                               
DEMCP08  MVC   ADJIPRC,DEMOPRC                                                  
         MVC   ADJOPRC,DEMPREC                                                  
         BRAS  RE,DEMADJP          ADJUST FIELD PRECISION                       
         J     DEMCP26                                                          
         DROP  R5                                                               
*                                                                               
* ESTIMATE NAD CATEGORIES ARE ONLY CARRIED AS OVERRIDES                         
* IF NO OVERRIDE ELEMENTS EXIST THEY CAN'T HAVE A VALUE                         
*                                                                               
DEMCP10  CLI   LASTNAD,0           NAD CATEGORY                                 
         JE    DEMCP12                                                          
         CLC   DBINTFIL(2),=AL3(EVNFILE) AND EVN FILE                           
         JNE   DEMCP12                                                          
         XR    R1,R1               NOT AVAILABLE - SO ZERO IT                   
         J     DEMCP08                                                          
*                                                                               
DEMCP12  CLC   =AL2(NTIFILE),DBINTFIL NETWORK FILE                              
         JE    *+14                                                             
         CLC   =AL2(CABFILE),DBINTFIL CABLE FILE                                
         JNE   DEMCP24                                                          
*                                                                               
         CLC   DEMOEXP+1(2),=X'0007'  T07 = MEAN AGE - SPECIAL                  
         JNE   *+16                                                             
         BRAS  RE,MEANAGE                                                       
         L     R1,MAPARAM                                                       
         J     DEMCP26                                                          
*                                                                               
         CLC   =AL2(NTIFILE),DBINTFIL NTI FILE ONLY                             
****     JNE   DEMCP24                                                          
         JNE   DEMCP14       REPLACE _R WITH _G FOR CABLE AS WELL               
                                                                                
         CLC   DEMOEXP,=AL1(DEMO_MODIFIER_T,0,15) T15 = REACH - SPECIAL         
         JNE   *+12                                                             
         BRAS  RE,NEREACH                                                       
         J     DEMCP26                                                          
*                                                                               
         CLC   DBACTBK,=AL1(YR_1987,WEEK_33)  PRIOR 8733 IS OK                  
         JL    DEMCP24                                                          
         CLI   DBSELSTA+4,C'S'                                                  
*        JE    *+12                                                             
         CLI   GAASW,1                                                          
         JNE   DEMCP13                                                          
*                                                                               
         CLI   DEMOEXP,DEMO_MODIFIER_T                                          
         JNE   DEMCP13                                                          
         CLI   UNCOUNT,0                                                        
         JE    DEMCP13                                                          
         MVI   DEMOEXP,DEMO_MODIFIER_N                                          
*                                                                               
DEMCP13  DS    0H                                                               
*                                  TRYING TO CALCULATE VPHS FROM IMPS           
         CLI   DEMOEXP,DEMO_MODIFIER_E                                          
         JNE   DEMCP14                                                          
         CLI   NEIDELEM,X'41'      ARE IMPS THERE                               
         JE    DEMCP24             YES - USE THEM                               
         MVI   DEMOEXP,DEMO_MODIFIER_F NO - USE VPH BASED FORMULAS              
         J     DEMCP24                                                          
*                                                                               
DEMCP14  CLI   DEMOEXP,DEMO_MODIFIER_R ONLY FOR RATINGS                         
         JNE   DEMCP24                                                          
         CLI   DEMOEXP+2,20        HOMES ARE OK                                 
         JL    DEMCP24                                                          
         CLI   NEIDELEM,X'41'      SPECIAL CODE FOR NTI FILE                    
         JE    *+12                USE OVERRIDE FORMULA IF BASE CELLS           
         MVI   DEMOEXP,DEMO_MODIFIER_G ARE NOT PRESENT                          
         J     DEMCP24                                                          
*                                                                               
         OC    AOVEREL,AOVEREL     CHECK FOR OVERRIDES                          
         JNZ   DEMCP24                                                          
         L     RF,AOVEREL                                                       
         XR    R0,R0                                                            
*                                                                               
DEMCP16  CLI   2(RF),0             NEW FORMAT                                   
         JNE   DEMCP18                                                          
*                                                                               
         CLI   LASTNAD,0           IF CATEGORY IS ZERO COMING IN                
         JNE   *+12                                                             
         CLI   3(RF),0             ONLY ACCEPT CAT ZERO OVERRIDES               
         JNE   DEMCP20              FOR HOMES                                   
*                                                                               
         CLI   5(RF),1             CHECK FOR HOMES                              
         JE    DEMCP22              YES - JUST ACCEPT IT                        
         CLC   3(1,RF),LASTNAD     CATEGORY MUST MATCH                          
         JNE   *+10                                                             
         CLC   5(1,RF),DEMOEXP+2   AND THIS DEMO                                
         JE    DEMCP22                                                          
         J     DEMCP20                                                          
*                                                                               
DEMCP18  CLI   3(RF),1             CHECK FOR HOMES                              
         JE    DEMCP22                                                          
         CLC   3(1,RF),DEMOEXP+2   OR THIS DEMO                                 
         JE    DEMCP22                                                          
*                                                                               
DEMCP20  IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLC   0(1,RF),OVERELEM                                                 
         JE    DEMCP16                                                          
         CLI   0(RF),0                                                          
         JE    DEMCP24                                                          
         J     DEMCP20                                                          
*                                                                               
DEMCP22  MVI   DEMOEXP,DEMO_MODIFIER_G                                          
*                                                                               
DEMCP24  CLC   DBFILE,=C'NTI'      TEST IF FILE IS NETWORK                      
         JNE   *+14                                                             
         XR    R1,R1                                                            
         CLI   LASTNAD,0           ONLY ALLOW USA LINES                         
         JNE   DEMCP28                                                          
*                                                                               
         BRAS  RE,DEMGET           TEST IF DEMO IS ON RECORD                    
         JZ    DEMCP28             NO - EXIT                                    
         MVC   ADJIPRC,DEMIPRC                                                  
         MVC   ADJOPRC,DEMPREC                                                  
         BRAS  RE,DEMADJP          ADJUST FIELD PRECISION                       
*                                                                               
DEMCP26  L     RE,DEMAOUT          RETURN DEMO VALUE                            
         STCM  R1,15,0(RE)                                                      
         REAR  ARS=OFF                                                          
         J     DCALCX                                                           
*                                                                               
DEMCP28  CLC   =AL2(NTIFILE),DBINTFIL NETWORK FILE                              
         JE    *+14                                                             
         CLC   =AL2(CABFILE),DBINTFIL CABLE FILE                                
         JNE   DEMCP40                                                          
*                                                                               
         CLC   =C'RLD',DBFILE      ALLOW ZHOMES FOR MINUTE BY MINUTE            
         BE    DEMCP29                                                          
         CLC   =AL2(CABFILE),DBINTFIL CABLE FILE ONLY                           
         JNE   DEMCP29                                                          
         CLI   DEMOEXP,DEMO_MODIFIER_Z PROGRAM PUTS                             
         JNE   DEMCP29                                                          
         CLC   DEMOEXP+1(2),=X'0001'  HOMES                                     
         JNE   DEMCP29                                                          
         SR    R1,R1               RETURN 0. BAD DATA ON RECD                   
         J     DEMCP26                                                          
*                                                                               
DEMCP29  CLC   DEMOEXP+1(2),=X'0007'  T07 = MEAN AGE - SPECIAL                  
         JNE   *+16                                                             
         BRAS  RE,MEANAGE                                                       
         L     R1,MAPARAM                                                       
         J     DEMCP26                                                          
*                                                                               
         CLC   =AL2(NTIFILE),DBINTFIL NTI FILE ONLY                             
****     JNE   DEMCP40                                                          
         JNE   DEMCP29E       REPLACE _R WITH TO _G FOR CABLE AS WELL           
         CLC   DEMOEXP,=AL1(DEMO_MODIFIER_T,0,15) T15 = REACH - SPECIAL         
         JNE   *+12                                                             
         BRAS  RE,NEREACH                                                       
         J     DEMCP26                                                          
*                                                                               
         CLC   DBACTBK,=AL1(YR_1987,WEEK_33)  PRIOR 8733 IS OK                  
         JL    DEMCP40                                                          
*                                                                               
         CLI   DBSELSTA+4,C'S'                                                  
*        JE    *+12                                                             
         CLI   GAASW,1                                                          
         JNE   DEMCP29E                                                         
*                                                                               
         CLI   DEMOEXP,DEMO_MODIFIER_T                                          
         JNE   DEMCP29E                                                         
         CLI   UNCOUNT,0                                                        
         JE    DEMCP29E                                                         
         MVI   DEMOEXP,DEMO_MODIFIER_N                                          
*                                                                               
DEMCP29E DS    0H                                                               
*                                  TRYING TO CALCULATE VPHS FROM IMPS           
         LLC   RF,DEMOEXP          ENCODED MODIFIER                             
         A     RF,APLPLDTB         A(DECODED PLD CHARACTER)                     
         MVC   BYTE2,0(RF)         SAVE DECODED PLD CHARACTER                   
         LLC   RF,DEMOEXP          ENCODED MODIFIER                             
         A     RF,APLMODTB         A(DECODED MODIFIER)                          
         CLC   =AL2(CABFILE),DBINTFIL    DON'T DO THIS FOR CABLE                
         BE    DEMCP30                                                          
         CLI   0(RF),DEMO_MODIFIER_E                                            
         JNE   DEMCP30                                                          
         CLI   NEIDELEM,X'41'      ARE IMPS THERE                               
         JE    DEMCP40             YES - USE THEM                               
         XC    FULL2,FULL2         DEMOCON EXPECTS A DEMO LIST ENTRY            
         MVI   FULL2+1,DEMO_MODIFIER_F   USE VPH BASED FORMULAS                 
         GOTO1 ADEMOCON,DMCB,(1,FULL2),('DEMOCON_17',FULL2),(R3),BYTE2          
         MVC   DEMOEXP(1),FULL2+1  USE ENCODED MODIFIER                         
         J     DEMCP40                                                          
*                                                                               
DEMCP30  DS    0H                                                               
         CLI   0(RF),DEMO_MODIFIER_R   ONLY FOR RATINGS                         
         JNE   DEMCP40                                                          
         CLI   DEMOEXP+2,20        HOMES ARE OK                                 
         JL    DEMCP40                                                          
         CLI   NEIDELEM,X'41'      SPECIAL CODE FOR NTI FILE                    
         JE    DEMCP31             USE OVERRIDE FORMULA IF BASE CELLS           
*                                   ...ARE NOT PRESENT                          
         XC    FULL2,FULL2         DEMOCON EXPECTS A DEMO LIST ENTRY            
         MVI   FULL2+1,DEMO_MODIFIER_G                                          
         GOTO1 ADEMOCON,DMCB,(1,FULL2),('DEMOCON_17',FULL2),(R3),BYTE2          
         MVC   DEMOEXP(1),FULL2+1  USE ENCODED MODIFIER                         
         J     DEMCP40                                                          
*                                                                               
DEMCP31  DS    0H                                                               
         OC    AOVEREL,AOVEREL     CHECK FOR OVERRIDES                          
         JZ    DEMCP40                                                          
         L     RF,AOVEREL                                                       
         XR    R0,R0                                                            
*                                                                               
DEMCP32  CLI   2(RF),0             CHECK FOR ALT OVR ELEMENT                    
         JNE   DEMCP34                                                          
*                                                                               
         CLI   LASTNAD,0           IF CATEGORY IS ZERO COMING IN                
         JNE   *+12                                                             
         CLI   3(RF),0             ONLY ACCEPT CAT ZERO OVERRIDES               
         JNE   DEMCP36              FOR HOMES                                   
*                                                                               
         CLI   5(RF),1             CHECK FOR HOMES                              
         JE    DEMCP38                                                          
         CLC   3(1,RF),LASTNAD     CATEGORY MUST MATCH                          
         JNE   *+10                                                             
         CLC   5(1,RF),DEMOEXP+2   OR THIS DEMO                                 
         JE    DEMCP38                                                          
         J     DEMCP36                                                          
*                                                                               
DEMCP34  CLI   3(RF),1             CHECK FOR HOMES                              
         JE    DEMCP38                                                          
         CLC   3(1,RF),DEMOEXP+2   OR THIS DEMO                                 
         JE    DEMCP38                                                          
*                                                                               
DEMCP36  IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLC   0(1,RF),OVERELEM                                                 
         JE    DEMCP32                                                          
         CLI   0(RF),0                                                          
         JE    DEMCP40                                                          
         J     DEMCP36                                                          
*                                                                               
DEMCP38  MVI   DEMOEXP,DEMO_MODIFIER_G                                          
*                                                                               
DEMCP40  CLC   =AL2(MVGFILE),DBINTFIL SETUP FOR MOVIE GOER                      
         JE    *+14                                                             
         CLC   =AL2(NADFILE),DBINTFIL SETUP FOR NAD GAA DEFAULT                 
         JNE   DEMCP42                                                          
*                                                                               
         CLI   UNCOUNT,0           ONLY FOR BASIC LEVEL                         
         JNE   DEMCP42                                                          
         CLI   GAASW,1             ANY GAA AVAILABLE                            
         JE    DEMCP42             YES - JUST PROCESS                           
         CLI   DEMOEXP,DEMO_MODIFIER_N IMP GAA REQUESTED                        
         JNE   *+8                 NO - JUST PROCESS                            
         MVI   DEMOEXP,DEMO_MODIFIER_T YES - DEFAULT TO NORMAL                  
         CLI   DEMOEXP,DEMO_MODIFIER_L RTG                                      
         JNE   *+8                                                              
         MVI   DEMOEXP,DEMO_MODIFIER_R                                          
         CLI   DEMOEXP,DEMO_MODIFIER_M VPH                                      
         JNE   *+8                                                              
         MVI   DEMOEXP,DEMO_MODIFIER_V                                          
         CLI   DEMOEXP,DEMO_MODIFIER_B RAW IMPRESSIONS                          
         JNE   *+8                                                              
         MVI   DEMOEXP,DEMO_MODIFIER_Y                                          
         CLI   DEMOEXP,DEMO_MODIFIER_F RAW IMPRESSIONS (NAD)                    
         JNE   *+8                                                              
         MVI   DEMOEXP,DEMO_MODIFIER_Y                                          
*                                                                               
DEMCP42  DS    0H                                                               
         TRAPIT                                                                 
         CLI   UNCOUNT,UNCOUNTM    TEST IF MAX RECURSION LEVEL REACHED          
         JH    DEMCN                                                            
         XR    RE,RE                                                            
         XR    R1,R1                                                            
         ICM   RE,1,UNCOUNT        BUMP RECURSION COUNT                         
         LA    R0,1(RE)                                                         
         STC   R0,UNCOUNT                                                       
         LA    RF,UNKNOWNS         RF=A(LIST OF UNRESOLVED DEMOS)               
         JZ    DEMCP44                                                          
         CLC   DEMOEXP,0(RF)       CIRCULAR FORMULA LOOP?                       
         JE    DEMCY               YES - EXIT                                   
         LA    RF,3(RF)                                                         
         BRCT  RE,*-14                                                          
*                                                                               
DEMCP44  MVC   0(3,RF),DEMOEXP     ADD DEMO TO LIST OF UNKNOWNS                 
*                                                                               
DEMCP46  DS    0H                                                               
         LLC   R1,DEMMODC          MODIFIER VALUE                               
         L     RE,APLPLDTB         A(PERSONAL LANGUAGE TRANSLATE TABLE)         
         AR    RE,R1               INDEX INTO TABLE                             
         TRAPIT                                                                 
         CLI   0(RE),LOWMODCD      IS THIS A VALID MODIFIER?                    
         JL    DEMCN               NO                                           
*                                                                               
         CLI   DENGINE,DENGONQ     TRY COMPUTING DEMO THRU DEMO ENGINE          
         BNE   DEMCP47                                                          
         MVI   ROUNDIND,0                                                       
         BRAS  RE,DEMGETE                                                       
         J     DEMCYE              EXIT WITH COMPUTED VALUE OR ZERO             
*                                                                               
DEMCP47  DS    0H                                                               
         LAM   AR0,ARF,=16F'0'     MAKE SURE THESE GET RESET                    
         LAM   AR6,AR6,ALET                                                     
         L     R6,AMSTHDR          SEARCH MASTER DEMO TABLE FOR DEMO            
         SAC   512                                                              
         USING MSTHDRD,R6          SET UP FOR TABLE SEARCH                      
*                                                                               
         MHI   R1,MSTMODNX_LEN     L'EACH TABLE ENTRY                           
         LA    R6,MSTMODNX(R1)     R6 = A(ENTRY) FOR THIS MODIFIER              
         TRAPIT                                                                 
         CLI   0(R6),0                                                          
         JNE   *+14                                                             
         CLC   1(MSTMODNX_LEN-1,R6),0(R6)                                       
         JE    DEMCN               NO MODIFIER TABLE                            
*                                                                               
*                                  SET RE TO A(START OF SEARCH)                 
         XR    RE,RE                                                            
         ICM   RE,7,(MSTMODNX_ASTART-MSTMODNX_ENTRY)(R6)                        
         LR    R7,RE                                                            
         XR    R5,R5                                                            
         XR    R1,R1                                                            
*                                  SET R1 TO A(END OF SEARCH - 1)               
         ICM   R1,7,(MSTMODNX_AEND-MSTMODNX_ENTRY)(R6)                          
*                                                                               
         L     R6,AMSTHDR          RESTORE A(MASTER DEMO TABLE)                 
         LA    R0,MSTDTALN                                                      
         LR    RF,R0                                                            
         SR    RE,R0               RE=A(START OF TABLE-L'ENTRY)                 
         SR    R1,RE                                                            
         AR    R0,R0               R0=LOWEST POWER OF 2 GE R1                   
         CR    R0,R1                                                            
         JNH   *-4                                                              
         AR    R1,RE               R1=A(END OF LAST ENTRY)                      
*                                                                               
DEMCP48  SRL   R0,1                1/2 REMAINING TABLE LENGTH                   
         CR    R0,RF               TEST IF LESS THAN AN ENTRY LENGTH            
         JL    DEMCP56                                                          
         BRXH  RE,R0,DEMCP52       COMPUTE NEW TABLE START ADDRESS              
*                                                                               
THIS     USING MSTDTAD,RE                                                       
         CPYA  ARE,AR6                                                          
         CLC   DEMOEXP,THIS.MSTMODC  TEST IF MODIFIER/DEMO FOUND                
         LAM   ARE,ARE,=F'0'                                                    
         JH    DEMCP48                                                          
         JL    DEMCP52                                                          
         CPYA  ARE,AR6                                                          
         CLC   BKNO,THIS.MSTBKNO   TEST IF BOOK FOUND                           
         LAM   ARE,ARE,=F'0'                                                    
         JE    DEMCP54                                                          
         JH    DEMCP48                                                          
*                                                                               
DEMCP50  CR    RE,R7               TEST POINTING TO S-O-T                       
         JE    DEMCP54                                                          
*                                                                               
         LR    R1,RE                                                            
         AHI   R1,-(MSTDTALN)      R1=A(PREVIOUS ENTRY)                         
         CPYA  AR1,AR6                                                          
PREV     USING MSTDTAD,R1                                                       
         CPYA  AR1,AR6                                                          
         CLC   DEMOEXP,PREV.MSTMODC                                             
         LAM   AR1,AR1,=F'0'                                                    
         JNE   DEMCP54                                                          
         CPYA  AR1,AR6                                                          
         CLC   BKNO,PREV.MSTBKNO                                                
         LAM   AR1,AR1,=F'0'                                                    
         JH    DEMCP54                                                          
*                                                                               
         LR    RE,R1               POINT RE TO PREVIOUS ENTRY                   
         JE    DEMCP54                                                          
         J     DEMCP50                                                          
         DROP  PREV                                                             
*                                                                               
DEMCP52  SR    RE,R0                                                            
         J     DEMCP48                                                          
*                                                                               
DEMCP54  LR    R5,RE               R5=A(DEMO ENTRY)                             
*                                                                               
DEMCP56  LR    RE,R7               LINEAR SEARCH FOR MACRO DEMO ENTRY           
         XR    R7,R7                                                            
*                                                                               
DEMCP60  CPYA  ARE,AR6                                                          
         CLC   THIS.MSTMODC,DEMMODC                                             
         LAM   ARE,ARE,=F'0'                                                    
         JNE   DEMCP64                                                          
         CPYA  ARE,AR6                                                          
         CLC   THIS.MSTDEMO,=H'0'  TEST THIS IS A MACRO FORMULA                 
         LAM   ARE,ARE,=F'0'                                                    
         JNE   DEMCP64             NO                                           
         CPYA  ARE,AR6                                                          
         CLC   BKNO,THIS.MSTBKNO   TEST CORRECT BOOK FOUND                      
         LAM   ARE,ARE,=F'0'                                                    
         JNH   *+12                                                             
         AHI   RE,MSTDTALN                                                      
         J     DEMCP60                                                          
*                                                                               
         LR    R7,RE               R7=A(MACRO ENTRY)                            
         CPYA  ARE,AR6                                                          
         MVC   DEMDTAW,0(RE)                                                    
         TM    THIS.MSTINDS,MSTUSEEQ    TEST IF A DEFAULT MACRO                 
         LAM   ARE,ARE,=F'0'                                                    
         JZ    DEMCP64                                                          
         NI    DEMDTAW+(MSTINDS-MSTDTAD),255-MSTUSEEQ                           
*                                                                               
DEMCP62  AHI   RE,MSTDTALN                                                      
         CPYA  ARE,AR6                                                          
         CLC   THIS.MSTMODC,DEMMODC                                             
         LAM   ARE,ARE,=F'0'                                                    
         JNE   DEMCP64                                                          
         CPYA  ARE,AR6                                                          
         CLC   THIS.MSTDEMO,=H'0'  TEST THIS IS A MACRO FORMULA                 
         LAM   ARE,ARE,=F'0'                                                    
         JNE   DEMCP64                                                          
         CPYA  ARE,AR6                                                          
         TM    THIS.MSTINDS,MSTUSEEQ    TEST IF A DEFAULT MACRO                 
         LAM   ARE,ARE,=F'0'                                                    
         JNZ   DEMCP62                                                          
         CPYA  ARE,AR6                                                          
         MVC   DEMDTAW+(MSTBKNO-MSTDTAD)(L'MSTBKNO),THIS.MSTBKNO                
         LAM   ARE,ARE,=F'0'                                                    
         DROP  THIS                                                             
*                                                                               
DEMCP64  LTR   R5,R5               TEST MICRO FOUND                             
         JZ    DEMCP66             NO                                           
         LTR   R7,R7               TEST MACRO FOUND                             
         JNZ   DEMCP68             YES                                          
         CPYA  AR5,AR6                                                          
         MVC   DEMDTAW,0(R5)                                                    
         LAM   AR5,AR5,=F'0'                                                    
         TM    DEMDTAW+(MSTINDS-MSTDTAD),MSTRECEQ                               
         JNZ   DEMCP74             USE THIS ENTRY IF A FORMULA                  
         MVI   DEMODUB,X'FF'       SET LOW BOOK VALUE                           
         J     DEMCP70             GO FIND FORMULA ENTRY                        
*                                                                               
DEMCP66  DS    0H                                                               
         TRAPIT                                                                 
         LTR   R7,R7               TEST MACRO FOUND                             
         JNZ   DEMCP74                                                          
         J     DEMCN                                                            
*                                                                               
DEMCP68  LA    R7,DEMDTAW          MERGE MACRO & MICRO                          
         USING MSTDTAD,R7                                                       
M        USING MSTDTAD,R5                                                       
         CPYA  AR5,AR6                                                          
         MVC   DEMODUB(1),M.MSTPREC                                             
         LAM   AR5,AR5,=F'0'                                                    
         NI    DEMODUB,MSTIPRC                                                  
         OC    MSTPREC,DEMODUB                                                  
         CPYA  AR5,AR6                                                          
         MVC   MSTELCD,M.MSTELCD                                                
         MVC   MSTFLDN,M.MSTFLDN                                                
         TM    M.MSTINDS,MSTTABEQ                                               
         LAM   AR5,AR5,=F'0'                                                    
         JZ    *+8                                                              
         OI    MSTINDS,MSTTABEQ                                                 
         CPYA  AR5,AR6                                                          
         CLC   MSTBKNO,M.MSTBKNO                                                
         LAM   AR5,AR5,=F'0'                                                    
         JL    DEMCP74                                                          
         CPYA  AR5,AR6                                                          
         TM    M.MSTINDS,MSTTABEQ                                               
         LAM   AR5,AR5,=F'0'                                                    
         JZ    DEMCP72                                                          
         CPYA  AR5,AR6                                                          
         TM    M.MSTINDS,MSTRECEQ                                               
         LAM   AR5,AR5,=F'0'                                                    
         JNZ   DEMCP72                                                          
         CPYA  AR5,AR6                                                          
         CLC   MSTBKNO,M.MSTBKNO                                                
         LAM   AR5,AR5,=F'0'                                                    
         JE    DEMCP74                                                          
         MVC   DEMODUB(1),MSTBKNO                                               
         CPYA  AR5,AR6                                                          
         MVC   MSTBKNO,M.MSTBKNO                                                
         LAM   AR5,AR5,=F'0'                                                    
         DROP  R7,M                                                             
*                                                                               
         USING MSTDTAD,R5          LOCATE FORMULA DEMO ENTRY                    
DEMCP70  AHI   R5,MSTDTALN                                                      
         CPYA  AR5,AR6                                                          
         CLC   DEMOEXP,MSTMODC     TEST MATCHING DEMO                           
         LAM   AR5,AR5,=F'0'                                                    
         JNE   DEMCP74                                                          
         CPYA  AR5,AR6                                                          
         TM    MSTINDS,MSTRECEQ    TEST FROM FORMULA RECORD                     
         LAM   AR5,AR5,=F'0'                                                    
         JZ    DEMCP70                                                          
         CPYA  AR5,AR6                                                          
         CLC   MSTBKNO,DEMODUB     =P.1                                         
         LAM   AR5,AR5,=F'0'                                                    
         JH    DEMCP74                                                          
*                                                                               
         LR    R7,R5                                                            
         LA    R5,DEMDTAW                                                       
M        USING MSTDTAD,R7                                                       
*                                                                               
         CPYA  AR7,AR6                                                          
         MVC   DEMODUB(1),M.MSTPREC                                             
         LAM   AR7,AR7,=F'0'                                                    
         NI    DEMODUB,MSTOPRC                                                  
         NI    MSTPREC,MSTIPRC                                                  
         OC    MSTPREC,DEMODUB                                                  
         NI    MSTINDS,MSTTABEQ+MSTRECEQ                                        
         CPYA  AR7,AR6                                                          
         OC    MSTINDS,M.MSTINDS                                                
         MVC   MSTDISP,M.MSTDISP                                                
         LAM   AR7,AR7,=F'0'                                                    
         J     DEMCP74                                                          
         DROP  M                                                                
*                                                                               
DEMCP72  CPYA  AR5,AR6                                                          
         MVC   DEMDTAW,0(R5)       USE MICRO                                    
         LAM   AR5,AR5,=F'0'                                                    
*                                                                               
         LA    R5,DEMDTAW                                                       
         TM    MSTPREC,MSTOPRC     SET PRECISION FROM MACRO IF NOT SET          
         JNZ   DEMCP74                                                          
         MVC   DEMODUB(1),MSTPREC-MSTDTAD(R7)                                   
         NI    DEMODUB,MSTOPRC                                                  
         OC    MSTPREC,DEMODUB                                                  
*                                                                               
DEMCP74  LA    R5,DEMDTAW          EXTRACT ENTRY INTO W/S                       
         TRAPIT                                                                 
         TM    MSTINDS,MSTDEMEQ    TEST FOR EQUATED FORMULA                     
         JZ    DEMCP76                                                          
         CLI   DEMMODQ,0           TEST FIRST EQUATED DEMO                      
         JNE   DEMCN               NO - ERROR                                   
         MVC   SAVMODC,MSTMODC     SAVE DEMO VALUES                             
         MVC   SAVBKNO,MSTBKNO                                                  
         MVC   SAVINDS,MSTINDS                                                  
         MVC   SAVPREC,MSTPREC                                                  
         MVC   SAVELCD,MSTELCD                                                  
         MVC   SAVFLDN,MSTFLDN                                                  
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,7,MSTDISP        RE=DISPLACEMENT TO TABLE ENTRY               
         AR    R6,RE                                                            
         MVC   MSTDTAD(MSTDTALN),0(R6)                                          
         L     R6,AMSTHDR          RESTORE A(TABLE HEADER)                      
*                                                                               
         MVC   DEMMODQ,MSTMODC     SET EQUATED DEMO FROM TABLE                  
         CLC   MSTDEMO,=XL2'00'    USING MACRO FORMULA?                         
         JNE   DEMCP76             NO - USE THIS ENTRY                          
         MVC   DEMMODC,DEMMODQ     SEARCH FOR SPECIFIC DEMO                     
         J     DEMCP46                                                          
*                                                                               
DEMCP76  MVC   MSTDEMO,DEMDEMO     SET DEMO NUMBER FOR MACROS                   
         TM    MSTINDS,MSTNOPEQ    TEST NO-OP DEMO                              
         JZ    *+10                                                             
         XR    R1,R1               YES - RETURN ZERO VALUE                      
         J     DEMCY                                                            
*                                                                               
         CLI   DEMMODQ,0           TEST EQUATED DEMO                            
         JE    DEMCP78                                                          
         MVC   MSTMODC,SAVMODC     YES - RESTORE FIELDS                         
         MVC   MSTBKNO,SAVBKNO                                                  
         TM    MSTINDS,MSTDIREQ    TEST IF A DIRECT DEMO                        
         JNZ   *+16                                                             
         TM    SAVINDS,MSTTABEQ    TEST DEMO ON RECORD                          
         JZ    *+8                                                              
         OI    MSTINDS,MSTTABEQ    YES - SET DEMO ON RECORD                     
         NI    SAVPREC,MSTIPRC                                                  
         NI    MSTPREC,MSTOPRC                                                  
         OC    MSTPREC,SAVPREC                                                  
         MVC   MSTELCD,SAVELCD                                                  
         MVC   MSTFLDN,SAVFLDN                                                  
*                                                                               
DEMCP78  TM    MSTPREC,MSTIPRC     SET INPUT PRECISION                          
         JZ    DEMCP80                                                          
         PACK  DEMIPRC,MSTPREC                                                  
         NI    DEMIPRC,X'07'                                                    
         TM    MSTPREC,X'80'                                                    
         JZ    *+12                                                             
         OI    DEMIPRC,X'80'                                                    
         J     *+8                                                              
         OI    DEMIPRC,X'40'                                                    
*                                                                               
DEMCP80  CLI   LISTTYPE,LISTORIG   TEST CALLER WANTS DEMO ORIGIN                
         JE    DEMCORG                                                          
*                                                                               
         TM    MSTPREC,MSTOPRC     SET OUTPUT PRECISION                         
         JZ    DEMCP82                                                          
         MVC   DEMOPRC,MSTPREC                                                  
         NI    DEMOPRC,X'07'                                                    
         TM    MSTPREC,X'08'                                                    
         JZ    *+12                                                             
         OI    DEMOPRC,X'80'                                                    
         J     *+8                                                              
         OI    DEMOPRC,X'40'                                                    
*                                                                               
DEMCP82  CLI   DEMPREC,0           TEST CALLED WITH PRECISION                   
         JNE   *+10                                                             
         MVC   DEMPREC,DEMOPRC     NO - SET OUTPUT PRECISION                    
         BRAS  RE,NADEST           ANY NAD ESTIMATES ON FILE                    
         JZ    DEMCP84              NO - RETURN A ZERO                          
         TM    DEMOFLAG,OVERPRES   TEST IF POTENTIAL OVERRIDE DEMO              
         JZ    DEMCP86                                                          
         BRAS  RE,FIXNADO                                                       
         GOTO1 =A(DEMGETO),RR=RELO YES - GET OVERRIDE VALUE                     
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         JZ    DEMCP86                                                          
*                                                                               
DEMCP84  MVC   ADJIPRC,DEMOPRC                                                  
         MVC   ADJOPRC,DEMPREC                                                  
         BRAS  RE,DEMADJP          ADJUST FIELD PRECISION                       
         J     DEMCY                                                            
*                                                                               
DEMCP86  TM    MSTINDS,MSTDIREQ    TEST IF A DIRECT TRANSFER FIELD              
         JO    DEMCP90             NO - USE FORMULA                             
         TM    MSTINDS,MSTTABEQ    TEST IF DEMO IS DEFINED                      
         JZ    DEMCP90             NO - USE FORMULA                             
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,1,MSTBKNO        POINT TO EFFECTIVE BOOK                      
         BCTR  R1,0                                                             
         MHI   R1,L'MSTBKNDX                                                    
         LA    R1,MSTBKNDX(R1)                                                  
         CPYA  AR1,AR6                                                          
         CLC   BKEF,2(R1)          TEST THIS IS EFFECTIVE BOOK                  
         LAM   AR1,AR1,=F'0'                                                    
         JNE   DEMCP90             NO - USE FORMULA                             
*                                                                               
         BRAS  RE,NADACT           CHECK FOR NAD/NTI VPH MERGE                  
         JNZ   *+12                NOT THERE - DON'T CALC                       
         BRAS  RE,DEMGET           TEST IF DEMO IS ON RECORD                    
         JNZ   DEMCP88             NO - USE FORMULA                             
*                                                                               
         TM    MSTINDS,MSTFPREQ    TEST FORMULA PRECISION ADJUSTMENT            
         JNZ   DEMCY               YES - EXIT                                   
         TM    MSTINDS,MSTFRMEQ    TEST FORMULA FOUND                           
         JNZ   DEMCP90             YES - USE FORMULA                            
         J     DEMCY               NO - EXIT                                    
*                                                                               
DEMCP88  MVC   ADJIPRC,DEMIPRC                                                  
         MVC   ADJOPRC,DEMPREC                                                  
         TM    MSTINDS,MSTFPREQ    TEST FORMULA PRECISION ADJUSTMENT            
*--->    JNZ   *+12                                                             
*--->    BRAS  RE,DEMADJP          NO - ADJUST FIELD PRECISION & EXIT           
         JNZ   *+8                                                              
         J     DEMCNDX                                                          
         ST    R1,DEMODUB          SAVE DEMO VALUE                              
*                                                                               
DEMCP90  DS    0H                                                               
         TRAPIT                                                                 
         TM    MSTINDS,MSTFRMEQ    TEST FORMULA FOUND                           
         JZ    DEMCN                                                            
         XR    R7,R7               PROCESS DEMO FORMULA                         
         ICM   R7,7,MSTDISP        R7=DISPLACEMENT TO FORMULA                   
         SR    R0,R0                                                            
         ICM   R0,7,MSTAFM                                                      
         AR    R7,R0                                                            
         LA    R4,DEMVALS          R4=A(DEMO VALUES/CALCULATION STACK)          
*                                                                               
* MODIFIERS CANNOT BE BELOW X'40' SO ASSUME WE HAVE                             
* ALL INPUT FIELDS AND START THE FORMULA PROCESS                                
* FIELDS X'30'-X'3F' ARE MULTIFIELD OPTIMISED FORMULAS                          
*                                                                               
DEMCP92  LAM   AR7,AR7,ALET                                                     
         SAC   512                                                              
         CLI   0(R7),X'30'         TEST IF QUICK OPERATION  1 2 + =             
         JE    QO30                                                             
         CLI   0(R7),X'31'         TEST IF QUICK OPERATION  1 2 - =             
         JE    QO31                                                             
         CLI   0(R7),LOWMODCD      TEST IF OPERATION                            
         JNL   DEMCP94                                                          
         AHI   R4,-8                                                            
         J     DEMCFRM                                                          
*                                                                               
DEMCP94  CLI   0(R7),X'FF'         NO - IS THIS A CONSTANT FIELD                
         JNE   DEMCP96                                                          
         XC    0(2,R4),0(R4)       YES - EXTRACT                                
         MVC   2(2,R4),2(R7)                                                    
*                                                                               
*                                                                               
         J     DEMCP100                                                         
*                                                                               
*  GATHER ALL OF THE INPUT DATA BEFORE PROCESSING FORMULA                       
*                                                                               
DEMCP96  MVC   DEMOEXP(3),0(R7)    CALL DEMCALC TO GET DEMO VALUE               
         CLC   DEMMODC,DEMMODQ     TEST IF AN EQUATED MODIFIER                  
         JNE   *+10                                                             
         MVC   DEMMODC,MSTMODC     YES - SET REAL MODIFIER                      
         OC    DEMDEMO,DEMDEMO     SET DEMO NUMBER IF A MACRO                   
         JNZ   *+10                                                             
         MVC   DEMDEMO,MSTDEMO                                                  
*                                                                               
         TM    MSTINDS,MSTFPREQ    TEST IF FORMULA PRECISION ADJUSTMENT         
         JZ    DEMCP98                                                          
         MVC   ADJIPRC,DEMIPRC                                                  
         CLI   ADJIPRC,0                                                        
         JNE   *+10                                                             
         MVC   ADJIPRC,DEMOPRC                                                  
         MVC   ADJOPRC,3(R7)                                                    
         L     R1,DEMODUB          GET SAVED VALUE                              
         BRAS  RE,DEMADJP          ADJUST FIELD PRECISION                       
         STCM  R1,15,0(R4)         SET VALUE IN STACK                           
         J     DEMCP100                                                         
*                                                                               
DEMCP98  LA    R1,DEMODUB          BUILD DEMCALC PARAM. LIST MANUALLY           
         LA    RE,DEMOEXP                                                       
         ST    RE,0(R1)                                                         
         MVC   0(1,R1),3(R7)                                                    
         ST    R4,4(R1)                                                         
         MVI   4(R1),3                                                          
*                                                                               
* (DEIS ASKS: SHOULD P3 BE SET HERE, PERHAPS TO 0? CAN THAT BE                  
*             SAFELY DONE BY CLEARING DEMODUB2?)                                
*                                                                               
         BAS   RE,DEMCALC          DEMCALC HAS STAR/REAR PAIR                   
*                                                                               
DEMCP100 AHI   R7,4                BUMP TO NEXT OPERAND/STACK ENTRY             
         AHI   R4,4                                                             
         J     DEMCP92                                                          
         EJECT                                                                  
*                                                                               
* THE GENERAL FORMAT OF SOME FORMULAS IS OPTIMIZED BELOW.                       
*                                                                               
QO30     LM    R0,R1,DEMVALS       1 2 + =                                      
         AR    R1,R0                                                            
         B     DEMCEND1                                                         
*                                                                               
QO31     LM    R0,R1,DEMVALS       1 2 - =                                      
         SR    R0,R1                                                            
         BP    *+6                                                              
         SR    R0,R0                                                            
         LR    R1,R0                                                            
         B     DEMCEND1                                                         
         EJECT                                                                  
***********************************************************************         
* EXECUTE POLISH FORMULA                                              *         
***********************************************************************         
         SPACE 1                                                                
DEMCFRM  XR    R2,R2                                                            
         LAY   RE,TRTTAB                                                        
         TRT   0(1,R7),0(RE)       <== NOTE AR7 MUST BE SET HERE                
         B     *+0(R2)                                                          
         DC    XL4'00'             +X'04' - INVALID OPERATION                   
         B     DEMCOPS             +X'08' - GET AN OPERAND                      
         B     DEMCADD             +X'0C' - ADD                                 
         B     DEMCSUB             +X'10' - SUBTRACT                            
         B     DEMCMUL             +X'14' - MULTIPLY                            
         B     DEMCDIV             +X'18' - DIVIDE                              
         B     DEMCPO1             +X'1C' - ! PSEUDO OPERATION                  
         B     DEMCPO2             +X'20' - ' PSEUDO OPERATION                  
         B     DEMCPO3             +X'24' - " PSEUDO OPERATION                  
         J     *+2                 +X'28' - SPARE                               
         J     *+2                 +X'2C' - SPARE                               
         J     *+2                 +X'30' - SPARE                               
         B     DEMCEND             +X'34' - END OF FORMULA                      
*                                                                               
DEMCOPS  AHI   R4,8                GET OPERAND                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,1,0(R7)                                                       
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         L     RF,DEMVALS(RF)                                                   
         B     DEMCNXT                                                          
*                                                                               
DEMCADD  AHI   R4,-8               ADD OPERAND2 TO OPERAND1                     
         LM    RE,R1,0(R4)                                                      
         AR    RF,R1                                                            
         XR    RE,RE                                                            
         J     DEMCNXT                                                          
*                                                                               
DEMCSUB  AHI   R4,-8               SUBTRACT OPERAND2 FROM OPERAND1              
         LM    RE,R1,0(R4)                                                      
         SR    RF,R1                                                            
         JP    *+6                                                              
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         J     DEMCNXT                                                          
*                                                                               
DEMCMUL  AHI   R4,-8               MULTIPLY OPERAND1 BY OPERAND2                
         LM    RE,R1,0(R4)                                                      
         MR    RE,R1                                                            
         J     DEMCNXT                                                          
DEMCDIV  AHI   R4,-8               DIVIDE OPERAND1 BY OPERAND2                  
*                                                                               
         LM    RE,R1,0(R4)                                                      
         TRAPIT                                                                 
         LTR   R1,R1                                                            
         JZ    DEMCN                                                            
         DR    RE,R1                                                            
         XR    RE,RE                                                            
         J     DEMCNXT                                                          
*                                                                               
DEMCPO1  L     RF,4(R4)            PSEUDO OPERATION (+5/10)                     
         XR    RE,RE                                                            
         AHI   RF,5                                                             
         D     RE,TEN                                                           
         XR    RE,RE                                                            
         J     DEMCNXT                                                          
*                                                                               
DEMCPO2  L     RF,4(R4)            PSEUDO OPERATION (+5/10*10)                  
         XR    RE,RE                                                            
         AHI   RF,5                                                             
         D     RE,TEN                                                           
         M     RE,TEN                                                           
         J     DEMCNXT                                                          
*                                                                               
DEMCPO3  L     RF,4(R4)            PSEUDO OPERATION (+500/1000)                 
         XR    RE,RE                                                            
         AHI   RF,500                                                           
         D     RE,THOUSAND                                                      
         XR    RE,RE                                                            
         J     DEMCNXT                                                          
*                                                                               
DEMCNXT  STM   RE,RF,0(R4)         SET OPERATION RESULT                         
         LA    R7,1(R7)            BUMP TO NEXT OPERATION                       
         J     DEMCFRM                                                          
*                                                                               
DEMCEND  L     R1,4(R4)            SET RESULT IN R1                             
*                                                                               
DEMCEND1 SAC   0                                                                
         LAM   AR7,AR7,=F'0'                                                    
         MVC   ADJIPRC,DEMOPRC                                                  
         MVC   ADJOPRC,DEMPREC                                                  
                                                                                
DEMCNDX  CLI   UNCOUNT,1           YES - TEST LEVEL ONE CALLER                  
****     JNE   DEMCNDX5                                                         
         JNE   DEMCNDX6                                                         
         TM    MSTINDS,MSTNDXEQ    TEST IF AN INDEXED DEMO                      
         JZ    DEMCNDX5                                                         
         TM    DEMOFLAG,INDXPRES   YES - TEST INDEX PRESENT                     
         JZ    DEMCNDX5                                                         
*                                                                               
         BRAS  RE,SETNDX           YES - APPLY INDEX TO VALUE                   
         J     DEMCNDX5            TEST FOR MIN VALUE                           
*                                                                               
****EMCNDX5 B     DEMCNDX6            DISABLE SYSCODE ADJUST                    
****        MVC   DBSELSYC,=H'1'      FORCE A VALID SYSCODE                     
DEMCNDX5 OC    DBSELSYC,DBSELSYC   IF A SYSCODE IS THERE                        
         JZ    DEMCNDX6                                                         
*                                                                               
         BRAS  RE,ADJSY            ADJUST TO SYSCODE AREA                       
*                                                                               
DEMCNDX6 BRAS  RE,DEMADJP          ADJUST FIELD PRECISION                       
         J     DEMCY                                                            
*                                                                               
DEMCORG  DS    0H                                                               
         TRAPIT                                                                 
         TM    MSTINDS,MSTTABEQ    RETURN DEMO ORIGIN & PRECISION               
         JZ    DEMCN                                                            
         TM    MSTINDS,MSTDIREQ+MSTFPREQ                                        
         JNZ   DEMCN                                                            
         L     RE,DEMAOUT                                                       
         MVC   0(2,RE),MSTELCD                                                  
         MVC   2(1,RE),DEMIPRC                                                  
         MVC   3(1,RE),MSTMODC     REFORMAT TO OLD                              
         MVC   4(1,RE),MSTDEMO+1   REFORMAT TO OLD                              
         J     DEMCN                                                            
*                                                                               
DEMCY    REAR  ARS=OFF                                                          
         L     RE,DEMAOUT                                                       
         STCM  R1,15,0(RE)         RETURN ADJUSTED FIELD                        
         XR    RE,RE                                                            
         IC    RE,UNCOUNT          DECREMENT UNKNOWN VALUES COUNT               
         BCTR  RE,0                                                             
         STC   RE,UNCOUNT                                                       
         J     DCALCX                                                           
*                                                                               
DEMCYE   REAR  ARS=OFF             STORE VALUE AND STOP RECURSION               
         L     RE,DEMAOUT                                                       
         STCM  R1,15,0(RE)         RETURN ADJUSTED FIELD                        
         XR    RE,RE                                                            
         MVI   UNCOUNT,0           RESET UNKNOWN FIELD COUNT                    
         J     DCALCX                                                           
*                                                                               
DEMCN    REAR  ARS=OFF                                                          
         MVI   UNCOUNT,0           RESET UNKNOWN FIELD COUNT                    
         L     RD,DEMOUTRD         EXIT BACK TO LEVEL ONE CALLER                
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
DCALCX   SAC   0                                                                
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DEAL WITH NTI REACH - IT IS NOT CURRENTLY A DEMO         *         
***********************************************************************         
         SPACE 1                                                                
NEREACH  L     R1,ARECORD                                                       
         XR    R0,R0                                                            
         USING PMKEY,R1                                                         
         CLI   PMCODE,PMCODEQU     'Q' PROGRAM DATA                             
         BNE   NEREACH4                                                         
         CLI   PMMEDIA,C'N'        NTI POCKETPIECE, ALL VIEWING TYPES           
         BE    *+12                                                             
         CLI   PMMEDIA,X'95'       AND COMMERCIAL AVERAGE                       
         BNE   NEREACH4                                                         
         DROP  R1                                                               
*                                                                               
         LA    R1,23(R1)           POINT TO FIRST ELEMENT                       
NEREACH2 CLI   0(R1),0                                                          
         JE    NEREACH4                                                         
         CLI   0(R1),X'10'         FIND THE REACH ELEMENT                       
         BE    *+16                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         XR    R0,R0                                                            
         B     NEREACH2                                                         
*                                                                               
         ICM   R0,3,7(R1)          GET VALUE IN 10,000                          
         MHI   R0,10               ADJUST TO THOUSANDS                          
NEREACH4 LR    R1,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SEE IF WE HAVE REQUIRED NAD VPH FIELDS FOR               *         
* NAD VPH/NTI HOMES MERGING FOR NETWORK POSTS                         *         
*   ON EXIT        ZERO CC = HAVE REQUIRED DATA                       *         
*              NON-ZERO CC = MISSING VPH DATA                         *         
***********************************************************************         
         SPACE 1                                                                
NADACT   XR    R1,R1               DEFAULT CC                                   
         CLC   DBFILE,=C'NTI'      ONLY NTI FILE                                
         JNE   NADACTXY                                                         
         CLI   LASTNAD,0           WITH NAD DEMO REQUEST                        
         JE    NADACTXY                                                         
         CLC   DBINTFIL(2),=AL2(MVGFILE) NTI+MNN = CABLE MOVIE GOER             
         JE    NADACTXY                                                         
         CLC   DBINTFIL(2),=AL2(IAGFILE) NTI+IAG = IAG FILE                     
         JE    NADACTXY                                                         
*                                                                               
         CLI   LASTNAD,TCAR181     BYPASS FOR TCAR RANGE 181-185                
         BL    *+12                                                             
         CLI   LASTNAD,TCAR189                                                  
         BNH   NADACTXY                                                         
*                                                                               
         CLI   OVREXP+1,15         HOMES ARE ALWAYS OK                          
         JL    NADACTXY                                                         
         ICM   R1,1,LASTNAD                                                     
         ICM   RF,15,AOVEREL       NO OVERRIDES IMPLY TP NAD'S                  
         JNZ   NADACT1                                                          
         CLC   DBINTFIL,=AL2(NHIFILE) DON'T CHANGE NTI LOGIC                    
         JE    NADACTXY              ALLOW NHI TP MARKET BREAKS                 
NADACT1  CLC   0(1,RF),OVERELEM                                                 
         JNE   NADACT2                                                          
         CLI   4(RF),DEMO_MODIFIER_V REQUIRE VPH                                
         JNE   NADACT2                                                          
         CLC   3(1,RF),LASTNAD                                                  
         JNE   NADACT2                                                          
         CLC   5(1,RF),OVREXP+1    FOR REQUESTED DEMO                           
         JE    NADACTXY                                                         
NADACT2  ZIC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),0                                                          
         JNE   NADACT1                                                          
         J     NADACTXN                                                         
*                                                                               
NADACTXY SR    R1,R1                                                            
NADACTXN LR    RF,R1                                                            
         SR    R1,R1                                                            
         LTR   RF,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SEE IF WE HAVE REQUIRED NAD ESTIMATED OVERRIDE           *         
*   ON EXIT    NON-ZERO CC = HAVE REQUIRED DATA                       *         
*                  ZERO CC = MISSING ALL OVERRIDE ELEMENTS            *         
***********************************************************************         
         SPACE 1                                                                
NADEST   LA    R1,1                DEFAULT CC                                   
         CLC   DBINTFIL(2),=AL3(EVNFILE) ONLY EVN FILE                          
         JNE   NADESTX                                                          
         CLI   LASTNAD,0           WITH NAD DEMO REQUEST                        
         JE    NADESTX                                                          
         TM    DEMOFLAG,OVERPRES   AND REQUESTED OVERRIDES                      
         JO    NADESTX             WILL BE OK                                   
         XR    R1,R1                                                            
*                                                                               
NADESTX  LTR   R1,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY INDEX TO DEMO VALUE                                *         
***********************************************************************         
         SPACE 1                                                                
SETNDX   L     RF,AINDXEL                                                       
         SR    R0,R0               R0=INDEX VALUE (2DP)                         
         ICM   R0,3,4(RF)                                                       
         CLI   1(RF),6             TEST FOR 6 BYTE ELEMENT                      
         BE    *+8                 YES-INDEX IS HALFWORD                        
         ICM   R0,7,4(RF)          NO-INDEX IS 3 BYTES                          
         MR    R0,R0               MULTIPLY BY DEMO VALUE                       
         CLI   3(RF),255           TEST REP ROUNDING                            
         BE    *+16                                                             
         AHI   R1,5000             SPOT INDEX ROUNDING                          
         D     R0,TENTHOU                                                       
         B     *+16                                                             
         A     R1,=F'50000'        REP INDEX ROUNDING                           
         D     R0,HUNDTHOU                                                      
         M     R0,TEN                                                           
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
DEMOX    B     *+10                                                             
         MVC   DBINTFIL(2),SVINTSM RESTORE ORIGINALS                            
         MVC   DBACTBK,SVACTBK                                                  
         MVC   DBACTSRC,SVACTSRC                                                
         SAC   0                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
*CALCULATION EXIT JUST EXITS TO INTERNAL ROUTINES                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
HIELCD   EQU   X'5F'               HIGHEST DEMO ELEMENT CODE                    
EOT      EQU   X'FF'                                                            
LOELQ    EQU   X'30'                                                            
HIELQ    EQU   X'5F'                                                            
BKELQ    EQU   X'5E'                                                            
OVELQ    EQU   X'DE'                                                            
UPGEL    EQU   X'05'               UPGRADE ELEMENT                              
UPGMIN   EQU   X'07'               FIELD IN UPG ELEM INDIC MIN VAL              
NDXDEMO  EQU   C'&&'                                                            
HINADLN  EQU   06                  6 BYTE NAD TABLE ENTRIES                     
*                                                                               
LOWMODCD EQU   X'40'               LOWEST POSSIBLE MODIFIER (VALUES             
*                                   BELOW THIS RESERVED FOR FORMULAS)           
*                                                                               
ICMMASK  DC    X'00',X'0103070F'   ARRAY INDEX IS ***ZERO-BASED***              
*                                                                               
NTIFILE  EQU   C'PN'                                                            
EINFILE  EQU   C'EIN'                                                           
NHIFILE  EQU   C'NH'                                                            
NADFILE  EQU   C'NN'                                                            
MVGFILE  EQU   C'MN'                                                            
EVNFILE  EQU   C'EVN'                                                           
CABFILE  EQU   C'CN'                                                            
CHNFILE  EQU   C'CH'               CABLE NHT FILE                               
IAGFILE  EQU   C'IA'               IAG FILE                                     
IUNFILE  EQU   C'IUN'                                                           
TTNFILE  EQU   C'TT'                                                            
CANFILE  EQU   C'TW'                                                            
*                                                                               
         DS    0H                                                               
CHARTAB  DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                    
         EJECT                                                                  
DEMTABCL CSECT                                                                  
*                                                                               
***********************************************************************         
* LIST OF DEMO SYSTEM TABLES REQUIRED BY MODULE                       *         
***********************************************************************         
*                                                                               
         DS    0D                                                               
         DC    CL8'TABLIST*'                                                    
TABLIST  DS    0X                                                               
TABMAST  DC    X'D3',3X'00'                                                     
TABFORM  DC    X'E2',3X'00'                                                     
TABNADU  DC    X'E4',3X'00'                                                     
TABFUSN  DC    X'E5',3X'00'                                                     
TABNFRM  DC    X'E7',3X'00'                                                     
         DC    AL1(EOT)                                                         
TABLISTL EQU   *-TABLIST                                                        
         EJECT                                                                  
DEMOUT   RSECT                                                                  
***********************************************************************         
* OTHER ASSORTED TABLES REQUIRED AT VARIOUS POINTS                    *         
***********************************************************************         
         SPACE 1                                                                
POWERTEN DS    0F                                                               
TEN      DC    F'10'                                                            
HUNDRED  DC    F'100'                                                           
THOUSAND DC    F'1000'                                                          
TENTHOU  DC    F'10000'                                                         
HUNDTHOU DC    F'100000'                                                        
MILLION  DC    F'1000000'                                                       
*                                                                               
         DROP  RB,RA,R8                                                         
         DROP  RC                                                               
*                                                                               
SPNETBL  DS    0CL7                 DBSELSRC/SPNETFLG/DEMDISP/DBINTFIL          
         DC    C'H',AL1(SPNHTQ),C'NHN',C'NH'                                    
         DC    C'K',AL1(SPNTIQ),C'PNN',C'PN'                                    
         DC    C'D',AL1(SPNADQ),C'NNN',C'NN'                                    
         DC    C'C',AL1(SPCBLQ),C'CNN',C'CN'                                    
         DC    X'FF'                                                            
                                                                                
*&&DO                                                                           
NHTBRKS  DS    0CL2         WHEN SPTNHT ACTIVE, ALLOW CHAR BKTYPES              
         DC    X'00',X'01'                                                      
         DC    C'1',X'01'          CONVERT CHAR TO NUMERIC                      
         DC    C'5',X'05'                                                       
         DC    C'6',X'06'                                                       
         DC    C'7',X'07'                                                       
         DC    C'H',X'05'          SET UP LETTER EQUATES                        
         DC    C'E',X'06'                                                       
         DC    C'B',X'07'                                                       
         DC    X'FFFFF'                                                         
*&&                                                                             
NTIADJ   DC    AL1(DEMO_MODIFIER_R),X'80',AL1(0)                                
         DC    AL1(DEMO_MODIFIER_P),X'80',AL1(1)                                
         DC    AL1(DEMO_MODIFIER_T),X'40',AL1(2)                                
         DC    AL1(DEMO_MODIFIER_C),X'80',AL1(0)                                
         DC    AL1(DEMO_MODIFIER_S),X'80',AL1(3)                                
         DC    X'FF'                                                            
*                                                                               
CABADJ   DC    AL1(DEMO_MODIFIER_R),X'80',AL1(0)                                
         DC    AL1(DEMO_MODIFIER_P),X'80',AL1(1)                                
         DC    AL1(DEMO_MODIFIER_T),X'40',AL1(2)                                
         DC    AL1(DEMO_MODIFIER_C),X'80',AL1(0)                                
         DC    AL1(DEMO_MODIFIER_S),X'80',AL1(3)                                
         DC    X'FF'                                                            
*                                                                               
SPOTADJ  DC    AL1(DEMO_MODIFIER_R),X'80',AL1(DBXTTRP-DBXTTID) RTG              
         DC    AL1(DEMO_MODIFIER_T),X'40',AL1(DBXTTIP-DBXTTID) IMP              
         DC    AL1(DEMO_MODIFIER_P),X'80',AL1(DBXTTRP-DBXTTID) PUT              
         DC    AL1(DEMO_MODIFIER_S),X'80',AL1(DBXTTSP-DBXTTID) SHR              
         DC    AL1(DEMO_MODIFIER_V),X'80',AL1(DBXTTSP-DBXTTID) SPT SHR          
         DC    AL1(DEMO_MODIFIER_X),X'80',AL1(DBXTTSP-DBXTTID) TSA SHR          
         DC    AL1(DEMO_MODIFIER_Q),X'40',AL1(DBXTTIP-DBXTTID) TOT              
         DC    AL1(DEMO_MODIFIER_A),X'40',AL1(DBXTTIP-DBXTTID) DMA IMP          
         DC    AL1(DEMO_MODIFIER_D),X'40',AL1(DBXTTIP-DBXTTID) DMA IMP          
         DC    X'FF'                                                            
*                                                                               
* NOTE - "R" RATINGS CALCULATED TO 0 DEC INTERNALLY                             
*        "D" DMA IMPS ALREADY IN UNITS                                          
SPOTADJ2 DC    AL1(DEMO_MODIFIER_I),X'80',AL1(DBXTTIP-DBXTTID) IMP              
         DC    AL1(DEMO_MODIFIER_T),X'40',AL1(DBXTTIP-DBXTTID) IMP              
         DC    AL1(DEMO_MODIFIER_P),X'80',AL1(DBXTTRP-DBXTTID) PUT              
         DC    AL1(DEMO_MODIFIER_S),X'80',AL1(DBXTTSP-DBXTTID) SHR              
         DC    AL1(DEMO_MODIFIER_V),X'80',AL1(DBXTTSP-DBXTTID) SPT SHR          
         DC    AL1(DEMO_MODIFIER_X),X'80',AL1(DBXTTSP-DBXTTID) TSA SHR          
         DC    AL1(DEMO_MODIFIER_Q),X'40',AL1(DBXTTIP-DBXTTID) TOT              
         DC    AL1(DEMO_MODIFIER_A),X'40',AL1(DBXTTIP-DBXTTID) DMA IMP          
         DC    AL1(DEMO_MODIFIER_D),X'40',AL1(DBXTTIP-DBXTTID) DMA IMP          
         DC    X'FF'                                                            
*                                                                               
ADJFTABC DC    C'OM',C'1',AL2(200),AL2(0)                                       
         DC    C'OM',C'2',AL2(140),AL2(0)                                       
         DC    C'OM',C'3',AL2(115),AL2(0)                                       
****     DC    C'SJ',C'1',AL2(200),AL2(0)                                       
****     DC    C'SJ',C'2',AL2(140),AL2(0)                                       
****     DC    C'SJ',C'3',AL2(115),AL2(0)                                       
         DC    C'YN',C'1',AL2(200),AL2(0)                                       
         DC    C'YN',C'2',AL2(140),AL2(0)                                       
         DC    C'YN',C'3',AL2(115),AL2(0)                                       
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* MULTIPLY DEMO ELEMENTS BY 10 TABLES                                 *         
***********************************************************************         
                                                                                
MULT10TB DS    0D                                                               
                                                                                
                                                                                
M10TIUN  DC    0X                                                               
         DC     C'IUN',AL1(M10TIUNX-M10TIUN)                                    
*                                                                               
         DC     X'33'                                                           
         DC     X'45'                                                           
****     DC     X'43'                                                           
         DC     X'49'                                                           
         DC     X'41'                                                           
*                                                                               
****     DC     X'47'                                                           
*                                                                               
         DC     AL1(EOT)                                                        
*&&DO                                                                           
M10IUN01 DS     0X                            RTG,JUN/91                        
         DC      AL1(M10IUN01X-M10IUN01)                                        
         DC       C'R',AL1(91,06)                                               
         DC        X'45'                                                        
         DC        AL1(EOT)                                                     
M10IUN01X EQU   *                                                               
*                                                                               
M10IUN02 DS     0X                            RTG,NOV/90                        
         DC      AL1(M10IUN02X-M10IUN02)                                        
         DC       C'R',AL1(90,11)                                               
         DC        X'41'                                                        
         DC        AL1(EOT)                                                     
M10IUN02X EQU   *                                                               
*                                                                               
M10IUN03 DS     0X                            SHR,JUN/91                        
         DC      AL1(M10IUN03X-M10IUN03)                                        
         DC       C'S',AL1(91,06)                                               
         DC        X'33'                                                        
         DC        X'49'                                                        
         DC        AL1(EOT)                                                     
M10IUN03X EQU   *                                                               
*                                                                               
M10IUN04 DS     0X                            SHR,DEC/90                        
         DC      AL1(M10IUN04X-M10IUN04)                                        
         DC       C'S',AL1(90,12)                                               
         DC        X'33'                                                        
         DC        X'49'                                                        
         DC        AL1(EOT)                                                     
M10IUN04X EQU   *                                                               
*                                                                               
M10IUN05 DS     0X                            SHR,NOV/90                        
         DC      AL1(M10IUN05X-M10IUN05)                                        
         DC       C'S',AL1(90,11)                                               
         DC        X'33'                                                        
         DC        X'49'                                                        
         DC        AL1(EOT)                                                     
M10IUN05X EQU   *                                                               
*                                                                               
M10IUN06 DS     0X                            PUT,NOV/90                        
         DC      AL1(M10IUN06X-M10IUN06)                                        
         DC       C'P',AL1(90,11)                                               
         DC        X'45'                                                        
         DC        AL1(EOT)                                                     
M10IUN06X EQU   *                                                               
*                                                                               
         DC     AL1(EOT)                      NO MORE FOR "IUN"                 
*&&                                                                             
M10TIUNX EQU   *                                                                
                                                                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TRANSLATION TABLE FOR FORMULA PROCESSOR                             *         
***********************************************************************         
         SPACE 1                                                                
TRTTAB   DS    0X                                                               
         DC    X'04080808080808080808080808080808' 00-0F                        
         DC    X'08080808080808080808080808080808' 10-1F                        
         DC    X'08080804040404040404040404040404' 20-2F                        
         DC    X'04040404040404040404040404040404' 30-3F                        
         DC    X'04040404040404040404040404040C04' 40-4F                        
         DC    X'040404040404040404041C0414040404' 50-5F                        
         DC    X'10180404040404040404040404040404' 60-6F                        
         DC    X'04040404040404040404040404203424' 70-7F                        
         DC    X'04040404040404040404040404040404' 80-8F                        
         DC    X'04040404040404040404040404040404' 90-9F                        
         DC    X'04040404040404040404040404040404' A0-AF                        
         DC    X'04040404040404040404040404040404' B0-BF                        
         DC    X'04040404040404040404040404040404' C0-CF                        
         DC    X'04040404040404040404040404040404' D0-DF                        
         DC    X'04040404040404040404040404040404' E0-EF                        
         DC    X'04040404040404040404040404040404' F0-FF                        
         EJECT                                                                  
***********************************************************************         
* OTHER EXTERNAL TABLES                                               *         
***********************************************************************         
         SPACE 1                                                                
* DEFSTDSP                                                                      
* DECSIUNIV                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEFSTDSPB                                                      
         EJECT                                                                  
       ++INCLUDE DECSIUNIV                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT A DEMO VALUE FROM A RECORD.                      *         
* IF DEMO ENGINE OPTION IS ON, WILL ATTEMPT TO COMPUTE THE DEMO       *         
*        USING THE NEW FORMULA TABLES.                                *         
*                                                                     *         
* EXIT - CC=EQ IF NO VALUE OR                                         *         
*        CC=NEQ WITH VALUE IN R1 & INPUT FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
DEMGET   NTR1  BASE=*,LABEL=*      SAVE RETURN ADDRESS                          
         USING DEMCALCD,RC         RC=A(DEMCALC LOCAL W/S)                      
*                                                                               
         XR    R1,R1               ANY ELEMENT CODE                             
         ICM   R1,1,MSTELCD                                                     
         BZ    DEMGETX                                                          
         CLC   =AL2(MVGFILE),DBINTFIL MOVIE GOER HAS FORMULAS                   
         BE    DEMGET1                                                          
         CLC   =AL2(NADFILE),DBINTFIL NAD N'S HAVE FORMULAS                     
         BE    DEMGET1                                                          
         CLI   GAASW,1                                                          
         BNE   DEMGET1                                                          
         CLI   MSTELCD,X'49'       DON'T ADJUST IF WE WANT UNIV ELEM            
         BE    *+8                                                              
         LA    R1,20(R1)           ADJUST GAA CODES                             
DEMGET1  STC   R1,EQUELCD          AND SAVE FOR LOOKUP                          
*                                                                               
         TM    DEMOFLAG,DEMOPRES   TEST ADDRESS LOOK-UP ACTIVE                  
         BZ    DEMGET2                                                          
         CLI   EQUELCD,HIELQ                                                    
         BH    DEMGET2             DISABLE FOR FSTDEM ELEMNTS > 5F              
         ZIC   R1,EQUELCD          YES - TEST ELEMENT ADDRESS SET               
         SHI   R1,LOELQ                                                         
         SRL   R1,1                                                             
         SLL   R1,2                                                             
         LA    R1,ADEMOEL(R1)                                                   
         ICM   R1,15,0(R1)                                                      
         BNZ   DEMGET8                                                          
*                                                                               
DEMGET2  L     R1,AQTHREL          SEARCH RECORD FOR ELEMENT                    
         USING DREELEM,R1          R1=A(QTR HOUR ELEMENT)                       
         SR    RF,RF                                                            
*                                                                               
         CLI   EQUELCD,X'67'       FAST DEM UNIVERSE LOOK UP?                   
         BE    DEMGET2A                                                         
         CLI   EQUELCD,X'4B'       BELOW CAN'T BE NHTI UNIV                     
         BL    DEMGET4                                                          
         CLI   EQUELCD,X'4C'       ABOVE CAN'T BE NHTI UNIV                     
         BH    DEMGET4                                                          
*                                                                               
DEMGET2A CLC   =AL2(NHIFILE),DBINTFIL ONLY FOR NHTI                             
         BNE   DEMGET4                                                          
         TM    SPNETFLG,SPNHTQ     SPOT-NHTI RECD HAVE UNIVS ON IT              
         BO    DEMGET4                                                          
         OC    DBSPANAD,DBSPANAD   AND WE HAVE READ A UNIVERSE                  
         BZ    DEMGET4                                                          
         ICM   RF,15,DBSPANAD                                                   
         CLC   0(4,RF),=C'NTHU'                                                 
         BNE   DEMGET3X                                                         
*                                                                               
****     TM    SPNETFLG,SPNHTQ     NHTI FILE BKTYPE EQUATES                     
****     BNO   DEMGET2F                                                         
****     MVC   DUB(1),DBBTYPE      DEFAULT IS TO INVALID                        
****     LA    RE,NHTBRKS          TRANSLATE BKTYPE TO INTERNAL CO              
*EMGET2D CLI   0(RE),X'FF'         EOT -> DUB GETS SET TO X'FF'                 
****     BE    *+10                                                             
****     CLC   DBBTYPE,0(RE)                                                    
****     BE    *+12                                                             
****     LA    RE,L'NHTBRKS(RE)                                                 
****     B     DEMGET2D                                                         
****     MVC   DUB(1),1(RE)                                                     
****     SR    RE,RE                                                            
*                                                                               
DEMGET2F LA    RF,4(RF)            FIND THE UNIVERSES                           
         USING PRKEY,RF                                                         
         LA    RF,PRFRSTEL                                                      
         SR    RE,RE                                                            
         DROP  RF                                                               
DEMGET3  CLI   0(RF),0             SEARCH FOR CURRENT SECTION                   
         BE    DEMGET3X                                                         
         CLI   0(RF),X'23'         LEAD ELEMENT                                 
         BE    DEMGET3C                                                         
DEMGET3A IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     DEMGET3                                                          
DEMGET3C CLI   LASTNAD,0           DEAL WITH DEFAULT                            
         BNE   DEMGET3E                                                         
         CLI   SPNETFLG,0                                                       
         BE    DEMGET3D                                                         
         CLC   2(1,RF),DUB         DBBTYPE=MKT BREAK                            
         BNE   DEMGET3A                                                         
         LR    R1,RF               SAVE AS DBAQUART                             
         B     DEMGET3X                                                         
*                                                                               
DEMGET3D CLI   2(RF),1               WHICH IS REALLY USA TOTAL                  
         BE    *+10                                                             
DEMGET3E CLC   2(1,RF),LASTNAD     CORRECT LEAD ELEMENT                         
         BNE   DEMGET3A                                                         
         LR    R1,RF               SET AS DBAQUART                              
DEMGET3X SR    RF,RF                                                            
*                                                                               
DEMGET4  CLC   DRECODE,QTHRELEM    TEST IF POINTING TO A DEMO ELEMENT           
         BH    DEMGET6             BYPASS FIRST QH ELEMENT                      
         CLI   DRECODE,0           END OF RECORD                                
         BE    DEMGETN                                                          
         SR    RF,RF                                                            
         IC    RF,DRELEN                                                        
         AR    R1,RF                                                            
         B     DEMGET4                                                          
*                                  FIND DEMO ELEMENT                            
DEMGET6  CLC   DRECODE,QTHRELEM    TEST IF END OF QTR HOUR CHUNK                
         BNH   DEMGETN                                                          
         MVC   DEMODUB(1),DRECODE  EXTRACT ELEMENT CODE                         
*                                                                               
         TM    DEMOFLAG,EVENPRES   FILE HAS EVEN DEMO ELEMENTS                  
         BO    *+8                  JUST ACCEPT THEM                            
         OI    DEMODUB,X'01'      ELSE - MAKE EVEN FOR DISP MATCH               
*                                                                               
DEMGET6A CLI   EQUELCD,X'61'                                                    
         BL    DEMGET7             CALC SOME DEMO GROUPS FOR NET FILES:         
         TM    DEMOFLAG,FASTPRES   CHECK FOR FASTDEM CALCS                      
         BO    FSTDEM                                                           
*                                                                               
DEMGET7  CLC   EQUELCD,DEMODUB     TEST ELEMENT CODE GR ARGUMENT                
         BL    DEMGET7A            YES - EXIT                                   
         BH    *+12                                                             
         BRAS  RE,SETADDR          SET ELEMENT ADDRESS IN LIST                  
         B     DEMGET8                                                          
         SR    RF,RF                                                            
         IC    RF,DRELEN           BUMP TO NEXT ELEMENT                         
         AR    R1,RF                                                            
         B     DEMGET6                                                          
*                                  DEMO ELEMENT FOUND                           
DEMGET7A B     DEMGET7B                                                         
*&&DO                                                                           
*EMGET7A DS    0C                                                               
         CLI   EQUELCD,X'5B'                                                    
         BNE   DEMGET7B            CALC SOME DEMO GROUPS                        
         CLC   DBINTFIL(2),=C'TC'      CSI FILE                                 
         BNE   DEMGET7B             NO - N/A                                    
         CLC   DBSELBK,=AL2(JAN_99)  YES - CHECK THE BOOK                       
         BL    DEMGET7B                                                         
         CLC   DBACTRMK,=H'440'                                                 
         BNE   *+14                                                             
         LAY   R1,CSIU99TO          POINT TO UNIVERSES                          
         B     DEMGET8                                                          
         CLC   DBACTRMK,=H'490'                                                 
         BNE   DEMGET7B                                                         
         LAY   R1,CSIU99VA          POINT TO UNIVERSES                          
         B     DEMGET8                                                          
*&&                                                                             
*                                                                               
DEMGET7B CLC   DBINTFIL(2),=AL2(CABFILE) CABLE FILE?                            
         BNE   DEMGET7C            NO, DEMO ELEM NOT FOUND                      
         CLI   EQUELCD,X'57'       CABLE UNIV?                                  
         BE    *+12                                                             
         CLI   EQUELCD,X'59'                                                    
         BNE   DEMGETN                                                          
         BAS   RE,CBLUNV           R1 WILL -> UNIV ELEM IN DBSPANAD             
         BZ    DEMGETN             IF NOT THERE, EXIT                           
         B     DEMGET8                                                          
*                                                                               
DEMGET7C CLC   DBINTFIL(2),=AL2(CHNFILE) CABLE NHT FILE?                        
         BNE   DEMGETN             NO, DEMO ELEM NOT FOUND                      
         CLI   EQUELCD,X'4B'       CABLE UNIV?                                  
         BE    *+8                                                              
         CLI   EQUELCD,X'4C'                                                    
         BE    *+8                                                              
         CLI   EQUELCD,X'57'                                                    
         BE    *+8                                                              
         CLI   EQUELCD,X'59'                                                    
         BNE   DEMGETN                                                          
         BAS   RE,CBLUNV           R1 WILL -> UNIV ELEM IN DBSPANAD             
         BZ    DEMGETN             IF NOT THERE, EXIT                           
*                                  DEMO ELEMENT FOUND                           
DEMGET8  TM    DREDUPA,X'80'       TEST IF DUPLICATE DATA ELEMENT               
         BZ    DEMGET10                                                         
         MVC   DEMODUB(2),DREDUPA  YES - GET ORIGINAL ELEMENT ADDRESS           
         NI    DEMODUB,255-X'80'                                                
         SR    R1,R1                                                            
         L     R1,ARECORD                                                       
         AH    R1,DEMODUB                                                       
*                                  EXTRACT CONTROL INFO FROM ELEMENT            
DEMGET10 MVI   DEMODUB,0           SET IMPLIED DECIMAL INDIC                    
         MVI   DEMODUB+1,1         SET FIELD LENGTH TO 1                        
         LA    RF,DRDATA                                                        
*                                   KEEP ELEM CODE AS IS, DON'T OR BIT          
         CLC   DBINTFIL(2),=AL2(CHNFILE)                                        
         BE    DEMGET11                                                         
*                                   NO ONE BYTE FORMAT FOR FILE                 
         CLC   DBINTFIL(2),=AL2(NHIFILE)                                        
         BE    DEMGET11                                                         
*                                   NO ONE BYTE FORMAT FOR FILE                 
         CLC   DBINTFIL(2),=AL2(MVGFILE)                                        
         BE    DEMGET11                                                         
*                                   NAD AFTER '9505' ALLOW EVEN ELEMNTS         
         CLC   DBINTFIL(2),=AL2(NADFILE)                                        
         BNE   *+14                                                             
         CLC   DBACTBK,=AL2(MAY_95)                                             
         BNL   DEMGET11            DON'T OR BIT AFTER 9505                      
*                                                                               
         TM    DRECODE,X'01'       TEST IF 1 BYTE DATA FORMAT                   
         BZ    DEMGET12            YES - EXTRACT DATA                           
*                                                                               
DEMGET11 MVC   DEMODUB+0(1),DREFCTRL                                            
         NI    DEMODUB+0,X'FF'-X'07'                                            
         MVC   DEMODUB+1(1),DREFCTRL                                            
         NI    DEMODUB+1,X'07'                                                  
         LA    RF,DRDATA1                                                       
*                                  EXTRACT DEMO VALUE FROM ELEMENT              
DEMGET12 ST    R1,DEMODUB+4        SAVE A(DEMO ELEMENT)                         
         LLC   R1,DEMODUB+1        R1=FIELD LENGTH                              
         LAY   RE,ICMMASK          RE=ICM MASK FOR EXECUTE                      
         LLC   RE,0(R1,RE)                                                      
*                                                                               
* DEIS NOV/2013: WE DISCOVERED THAT OCCASIONALLY (RARELY?) RE CONTAINS          
*   NULLS AT THIS POINT, WHICH MEANS THAT WE DIDN'T INDEX INTO ICMMASK.         
*   A NULL WAS ADDED TO THE FIRST BYTE OF ICMMASK AS A PROTECTIVE               
*   MEASURE, ALTHOUGH WE'RE NOT 100% CERTAIN THAT THIS IS CORRECT IN            
*   ALL CASES. IT'S BETTER TO POPULATE RE WITH NULLS THAN GARBAGE.              
*                                                                               
         ZIC   R0,MSTFLDN          R0=FIELD NUMBER                              
         MR    R0,R0               R1=FIELD LENGTH*FIELD NUMBER                 
         LA    RF,0(RF,R1)         RF=A(DEMO VALUE)                             
         L     R1,DEMODUB+4                                                     
*                                                                               
         ZIC   R0,1(R1)            R0 = ELEMENT LENGTH                          
         AR    R1,R0               R1=A(NEXT DEMO ELEMENT)                      
         CR    RF,R1               TEST IF FIELD IN ELEMENT                     
         BNL   DEMGETN             NO - EXIT WITH ZERO VALUE                    
         SR    R1,R1                                                            
         TM    DEMODUB,X'20'       TEST IF -VE FIELDS PRESENT                   
         JZ    DEMGET14                                                         
         TM    0(RF),X'80'         YES - TEST IF THIS FIELD -VE                 
         JZ    DEMGET14                                                         
         LCR   R1,R1               YES - SET R1 TO ALL EFFS                     
*                                                                               
DEMGET14 DS    0H                                                               
         EX    RE,*+8                                                           
         B     *+8                                                              
         ICM   R1,0,0(RF)          GET DEMO VALUE INTO R1                       
         BZ    DEMGETN                                                          
*****************************************************************               
         CHI   R1,1                TEMP FIX FOR BAD NTI                         
         BNE   NTI5BFX              RECORDS                                     
         CLI   MSTFLDN,X'1F'       FIELD IS FOR M45+                            
         BNE   NTI5BFX                                                          
         CLI   EQUELCD,X'5B'                                                    
         BNE   NTI5BFX                                                          
         SR    R1,R1                                                            
         B     DEMGETN                                                          
NTI5BFX  DS    0C                                                               
*********************************************************************           
*---->   B     PREADJX    <-----2DEC DISABLE FOR NOW                            
* TEMP FIX FOR WIRED SHARE-SUPRESS FROM RECORD.                                 
*  NSI TIME PERIOD START NOV/03                                                 
         CLI   DBBTYPE,BOOKTYPE_W3 BOOK TYPE W3                                 
         JE    *+8                                                              
         CLI   DBBTYPE,C'Z'        BOOK TYPE Z                                  
         JE    *+8                                                              
         CLI   DBBTYPE,C'4'        BOOK TYPE 4                                  
         JE    *+8                                                              
         CLI   DBBTYPE,C'W'        BOOK TYPE W                                  
         JNE   WIRSHRX                                                          
* CHECK FOR TT RECORD EXTRACTION                                                
         CLC   DBINTFIL(2),=AL2(TTNFILE)                                        
         BNE   WIRSHR4                                                          
         CLI   MSTFLDN,X'05'       FIELD IS FOR HOME SHARE                      
         JNE   WIRSHRX                                                          
         CLI   EQUELCD,X'31'       AND ELEMENT CODE IS CORRECT                  
         JE    WIRSHR6                                                          
         J     WIRSHRX                                                          
*                                                                               
* CHECK FOR IUN RECORD EXTRACTION                                               
WIRSHR4  CLI   MSTFLDN,X'00'       FIELD IS FOR HOME SHARE                      
         JNE   WIRSHRX                                                          
         CLI   EQUELCD,X'49'       AND ELEMENT CODE IS CORRECT                  
         JNE   WIRSHRX                                                          
WIRSHR6  SR    R1,R1                                                            
WIRSHRX  DS    0C                                                               
*                                                                               
         CLC   DBINTFIL(2),=AL2(CANFILE)  REP                                   
         JNE   PREADJ1                                                          
         CLI   BASEMOD,DEMO_MODIFIER_R ONLY IF RATINGS BEING PROCESSED          
         BE    *+12                FROM USER INPUT LIST                         
         CLI   BASEMOD,DEMO_MODIFIER_E ONLY IF RATINGS BEING PROCESSED          
         BNE   PREADJ1             FROM USER INPUT LIST                         
         CLI   RTGADJ,C'2'                                                      
         BNE   PREADJ1                                                          
         CLI   EQUELCD,X'33'       OLD RATINGS FOR UPGRADES                     
         BNE   PREADJX                                                          
         MHI   R1,10               MULTIPLY DMA IMP BY 10                       
*                                                                               
PREADJ1  CLC   DBINTFIL(2),=AL3(IUNFILE)  REP                                   
         JNE   PREADJX                                                          
* IF 2 DECIMAL IMPRESSIONS OPTION IS ON - TSA IMPRESSIONS ARE                   
* 10 TIMES GREATER.  WE DO NOT WANT TO IMPACT THE TSA SHARES                    
* X0=K0/J0 SO LETS MULTIPLE J0 BY 10                                            
         TM    DBBSTFLG,DBBST2DI                                                
         BZ    PREADJ1X                                                         
         CLI   BASEMOD,DEMO_MODIFIER_X  TSA SHARES REQUESTED                    
         BNE   PREADJ1X                                                         
         CLI   EQUELCD,X'39'       ADJUST STATION TOTALS                        
         BNE   PREADJ1X                                                         
         MHI   R1,10               MULTIPLE STATION TOTALS BY 10                
*                                                                               
*                                                                               
PREADJ1X DS    0H                                                               
         CLI   BASEMOD,DEMO_MODIFIER_R ONLY IF RATINGS BEING PROCESSED          
         BNE   PREADJX             FROM USER INPUT LIST                         
         CLI   RTGADJ,C'2'                                                      
         BNE   PREADJ2                                                          
         CLI   EQUELCD,X'49'       HOMES SHARES                                 
         BE    *+8                                                              
         CLI   EQUELCD,X'33'       OLD RATINGS FOR UPGRADES                     
         BE    *+8                                                              
         CLI   EQUELCD,X'41'       PRECISION ADJUST FOR RATING(2DEC)            
         BNE   PREADJX                                                          
         MHI   R1,10               MULTIPLY DMA IMP BY 10                       
*                                                                               
PREADJ2  CLI   RTGADJ,C'0'         PRECISION ADJUST FOR RATING(0DEC)            
         BNE   PREADJX                                                          
         CLI   EQUELCD,X'31'       MULTIPLY UNIVERSE BY 10                      
         BNE   PREADJX                                                          
         MHI   R1,10                                                            
*                                                                               
PREADJX  TM    DEMODUB,X'40'       TEST IF IMPLIED DECIMAL FIELD                
         BO    DEMGETX                                                          
         MHI   R1,10               YES - MULTIPLY FIELD BY 10                   
         B     DEMGETX                                                          
*                                                                               
DEMGETN  DS    0H                                                               
         CLC   =AL2(NHIFILE),DBINTFIL                                           
         BE    DEMGETNB            BYPASS NEW CODE THAT BRKS NHTI               
         CLC   BOOK,=X'A0FA'       TEST IF UNIV REQST AFTER MAY_95              
         BH    DEMGETNB            FOR NAD/NHTI/NTI-MKTBRK<>0                   
         CLC   =AL2(NTIFILE),DBINTFIL NETWORK REQUEST                           
         BNE   *+12                                                             
         CLI   LASTNAD,0           WITH CAT. IMPLIES NAD                        
         BNE   DEMGETNA              (FOR NEPPAK POSTING)                       
         CLC   =AL2(NHIFILE),DBINTFIL TEST NTI HISPANIC FILE LOOKUP             
         BE    *+10                                                             
         CLC   =AL2(MVGFILE),DBINTFIL TEST NETW MOVIE GOER FILE LOOKUP          
         BE    *+10                                                             
         CLC   =AL2(NADFILE),DBINTFIL TEST NAD FILE LOOK-UP                     
         BNE   DEMGETNB                                                         
*                                                                               
DEMGETNA CLI   DBSELMED,C'W'       WKLY UNIV NEEDED?                            
         BE    DEMGTNAW                                                         
*                                                                               
         CLI   DBSELSTA+4,C'C'     CABLE NAD HAS WEEKLY BOOK TOO                
         BNE   DEMGTNAM                                                         
         CLC   =AL2(NADFILE),DBINTFIL                                           
         BNE   DEMGTNAM                                                         
         CLI   DBBTYPE,C'N'                                                     
         BNE   DEMGTNAM                                                         
*                                                                               
DEMGTNAW BRAS  RE,WKTOMON          WK TO MON THEN LK UP NAD UNIV                
         B     *+8                                                              
DEMGTNAM BRAS  RE,NADUNV           LOOK UP POST 9505 NAD UNIVS                  
         LTR   R1,R1                                                            
         JNZ   DEMGETX                                                          
*                                                                               
DEMGETNB SR    R1,R1               NOT FOUND                                    
         CLI   UNCOUNT,2           ONLY FLAG IF ALREADY IN FORMULA              
         BL    *+8                                                              
         OI    ZEROVAL,1           INDICATE ZERO VALUE FOUND                    
         TM    MSTINDS,MSTFRMEQ    TEST IF FORMULA PRESENT                      
         BNZ   DEMGETX             YES - FORCE FORMULA LOOKUP                   
*                                                                               
         CLC   =AL2(NTIFILE),DBINTFIL NETWORK REQUEST                           
         BNE   *+12                                                             
         CLI   LASTNAD,0           WITH CAT. IMPLIES NAD                        
         BNE   DEMGETN1              (FOR NEPPAK POSTING)                       
         CLC   =AL2(NHIFILE),DBINTFIL TEST NTI HISPANIC FILE LOOKUP             
         BE    *+10                                                             
         CLC   =AL2(MVGFILE),DBINTFIL TEST NTI HISPANIC FILE LOOKUP             
         BE    *+10                                                             
         CLC   =AL2(NADFILE),DBINTFIL TEST NAD FILE LOOK-UP                     
         BNE   DEMGETX                                                          
DEMGETN1 DS    0H                                                               
         TM    SPNETFLG,SPNHTQ     BYPASS FOR SPT-NHTI                          
         BO    DEMGETX                                                          
*                                                                               
         CLI   DBSELMED,C'W'       WEEKLY NAD LK UP?                            
         BNE   DEMGETN5                                                         
         XR    R0,R0                                                            
         BRAS  RE,WKTOMON          CONVERT WK TO MONTH                          
         B     DEMGETX                                                          
*                                                                               
DEMGETN5 BRAS  RE,NADUNV                                                        
         LTR   R1,R1                                                            
         BNZ   DEMGETX             IF YEAR FOUND EXIT                           
         BRAS  RE,NADDEMO          ELSE TRY SEARCHING OLDER UNV TABLE           
*                                                                               
DEMGETX  LTR   R1,R1               SET CONDITION CODE                           
         B     DEMGETXX            RETURN TO CALLER                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* FSTDEM - FAST DEMO CALCULATIONS. USES DUMMY ELEMENT CODE > X'60'    *         
*          SPEED UP READS BY CALC ADDITION DEMO GROUPS                *         
***********************************************************************         
         SPACE 1                                                                
FSTDEM   MVI   DMCB,0                                                           
         XR    R0,R0                                                            
         LARL  RF,FSTTBL           MATCH ELEMENT CODE WITH TABLE                
         USING FSTTBLD,RF                                                       
*                                                                               
FSTD02   CLI   FSTFILE,X'FF'       END OF TABLE- ELEM NOT FOUND                 
         JE    DEMGET7                                                          
         CLC   DBINTFIL(2),FSTFILE   MATCH ON FILE                              
         JNE   *+14                                                             
         CLC   EQUELCD,FSTELEM                                                  
         JE    *+12                                                             
         LA    RF,L'FSTTBL(RF)        NEXT ENTRY                                
         J     FSTD02                                                           
*                                                                               
         ICM   R0,7,FSTADDR        ADDRESS OF TABLE                             
         DROP  RF                                                               
*                                                                               
         LARL  RF,FSTTOP           PT TO WHERE SPECIAL DISP TBLS BEGIN          
         AR    RF,R0               ADD ON DISP TO TBL WE WANT                   
         SR    R0,R0                                                            
*                                                                               
         USING FSTD,RF                                                          
FSTD04   CLI   FSTDSP,X'FF'        END OF DISPS FOR ELEMENT TABLE               
         JE    FSTDEMN             NOT FOUND                                    
         CLC   MSTFLDN,FSTDSP      COMPARE DSP TO TABLE                         
         JE    *+14                FOUND                                        
         IC    R0,FSTLEN           NEXT ENTRY IN TABLE                          
         AR    RF,R0                                                            
         J     FSTD04                                                           
*                                                                               
         MVC   DMCB(1),FSTFLGS     SAVE FLAGS FOR THIS FORMULA CATGRY           
         MVC   EQUELCD,FSTELQ      PICK OUT ELEM CODE FROM TABLE                
         XC    DUB,DUB                                                          
         XR    R0,R0                                                            
         IC    R0,FSTLEN                                                        
         AHI   R0,-4               CALC # OF LOOKUPS (SUB OVERHEAD)             
         STC   R0,DUB              DUB+0(1) = # FIELDS                          
         LA    RF,FSTDEMS-FSTD(RF)                                              
         DROP  RF                                                               
*                                                                               
         MVC   MSTFLDN,0(RF)       SET 1ST DISP AMT IN DEMO ELEMNT              
         STCM  RF,7,DUB+1          DUB+1(3) = A(1ST DISP FIELD)                 
*                                                                               
         USING DREELEM,R1                                                       
FSTD06   CLC   DRECODE,QTHRELEM                                                 
         JNH   FSTD10              NO DEMO ELEMS FOUND                          
         MVC   DEMODUB(1),DRECODE                                               
         CLC   DBINTFIL(2),=AL2(CHNFILE) ALLOW EVEN ELEMENTS                    
         JE    FSTD08              FOR NHTI AND                                 
         CLC   DBINTFIL(2),=C'NH'  ALLOW EVEN ELEMENTS                          
         JE    FSTD08              FOR NHTI AND                                 
         CLC   DBINTFIL(2),=C'NN'  NAD AFTER 9405                               
         JNE   *+14                                                             
         CLC   DBACTBK,=AL2(MAY_95)                                             
         JNL   FSTD08                                                           
         OI    DEMODUB,X'01'       ELSE DON'T ALLOW EVEN ELEMENTS               
*                                                                               
FSTD08   CLC   EQUELCD,DEMODUB                                                  
         JL    FSTD10              THIS DEMO ELEM - NOT FOUND                   
         JE    FSTD12                                                           
         XR    RE,RE                                                            
         ICM   RE,1,DRELEN                                                      
         AR    R1,RE               R1=PT TO ELEMS IN FILE                       
         J     FSTD06                                                           
*                                                                               
FSTD10   CLC   DBINTFIL(2),=AL2(CABFILE) CABLE FILE?                            
         JNE   FSTD11              NO, DEMO ELEM NOT FOUND                      
         CLI   EQUELCD,X'57'       CABLE UNIV?                                  
         JE    *+12                                                             
         CLI   EQUELCD,X'59'                                                    
         JNE   FSTDEMZ                                                          
         BAS   RE,CBLUNV           R1 WILL -> UNIV ELEM IN DBSPANAD             
         JZ    FSTDEMZ             IF NOT THERE, EXIT                           
         J     FSTD12                                                           
*                                                                               
FSTD11   CLC   DBINTFIL(2),=AL2(CHNFILE) CABLE NHT FILE?                        
         JNE   FSTDEMZ             NO, DEMO ELEM NOT FOUND                      
         CLI   EQUELCD,X'4B'       CABLE UNIV?                                  
         JE    FSTD11A                                                          
         CLI   EQUELCD,X'4C'                                                    
         JE    FSTD11A                                                          
         CLI   EQUELCD,X'57'       CABLE UNIV?                                  
         JE    FSTD11A                                                          
         CLI   EQUELCD,X'59'                                                    
         JNE   FSTDEMZ                                                          
*                                                                               
FSTD11A  BAS   RE,CBLUNV           R1 WILL -> UNIV ELEM IN DBSPANAD             
         JZ    FSTDEMZ             IF NOT THERE, EXIT                           
*                                                                               
FSTD12   BRAS  RE,SETADDR          SET ELEMENT ADDRESS IN LIST                  
         MVI   DEMODUB,0           SET IMPLIED DECIMAL INDIC                    
         MVI   DEMODUB+1,1         SET FIELD LENGTH TO 1                        
         LA    RF,DRDATA                                                        
         CLC   DBINTFIL(2),=AL2(CHNFILE) ALLOW EVEN BYTES FOR NTI-CABLE         
         JE    FSTD14                                                           
         CLC   DBINTFIL(2),=C'NH'  NO ONE BYTE FORMAT FOR FILE                  
         JE    FSTD14                                                           
         CLC   DBINTFIL(2),=C'NN'  NAD AFTER '9405' ALLOW EVEN ELEMNTS          
         JNE   *+14                                                             
         CLC   DBACTBK,=AL2(MAY_95)                                             
         JNL   FSTD14              DON'T OR BIT AFTER 9405                      
*                                                                               
         TM    DRECODE,X'01'       TEST IF 1 BYTE DATA FORMAT                   
         JZ    FSTD16              YES - EXTRACT DATA                           
*                                                                               
FSTD14   MVC   DEMODUB+0(1),DREFCTRL                                            
         NI    DEMODUB+0,X'FF'-X'07'                                            
         MVC   DEMODUB+1(1),DREFCTRL                                            
         NI    DEMODUB+1,X'07'                                                  
         LA    RF,DRDATA1                                                       
*                                  EXTRACT DEMO VALUE FROM ELEMENT              
FSTD16   ST    R1,DEMODUB+4        SAVE A(DEMO ELEMENT)                         
         LLC   R1,DEMODUB+1        R1=FIELD LENGTH                              
         LAY   RE,ICMMASK          RE=ICM MASK FOR EXECUTE                      
         LLC   RE,0(R1,RE)                                                      
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,MSTFLDN        R0=FIELD NUMBER                              
         MR    R0,R0               R1=FIELD LENGTH*FIELD NUMBER                 
         AR    RF,R1               RF=A(DEMO VALUE)                             
         L     R1,DEMODUB+4                                                     
         XR    R0,R0                                                            
         ICM   R0,1,1(R1)          R0 = ELEMENT LENGTH                          
         AR    R1,R0               R1=A(NEXT DEMO ELEMENT)                      
         CR    RF,R1               TEST IF FIELD IN ELEMENT                     
         JNL   FSTD20              NO - EXIT WITH ZERO VALUE                    
*                                                                               
         SR    R1,R1                                                            
         TM    DEMODUB,X'20'       TEST IF -VE FIELDS PRESENT                   
         JZ    FSTD17                                                           
         TM    0(RF),X'80'         YES - TEST IF THIS FIELD -VE                 
         JZ    FSTD17                                                           
         LCR   R1,R1               YES - SET R1 TO ALL EFFS                     
*                                                                               
* DEIS NOV/2013: IF RE CONTAINS NULLS AT THIS POINT (WE'RE NOT SURE IF          
*   THAT COULD EVER REALLY HAPPEN), THEN THIS ICM WON'T DO ANYTHING.            
*                                                                               
FSTD17   DS    0H                                                               
         EX    RE,*+8                                                           
         J     *+8                                                              
         ICM   R1,0,0(RF)          GET DEMO VALUE INTO R1                       
         JZ    FSTD20                                                           
*                                                                               
FSTD18   TM    DEMODUB,X'40'       FOUND TEST IF IMPLIED DECIMAL FIELD          
         JO    *+8                 DON'T MULTIPLY                               
         MHI   R1,10               YES - MULTIPLY FIELD BY 10                   
         ICM   R0,15,DUB+4         ADD TO TOTAL                                 
         AR    R1,R0                                                            
         STCM  R1,15,DUB+4                                                      
*                                                                               
FSTD20   XR    R0,R0                                                            
         IC    R0,DUB              REDUCE # DEMOS TO MERGE                      
         BCTR  R0,0                                                             
         STC   R0,DUB                                                           
         XR    R1,R1                                                            
         ICM   R1,7,DUB+1          ADDR OF NEXT DISP                            
         LA    R1,1(R1)            BUMP PTR                                     
         STCM  R1,7,DUB+1                                                       
         MVC   MSTFLDN,0(R1)                                                    
*                                                                               
         ICM   R1,15,DEMODUB+4     RESTORE ADDRESS OF START OF ELEM             
         CLI   DUB,0                                                            
         JNE   FSTD06              LOOK FOR NEXT DEMO VALUE IN ELEMENT          
         ICM   R1,15,DUB+4         LOAD TOTAL DEMO VALUE                        
         LA    RF,1                SET CONDITION CODE TO NON-ZERO               
         J     FSTDEMX             NON ZERO = DEMOS FOUND                       
*                                                                               
FSTDEMN  XR    R1,R1               NOT FOUND - USE FORMULA IF AVAILBLE          
         XR    RF,RF               CC =  ZERO                                   
         J     FSTDEMX                                                          
*                                                                               
FSTDEMZ  XR    R1,R1               DEMO VALUE = ZERO BUT CATEGORY FND           
         XR    RF,RF                                                            
         TM    DMCB,X'01'          IS DEFAULT TO FORMULA FLAG ON?               
         JO    *+8                 YES - RF=ZERO, DEFAULT TO FORMULA            
         LA    RF,1                CC = NON ZERO BYPASS FORMULA                 
*                                                                               
FSTDEMX  LTR   RF,RF                                                            
*                                                                               
DEMGETXX DS    0H                                                               
         XIT1  REGS=(R1)                                                        
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*CBLUNV  - SEARCH FOR CABLE UNIVS IN DBSPANAD                                   
*          RTN CC=ZERO IF NOT FOUND                                             
***********************************************************************         
CBLUNV   DS    0H                                                               
         ICM   RF,15,DBSPANAD                                                   
         BZ    CBLUNVNX                                                         
         SR    R0,R0                                                            
         CLI   EQUELCD,X'57'       REGULAR CABLE UNIVS?                         
         BE    *+12                                                             
         CLI   EQUELCD,X'59'                                                    
         BNE   *+12                                                             
         LA    RF,CBLUDSP(RF)      DISP TO CBL UNIVS                            
         B     CBLUNV5                                                          
*                                                                               
         CLC   DBINTFIL(2),=AL2(CHNFILE) USA LEVEL HISP UNIVS?                  
         BNE   CBLUNV5                                                          
         AHI   RF,CHNUDSP          DISP TO CABLE HISP UNIVS                     
         CLC   0(3,RF),=C'HCAB'                                                 
         BNE   CBLUNVNX                                                         
         LA    RF,4(RF)            BUMP PAST LABEL TO DEMOS                     
*                                                                               
CBLUNV5  CLI   0(RF),0             END OF LIST?                                 
         BE    CBLUNVNX                                                         
         CLC   EQUELCD,0(RF)                                                    
         BE    *+14                                                             
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     CBLUNV5                                                          
         LR    R1,RF               ELEM FOUND SET R1 -> UNIV ELEM               
         MVC   DEMODUB(1),0(R1)    SAVE ELEMENT CODE                            
         B     CBLUNVX                                                          
*                                                                               
CBLUNVNX SR    RF,RF                                                            
*                                                                               
CBLUNVX  LTR   RF,RF                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RC                                                               
         DROP  RB                                                               
***********************************************************************         
* ROUTINE TO SET ADDRESS OF ELEMENT IN ADEMOEL                        *         
*                                                                     *         
* NTRY - R1=A(DEMO ELEMENT)                                           *         
***********************************************************************         
*                                                                               
SETADDR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   0(R1),LOELQ                                                      
         BL    SETADDRX                                                         
         CLI   0(R1),HIELQ                                                      
         BH    SETADDRX                                                         
         CLC   DBINTFIL(2),=C'NH'  DISABLE SAVING ADDR FOR NHTI                 
         BE    SETADDRX                                                         
         CLC   DBINTFIL(2),=C'PN'                                               
         BNE   SETADDR1                                                         
         CLI   DBBTYPE,C'V'         BYPASS FOR TCR?                             
         BE    SETADDRX                                                         
         CLI   DBBTYPE,C'W'        TCAR LK UP (EXL VCR)                         
         BE    SETADDRX                                                         
*                                                                               
SETADDR1 DS    0H                                                               
         LR    RF,R1               RF=A(ELEMENT)                                
         TM    2(R1),X'80'         TEST COMPRESSED DEMO ELEMENT                 
         BZ    SETADDR2                                                         
         SR    RF,RF                                                            
         ICM   RF,3,2(R1)          RF=DISP. TO UNCOMPRESSED ELEMENT             
         SLL   RF,17                                                            
         SRL   RF,17                                                            
         A     RF,ARECORD                                                       
SETADDR2 ZIC   RE,0(R1)            RE=ELEMENT CODE (COMPRESSED ELEMENT)         
         SHI   RE,LOELQ                                                         
         SRL   RE,1                                                             
         SLL   RE,2                                                             
         ST    RF,ADEMOEL(RE)      SET ELEMENT ADDRESS IN LIST                  
         OI    DEMOFLAG,DEMOPRES                                                
*                                                                               
SETADDRX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DEAL WITH NAD DEMOS NOT FOUND ON DEMO RECORD             *         
* (SPECIFICALLY THE UNIVERSES WHICH ARE IN A TABLE)                   *         
* R1 IS RETURNED                                                      *         
***********************************************************************         
*                                                                               
NADDEMO  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    BOOK,=X'FFFF'       CHANGE BOOK TO BINARY                        
*                                                                               
         LLC   RF,MSTMODC          ENCODED MODIFIER                             
         A     RF,APLMODTB         A(DECODED MODIFIER)                          
         CLI   0(RF),DEMO_MODIFIER_U ONLY UNIVERSES ARE IN THE TABLE            
         BNE   NADDEMOX                                                         
*                                                                               
         LARL  RF,NADUTAB          POINT TO UNIVERSES                           
NADDEMO2 CLI   0(RF),X'FF'         END OF TABLE                                 
         BE    NADDEMOX                                                         
         CLC   LASTNAD,0(RF)       DO I WANT THIS CATEGORY                      
         BE    NADDEMO4                                                         
NADDEMO3 ZIC   R0,1(RF)            NO GET THE NEXT ENTRY                        
         AR    RF,R0                                                            
         B     NADDEMO2                                                         
*                                                                               
NADDEMO4 CLC   BOOK,2(RF)          IS IT THE RIGHT BOOK                         
         BL    NADDEMO3            NO - TRY THE NEXT                            
         ZIC   R1,MSTFLDN          GET THE FIELD NUMBER                         
         AR    R1,R1               ADJUST FOR FIELD SIZE                        
         LH    R1,4(RF,R1)         GET THE VALUE                                
*                                                                               
NADDEMOX XC    BOOK,=X'FFFF'       RESTORE THE INVERTED BOOK                    
*                                                                               
         XIT1  REGS=(R1)           RETURN TO CALLER                             
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* TEST IF DEMO NEEDS TO BE CALCULATED TO 2 DECIMAL PLACES AND TRUNCATE*         
* AT ENTRY:                                                           *         
*   R2-->CURRENT DEMO IN LIST                                         *         
***********************************************************************         
*                                                                               
TESTC2T  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    CALC2TRU,X'FF'-C2TYES                                            
*                                                                               
         LLC   RF,1(R2)            ENCODED MODIFIER                             
         A     RF,APLMODTB         A(DECODED MODIFIER)                          
*                                                                               
****     CLI   0(RF),DEMO_MODIFIER_Q                                            
****     BE    *+8                                                              
         CLI   0(RF),DEMO_MODIFIER_C                                            
         BE    *+12                                                             
         LHI   R1,C2TRTG                                                        
         CLI   0(RF),DEMO_MODIFIER_A                                            
         BE    *+12                                                             
         LHI   R1,C2TRTG                                                        
         CLI   0(RF),DEMO_MODIFIER_D                                            
         BE    *+12                                                             
         LHI   R1,C2TRTG                                                        
****     LHI   R1,C2TRTG                                                        
         CLI   0(RF),DEMO_MODIFIER_R                                            
         BE    *+12                                                             
         LHI   R1,C2TSHR                                                        
         CLI   0(RF),DEMO_MODIFIER_S                                            
         BE    *+12                                                             
         LHI   R1,C2TPUT                                                        
         CLI   0(RF),DEMO_MODIFIER_P                                            
         BNE   TESTC2TX                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    CALC2TRU,0                                                       
         BZ    TESTC2TX                                                         
*                                                                               
TESTC2T5 OI    CALC2TRU,C2TYES                                                  
*                                                                               
TESTC2TX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* I HAVE NO IDEA WHAT THIS IS FOR - PLEASE ADD A COMMENT              *         
***********************************************************************         
*                                                                               
SPCLARB  NTR1  BASE=*,LABEL=*                                                   
         XR    R7,R7               CLEAR FOR EXTENSION SAVE                     
         ICM   RE,15,DBEXTEND                                                   
         JZ    SPCLR06                                                          
         USING DBEXTRAD,RE                                                      
*                                                                               
SPCLR02  CLC   DBRID,=C'RADI'      CHECK FOR RADIO EXTENSIONS                   
         JE    SPCLR04                                                          
         ICM   RE,15,4(RE)         TEST FOR DBLOCK EXTENSIONS                   
         JNZ   SPCLR02             TRY NEXT ONE                                 
         J     SPCLR06                                                          
*                                                                               
SPCLR04  LR    R7,RE               SAVE EXTENSION ADDRESS                       
         CLI   DBRCOPT,C'Y'        WILL APPLIC HANDLE                           
         JNE   SPCLR06                                                          
         CLI   DBRAMTYP,C'S'       NORMAL MARKET                                
         JE    SPCLARBX            YES - I'M DONE                               
         CLI   DBRADEMT,C'R'       ALREADY RESTRICTED                           
         JE    SPCLARBX                  I'M DONE                               
*                                                                               
****PCLR06  CLI   DBSELSRC,C'N'       SWITCH TO BIRCH LISTS                     
****        JE    SPCLBIR1                                                      
*                                                                               
SPCLR06  CLI   DBSELSRC,C'A'       EXIT IF NOT ARB                              
         JNE   SPCLARBX                                                         
         CLI   DBRAMTYP,0          ALREADY HAVE MTYPE ?                         
         JNE   SPCLR12             YES - CHECK THE DEMOS                        
         DROP  RE                                                               
*                                                                               
         L     RE,ARECORD                                                       
         LA    RE,23(RE)           POINT TO FIRST ELEMENT                       
         USING MARELEM,RE                                                       
         CLI   MARCODE,MARCODEQ                                                 
         JE    *+6                                                              
         DC    H'0'                NOT A MARKET ELEMENT ?!?                     
         LARL  RF,ARBRMK2                                                       
*                                                                               
X        USING DBRID,R7                                                         
         MVC   X.DBRAMTYP,MARTYPE  SET MKT TYPE FROM RECORD                     
         CLI   X.DBRAMTYP,C'C'     CONDENSED?                                   
         JE    SPCLR10                                                          
         CLI   X.DBRAMTYP,C'S'     STANDARD?                                    
         JE    SPCLARBX                                                         
*                                                                               
         LARL  RF,ARBRMK2                                                       
* DEIS IN OCT/13: THIS IS OBVIOUSLY A BUG. SPRING/91 IS MAY_91, WHICH           
*   IS X'5B05', NOT X'9105'. RF WILL NEVER BE POINTED TO ARBRMK3.               
*   A DELIBERATE ABEND IS BEING ADDED HERE TO FIND OUT IF WE EVER               
*   ACTUALLY REACH THIS CODE. IF WE GET A HIT HERE, WE CAN TAKE OUT             
*   THE DC H'0', BUT WE MUST INVESTIGATE THIS. IF WE NEVER REACH THIS           
*   CODE, THEN WE WON'T WORRY ABOUT IT!                                         
         DC    H'0'                *** SEE COMMENT ABOVE ***                    
         CLC   DBSELBK,=X'9105'    NEW LIST FOR SPRING/91                       
         JL    *+10                                                             
         LARL  RF,ARBRMK3                                                       
*                                                                               
         LA    RF,5(RF)            POSITION TO MARKET LIST                      
         LTR   R7,R7                                                            
         JZ    SPCLR08                                                          
         MVI   X.DBRAMTYP,C'S'     PRESET TO STANDARD                           
*                                                                               
SPCLR08  CLC   0(2,RF),=H'0'       END OF LIST                                  
         JE    SPCLARBX            MARKET IS NOT SPECIAL                        
         CLC   0(2,RF),MARNO                                                    
         JH    SPCLARBX            MARKET IS HIGH SO GET OUT                    
         JE    SPCLR10             MARKET IS SPECIAL                            
         LA    RF,2(RF)            ??? SO GET THE NEXT ONE                      
         J     SPCLR08                                                          
         DROP  RE                                                               
*                                                                               
*              SELBK MUST BE USED HERE SINCE ACTBK CONTROLS                     
*              FORMULAS AND MAY BE INACCURATE                                   
*                                                                               
SPCLR10  LTR   R7,R7               ANY EXTENSION                                
         JZ    SPCLR14                                                          
         MVI   X.DBRAMTYP,C'C'     SET TO CONDENSED                             
*                                                                               
SPCLR12  CLI   X.DBRAMTYP,C'S'     STANDARD JUST EXIT                           
         JE    SPCLARBX                                                         
*                                                                               
SPCLR14  LARL  RE,ACRDEMS2                                                      
         CLC   DBSELBK,=AL2(MAY_88)  NEW LIST FOR SPRING/88                     
         JL    *+10                                                             
         LARL  RE,ACRDEMS3                                                      
*                                                                               
         L     R2,0(R1)                                                         
SPCLR16  CLI   0(RE),0             INVALID DEMO                                 
         JE    SPCLR18                                                          
         CLC   1(1,R2),0(RE)                                                    
         JE    SPCLARBX            VALID DEMO SO EXIT                           
         LA    RE,1(RE)            ??? SO GET THE NEXT ONE                      
         J     SPCLR16                                                          
*                                                                               
SPCLR18  LTR   R7,R7               ANY EXTENSION                                
         JZ    SPCLR20                                                          
         OI    X.DBRADEMT,C'R'     CONDENSED DEMO                               
         CLI   X.DBRCOPT,C'Y'      ONLY ALLOW IF REPORT CAN HANDLE IT           
         JE    SPCLARBX                                                         
         DROP  X                                                                
*                                                                               
SPCLR20  L     RE,4(R1)            INVALID DEMO                                 
         XC    0(4,RE),0(RE)       SO KILL THE VALUE                            
*                                                                               
SPCLARBX XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISATION FOR DEMOUT                                           *         
***********************************************************************         
*                                                                               
         USING DBLOCKD,R3          SET & EXTRACT DBLOCK VALUES                  
INIT     NTR1  BASE=*,LABEL=*                                                   
         XC    DUB,DUB             SPECIAL CALL FOR SYSFACS                     
         MVC   DUB(4),=XL4'FEFFFFFF'                                            
         ICM   RF,15,DBCOMFCS                                                   
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LTR   RF,RF                                                            
         BNZ   INIT01                                                           
         ICM   RF,15,DBCOMFCS                                                   
         ICM   RF,15,CMASTC-COMFACSD(RF)                                        
         ICM   RF,15,MCSSB-MASTD(RF)       RF=ASSB                              
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
         B     INIT01C                                                          
*                                                                               
INIT01   GOTO1 (RF),DUB            CALL SWITCH                                  
         L     RF,0(R1)            RF=V(SYSFACS)                                
         L     RF,VSSB-SYSFACD(RF) RF=V(SSB)                                    
         ST    RF,ASSB             SAVE A(SSB)                                  
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
*                                                                               
INIT01C  L     R4,=A(TABLIST)      R4=A(LIST OF TABLES IN PROGRAM)              
         A     R4,RELO                                                          
T        USING TABLIST,R4                                                       
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,7,T.TABMAST+1                                                 
         JNZ   INIT04                                                           
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
INIT02   XC    T.TABMAST(4),T.TABMAST  SO I CAN REBUILD IF NEEDED               
         XC    T.TABFORM(4),T.TABFORM                                           
         XC    T.TABNADU(4),T.TABNADU                                           
         XC    T.TABFUSN(4),T.TABFUSN                                           
         XC    T.TABNFRM(4),T.TABNFRM                                           
         MVI   T.TABMAST,X'D3'     BUILD T00AD3,T00AD4                          
         MVI   T.TABFORM,X'E2'     ADDRESS OF T00AE2 (IN DEMADDR)               
         MVI   T.TABNADU,X'E4'     BUILD NAD UNIV TABLE                         
         MVI   T.TABFUSN,X'E5'     BUILD FUSION SYSCODE ADJUST TABLE            
         MVI   T.TABNFRM,X'E7'     BUILD NEW FORMULA TABLE                      
         MVC   TABLISTW,T.TABLIST                                               
*                                                                               
         L     RF,DBCOMFCS                                                      
         LR    R0,RF                                                            
         L     RF,CDEMADDR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'FF',TABLISTW),(R0)                                  
         MVC   T.TABLIST(TABLISTL),TABLISTW                                     
*                                                                               
         ICM   R2,15,T.TABMAST                                                  
INIT04   LAM   AR2,AR2,ALET        PROTECT AGAINST ZAPS AND LOADS               
         SAC   512                                                              
         SHI   R2,16                                                            
         CLC   =C'DMASTER',0(R2)   SEE IF TABLE ZAPPED                          
         SAC   0                                                                
         LAM   AR2,AR2,=F'0'                                                    
         JNE   INIT02              YES - REBUILD IT                             
         DROP  T                                                                
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
         MVI   DBERROR,0                                                        
         MVI   DBERRMOD,EDEMOUT                                                 
         MVC   ARECORD,DBAREC                                                   
         MVI   ARECORD,0                                                        
         MVC   AQTHREL,DBAQUART                                                 
         MVI   AQTHREL,0                                                        
         MVC   SVINTSM,DBINTFIL    SAVE ORIGINAL                                
         MVC   SVACTBK,DBACTBK                                                  
         MVC   SVACTSRC,DBACTSRC                                                
*                                                                               
         LA    RF,DBEXTEND-4                                                    
         MVI   DENGINE,DENGOFFQ    DEFAULT IS DEMO ENGINE OFF                   
INIT10   ICM   RF,15,4(RF)                                                      
         BZ    INIT15                                                           
         CLC   0(4,RF),=C'DENG'                                                 
         BNE   INIT10                                                           
         USING DBXDEND,RF                                                       
         CLI   DBXDENG,DBXDEONQ                                                 
         BNE   *+8                                                              
         MVI   DENGINE,DENGONQ     DEMO ENGINE OPTION IS ON                     
         DROP  RF                                                               
*                                                                               
INIT15   DS    0H                                                               
         L     R4,DBCOMFCS         A(COMFACS)                                   
         USING COMFACSD,R4                                                      
         CLC   CDEMOCON,=F'0'      IS A(DEMOCON) SET IN COMFACS?                
         BNE   INIT20              YES                                          
*                                                                               
         GOTO1 CCALLOV,DMCB,0,X'D9000AE0'  NO: GET A(DEMOCON)                   
         CLI   DMCB+4,X'FF'        WAS A(DEMOCON) RETURNED FROM CALLOV?         
         BNE   *+6                                                              
         DC    H'0'                NO                                           
         MVC   CDEMOCON,DMCB       SAVE A(DEMOCON) IN COMFACS                   
*                                                                               
INIT20   DS    0H                                                               
         MVC   ADEMOCON,CDEMOCON   SAVE A(DEMOCON) IN WORKING STORAGE           
         DROP  R4                                                               
*                                                                               
         GOTO1 ADEMOCON,DMCB,(0,0),('DEMOCON_14',APLDTABS),0                    
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* FIX NAD HOMES                                                       *         
***********************************************************************         
         SPACE 1                                                                
FIXNADO  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =AL2(NTIFILE),DBINTFIL                                           
         BE    *+14                                                             
         CLC   DBFILE,=AL3(EVNFILE)                                             
         BNE   FIXNADOX                                                         
*                                                                               
         L     RF,AOVEREL                                                       
         XR    R1,R1                                                            
FIXNADO2 CLI   5(RF),1                                                          
         BNE   FIXNADO4                                                         
         CLI   3(RF),2                                                          
         BL    *+8                                                              
         MVI   5(RF),249                                                        
*                                                                               
FIXNADO4 IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLC   0(1,RF),OVERELEM                                                 
         BE    FIXNADO2                                                         
*                                                                               
FIXNADOX XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* FIX NAD                                                             *         
***********************************************************************         
         SPACE 1                                                                
FIXNADI  NTR1  BASE=*,LABEL=*                                                   
         CLC   =AL2(NTIFILE),DBINTFIL                                           
         BE    *+14                                                             
         CLC   DBFILE,=AL3(EVNFILE)                                             
         B     FIXNADIX                                                         
*                                                                               
         XR    R1,R1                                                            
         L     RF,AOVEREL                                                       
FIXNADI2 CLI   5(RF),249                                                        
         BNE   FIXNADI4                                                         
         CLI   3(RF),2                                                          
         BL    *+8                                                              
         MVI   5(RF),1                                                          
*                                                                               
FIXNADI4 IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         CLC   0(1,RF),OVERELEM                                                 
         BE    FIXNADI2                                                         
*                                                                               
FIXNADIX XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*WKTOMON - CONVERT WK TO MONTH AND LOOK UP NAD UNIV                             
*        OUTPUT: R1 = UNIVERSE VALUE OR ZERO IF NOT FOUND                       
***********************************************************************         
         SPACE 1                                                                
WKTOMON  NTR1  BASE=*,LABEL=*                                                   
         XR    R1,R1                                                            
         ICM   R0,3,BOOK           SAVE WKLY BOOK IN R0                         
         OC    BOOK,BOOK                                                        
         BZ    WKXIT                                                            
         XC    BOOK,=X'FFFF'                                                    
         CLC   BOOK,=AL1(YR_1999,WEEK_36)  WKLY NAD DEMOS FROM SEP99 ON         
         BL    WKXIT                                                            
*                                                                               
         ICM   RF,15,DBCOMFCS      GRAB THE TABLE                               
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,NENACWKS  GET MONTH<->WEEKS TABLE                      
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLE ID PASSED                          
         ICM   RF,15,4(R1)         L'TABLE ENTRY                                
         USING NEWKSD,RE                                                        
*                                                                               
WKTOM5   CLC   BOOK(1),NEWKSYR     MATCH YEAR                                   
         BNE   WKTOM10                                                          
         CLC   BOOK+1(1),NEWKSLST                                               
         BH    WKTOM10                                                          
         MVC   BOOK+1(1),NEWKSMO   SET MONTH                                    
         XC    BOOK,=X'FFFF'       REVERSE THE BOOK                             
         BRAS  RE,NADUNV           LK UP UNIV RTNS (R1)                         
         B     WKXIT                                                            
*                                                                               
WKTOM10  DS    0H                                                               
         AR    RE,RF               BUMP TO NEXT WK ENTRY                        
         CLI   0(RE),X'FF'         END OF TABLE?                                
         BNE   WKTOM5                                                           
*                                                                               
WKXIT    STCM  R0,3,BOOK           RESTORE WKLY BOOK                            
         LTR   R1,R1               WAS A UNIV VALUE RTND?                       
*                                                                               
         XIT1  REGS=(R1)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RE                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK NAD UNIVERSE TABLE BUILT IN DATASPACE                         *         
***********************************************************************         
         SPACE 1                                                                
NADUCHK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,=A(DEMTABCL)     A(LIST OF TABLES IN PROGRAM)                 
         A     R2,RELO                                                          
         XR    RF,RF                                                            
         ICM   RF,7,((TABNADU+1)-DEMTABCL)(R2)                                  
         JNE   NADUCHKX                                                         
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTOFF-COMFACSD(RF)                                      
         JZ    *+6                                                              
         BASR  RE,RF                                                            
*                                                                               
         MVC   TABLISTW(5),=X'E4000000FF'   BUILD NAD UNIV TABLE                
         L     RF,DBCOMFCS                                                      
         LR    R0,RF                                                            
         L     RF,CDEMADDR-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB2,(X'FF',TABLISTW),(R0)                                 
         MVC   TABNADU-DEMTABCL(4,R2),TABLISTW                                  
*                                                                               
         L     RF,DBCOMFCS                                                      
         ICM   RF,15,CPROTON-COMFACSD(RF)                                       
         JZ    NADUCHKX                                                         
         BASR  RE,RF                                                            
*                                                                               
NADUCHKX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* CALCULATE THE MEAN AGE OF A DEMO RECORD                            *          
* (POINT AT WHICH VEIWING CROSSES 1/2 THE TOTAL AUDIENCE             *          
**********************************************************************          
*                                                                               
MEANAGE  NTR1  BASE=*,LABEL=*                                                   
         USING DEMCALCD,RC         RC=A(DEMCALC LOCAL W/S)                      
*                                                                               
         L     R1,APARM                                                         
         MVC   MAPARAM(12),0(R1)                                                
*                                                                               
         DC    H'0'                DEIS SAYS THIS SHOULDN'T BE CALLED           
*                                   ANYMORE: SUPPORT WITHDRAWN                  
*                                                                               
         LA    RE,MEANAGEO                                                      
         ST    RE,MAPARAM+8                                                     
         LA    RE,MEANAGEI                                                      
         ST    RE,MAPARAM                                                       
         MVI   MAPARAM,LISTMULT                                                 
         XC    MEANAGEO(80),MEANAGEO                                            
         SR    R1,R1                                                            
         CLI   DEMOEXP,DEMO_MODIFIER_T 0DEC                                     
         BE    *+12                                                             
         CLI   DEMOEXP,DEMO_MODIFIER_O 1DEC                                     
         BNE   MAGEX                                                            
* BUILD THE INPUT DEMO LIST                                                     
         LA    RE,MEANAGEI                                                      
         LA    RF,MAGPNN                                                        
MA01     CLI   0(RF),X'FF'                                                      
         BE    MACALL                                                           
         MVI   0(RE),0                                                          
         MVC   1(2,RE),0(RF)                                                    
         LA    RE,3(RE)                                                         
         LA    RF,3(RF)                                                         
         B     MA01                                                             
* GATHER THE DEMOS                                                              
MACALL   MVI   0(RE),X'FF'                                                      
         MVC   MADEMTYP,DBDEMTYP                                                
         MVI   DBDEMTYP,0                                                       
         LA    R1,MAPARAM                                                       
         BRAS  RE,DEMOUT                                                        
         MVC   DBDEMTYP,MADEMTYP                                                
* GET THE TOTALS                                                                
         L     R4,MEANAGEO                                                      
         SRL   R4,1                                                             
         LA    RF,MEANAGEO+4                                                    
         LA    RE,MAGPNN+3                                                      
         LA    R1,2                START AGE                                    
         SR    R0,R0                                                            
MA10     CLI   0(RE),X'FF'         LOOP PROTECT                                 
         BNE   MA11                                                             
* ROUNDING ERRO FUDGE AND EXIT                                                  
         LHI   R1,40                                                            
         ST    R1,MAPARAM                                                       
         J     MAGEXIT                                                          
*                                                                               
MA11     A     R0,0(RF)                                                         
         CR    R0,R4                                                            
         BNL   MA12                                                             
         ZIC   R2,2(RE)            GET THE AGE VALUE                            
         AR    R1,R2               INCREMENT LAST BASE                          
         LA    RF,4(RF)                                                         
         LA    RE,3(RE)                                                         
         B     MA10                                                             
*                                                                               
MA12     SR    R0,R4               GET THE EXCESS                               
****     MH    R0,=H'1000'         ADJUST TO CALC PERCENT                       
         MHI   R0,100              ADJUST TO CALC PERCENT                       
         LR    R4,R1               SAVE RUNNING AGE                             
         LR    R1,R0               GET THE ADJUSTED EXCESS                      
         SR    R0,R0                                                            
         ICM   R5,15,0(RF)         DIVIDE BY THE TOTAL AGE VALUE                
         LTR   R5,R5                                                            
         BZ    *+6                                                              
         DR    R0,R5               TO GET A PERCENT                             
         SHI   R1,100              NOW ADJUST TO START AGE PORTION              
         LPR   R1,R1                                                            
         ZIC   R0,2(RE)            GET NUMBER OF YEARS IN GROUP                 
         MR    R0,R0               MULTIPLY BY NUMBER OF YEARS IN AGE           
         CLI   DEMOEXP,DEMO_MODIFIER_T                                          
         JNE   *+16                                                             
         AHI   R1,50              AND AJUST PRECISION (0 DEC)                   
         D     R0,=F'100'                                                       
         J     *+16                                                             
         AHI   R1,5               AND AJUST PRECISION (1 DEC)                   
         D     R0,=F'10'                                                        
         MHI   R4,10                                                            
         AR    R1,R4               ADD TO PREV TOTAL AGE TO GET MEAN            
MAGEX    ST    R1,MAPARAM                                                       
*                                                                               
MAGEXIT  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
* BASE LIST FOR PNN RECORDS                                                     
MAGPNN   DC    AL1(DEMO_MODIFIER_Y),AL1(127),AL1(3)  PERSONS 2+                 
         DC    AL1(DEMO_MODIFIER_Y),AL1(121),AL1(4)  PERSONS 2-5                
         DC    AL1(DEMO_MODIFIER_Y),AL1(161),AL1(3)  PERSONS 6-8                
         DC    AL1(DEMO_MODIFIER_Y),AL1(162),AL1(3)  PERSONS 9-11               
         DC    AL1(DEMO_MODIFIER_Y),AL1(124),AL1(3)  PERSONS 12-14              
         DC    AL1(DEMO_MODIFIER_Y),AL1(126),AL1(3)  PERSONS 15-17              
         DC    AL1(DEMO_MODIFIER_Y),AL1(188),AL1(3)  PERSONS 18-20              
         DC    AL1(DEMO_MODIFIER_Y),AL1(189),AL1(4)  PERSONS 21-24              
         DC    AL1(DEMO_MODIFIER_Y),AL1(163),AL1(5)  PERSONS 25-29              
         DC    AL1(DEMO_MODIFIER_Y),AL1(164),AL1(5)  PERSONS 30-34              
         DC    AL1(DEMO_MODIFIER_Y),AL1(165),AL1(5)  PERSONS 35-39              
         DC    AL1(DEMO_MODIFIER_Y),AL1(166),AL1(5)  PERSONS 40-44              
         DC    AL1(DEMO_MODIFIER_Y),AL1(223),AL1(5)  PERSONS 45-49              
         DC    AL1(DEMO_MODIFIER_Y),AL1(155),AL1(5)  PERSONS 50-54              
         DC    AL1(DEMO_MODIFIER_Y),AL1(158),AL1(10) PERSONS 55-64              
         DC    AL1(DEMO_MODIFIER_Y),AL1(160),AL1(9)  PERSONS 65+                
*                                                     (ONLY 9 YEARS)            
         DC    X'FF'                                                            
*                                                                               
         DROP  RC                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ADJSY - DIG OUT SYSCODE ADJUSTMENT FACTOR FOR FUSION DATA           *         
* R1 IS RETURNED                                                      *         
***********************************************************************         
ADJSY    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* CHECK DBEXTEND FOR UPBK EXTENSION                                             
* IF BOOK IN EXTENSION IS JAN08 OR LATER THAT MEANS THIS IS AN UPGRADE          
* LOOKUP AND EVERYTHING HAS BE ADJUSTED TO A JAN08 OR LATER AIUE INDEX          
         ZICM  RE,DBEXTEND,(15)                                                 
         LTR   RE,RE                                                            
ADJS06   BZ    ADJS09                                                           
         CLC   =C'UPBK',0(RE)                                                   
         BE    ADJS08                                                           
         ICM   RE,15,4(RE)                                                      
         B     ADJS06                                                           
ADJS08   DS    0C                                                               
         USING DBLATUBK,RE                                                      
         CLC   DBLUPBK,=AL2(JAN_08)                                             
         BNL   ADJSYX                                                           
*                                                                               
*THIS WILL CHANGE THE OLD METHODOLDY OF ALWAYS USING THE LATEST BOOK            
*TO GET INDEX FOR UPGRADES                                                      
         OC    SVACTBK,SVACTBK                                                  
         BNZ   *+10                                                             
         MVC   SVACTBK,DBLUPBK                                                  
         DROP  RE                                                               
* IF THERE IS AIUE LINK IN DBSAPNAD THEN DONT DO SYSCODE ADJUSTMENT             
* BECAUSE WE ARE ADJUSTING IN SPGETIUN                                          
*                                                                               
ADJS09   ZICM  RE,DBSPANAD,(15)                                                 
         LTR   RE,RE                                                            
         BZ    ADJS10                                                           
         CLC   =C'AIUE',0(RE)                                                   
         JE    ADJSYX                                                           
         CLC   DBSELBK,=AL2(JAN_08)                                             
         BNL   ADJSYX                                                           
         CLC   SVACTBK,=AL2(JAN_08)                                             
         BNL   ADJSYX                                                           
         OC    SVACTBK,SVACTBK                                                  
         BNZ   ADJS10                                                           
*                                                                               
* NO BOOK SPECIFED ...LETS LOOK AT RECORD TO SEE IF WE ARE POST                 
* JAN08                                                                         
         L     RE,DBAREC                                                        
         USING DRKEY,RE                                                         
         CLC   DRBOOK,=X'93FE'     AFTER JAN_08?                                
         JH    ADJS10              NO                                           
         B     ADJSYX                                                           
         DROP  RE                                                               
*                                                                               
*                                                                               
ADJS10   LR    R4,R1               SAVE THE DEMO VALUE                          
*                                                                               
         LA    RE,NONADJT                                                       
ADJS14   CLI   0(RE),X'FF'                                                      
         BE    ADJS16                                                           
         CLC   BASEMOD,0(RE)                                                    
         BE    ADJSY_Y             DON'T ADJUST THIS DEMO                       
         LA    RE,L'NONADJT(RE)                                                 
         B     ADJS14                                                           
*                                                                               
ADJS16   DS    0H                                                               
         L     R2,=A(DEMTABCL)     A(LIST OF TABLES IN PROGRAM)                 
         A     R2,RELO                                                          
         ICM   R2,15,TABFUSN-DEMTABCL(R2)                                       
         BZ    ADJSYL              ADDR OF TABLE NOT FOUND                      
         USING FUSTABD,R2                                                       
         LAM   AR2,AR2,ALET                                                     
         SAC   512                                                              
*                                                                               
         SAM31                     GO INTO 31-BIT MODE                          
*                                                                               
         MVC   AENDFTAB,8(R2)      SAVE # OF ENTRIES                            
         AHI   R2,FUSTABLQ         1ST TABLE ENTRY IS BINSR31 PARM LIST         
*                                   (BUMP PAST IT)                              
         BC    15,ADJSY18   <---PATCH FOR TESTING (GO TO BINARY SRCH)           
         BC    15,ADJSY20   <---PATCH FOR TESTING (GO TO SEQ SEARCH)            
*                                                                               
* FIND THE SYSCODE FACTOR HERE (SYSCODE IS AT DBSELSYC)                         
* AND PUT IT IN R0, R1 HAS THE DEMO VALUE                                       
*                                                                               
         LHI   R0,1000             FORCE ADJUSTMENT 200%                        
         MR    R0,R0               MULTIPLY BY DEMO VALUE                       
         AHI   R1,500              AND ROUND IT                                 
         D     R0,=F'500'                                                       
***      B     ADJSY_Y                                                          
                                                                                
*  BINARY SEARCH THROUGH FUSION TABLE                                           
                                                                                
ADJSY18  XC    DMCB,DMCB                                                        
*    CALCULATE END OF FUSION TABLE ADDRESS                                      
         L     R0,AENDFTAB         NUMBER OF FUSION RECORD                      
         SR    RE,RE                                                            
         LA    RF,FUSTABLQ         L'FUSTAB TBL ENTRY                           
         MR    RE,R0               # OF RECS * L'REC                            
         AR    RF,R2                                                            
         SHI   RF,1                                                             
         ST    RF,AENDFTAB         END OF FUSION TABLE                          
*                                                                               
         ST    R1,DMCB+8           SAVE DEMO VALUE (PRECAUTION)                 
         GOTOR FINDSYSC,DMCB,(R2)  BINSRCH                                      
         LAM   AR2,AR2,ALET                                                     
         L     R2,DMCB+4                                                        
         L     R1,DMCB+8           RESTORE DEMO VALUE (PRECAUTION)              
         OC    DMCB+4(4),DMCB+4    EXIT -NOT FOUND                              
         BZ    ADJSYL                                                           
*                                                                               
         B     ADJSY22                                                          
                                                                                
* SEQUENTIAL SEARCH THROUGH FUSION TABLE                                        
                                                                                
ADJSY20  CLC   FUSSYSCD,=XL2'00'   END OF TABLE?                                
         BE    ADJSYL              YES, SYSCODE NOT PRESENT                     
         CLC   DBSELSYC,FUSSYSCD   LOOK FOR CORRECT SYSCODE                     
         BE    *+16                GOT IT                                       
         BL    ADJSYL              SYSCODE NOT IN TABLE                         
         AHI   R2,FUSTABLQ         L'FUSTAB TBL ENTRY                           
         B     ADJSY20                                                          
ADJSY22  DS    0C                                                               
*                                                                               
* THE EARLIEST ALL-MARKET FUSION SUBSCRIBER BASE DATA WE HAVE IS FOR            
* FEB/04. THERE IS DATA FOR DEC/03 AND JAN/04, BUT THESE ARE NOT                
* SWEEP MONTHS, AND THEREFORE CONTAIN INCOMPLETE SUBSCRIBER DATA. SO            
* HERE'S WHAT WE DO, DEPENDING UPON THE REQUESTED BOOK:                         
*   -- PRIOR TO DEC/03: LOOKUP FEB/04.                                          
*   -- DEC/03:          LOOKUP DEC/03. IF NOT PRESENT, USE FEB/04.              
*   -- JAN/04:          LOOKUP JAN/04. IF NOT PRESENT, USE FEB/04.              
*   -- AFTER JAN/04:    LOOK UP REQUESTED BOOK ONLY.                            
*                                                                               
         LR    R0,R2               SAVE A(1ST TABLE ENTRY FOR SYSCODE)          
***      MVC   HALF,DBSELBK        'HALF' WILL CONTAIN LOOKUP BOOK              
         MVC   HALF,SVACTBK        'HALF' WILL CONTAIN LOOKUP BOOK              
*                                                                               
*  SPECIAL CONDITION IF SVACTBK IS NOT KNOWN SUCH AS FOR UPGRADES               
*  WE NEED TO LOOK FOR THE LATEST OCCURANCE OF SYSCODE IN THE                   
*  FUSION TABLE. AT THIS POINT WE SHOULD BE ALREADY AT THE LATEST YEAR          
*  WHERE SYSCODE APPEARS . R2 POINTS TO TABLE                                   
         OC    SVACTBK,SVACTBK                                                  
         BNZ   ADJSY30                                                          
*                                                                               
*  NO BOOK SPECIFIED , WE WANT TO USE  LATEST SYSCODE                           
*  SEE IF NEXT ENTRIES HAVE MATCHING SYSCODE                                    
*  WE WANT TO KEEP LOOKING FOR LATEST YEAR AND USE THE LATEST SYSCODE           
*  NUMBERS                                                                      
ADJSY24  AHI   R2,FUSTABLQ                                                      
         CLC   FUSSYSCD,=XL2'00'   END OF TABLE?                                
         BE    ADJSY26             YES, SYSCODE NOT PRESENT                     
         CLC   DBSELSYC,FUSSYSCD   LOOK FOR CORRECT SYSCODE                     
         BE    ADJSY24             GOT IT                                       
ADJSY26  SHI   R2,FUSTABLQ         GO BACK TO LAST MATCH                        
*                                                                               
         LA    RF,12               MAXIMUM 12 MONTHS                            
         AHI   R2,FUSTABLQ         L'FUSTAB TBL ENTRY                           
         SHI   R2,L'FUSSBASE+L'FUSUNIV                                          
ADJSY28  CLC   0(4,R2),=XL4'00'    L'FUSSBASE                                   
         BE    *+14                                                             
         CLC   4(4,R2),=XL4'00'    L'FUSUNIV                                    
         BNE   ADJSY60             FOUND IT                                     
         SHI   R2,L'FUSSBASE+L'FUSUNIV                                          
         BCT   RF,ADJSY28                                                       
                                                                                
*                                                                               
*                                                                               
***      CLC   DBSELBK,=X'670C'    REQUESTED BOOK IS DEC/03?   *Y2K*            
ADJSY30  CLC   SVACTBK,=AL2(DEC_03) REQUESTED BOOK IS DEC/03?   *Y2K*           
         BL    ADJSY50             PRIOR: USE FEB/04 DATA                       
         BH    *+16                AFTER: TRY AGAIN                             
         CLI   FUSYEAR,YR_2003     ARE WE POINTING AT YEAR 2003?                
         BE    ADJSY56             YES: WE HAVE DEC/03 DATA                     
         B     ADJSY50             USE FEB/04 SUBSCRIBER BASE DATA              
*                                                                               
***      CLC   DBSELBK,=X'6801'    REQUESTED BOOK IS JAN/04?   *Y2K*            
         CLC   SVACTBK,=AL2(JAN_04) REQUESTED BOOK IS JAN/04?   *Y2K*           
         BNE   ADJSY52             NO: USE REQUESTED BOOK ONLY                  
*                                                                               
ADJSY36  CLI   FUSYEAR,YR_2004     YEAR 2004?                                   
         BH    ADJSY50             USE FEB/04 SUBSCRIBER BASE DATA              
         BL    ADJSY40             NO                                           
*                                                                               
         LR    RF,R2                                                            
F        USING FUSVALS,RF                                                       
         CLC   F.FUSSBASE,=XL4'00' ANY JAN/04 SUBSCRIBER BASE DATA?             
         BE    ADJSY50             NO: USE FEB/04 SUBSCRIBER BASE DATA          
         CLC   F.FUSUNIV,=XL4'00'  ANY JAN/04 UNIVERSE DATA?                    
         BE    ADJSY50             NO: USE FEB/04 SUBSCRIBER BASE DATA          
         B     ADJSY56             YES: USE IT                                  
         DROP  F                                                                
*                                                                               
ADJSY40  AHI   R2,FUSTABLQ         L'FUSTAB TBL ENTRY                           
         CLC   DBSELSYC,FUSSYSCD   SAME SYSCODE?                                
         BE    ADJSY36             YES: CHECK THE YEAR                          
*                                                                               
ADJSY50  MVC   HALF,=AL2(FEB_04)   USE FEB/04 SUBSCRIBER BASE DATA              
         LR    R2,R0               RESTORE A(1ST ENTRY FOR SYSCODE)             
*                                                                               
ADJSY52  DS    0H                  LOOKUP BOOK IN 'HALF'                        
         CLC   DBSELSYC,FUSSYSCD   SAME SYSCODE?                                
         BNE   ADJSYL              NO: NO SUBSCRIBER DATA AVAILABLE             
         CLC   HALF(1),FUSYEAR     SAME YEAR?                                   
         BE    *+16                YES: GOT THE TABLE ENTRY                     
         BL    ADJSYL              NO: NO SUBSCRIBER DATA AVAILABLE             
         AHI   R2,FUSTABLQ         L'FUSTAB TBL ENTRY                           
         B     ADJSY52                                                          
*                                                                               
ADJSY56  DS    0H                  R2 POINTS TO TABLE ENTRY                     
         LA    R2,FUSVALS                                                       
         DROP  R2                                                               
         ZIC   R1,HALF+1           GET THE MONTH                                
         BCTR  R1,0                MONTHS ARE 1-BASED                           
         MHI   R1,L'FUSSBASE+L'FUSUNIV  INDEX INTO MONTHLY DATA                 
         AR    R2,R1                                                            
*                                                                               
         USING FUSVALS,R2                                                       
ADJSY60  ICM   RF,15,FUSSBASE      GET THE SUBS BASE                            
         BZ    ADJSYL              EXIT IF ZERO                                 
         ICM   R1,15,FUSUNIV       AND THE UNIVERSE                             
         BZ    ADJSYL              EXIT IF ZERO                                 
         CLC   FUSSBASE,FUSUNIV    MAKE SUBBASE SAME AS UNIV IF                 
         BL    *+6                 SUBBASE> UNIV                                
         LR    RF,R1                                                            
         DROP  R2                                                               
*                                                                               
         SR    RE,RE               PREPARE FOR MULTIPLY                         
****     M     RE,=F'2000'         CALC THE INDEX                               
         M     RE,=F'200000'         CALC THE INDEX                             
         DR    RE,R1                                                            
         LR    R1,RF               SAVE THE INDEX                               
         LR    RF,R4               RESTORE THE DEMO                             
         SR    RE,RE                                                            
         MR    RE,R1               NOW ADJUST THE DEMO                          
         D     RE,=F'100000'                                                    
         AHI   RF,1                AND ROUND IT                                 
         SRA   RF,1                                                             
         LR    R1,RF               AND MOVE TO OUTPUT REG                       
         B     ADJSY_Y                                                          
*                                                                               
*                                                                               
ADJSYL   XR    R1,R1               NO REAL DEMO DATA - CLEAR VALUE              
         CLI   *,255               SET CC NE                                    
         J     ADJSYX                                                           
*                                                                               
ADJSY_YN XR    R1,R1               NO REAL DEMO DATA - CLEAR VALUE              
*                                                                               
ADJSY_Y  CR    RB,RB               SET CC EQ AS YEAR FOUND                      
*                                                                               
ADJSYX   LAM   AR1,AR2,=2F'0'                                                   
         XIT1  REGS=(R1)                                                        
*                                                                               
* TABLE OF MODIFIERS THAT DON'T HAVE TO BE SYSCODE ADJUSTED                     
NONADJT  DS    0CL1                                                             
         DC    AL1(DEMO_MODIFIER_G)                                             
         DC    AL1(DEMO_MODIFIER_U)                                             
         DC    AL1(DEMO_MODIFIER_P)                                             
         DC    AL1(DEMO_MODIFIER_Q)                                             
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*********************************************************************           
* ROUTINE TO BINSRCH THROUGH FUSION TABLE TO FIND THE SYSCODE       *           
* ENTRY = DMCB(4), =A(START OF TABLE)                               *           
* EXIT  = DMCB+4(4) = A(OF FOUND ENTRY)                             *           
*         DMCB+4(4) = NULLS IF NOT FOUND                            *           
*         DMCB+8(4) = CONTENDS OF R1- DEMO VALUE-RESTORE AFTER CALL *           
*********************************************************************           
FINDSYSC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAM   AR6,AR6,ALET                                                     
         L     R6,DMCB             ADDRESS OF 1ST RECORD IN DATASPACE           
         SAC   512                                                              
         CPYA  ARE,AR6             POINT TO SAME PAGE                           
                                                                                
         LR    RE,R6                                                            
         ST    RE,AFSTFTAB         SAVE START OF SEARCH                         
         CPYA  AR1,AR6             POINT TO SAME PAGE                           
         ZICM  R1,AENDFTAB,(15)    END OF TABLE                                 
                                                                                
         LA    R0,FUSTABLQ         LENGTH OF EACH REC                           
         LR    RF,R0                                                            
         SR    RE,R0               RE=A(START OF TABLE-L'ENTRY)                 
         SR    R1,RE                                                            
         AR    R0,R0               R0=LOWEST POWER OF 2 >= R1                   
         CR    R0,R1                                                            
         JNH   *-4                                                              
         AR    R1,RE               R1=A(END OF LAST ENTRY)                      
                                                                                
FUSSH48  SRL   R0,1                1/2 REMAINING TABLE LENGTH                   
         CR    R0,RF               TEST IF LESS THAN AN ENTRY LENGTH            
         JL    FUSSH56                                                          
         BRXH  RE,R0,FUSSH52       COMPUTE NEW TABLE START ADDRESS              
*                                  RE POINTS TO HALF OF REMAINING               
         CLC   DBSELSYC,0(RE)      TEST IF SYSCODE FOUND                        
         JH    FUSSH48                                                          
         JL    FUSSH52                                                          
         JE    FUSSH53             FOUND IT                                     
                                                                                
FUSSH52  SR    RE,R0               POINT BACK TO 1ST HALF                       
         J     FUSSH48                                                          
                                                                                
FUSSH53  DS    0H                 FOUND IT- MATCHED AGENCY CODE                 
         ST    RE,DMCB+4          RETURN ADDRESS OF FUSION ENTRY                
* MAKE SURE WE GOT EARLIEST ONE                                                 
FUSH54   C     RE,AFSTFTAB                                                      
         JL    FUSSHX                                                           
         SHI   RE,FUSTABLQ         GO BACK AN ENTRY                             
         CLC   DBSELSYC,0(RE)      TEST IF SYSCODE FOUND                        
         JNE   FUSSHX                                                           
         ST    RE,DMCB+4                                                        
         J     FUSH54                                                           
                                                                                
         J     FUSSHX                                                           
                                                                                
FUSSH56  XC    DMCB+4(4),DMCB+4   SEARCH IS LESS THAT FIRST TABLE ENTRY         
                                                                                
FUSSHX   DS    0H                                                               
         XR    RE,RE                                                            
         SAR   ARE,RE              CLEAR ARE                                    
         XR    R1,R1                                                            
         SAR   AR1,R1              CLEAR AR1                                    
         XR    R6,R6                                                            
         SAR   AR6,R6              CLEAR AR6                                    
                                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
***********************************************************************         
* NADUNV - SEE IF UNIVERSE IS IN DATASPACE NAD TABLE                  *         
*          TABLE BEGINS 9/95                                          *         
* NTRY: R5      = A(MASTER TABLE ENTRY)                               *         
*       R6      = A(MASTER TABLE HEADER)                              *         
*       AR5/AR6 = SET TO TABSDSP                                      *         
*       ACCESS REGISTER MODE IS ON -- BFC WHEN CHANGING CODE          *         
* EXIT: CC EQ   = UNIVERSE FOUND                                      *         
*               = R1 = DEMO VALUE OR ZERO                             *         
* EXIT: CC NE   = UNIVERSE NOT FOUND - UPON RETURN WILL GOTO NADDEMO  *         
***********************************************************************         
         SPACE 1                                                                
         USING MSTDTAD,R5                                                       
         USING MSTHDRD,R6                                                       
NADUNV   NTR1  BASE=*,LABEL=*                                                   
         USING DEMCALCD,RC         RC=A(DEMCALC LOCAL W/S)                      
*                                                                               
         LLC   RF,MSTMODC          ENCODED MODIFIER                             
         A     RF,APLPLDTB                                                      
         CLI   0(RF),C' '          IS THIS A PERSONAL LANGUAGE DEMO?            
         JH    NADUNL              YES: DON'T GET NAD UNIVERSE                  
*                                                                               
         LLC   RF,MSTMODC          ENCODED MODIFIER                             
         A     RF,APLMODTB         A(DECODED MODIFIER)                          
         CLI   0(RF),DEMO_MODIFIER_K TABLE ONLY CONTAINS UNVS                   
         JNE   NADUNL              NOT PRESENT                                  
*                                  TABLE BEGINS MAY_95                          
         CLC   BOOK,=X'A0FA'       DATE PRIOR TO START OF TABLE?                
         JH    NADUNL              YES, YEAR NOT IN HI CORE TABLE               
*****                                                                           
* THIS IS JUST TEMPORARY UNTIL WE CHANGE DEMADDR TO BUILD A TABLE               
* FOR MOVIEGOER UNIVERSES. FOR NOW, GET CABLE MOVIE GOER UNIVERSES              
* FROM INTERNAL TABLE. NUMBERS COME FROM NETW MVGO FILE                         
*****                                                                           
*   -- CHECK IF CABLE MOVIE GOER HERE - EVENTUALLY NETW MVGO TOO                
         CLC   =AL2(MVGFILE),DBINTFIL (MOVIEGOER = MNN)                         
         BNE   NADUN01                                                          
         CLI   DBACTMED,C'C'        CABLE                                       
         BNE   NADUN01                                                          
*                                                                               
         ICM   RF,15,DBCOMFCS      GRAB THE TABLE                               
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,MOVIEGOU  GET MOVIE GOER UNIVERSE TABLE                
         ICM   R2,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLE ID PASSED                          
         USING MGUD,R2                                                          
*                                                                               
MVGUN01  CLC   =X'FFFF',MGUYR      END OF TABLE?                                
         JE    NADUNL              YES, YEAR NOT PRESENT                        
         CLC   BOOK,MGUYR          GET CORRECT BOOK                             
         JNH   MVGUN05                                                          
         SR    R0,R0                                                            
         ICM   R0,3,MGUYRQ         L'TABLE                                      
         AR    R2,R0                                                            
         J     MVGUN01                                                          
MVGUN05  LA    R2,MGUELEM          GET ADDR OF START OF DATA ELEMS              
         XR    RE,RE                                                            
         J     NADUN04                                                          
         DROP  R2                                                               
*                                                                               
NADUN01  DS    0H                                                               
         L     R2,=A(DEMTABCL)     A(LIST OF TABLES IN PROGRAM)                 
         A     R2,RELO                                                          
         ICM   R2,15,TABNADU-DEMTABCL(R2)                                       
         JZ    NADUNL              ADDR OF TABLE NOT FOUND                      
         LAM   AR2,AR2,ALET                                                     
         SAC   512                                                              
*                                                                               
         SAM31                     GO INTO 31-BIT MODE                          
*                                                                               
NADUN02  CLC   =X'FFFF',0(R2)      END OF TABLE?                                
         JE    NADUNL              YES, YEAR NOT PRESENT                        
         CLC   BOOK,0(R2)          GET CORRECT BOOK                             
         JNH   *+12                                                             
         AHI   R2,HINADLN          L'NADUN TBL ENTRY                            
         J     NADUN02                                                          
*                                                                               
         ICM   R2,15,2(R2)         GET ADDR OF START OF DATA ELEMS              
         XR    RE,RE                                                            
*                                                                               
NADUN04  CLI   0(R2),0             EOR?                                         
         JE    NADUN_YN            YEAR FND BUT NOT SECTION LEAD                
         CLI   0(R2),X'23'         SEARCH FOR CURRENT SECTION LEAD ELEM         
         JE    NADUN08                                                          
*                                                                               
NADUN06  ICM   RE,1,1(R2)                                                       
         AR    R2,RE                                                            
         J     NADUN04                                                          
*                                                                               
NADUN08  CLI   LASTNAD,0           DEAL WITH DEFAULT                            
         JNE   *+12                                                             
         CLI   2(R2),1             WHICH IS REALLY USA TOTAL                    
         JE    NADUN10                                                          
         CLC   2(1,R2),LASTNAD     CORRECT LEAD ELEMENT                         
         JNE   NADUN06                                                          
*                                                                               
         USING DREELEM,R2                                                       
NADUN10  CLC   EQUELCD,DRECODE     TEST ELEMENT CODE GR ARGUMENT                
         JE    NADUN12                                                          
         JL    NADUN_YN            YEAR FND BUT NOT DEMO ELEM                   
         CLI   DRECODE,0            RECORD IS TERMINATED WITH A ZERO            
         JE    NADUN_YN             SO NO DEMOS FOR THIS                        
         XR    RF,RF                                                            
         ICM   RF,1,DRELEN         BUMP TO NEXT DEMO ELEMENT                    
         LTR   RF,RF               STOP IT FROM LOOPING                         
         JNZ   *+6                 IF BAD DATA                                  
         DC    H'0'                                                             
         AR    R2,RF                                                            
         J     NADUN10                                                          
*                                                                               
NADUN12  DS    0H                                                               
**DEIS** MVI   DEMODUB,0           SET IMPLIED DECIMAL INDIC                    
**DEIS** MVI   DEMODUB+1,1         SET FIELD LENGTH TO 1                        
         LA    RF,DRDATA                                                        
         MVC   DEMODUB+0(1),DREFCTRL                                            
         NI    DEMODUB+0,X'FF'-X'07'                                            
         MVC   DEMODUB+1(1),DREFCTRL                                            
         NI    DEMODUB+1,X'07'                                                  
*                                                                               
         LA    RF,DRDATA1                                                       
         STCM  R2,15,DEMODUB+4     SAVE A(DEMO ELEMENT)                         
         DROP  R2                                                               
*                                                                               
         LLC   R1,DEMODUB+1        R1=FIELD LENGTH                              
         LAY   RE,ICMMASK          RE=ICM MASK FOR EXECUTE                      
         LLC   RE,0(R1,RE)                                                      
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,MSTFLDN        R0=FIELD NUMBER                              
         MR    R0,R0               R1=FIELD LENGTH*FIELD NUMBER                 
         LA    R2,0(R1,RF)         R2=A(DEMO VALUE)                             
*                                                                               
         ICM   R1,15,DEMODUB+4                                                  
         CPYA  AR1,AR2                                                          
         XR    R0,R0                                                            
         ICM   R0,1,1(R1)          R0 = ELEMENT LENGTH                          
         AR    R1,R0               R1=A(NEXT DEMO ELEMENT)                      
         CR    R2,R1               TEST IF FIELD IN ELEMENT                     
         JNL   NADUN_YN            NO - EXIT WITH ZERO VALUE                    
*                                                                               
         XR    R1,R1                                                            
         SAR   AR1,R1                                                           
         TM    DEMODUB,X'20'       TEST IF -VE FIELDS PRESENT                   
         JZ    NADUN16                                                          
         TM    0(R2),X'80'         YES - TEST IF THIS FIELD -VE                 
         JZ    NADUN16                                                          
         LCR   R1,R1               YES - SET R1 TO ALL EFFS                     
*                                                                               
* DEIS NOV/2013: IF RE CONTAINS NULLS AT THIS POINT (WE'RE NOT SURE IF          
*   THAT COULD EVER REALLY HAPPEN), THEN THIS ICM WON'T DO ANYTHING.            
*                                                                               
NADUN16  DS    0H                                                               
         EX    RE,*+8                                                           
         J     *+8                                                              
         ICM   R1,0,0(R2)          GET DEMO VALUE INTO R1                       
         JZ    NADUN_YN                                                         
*                                                                               
         TM    DEMODUB,X'40'       TEST IF IMPLIED DECIMAL FIELD                
         JO    NADUN_Y                                                          
         MHI   R1,10               YES - MULTIPLY FIELD BY 10                   
         J     NADUN_Y                                                          
*                                                                               
NADUNL   XR    R1,R1               NO REAL DEMO DATA - CLEAR VALUE              
         CLI   *,255               SET CC NE                                    
         J     NADUNX                                                           
*                                                                               
NADUN_YN XR    R1,R1               NO REAL DEMO DATA - CLEAR VALUE              
*                                                                               
NADUN_Y  CR    RB,RB               SET CC EQ AS YEAR FOUND                      
*                                                                               
NADUNX   LAM   AR1,AR2,=2F'0'                                                   
         XIT1  REGS=(R1)                                                        
         DROP  R5,R6                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RC                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SETNUM - SET DEMO NUMBER. SAME FUNCTION AS START OF DEMCALC,        *         
*          BUT SAVED IN GENERAL WORKING STORAGE                       *         
* NTRY: 0(R2)    = 3 OR 4 BYTE DEMO LIST ENTRY                        *         
*       DBDEMTYP = TYPE OF DEMO LIST                                  *         
* EXIT: DEM2BYT  = 2 BYTE DEMO EXPRN                                  *         
*       DEM3BYT  = 3 BYTE DEMO EXPRN                                  *         
***********************************************************************         
         SPACE 1                                                                
SETNUM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,1(R2)                                                         
         MVC   DEM2BYT,0(RE)                                                    
         MVC   DEM3BYT(1),0(RE)    SAVE DEMO EXPRESSION                         
         MVI   DEM3BYT+1,0         CONVERT 2 CHAR EXPR TO 3 FOR NOW             
         MVC   DEM3BYT+2(1),1(RE)                                               
         CLI   DBDEMTYP,0          UNPACKED 2 CHARACTER DEMO EXPRESSION         
         JE    SETNUMX                                                          
*                                                                               
         CLI   DBDEMTYP,C'P'       PACKED 3 CHARACTER DEMO EXPRESSION           
         JNE   SET3CHR                                                          
         LLC   R0,0(RE)            SET UP THE MODIFIER                          
         SRL   R0,3                                                             
         BCTR  R0,0                                                             
         LAY   RF,CHARTAB                                                       
         AR    RF,R0                                                            
         MVC   DEM3BYT(1),0(RF)                                                 
         XR    RF,RF                                                            
         ICM   RF,3,0(RE)          SET UP THE SEX CODE                          
         SLL   RF,21                                                            
         SRL   RF,28                                                            
         STC   RF,DEM3BYT+1        SET UP AGE CODE                              
         IC    RF,1(RE)                                                         
         SLL   RF,25                                                            
         SRL   RF,25                                                            
         STC   RF,DEM3BYT+2                                                     
         CLI   DEM3BYT+1,1         0-1 ARE OLD SEX/AGE CODES PACKED             
         JH    SETNUMX                                                          
         ZIC   RF,DEM3BYT+1        RUN NUMBER FROM 1-256                        
         SLL   RF,7                 (THIS ALLOWS USE OF OLD FORMULAS)           
         STC   RF,DEM3BYT+1                                                     
         OC    DEM3BYT+2(1),DEM3BYT+1                                           
         MVI   DEM3BYT+1,0         ZAP THE SEX CODE                             
         J     SETNUMX                                                          
*                                                                               
SET3CHR  CLI   DBDEMTYP,3                                                       
         JNE   SET4CHR                                                          
         MVC   DEM3BYT,0(RE)                                                    
         XC    DEM2BYT,DEM2BYT                                                  
         CLI   1(RE),0                                                          
         JNE   SETNUMX                                                          
         MVC   DEM2BYT(1),0(RE)                                                 
         MVC   DEM2BYT+1(1),2(RE)                                               
*                                                                               
SET4CHR  CLI   DBDEMTYP,C'4'       4 CHARACTER CODE                             
         JNE   SETNUMX                                                          
         MVC   DEM3BYT,0(RE)                                                    
         XC    DEM2BYT,DEM2BYT                                                  
         CLI   DEM3BYT+1,1         NEED 4 CHAR - NO OVERRIDE SUPPORT            
         JH    SETNUMX                                                          
         CLI   DEM3BYT+1,1         0 AND 1 RUN 1-250                            
         JNE   *+12                FOR OLD NUMBERS                              
         OI    DEM3BYT+2,X'80'                                                  
         MVI   DEM3BYT+1,0                                                      
         MVC   DEM2BYT(1),DEM3BYT                                               
         MVC   DEM2BYT+1(1),DEM3BYT+2                                           
*                                                                               
SETNUMX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO APPLY INDEX TO DEMO VALUE                                *         
* R1 IS RETURNED                                                      *         
***********************************************************************         
         SPACE 1                                                                
SETMIN   NTR1  BASE=*,LABEL=*                                                   
         XR    RE,RE                                                            
         TM    DEMOFLAG,OVERPRES   ANY OVERRIDES?                               
         JNO   SETMIN06                                                         
*                                                                               
         L     RF,AOVEREL          SEE IF THIS WAS AN OVERRIDE                  
SETMIN02 CLI   2(RF),0             NO MORE OVRS/NOT FND FOR THIS DEMO           
         JE    SETMIN06                                                         
         CLC   0(1,RF),OVERELEM                                                 
         JNE   SETMIN04                                                         
         CLC   3(1,RF),LASTNAD     CHECK CATEGORY CODE                          
         JNE   SETMIN04                                                         
*&&DO                                                                           
**** DEIS COMMENTED OUT THESE TWO INSTRUCTIONS BECAUSE THIS MUST BE A           
**** BUG. FIELD 'OVREXP' IS LOCATED IN DEMCALCD, WHICH IS NOT                   
**** ADDRESSABLE AT THIS POINT. ON THE ASSUMPTION THAT THIS CODE COULD          
**** NEVER HAVE BEEN RIGHT, AND THAT THE CLC BELOW WOULD RESULT IN A            
**** NOT-EQUAL CONDITION, IT SEEMS FAIR TO SIMPLY FALL THROUGH. NO ONE          
**** KNOWS WHAT THE EFFECT OF THIS BUG REALLY IS, IF ANY.                       
         CLC   4(2,RF),OVREXP      CHECK ACTUAL DEMO                            
         JE    SETMINX             THIS WAS AN OVR ->DON'T APPLY MINVAL         
*&&                                                                             
*                                                                               
SETMIN04 IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         J     SETMIN02                                                         
*                                                                               
SETMIN06 ST    R1,DUB              SAVE VALUE FROM RECD                         
         XR    R1,R1                                                            
         L     RF,AUPGREL          A(UPGRADE ELEMENT)                           
         ICM   R1,3,6(RF)          R1 = MIN VALUE IN UPGRADE ELM                
         STC   R0,OPREC            ADJUST MINVALUE PREC TO VALUE PREC           
         MVI   IPREC,X'81'                                                      
         GOTO1 =A(DEMADJE),RR=RELO   ADJUST FIELD PRECISION                     
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         C     R1,DUB              COMPARE MINVALUE TO FILE VALUE               
         JH    *+8                 USE MIN VALUE IF HIGHER                      
         L     R1,DUB              ELSE RESTORE FILE'S VALUE                    
*                                                                               
SETMINX  XIT1  REGS=(R1)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DO PRECISION ADJUSTMENT ON OUTPUT FIELD                  *         
*                                                                     *         
* NTRY - R1 CONTAINS UNADJUSTED VALUE                                 *         
*        ADJIPRC IS INPUT FIELD PRECISION                             *         
*        ADJOPRC IS OUTPUT FIELD PRECISION                            *         
* R1 IS RETURNED                                                      *         
***********************************************************************         
         SPACE 1                                                                
DEMADJP  NTR1  BASE=*,LABEL=*                                                   
         USING DEMCALCD,RC         RC=A(DEMCALC LOCAL W/S)                      
         STAR  CLEAR=Y,ARS=OFF                                                  
*                                                                               
         CLI   ADJIPRC,0                                                        
         JE    DEMADJPX                                                         
         CLI   ADJOPRC,X'FF'                                                    
         JE    DEMADJPX                                                         
         CLC   ADJIPRC,ADJOPRC                                                  
         JE    DEMADJPX                                                         
*                                                                               
         MVC   DEMODUB(1),ADJIPRC                                               
         NI    DEMODUB,X'07'                                                    
         XR    RE,RE                                                            
         IC    RE,DEMODUB                                                       
         TM    ADJIPRC,X'40'                                                    
         JNZ   *+6                                                              
         LNR   RE,RE                                                            
         MVC   DEMODUB(1),ADJOPRC                                               
         NI    DEMODUB,X'07'                                                    
         XR    RF,RF                                                            
         IC    RF,DEMODUB                                                       
         TM    ADJOPRC,X'40'                                                    
         JNZ   *+6                                                              
         LNR   RF,RF                                                            
         SR    RE,RF               RE=PRECISION ADJUSTMENT                      
         JZ    DEMADJPX                                                         
         LPR   RF,RE               GET NTH POWER OF 10 IN R1                    
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         LARL  R2,POWERTEN                                                      
         L     RF,0(R2,RF)                                                      
*                                                                               
         LTR   RE,RE                                                            
         JM    *+10                                                             
         MR    R0,RF               MULTIPLY OR DIVIDE FIELD                     
         J     DEMADJPX                                                         
         LR    R0,R1                                                            
*                                                                               
****     CLI   UNCOUNT,2           TOP LEVEL GETS ROUNDED                       
****     JL    DEMADJP1            INTERMEDIATE LEVELS GET TRUNCATED            
****                                                                            
****     XR    R1,R1                                                            
****     SRDA  R0,31                                                            
****     DR    R0,RF                                                            
****     LTR   R1,R1                                                            
****     JM    *+8                                                              
****     AHI   R1,1                                                             
****     SRA   R1,1                                                             
****     J     DEMADJPX                                                         
*                                                                               
         LR    R1,RF                ADD HALF OF DIVISOR TO BASE NUMBER          
         SRL   R1,1                 THIS IS OK BECAUSE WE ONLY DEAL             
         AR    R1,R0                WITH EVEN NUMBERS HERE                      
         SR    R0,R0                SPECIFICALLY POWERS OF 10                   
         DR    R0,RF                                                            
*                                                                               
DEMADJPX REAR  ARS=ON                                                           
         XIT1  REGS=(R1)           EXIT TO CALLER                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         DROP  RC                                                               
         EJECT                                                                  
***********************************************************************         
* ADJUST FIELD PRECISION                                              *         
*                                                                     *         
* R1 IS RETURNED                                                      *         
***********************************************************************         
*                                                                               
DEMADJE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   IPREC,0             ADJUST PREC FROM BASE ROUTINE                
         BE    DEMADJEX                                                         
         CLI   OPREC,X'FF'                                                      
         BE    DEMADJEX                                                         
         CLC   IPREC,OPREC                                                      
         BE    DEMADJEX                                                         
         MVC   PRECSAV(1),IPREC                                                 
         NI    PRECSAV,X'07'                                                    
         ZIC   RE,PRECSAV                                                       
         TM    IPREC,X'40'                                                      
         BNZ   *+6                                                              
         LNR   RE,RE                                                            
         MVC   PRECSAV(1),OPREC                                                 
         NI    PRECSAV,X'07'                                                    
         ZIC   RF,PRECSAV                                                       
         TM    OPREC,X'40'                                                      
         BNZ   *+6                                                              
         LNR   RF,RF                                                            
         SR    RE,RF               RE=PRECISION ADJUSTMENT                      
         BZ    DEMADJEX                                                         
         LPR   RF,RE               GET NTH POWER OF 10 IN R1                    
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         LARL  R2,POWERTEN                                                      
         L     RF,0(R2,RF)                                                      
*                                                                               
         LTR   RE,RE                                                            
         BM    *+10                                                             
         MR    R0,RF               MULTIPLY OR DIVIDE FIELD                     
         B     DEMADJEX                                                         
*                                                                               
         LR    R0,R1                                                            
         LR    R1,RF                ADD HALF OF DIVISOR TO BASE NUMBER          
         SRL   R1,1                 THIS IS OK BECAUSE WE ONLY DEAL             
         AR    R1,R0                WITH EVEN NUMBERS HERE                      
         SR    R0,R0                SPECIFICALLY POWERS OF 10                   
         DR    R0,RF                                                            
*                                                                               
DEMADJEX DS    0H                                                               
         XIT1  REGS=(R1)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* NAD170 - FIND X'DD' INDEX OVERRIDE ELEMS AND APPLY INDEX            *         
* R1 IS RETURNED                                                      *         
***********************************************************************         
*                                                                               
NAD170   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    R1,DUB              SAVE ORIG VALUE                              
         MVC   DUB+4(4),=F'1000'   DEFAULT IF NO OVERIDE FND                    
         XR    RE,RE                                                            
         TM    DEMOFLAG,OVERPRES   ANY OVERRIDES?                               
         JNO   NAD1708                                                          
         L     RF,AOVEREL          SEE IF THIS WAS AN OVERRIDE                  
NAD1702  CLI   0(RF),0             NO MORE OVRS/NOT FND FOR THIS DEMO           
         JE    NAD1708             NOT FOUND --> SET DEMO# = 0                  
         CLC   0(1,RF),OVERELEM    DD ELEM?                                     
         JNE   NAD1704                                                          
         CLI   3(RF),NAD170Q       CTGY 170?                                    
         JNE   NAD1704                                                          
*                                  ONLY USE MODIF = X'20' ELEMS OR C'V'         
         CLI   4(RF),DEMO_MODIFIER_V                                            
         JNE   NAD1704                                                          
         CLI   5(RF),0            DEFINED DEFAULT VALUE                         
         JNE   *+10                                                             
         MVC   DUB+4(4),8(RF)      SET DEFAULT BUT KEEP LKG FOR MATCH           
         CLC   5(1,RF),DEM2BYT+1   CHECK DEMO NUMBER                            
         JE    NAD1706                                                          
         CLI   3(R2),1             HOMES?                                       
         JNE   NAD1704                                                          
         CLI   5(RF),249                                                        
         JE    NAD1706                                                          
*                                                                               
NAD1704  IC    RE,1(RF)            PICK UP NEXT DD OVERIDE ELEM                 
         AR    RF,RE                                                            
         J     NAD1702                                                          
*                                                                               
NAD1706  MVC   DUB+4(4),8(RF)      SET INDEX AND GO CALCULATE                   
*                                                                               
NAD1708  ICM   RE,15,DUB+4         RE = OVVERIDE INDEX VALUE                    
         L     RF,DUB              ORIG DEMO VALUE                              
         MR    RE,RE               MULTIPY INDEX BY USA DEMO VALUE              
         SR    RE,RE                                                            
         LTR   RF,RF                                                            
         JZ    *+8                                                              
         D     RE,=F'1000'                                                      
         LR    R1,RF               R1 = NEW DEMO VALUE                          
*                                                                               
NAD170X  XIT1  REGS=(R1)                                                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         TITLE 'GET OVERRIDE DEMO VALUE FROM RECORD'                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT OVERRIDE DEMO VALUE FROM A RECORD                *         
*                                                                     *         
* NTRY:  MSTMODC/MSTDEMO IS DEMO VALUE TO BE EXTRACTED                *         
*        ACCESS REGISTERS ARE OFF/CLEARED                             *         
* EXIT:  CC SET ZERO/NON-ZERO ACCORDING TO OVERRIDE VALUE             *         
***********************************************************************         
*                                                                               
DEMGETO  NTR1  BASE=*,LABEL=*,WORK=(R7,GETOWRKL)                                
         USING GETOWRKD,R7                                                      
         USING DEMCALCD,RC         RC=A(DEMCALC LOCAL W/S)                      
*                                                                               
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVI   PREVOVR,C'N'        RESET OVERRIDE FOUND                         
         L     RF,AOVEREL          RF=A(FIRST OVERRIDE ELEMENT)                 
         MVC   OVRALI,OVREXP                                                    
         MVC   OVRALI2,OVREXP                                                   
         CLC   DBINTFIL(2),=AL3(IUNFILE) REP IMP PRECISION                      
         JNE   DEMGT02                                                          
         CLC   DBACTBK,=AL2(JUN_91)    PUT UPGRADE                              
         JE    *+10                                                             
         CLC   DBACTBK,=AL2(NOV_90)    NORMAL                                   
         JNE   DEMGT02                                                          
         CLI   OVRALI,DEMO_MODIFIER_A                                           
         JNE   *+8                                                              
         MVI   OVRALI,DEMO_MODIFIER_R                                           
*                                                                               
* THIS CODE BECAUSE 'H' AND 'T' ARE REALLY THE SAME W/DIFF. PRECISION           
* FOR THE NETWORK SYSTEM OVERRIDES                                              
*                                                                               
DEMGT02  CLC   DBFILE,=AL3(EVNFILE) FOR EVN FILE                                
         JE    DEMGT03                                                          
         CLC   =AL2(NHIFILE),DBINTFIL FOR NHI FILE                              
         JE    DEMGT03                                                          
         CLC   =AL2(NTIFILE),DBINTFIL AND NTI FILE                              
         JNE   DEMGT04                                                          
*                                                                               
DEMGT03  DS    0H                                                               
         CLI   OVRALI+1,249                                                     
         JNE   *+8                                                              
         MVI   OVRALI+1,1                                                       
*                                                                               
         LLC   RE,OVRALI           ENCODED MODIFIER                             
         A     RE,APLMODTB         A(DECODED MODIFIER)                          
         MVC   BYTE,0(RE)          SAVE DECODED MODIFIER                        
         LLC   RE,OVRALI           ENCODED MODIFIER                             
         A     RE,APLPLDTB         A(DECODED PLD CHARACTER)                     
         MVC   BYTE2,0(RE)         SAVE DECODED PLD CHARACTER                   
*                                                                               
         CLI   BYTE,DEMO_MODIFIER_T     EQUATE 'T' TO 'H'                       
         JNE   DEMGT03C                                                         
         XC    FULL3,FULL3         DEMOCON EXPECTS A DEMO LIST ENTRY            
         MVI   FULL3+1,DEMO_MODIFIER_H                                          
         STM   RE,R1,SAV_EF01                                                   
         GOTO1 ADEMOCON,DMCB,(1,FULL3),('DEMOCON_17',FULL3),(R3),BYTE2          
         LM    RE,R1,SAV_EF01                                                   
         MVC   OVRALI2(1),FULL3+1  USE ENCODED MODIFIER                         
         J     DEMGT04                                                          
*                                                                               
DEMGT03C DS    0H                                                               
         CLI   BYTE,DEMO_MODIFIER_H     EQUATE 'H' TO 'T'                       
         JNE   DEMGT03F                                                         
         XC    FULL3,FULL3         DEMOCON EXPECTS A DEMO LIST ENTRY            
         MVI   FULL3+1,DEMO_MODIFIER_T                                          
         STM   RE,R1,SAV_EF01                                                   
         GOTO1 ADEMOCON,DMCB,(1,FULL3),('DEMOCON_17',FULL3),(R3),BYTE2          
         LM    RE,R1,SAV_EF01                                                   
         MVC   OVRALI2(1),FULL3+1  USE ENCODED MODIFIER                         
         J     DEMGT04                                                          
*                                                                               
DEMGT03F DS    0H                                                               
         CLI   BYTE,DEMO_MODIFIER_Y EXPANDED PRECISION GETS                     
         JNE   DEMGT04                                                          
*                                                                               
         XC    FULL3,FULL3         DEMOCON EXPECTS A DEMO LIST ENTRY            
         MVI   FULL3+1,DEMO_MODIFIER_T                                          
         STM   RE,R1,SAV_EF01                                                   
         GOTO1 ADEMOCON,DMCB,(1,FULL3),('DEMOCON_17',FULL3),(R3),BYTE2          
         LM    RE,R1,SAV_EF01                                                   
         MVC   OVRALI(1),FULL3+1   USE ENCODED MODIFIER                         
*                                                                               
         XC    FULL3,FULL3         DEMOCON EXPECTS A DEMO LIST ENTRY            
         MVI   FULL3+1,DEMO_MODIFIER_H                                          
         STM   RE,R1,SAV_EF01                                                   
         GOTO1 ADEMOCON,DMCB,(1,FULL3),('DEMOCON_17',FULL3),(R3),BYTE2          
         LM    RE,R1,SAV_EF01                                                   
         MVC   OVRALI2(1),FULL3+1  USE ENCODED MODIFIER                         
*                                                                               
DEMGT04  CLI   2(RF),0                                                          
         JNE   DEMGT08                                                          
         CLI   LASTNAD,0                                                        
         JE    DEMGT06                                                          
         CLC   =AL2(NTIFILE),DBINTFIL FOR NTI FILE                              
         JE    *+10                                                             
         CLC   DBFILE,=AL3(EVNFILE) FOR EVN FILE                                
         JNE   DEMGT06                                                          
         CLI   OVREXP+1,20         ALWAYS USE USA HOMES                         
         JL    *+14                                                             
DEMGT06  CLC   3(1,RF),LASTNAD     CHECK CATEGORY CODE                          
         JNE   DEMGT12                                                          
         CLC   4(2,RF),OVRALI      CHECK ALIAS DEMO                             
         JE    DEMGET07                                                         
         CLC   4(2,RF),OVRALI2     CHECK ALIAS2 DEMO                            
         JE    DEMGET07                                                         
         CLC   4(2,RF),OVREXP      CHECK ACTUAL DEMO                            
         JNE   DEMGT12                                                          
DEMGET07 ICM   R1,15,8(RF)         SET ITS VALUE                                
*                                                                               
         MVC   DEMOPRC,7(RF)                                                    
         CLC   4(2,RF),OVREXP                                                   
         JE    DEMGT10                                                          
         CLC   =AL3(IUNFILE)(2),DBINTFIL                                        
         JNE   DEMGT10                                                          
*                                                                               
         STM   RE,R1,DMCB3                                                      
         MVC   DMCB3+16(L'DEMOEXP),DEMOEXP                                      
         MVI   DMCB3+16,DEMO_MODIFIER_U                                         
         GOTO1 =A(DEMCALC),DMCB3+20,DMCB3+16,(3,DMCB3+20),0,RR=RELO             
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         LM    RE,R1,DMCB3                                                      
*                                                                               
         XR    R0,R0                                                            
         M     R0,DMCB3+20                                                      
         MVI   DEMOPRC,X'40'                                                    
         J     DEMGT14                                                          
*                                                                               
DEMGT08  CLC   2(2,RF),OVRALI      FOR THE CORRECT ALIAS                        
         BE    *+14                                                             
         CLC   2(2,RF),OVREXP      OR THE CORRECT DEMO                          
         BNE   DEMGT12                                                          
         ICM   R1,3,4(RF)          GET OVERRIDE DEMO VALUE                      
         CLI   1(RF),8             INDICATES 4 BYTE DEMO                        
         BNE   *+8                                                              
         ICM   R1,15,4(RF)                                                      
         CLC   2(2,RF),OVREXP                                                   
         JE    DEMGT10                                                          
         CLC   =AL3(IUNFILE)(2),DBINTFIL                                        
         JNE   DEMGT10                                                          
*                                                                               
         STM   RE,R1,DMCB3                                                      
         MVC   DMCB3+16(L'DEMOEXP),DEMOEXP                                      
         MVI   DMCB3+16,DEMO_MODIFIER_U                                         
         GOTO1 =A(DEMCALC),DMCB3+20,DMCB3+16,(3,DMCB3+20),0,RR=RELO             
         SAFE  CLEAR=Y             ** SAFE MUST BE PRECEDED BY BASR!!!          
         LM    RE,R1,DMCB3                                                      
*                                                                               
         XR    R0,R0                                                            
         M     R0,DMCB3+20                                                      
         MVI   DEMOPRC,X'40'                                                    
*                                                                               
DEMGT10  MVI   PREVOVR,C'Y'        FOUND AN OVERRIDE                            
         CLI   DBINTFIL+1,C'N'     SET INPUT PRECISION                          
         JNE   DEMGT10C             FOR NETWORK                                 
         LLC   RE,2(RF)            ENCODED MODIFIER                             
         A     RE,APLMODTB         A(DECODED MODIFIER)                          
         CLI   0(RE),DEMO_MODIFIER_R                                            
         JNE   DEMGT10C                                                         
         MVI   DEMOPRC,X'81'                                                    
*                                                                               
* SUPPORT ADJUSTMENT OF DEMO OVERRIDES TO 2 DECIMALS                            
*                                                                               
DEMGT10C DS    0H                                                               
         CLC   DBINTFIL(2),=AL3(IUNFILE) ONLY FOR IUN FORMAT                    
         JNE   DEMGT14                                                          
*                                                                               
         TM    DEMOFLAG,INDXPRES   IF INDEX DEMO                                
         JZ    DEMGT10E            FOR ALL UNCOUNT>1 UNINDEX                    
         CLI   UNCOUNT,1           THE OVERRIDE VALUE                           
         BNH   DEMGT10E            SO WE WILL USE UNINDEXED OVERRIDE            
*                                  TO CALCULATE OUR DEMOS WITH                  
*                                  THE INDEXPRES FLAG INDEXES                   
*                                  THE FINAL NUMBER AT THE END                  
         SR    R0,R0                                                            
         M     R0,=F'10000'        INDEX IS IN TEN THOUSAND MAGNITUDE           
         L     RF,AINDXEL                                                       
         SR    R0,R0                                                            
         SR    RE,RE               RE=INDEX VALUE (2DP)                         
         ICM   RE,3,4(RF)                                                       
         CLI   1(RF),6             TEST FOR 6 BYTE ELEMENT                      
         BE    *+8                 YES-INDEX IS HALFWORD                        
         ICM   RE,7,4(RF)          NO-INDEX IS 3 BYTES                          
         DR    R0,RE               DIVIDE  BY DEMO VALUE                        
*                                                                               
DEMGT10E DS    0H                                                               
         LLC   RE,1(R2)            ENCODED MODIFIER                             
         A     RE,BASEMOD          A(DECODED MODIFIER)                          
         CLI   0(RE),DEMO_MODIFIER_R ONLY IF RATINGS BEING PROCESSED            
         BNE   DEMGT14                                                          
         CLI   UNCOUNT,1                                                        
         BNE   DEMGT14                                                          
*********BNE   DEMGT14             DEIS REMOVED THIS REDUNDANT INSTRUC.         
         CLI   RTGADJ,C'2'                                                      
         BNE   *+8                                                              
         MVI   DEMPREC,X'82'                                                    
         CLI   RTGADJ,C'0'         PRECISION ADJUST FOR RATING(0DEC)            
         BNE   DEMGT14                                                          
         MVI   DEMPREC,X'80'                                                    
         J     DEMGT14                                                          
*                                                                               
DEMGT12  ICM   R0,1,1(RF)            BUMP TO NEXT ELEMENT                       
         AR    RF,R0                                                            
         CLI   0(RF),0             TEST E-O-R                                   
         JE    DEMGT16                                                          
         CLC   0(1,RF),OVERELEM    TEST DEMO OVERRIDE ELEMENT                   
         JE    DEMGT04                                                          
         J     DEMGT12                                                          
*                                                                               
DEMGT14  LTR   RF,RF               ALLOW FOR ZERO OVERRIDES                     
         J     DEMGETOX                                                         
         EJECT                                                                  
***********************************************************************         
* NETWORK EVN FILE ONLY CARRIES A LIMITED SET OF NAD DEMOS            *         
* THIS CODE WILL FORCE MISSING DEMOS TO ZERO AND STOP DEMOUT FROM     *         
* DEFAULTING TO THE CATEGORY ZERO DEMOS.                              *         
***********************************************************************         
*                                                                               
DEMGT16  CLC   =AL2(NTIFILE),DBINTFIL                                           
         JE    *+10                                                             
         CLC   =AL2(NHIFILE),DBINTFIL                                           
         JE    *+10                                                             
         CLC   DBFILE,=AL3(EVNFILE)                                             
         JNE   DEMGT18                                                          
*                                                                               
         CLI   OVREXP+1,1          ALWAYS ALLOW HOMES (USA ONLY)                
         JE    DEMGT18                                                          
         CLI   OVREXP+1,158        55-64 NOT ALWAYS IN BASIC SET                
         JE    DEMGT18                                                          
         CLI   OVREXP+1,58         W55-64                                       
         JE    DEMGT18                                                          
         CLI   OVREXP+1,108        M55-64                                       
         JE    DEMGT18                                                          
*                                                                               
         LLC   RE,OVREXP           ENCODED MODIFIER                             
         A     RE,APLMODTB         A(DECODED MODIFIER)                          
*                                                                               
*                                  ALWAYS ALLOW DEFAULT FORMULA(IMPS)           
         CLI   0(RE),DEMO_MODIFIER_T                                            
         JE    DEMGT18                    (NETWORKS)                            
*                                  ALWAYS ALLOW DEFAULT FORMULA(IMPS)           
         CLI   0(RE),DEMO_MODIFIER_H                                            
         JE    DEMGT18                    (CABLE)                               
*                                  ALWAYS ALLOW DEFAULT FORMULA(RTGS)           
         CLI   0(RE),DEMO_MODIFIER_R                                            
         JE    DEMGT18                                                          
         CLI   LASTNAD,0                                                        
         JNE   DEMGT14                                                          
*                                                                               
DEMGT18  LTR   R1,R1                                                            
         J     DEMGETOX                                                         
*                                                                               
DEMGETOX XIT1  REGS=(R1)                                                        
*                                                                               
         DROP  R7                                                               
         DROP  RC                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADJUST DEMO ELEMENTS TO CALCULATE W/ 2 DECIMAL PRECISION *         
*                                                                     *         
* NTRY:  R2-->CURRENT DEMO IN LIST                                    *         
*        R3-->DBLOCK                                                  *         
*        MSTMODC/MSTDEMO IS DEMO VALUE TO BE EXTRACTED                *         
*        ACCESS REGISTERS ARE OFF/CLEARED                             *         
* EXIT:  CC SET ZERO/NON-ZERO ACCORDING TO OVERRIDE VALUE             *         
***********************************************************************         
         SPACE 1                                                                
ADJDREL  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         NI    CALC2TRU,X'FF'-C2T2DEC                                           
*                                                                               
** COPY CALLER'S RECORD TO MY OWN AREA **                                       
*                                                                               
         LA    R0,MYREC                                                         
         LHI   R1,L'MYREC                                                       
         CLI   DBACTEQC,0                                                       
         BNE   AJDRE020                                                         
         LA    R1,1696                                                          
****     LR    R1,R0                                                            
****     L     RF,ARECORD                                                       
****     SR    R1,RF                                                            
AJDRE020 L     RE,ARECORD                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         BNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
*                                                                               
** LOOK UP WHICH DEMO ELEMENTS TO MODIFY **                                     
*                                                                               
         LARL  RF,MULT10TB                                                      
         SR    RE,RE                                                            
AJDRE022 DS    0H                                                               
         CLI   0(RF),EOT                                                        
         BE    AJDRE109                                                         
         CLC   0(2,RF),DBINTFIL     MATCH ON FILE/MEDIA                         
         BNE   AJDRE028                                                         
         CLC   2(1,RF),DBACTSRC      AND SOURCE                                 
         BNE   AJDRE028                                                         
         B     AJDRE029                                                         
                                                                                
AJDRE028 DS    0H                                                               
         IC    RE,3(RF)                                                         
         AR    RF,RE                                                            
         B     AJDRE022                                                         
AJDRE029 EQU   *                                                                
                                                                                
* TAKE OUT THE FOLLOWING CODE - ALWAYS CONVERT THE ELEMENTS FOR NOW             
* NO NEED TO CHECK FOR THE MODIFIERS ALREADY CHECKED IN TESTC2T                 
*&&DO                                                                           
*                                                                               
*                                  FIND THE RIGHT MODIFIER & BOOK               
         LA    R4,4(RF)                                                         
         SR    R0,R0                                                            
AJDRE042 DS    0H                                                               
         CLI   0(R4),EOT                                                        
         BE    AJDRE099                                                         
         CLC   1(1,R4),1(R2)        MATCH ON MODIFIER CODE FIRST                
         BNE   AJDRE047                                                         
         CLC   2(2,R4),DBACTBK      FIND THE CORRECT START BOOK                 
         BH    AJDRE047                                                         
         B     AJDRE049                                                         
                                                                                
AJDRE047 DS    0H                                                               
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         B     AJDRE042                                                         
AJDRE049 EQU   *                                                                
*&&                                                                             
*                                                                               
** PROCEDURES TO ADJUST DEMO ELEMENTS **                                        
*                                                                               
         LA    R5,0(RF)                                                         
         LA    R4,SVELEMTB       SAVE ELEMENT CHANGED IN TABLE                  
*                                   OF ADDRESSES                                
AJDRE062 DS    0H                   START OF LOOP                               
         CLI   0(R5),EOT             CONTINUE UNTIL WE REACH END                
         BE    AJDRE089                                                         
                                                                                
         L     R1,AQTHREL          R1-->ORIGINAL QTR HR ELEM                    
         S     R1,ARECORD          R1 = DISPL TO QTR HR ELEM                    
         BNP   AJDREX               (MUST BE POSITIVE)                          
         LA    R1,MYREC(R1)        R1-->QTR HR ELEM IN OUR COPY OF RECD         
         USING DREELEM,R1                                                       
         SR    R0,R0                                                            
AJDRE072 DS    0H                                                               
         CLI   DRECODE,0                                                        
         BE    AJDRE079                                                         
         CLC   DRECODE,0(R5)                                                    
         BE    AJDRE073                                                         
         IC    R0,DRELEN                                                        
         AR    R1,R0                                                            
         B     AJDRE072                                                         
                                                                                
AJDRE073 DS    0H                                                               
         TM    DREFCTRL,X'80'       TEST FOR DUPLICATE ELEMENT                  
         BZ    AJDRE074X                                                        
*                                                                               
         ZICM  RE,2(R1),(12)                                                    
         SLL   RE,1                                                             
         SRL   RE,17                                                            
         LA    R1,MYREC(RE)                                                     
AJDRE074X EQU  *                                                                
*    SAVE ADDRESS OF ELEMENTS AND VALUE CHANGED                                 
         ST    R1,0(R4)          ADDRESSS OF ELEMENT CHANGED                    
         MVC   4(1,R4),DREFCTRL    BYTE CHANGED                                 
                                                                                
         NI    DREFCTRL,X'FF'-X'40'                                             
         OI    CALC2TRU,C2T2DEC    FUDGED TO CALC TO 2 DECIMAL PLACES           
AJDRE079 EQU   *                                                                
         DROP  R1                                                               
                                                                                
*                                                                               
         AHI   R5,1                                                             
         LA    R4,5(R4)                                                         
         B     AJDRE062                                                         
AJDRE089 EQU   *                                                                
         MVC   0(4,R4),=X'00000000'                                             
*                                                                               
AJDRE099 EQU   *                                                                
                                                                                
*                                                                               
AJDRE109 EQU   *                                                                
                                                                                
*                                                                               
         TM    CALC2TRU,C2T2DEC    FUDGED TO CALC TO 2 DECIMAL PLACES?          
         BZ    AJDRE129             NOPE                                        
*                                                                               
         L     R1,AQTHREL          R1-->ORIGINAL QTR HR ELEMENT                 
         S     R1,ARECORD          R1 = DISPL TO QTR HR ELEMENT                 
         BNP   AJDREX                                                           
         LA    R0,MYREC            LET CALCULATION ROUTINE USE                  
         ST    R0,ARECORD           OUR COPY OF THE RECORD                      
         AR    R1,R0                                                            
         ST    R1,AQTHREL                                                       
AJDRE129 EQU   *                                                                
                                                                                
*                                                                               
AJDREX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COMPUTE A DEMO VALUE FROM A RECORD                       *         
* USING THE DEMO ENGINE                                               *         
* ENGEXP = DEMO TO EVALUATE (MODIFIER + DEMO NUMBER)                  *         
*                                                                     *         
* EXIT - CC=EQ IF NO VALUE OR                                         *         
*        CC=NEQ WITH VALUE IN R1 & INPUT FIELD                        *         
***********************************************************************         
                                                                                
DEMGETE  NTR1  BASE=*,LABEL=*                                                   
         USING DEMCALCD,RC         RC=A(DEMCALC LOCAL W/S)                      
*                                                                               
         STAR  CLEAR=Y,ARS=ON                                                   
*                                                                               
         ICM   RF,15,DBCOMFCS                                                   
         L     R7,CT00AD0-COMFACSD(RF)    R7=DEMDISP                            
         LTR   R7,R7                                                            
         BZ    DEMGTEX                                                          
         LA    R7,16(R7)           GO PAST HEADER                               
                                                                                
         USING DSPHDRD,R7                                                       
DMGE10   CLC   0(2,R7),=XL2'00'                                                 
         BE    DEMGETEN                                                         
         CLC   SVFMS,DSPFILE       MATCH ON FILE/MEDIA/SOURCE                   
         BNE   DMGE15                                                           
         CLC   BOOK,DSPSBOOK       MATCH ON BOOK                                
         BNH   DMGE20                                                           
DMGE15   SR    R0,R0               TRY NEXT DEMDISP TABLE                       
         ICM   R0,7,DSPAET                                                      
         AR    R7,R0                                                            
         AHI   R7,1                                                             
         B     DMGE10                                                           
DMGE20   ST    R7,ADSPTBL          FOUND THE DEMDISP TABLE                      
         DROP  R7                                                               
************************************************************                    
* READ NEW FORMULAS FROM DATASPACE.                                             
************************************************************                    
DMGE40   L     RE,=A(DEMTABCL)     A(LIST OF TABLES IN PROGRAM)                 
         A     RE,RELO                                                          
         LAM   AR7,AR7,ALET                                                     
         ICM   R7,15,TABNFRM-DEMTABCL(RE)     RE=A(FILE/MEDIA TABLE)            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING DEMFFMSD,R7                                                      
*                                                                               
DMGE41   CLI   0(R2),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                BAD F/M/S/BOOK COMBINATION                   
         CLC   SVFMS,DEMFFFMS      CK ON FILE/MEDIA/SOURCE                      
         BNE   DMGE42                                                           
         CLC   BOOK,DEMFFBK        CK ON BOOK                                   
         BNL   DMGE44                                                           
DMGE42   LA    R7,DEMFFLQ(R7)                                                   
         B     DMGE41                                                           
         DROP  R7                  DONE WITH FORMULA KEY TABLE                  
                                                                                
DMGE44   LA    R7,DEMFFLQ+4(R7)    POINT TO FORMULAS                            
         USING DEMFFRMD,R7                                                      
         CPYA  ARE,AR7                                                          
         ICM   RE,15,DEMFGMAC                                                   
         BZ    *+6                                                              
         AR    RE,R7                                                            
         ST    RE,ADEMMAC          A(GENERAL MACROS)                            
         ICM   RE,15,DEMFSMAC                                                   
         BZ    *+6                                                              
         AR    RE,R7                                                            
         ST    RE,ADEMOVF          A(SPECIFIC MACROS)                           
         ICM   RE,15,DEMFFFOR                                                   
         AR    R7,RE               A(DEMO FORMULAS)                             
                                                                                
         USING DEMFMHDD,R7         SEARCH FOR MODIFIER IN FORM TABLE            
DMGE50   CLC   0(4,R7),=XL4'00'                                                 
         BE    DMGE200             MODFR NOT FOUND IN TABLE,TRY MACROS          
                                                                                
DMGE55   CLC   DEMFM1MD,ENGMODC    COMPARE ON MODIFIER FROM ENGEXP              
         BE    DMGE60                                                           
         ICM   RE,15,DEMFMLN                                                    
         AR    R7,RE                                                            
         B     DMGE50                                                           
                                                                                
DMGE60   ICM   RE,15,DEMFMAB3                                                   
         AR    RE,R7                                                            
         ST    RE,ABBLIST          SAVE A(BUILDING BLOCK LIST)                  
         ICM   RE,15,DEMFMAFT                                                   
         AR    R7,RE               A(DEMO FORMULAS, ONE DEMO AT A TIME)         
         USING DEMFDEMD,R7                                                      
DMGE65   CLI   0(R7),X'FF'                                                      
         BE    DMGE200             NO FORMULA FOR THIS DEMO. TRY MACROS         
         CLC   DEMFDNUM,ENGDEMO    COMPARE ON DEMO NUMBER FROM ENGEXP           
         BE    DMGE70                                                           
         SR    RE,RE                                                            
         ICM   RE,3,DEMFDLN                                                     
         AR    R7,RE                                                            
         B     DMGE65                                                           
*                                  COMPUTE THE DEMO VALUE                       
DMGE70   ST    R7,ACURFORM         SAVE A(CURRENT FORMULA)                      
         XC    NEGSUM,NEGSUM                                                    
         CPYA  AR5,AR7                                                          
         LA    R5,DEMFDILS         R5->INDEX TO COMPONENT DEMO                  
         DROP  R7                                                               
         SR    R1,R1               CLEAR RESULT VALUE                           
DMGE75   SR    RF,RF                                                            
         ICM   RF,3,0(R5)                                                       
         BP    *+6                                                              
         LCR   RF,RF               IF NEGATIVE, GET COMPLEMENT                  
                                                                                
         L     RE,ABBLIST                                                       
         LA    RE,DEMFB3DL-DEMFBL3D(RE)                                         
         BCTR  RF,0                                                             
         MHI   RF,L'DEMFB3DL                                                    
         AR    RE,RF               RE->COMPONENT DEMO NUMBER                    
                                                                                
         L     R2,ADSPTBL          FIND DEMO CATEGORY IN DEMDISP                
         SR    R0,R0                                                            
         ICM   R0,3,DSPLDE-DSPHDRD(R2)   LENGTH OF TABLE ENTRY                  
         LA    R2,DSPHDRLN(R2)     GO PAST HEADER                               
DMGE80   CLI   0(R2),X'FF'                                                      
         BE    DEMGETEN                                                         
         CLC   ENGMODC,0(R2)       MATCH ON MODIFIER                            
         BNE   DMGE90                                                           
         CLC   0(1,RE),1(R2)       MATCH ON DEMO CATEGORY                       
         BNE   DMGE90                                                           
         MVC   SVDEMOEL,2(R2)                                                   
         MVC   SVDEMINX,3(R2)                                                   
         B     DMGE95                                                           
                                                                                
DMGE90   AR    R2,R0                                                            
         B     DMGE80                                                           
                                                                                
DMGE95   LAM   ARE,ARE,=F'0'       CLEAR "ARE"                                  
         L     RE,DBAQUART                                                      
DMGE97   CLI   0(RE),0             END OF RECORD                                
         BE    DEMGETEN                                                         
         CLI   0(RE),X'23'         END OF MARKET BREAK                          
         BE    DEMGETEN                                                         
         CLI   0(RE),X'0F'         END OF TRACK                                 
         BE    DEMGETEN                                                         
         CLI   0(RE),X'5E'         END OF DEMOS                                 
         BE    DEMGETEN                                                         
         CLC   SVDEMOEL,0(RE)                                                   
         BE    DMGE100                                                          
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     DMGE97                                                           
                                                                                
DMGE100  XC    HALF,HALF                                                        
         MVC   HALF+1(1),2(RE)                                                  
         NI    HALF+1,X'0F'        LENGTH OF ONE DEMO CELL IN ELEM              
         ZIC   R0,SVDEMINX                                                      
         MH    R0,HALF                                                          
         LA    RF,3(RE)                                                         
         AR    RF,R0               RF->DEMO VALUE ON RECORD                     
                                                                                
         CLI   HALF+1,1                                                         
         BNE   *+8                                                              
         LA    RE,1                                                             
         CLI   HALF+1,2                                                         
         BNE   *+8                                                              
         LA    RE,3                                                             
         CLI   HALF+1,3                                                         
         BNE   *+8                                                              
         LA    RE,7                                                             
         CLI   HALF+1,4                                                         
         BNE   *+8                                                              
         LA    RE,15                                                            
         EX    RE,*+8                                                           
         B     *+8                                                              
         ICM   R0,0,0(RF)                                                       
                                                                                
         ICM   RF,3,0(R5)                                                       
         BP    DMGE110                                                          
         ICM   RE,15,NEGSUM        TO BE SUBTRACTED FROM FINAL RESULT           
         AR    RE,R0                                                            
         B     *+6                                                              
DMGE110  AR    R1,R0               POSITIVE VALUE,ADD TO FINAL RESULT           
                                                                                
         L     R7,ACURFORM                                                      
         CPYA  ARE,AR7                                                          
         SR    RE,RE                                                            
         ICM   RE,3,DEMFDLN-DEMFDEMD(R7)                                        
         AR    RE,R7               RE-> BYTE AFTER END OF FORMULA               
                                                                                
         LA    R5,L'DEMFDILS(R5)   ADVANCE TO NEXT COMPONENT INDEX              
         CR    R5,RE                                                            
         BNL   DMGE120             EXIT WITH VALUE IN R1                        
         B     DMGE75              GET NEXT COMPONENT'S VALUE                   
                                                                                
DMGE120  ICM   RE,15,NEGSUM                                                     
         LAM   ARE,ARE,=F'0'                                                    
         SR    R1,RE               SUBTRACT NEGATIVE INDECES                    
         B     DEMGETEY            RETURN RESULT OF FORMULA                     
                                                                                
DMGE200  STAR  CLEAR=Y,ARS=OFF                                                  
         BRAS  RE,DEMACROS         COMPUTE USING MACROS                         
         REAR  ARS=ON              REAR MACRO DOES NOT SET CC                   
         BNZ   DEMGETEY                                                         
         B     DEMGETEN                                                         
                                                                                
DEMGETEY REAR  ARS=OFF                                                          
         B     DEMGTEX                                                          
                                                                                
DEMGETEN REAR  ARS=OFF                                                          
         XR    R1,R1                                                            
                                                                                
DEMGTEX  LTR   R1,R1               SET CONDITION CODE                           
         XIT1  REGS=(R1)           RETURN TO CALLER W/VALUE IN R1               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RC                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COMPUTE A DEMO VALUE USING MACROS                        *         
* ENGEXP = DEMO TO EVALUATE (MODIFIER + DEMO NUMBER)                  *         
*                                                                     *         
* EXIT - CC=EQ IF NO VALUE OR                                         *         
*        CC=NEQ WITH VALUE IN R1 & INPUT FIELD                        *         
***********************************************************************         
                                                                                
DEMACROS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         STAR  CLEAR=Y,ARS=ON                                                   
         LAM   AR7,AR7,ALET                                                     
                                                                                
         ICM   R7,15,ADEMOVF       TRY SPECIFIC MACROS FIRST                    
         BZ    DEMM10                                                           
         USING DEMFSMCD,R7                                                      
DEMM01   CLI   0(R7),X'FF'                                                      
         BE    DEMM10              NOT FOUND.TRY GENERAL MACROS                 
         CLC   ENGMODC,DEMFSMD     MATCH ON MODIFIER                            
         BNE   *+14                                                             
         CLC   ENGDEMO,DEMFSDNO    AND DEMO CATEGORY                            
         BE    DEMM05                                                           
         LA    R7,DEMFSMLQ(R7)                                                  
         B     DEMM01                                                           
DEMM05   MVC   EXTOCOMP,DEMFSEXP   EXPRESSION TO COMPUTE                        
         DROP  R7                                                               
         B     DEMM25              GO COMPUTE IT                                
                                                                                
DEMM10   ICM   R7,15,ADEMMAC       TRY GENERAL MACROS NEXT                      
         BZ    DEMACN                                                           
         USING DEMFGMCD,R7                                                      
DEMM15   CLI   0(R7),X'FF'                                                      
         BE    DEMACN              NOT FOUND. EXIT WITH CC                      
         CLC   ENGMODC,DEMFGMD     MATCH ON MODIFIER                            
         BE    DEMM20                                                           
         LA    R7,DEMFGMLQ(R7)                                                  
         B     DEMM15                                                           
DEMM20   MVC   EXTOCOMP,DEMFGEXP   EXPRESSION TO COMPUTE                        
         DROP  R7                                                               
                                                                                
DEMM25   SR    R1,R1                                                            
         XC    EXRESULT,EXRESULT   CLEAR EXPRESSION RESULT                      
                                                                                
DEMM30   BRAS  RE,NXTOPRND         GET NEXT OPERAND/OPERATOR IN EXPRSN          
         OC    MOPERAND,MOPERAND                                                
         BZ    DEMACY              DONE COMPUTING EXPRESSION                    
                                                                                
         BRAS  RE,OPNDVALU         RETURNS VALUE OF OPERAND IN R1               
                                                                                
         CLI   MOPERTOR,0          FIRST OPERAND, JUST STORE IT                 
         BNE   DEMMMUL                                                          
         ST    R1,EXRESULT+4                                                    
         B     DEMM30                                                           
                                                                                
DEMMMUL  CLI   MOPERTOR,C'*'       PERFORM OPERATIONS                           
         BNE   DEMMDIV                                                          
         SR    RE,RE                                                            
         LM    RE,RF,EXRESULT                                                   
         MR    RE,R1                                                            
         STM   RE,RF,EXRESULT                                                   
         B     DEMM30                                                           
                                                                                
DEMMDIV  CLI   MOPERTOR,C'/'                                                    
         BNE   DEMMADD                                                          
         LTR   R1,R1               STOP DIVISION BY 0                           
         BNZ   *+14                                                             
         XC    EXRESULT,EXRESULT                                                
         B     DEMACY                                                           
         LM    RE,RF,EXRESULT                                                   
                                                                                
         ZIC   R0,ROUNDIND         ROUND TO NUM OF DIGITS IN ROUNDIND           
         LTR   R0,R0                                                            
         BZ    DEMDIV20                                                         
         LA    R5,1                                                             
DEMDIV10 MHI   R5,10                                                            
         BCT   R0,DEMDIV10                                                      
         MR    RE,R5               R1=DIVIDEND * POWER OF 10                    
                                                                                
DEMDIV20 DR    RE,R1               DIDVIDEND/DIVSOR                             
         MHI   RE,2                ROUND IT                                     
         CR    RE,R1                                                            
         BL    *+8                                                              
         LA    RF,1(RF)                                                         
                                                                                
DEMDIV30 CLI   ROUNDIND,0                                                       
         BE    DEMDIV40                                                         
         SR    RE,RE                                                            
         DR    RE,R5               RESULT/SAME POWER OF 10                      
         MHI   RE,2                ROUND IT                                     
         CR    RE,R5                                                            
         BL    *+8                                                              
         LA    RF,1(RF)                                                         
DEMDIV40 SR    RE,RE                                                            
         STM   RE,RF,EXRESULT                                                   
         MVI   ROUNDIND,0          RESET TO DEFAULT AFTER ROUNDING              
         B     DEMM30                                                           
                                                                                
DEMMADD  CLI   MOPERTOR,C'+'                                                    
         BNE   DEMMSUB                                                          
         LM    RE,RF,EXRESULT                                                   
         AR    RF,R1                                                            
         SR    RE,RE                                                            
         STM   RE,RF,EXRESULT                                                   
         B     DEMM30                                                           
                                                                                
DEMMSUB  CLI   MOPERTOR,C'-'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LM    RE,RF,EXRESULT                                                   
         SR    RF,R1                                                            
         BP    *+6                                                              
         SR    RF,RF               DON'T ALLOW NEGATIVE RESULT                  
         SR    RE,RE                                                            
         STM   RE,RF,EXRESULT                                                   
         B     DEMM30                                                           
                                                                                
DEMACY   LAM   AR7,AR7,=F'0'                                                    
         REAR  ARS=OFF                                                          
         L     R1,EXRESULT+4       GET RESULT VALUE IN R1                       
         B     DEMACX                                                           
                                                                                
DEMACN   LAM   AR7,AR7,=F'0'                                                    
         REAR  ARS=OFF                                                          
         XR    R1,R1                                                            
                                                                                
DEMACX   LTR   R1,R1               SET CONDITION CODE                           
         XIT1  REGS=(R1)           RETURN TO CALLER W/VALUE IN R1               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE NEXT OPERAND IN EXTOCOMP                         *         
* IN   : EXTOCOMP IS THE EXPRESSION STILL LEFT TO COMPUTE             *         
*                                                                     *         
* OUT  : MOPERAND IS THE NEXT OPERAND                                 *         
*        MOPERTOR IS THE NEXT OPERATOR (PRECEEDING THE OPERAND)       *         
*        EXTOCOMP GETS UPDATED TO EXCLUDE THIS OPERAND AND OPERATOR   *         
***********************************************************************         
                                                                                
NXTOPRND NTR1  BASE=*,LABEL=*                                                   
         XC    MOPERAND,MOPERAND                                                
         XC    MOPERTOR,MOPERTOR                                                
                                                                                
         LA    RE,EXTOCOMP                                                      
                                                                                
         CLI   0(RE),C' '          BLANK EXPRESSION, EXIT                       
         BE    NXTOPX                                                           
         CLI   0(RE),0                                                          
         BE    NXTOPX                                                           
                                                                                
         CLI   0(RE),C'*'          FIRST OPERATOR IS THE ONE TO USE             
         BE    NXOP10                                                           
         CLI   0(RE),C'/'                                                       
         BE    NXOP10                                                           
         CLI   0(RE),C'+'                                                       
         BE    NXOP10                                                           
         CLI   0(RE),C'-'                                                       
         BE    NXOP10                                                           
         B     NXOP15                                                           
NXOP10   MVC   MOPERTOR,0(RE)                                                   
         LA    RE,1(RE)                                                         
                                                                                
NXOP15   LA    RF,MOPERAND                                                      
NXOP20   CLI   0(RE),C' '          END OF EXPRESSION                            
         BE    NXOP50                                                           
         CLI   0(RE),0                                                          
         BE    NXOP50                                                           
         CLI   0(RE),C'*'          SECOND OPERATOR IS END OF OPERAND            
         BE    NXOP50                                                           
         CLI   0(RE),C'/'                                                       
         BE    NXOP50                                                           
         CLI   0(RE),C'+'                                                       
         BE    NXOP50                                                           
         CLI   0(RE),C'-'                                                       
         BE    NXOP50                                                           
         MVC   0(1,RF),0(RE)       BUILD OPERAND ONE CHAR AT A TIME             
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     NXOP20                                                           
                                                                                
NXOP50   XC    WORK(L'EXTOCOMP),WORK    UPDATE EXTOCOMP AND EXIT                
         LA    RF,WORK                                                          
NXOP55   CLI   0(RE),C' '                                                       
         BE    NXOP60                                                           
         CLI   0(RE),0                                                          
         BE    NXOP60                                                           
         MVC   0(1,RF),0(RE)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     NXOP55                                                           
NXOP60   MVC   EXTOCOMP,WORK       UPDATE EXPRESSION TO COMPUTE                 
*                                                                               
NXTOPX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET THE VALUE OF AN OPERAND IN R1                        *         
* IN   : MOPERAND                                                     *         
* OUT  : R1 = VALUE OF OPERAND                                        *         
***********************************************************************         
                                                                                
OPNDVALU NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVC   SVEXP,ENGEXP        SAVE ORIGINAL DEMO EXPRESSION                
                                                                                
         MVI   ROUNDIND,0          DEFAULT IS NO DECIMALS                       
         CLC   =C'ROUND',MOPERAND  HANDLE ROUND FUNCTION                        
         BNE   OPV05                                                            
         LA    RE,MOPERAND                                                      
         XC    WORK(L'MOPERAND),WORK                                            
         LA    RF,WORK                                                          
         SR    R0,R0                                                            
OPV01    CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                INVALID ROUND FUNCTION                       
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         DC    H'0'                INVALID ROUND FUNCTION                       
         CLI   0(RE),C'('                                                       
         BNE   *+12                                                             
         LA    R0,1                PARENS FOUND.NEXT CHAR WILL BE MODFR         
         B     OPV03                                                            
         CLI   0(RE),C','                                                       
         BE    OPV04                                                            
         CHI   R0,1                                                             
         BE    OPV02                                                            
         B     OPV03                                                            
OPV02    MVC   0(1,RF),0(RE)       MODIFIER FOUND, COPY IT                      
         LA    RF,1(RF)                                                         
OPV03    LA    RE,1(RE)                                                         
         B     OPV01                                                            
                                                                                
OPV04    MVC   ROUNDIND,1(RE)      ROUNDING TO THIS MANY DIGITS                 
         NI    ROUNDIND,X'0F'      MAKE IT BINARY                               
         XC    MOPERAND,MOPERAND                                                
         MVC   MOPERAND,WORK                                                    
                                                                                
OPV05    TM    MOPERAND,X'F0'      OPERAND IS A CONSTANT                        
         BNO   OPV30                                                            
         LA    RE,MOPERAND                                                      
         SR    R5,R5                                                            
OPV10    CLI   0(RE),0             GET LENTH OF OPERAND IN R5                   
         BE    OPV20                                                            
         CLI   0(RE),C' '                                                       
         BE    OPV20                                                            
         CLI   0(RE),C'.'          DECIMAL POINT?                               
         BE    OPV20               YES. JUST USE THE INTEGER CONSTANT           
         AHI   R5,1                                                             
         LA    RE,1(RE)                                                         
         B     OPV10                                                            
OPV20    BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,MOPERAND(0)                                                  
         CVB   R1,DUB                                                           
         B     OPVX                RETURN VALUE IN R1                           
                                                                                
OPV30    LA    RE,MOPERAND         MUST BE A MODIFIER                           
         LA    RF,ENGEXP                                                        
OPV35    CLI   0(RE),0             CK IF MODIFIER IS FOLLOWED BY DEMO#          
         BE    OPV50                                                            
         CLI   0(RE),C' '                                                       
         BE    OPV50               MODFR NOT FOLLOWED BY DEMO#.USE ORIG         
         CLI   0(RE),C'#'                                                       
         BE    OPV40                                                            
         MVC   0(1,RF),0(RE)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     OPV35                                                            
                                                                                
OPV40    LA    RE,1(RE)            SKIP C'#'                                    
         LR    RF,RE               MODFR IS FOLLOWED BY DEMO#. USE IT           
         SR    R5,R5               GET LENGTH OF DEMO#                          
OPV43    CLI   0(RE),0                                                          
         BE    OPV45                                                            
         CLI   0(RE),C' '                                                       
         BE    OPV45                                                            
         AHI   R5,1                                                             
         LA    RE,1(RE)                                                         
         B     OPV43                                                            
*                                                                               
OPV45    BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RF)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,ENGDEMO        STORE DEMO#                                  
                                                                                
OPV50    BRAS  RE,DEMGETE          R1=VALUE OF DEMO EXP IN ENGEXP               
                                                                                
OPV70    MVC   ENGEXP,SVEXP        RESTORE ORIGINAL DEMO EXPRESSION             
                                                                                
OPVX     LTR   R1,R1               SET CONDITION CODE                           
         XIT1  REGS=(R1)           RETURN TO CALLER W/VALUE IN R1               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
         TITLE 'DEMOUT - INCLUDED BOOKS'                                        
***********************************************************************         
* OTHER INCLUDED BOOKS                                                *         
***********************************************************************         
         SPACE 1                                                                
* DEARBCMKTS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEARBCMKTS                                                     
         EJECT                                                                  
         PRINT ON                                                               
* DEBIRCMKTS (NO LONGER ++INCLUDED)                                             
         PRINT OFF                                                              
*******++INCLUDE DEBIRCMKTS                                                     
         EJECT                                                                  
         PRINT ON                                                               
*DENADUNIV                                                                      
         PRINT OFF                                                              
       ++INCLUDE DENADUNIV                                                      
         EJECT                                                                  
         PRINT ON                                                               
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
         PRINT ON                                                               
* DEDBEXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEDBEXTRAD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* DEDEMEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMEQUS                                                      
         EJECT                                                                  
         PRINT ON                                                               
* DEDEMEQUS2                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEDEMEQUS2                                                     
         EJECT                                                                  
         PRINT ON                                                               
* DDMONYREQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMONYREQU                                                     
         EJECT                                                                  
         PRINT ON                                                               
* DEDEMTABD                                                                     
*********PRINT OFF                                                              
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
         PRINT ON                                                               
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         EJECT                                                                  
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         EJECT                                                                  
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         EJECT                                                                  
         PRINT ON                                                               
* DEFSTDSPD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEFSTDSPD                                                      
         EJECT                                                                  
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL DSECTS - DEMOUTD                                              *         
***********************************************************************         
         SPACE 1                                                                
DEMOUTD  DSECT                     ** DEMOUT GLOBAL W/S **                      
RELO     DS    F                                                                
ALET     DS    F                                                                
DUB      DS    D                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
DMCB     DS    6F                                                               
DMCB2    DS    10F                                                              
WORK     DS    CL80                                                             
APARM    DS    A                   A(CALLING PARAMETER LIST)                    
DEMOUTRD DS    A                   FOR DEMCALC ERROR EXIT TO DEMOUT             
ASSB     DS    A                   A(ONLINE SSB)                                
ADEMOLST DS    A                   A(CALLER'S DEMO LIST)                        
NUMDEMOS DS    F                   # OF DEMOS IN INPUT DEMO LIST                
TVQADDR  DS    F                   A(TVQ QUARTER HOUR)                          
SVDBQRT  DS    F                   SAVE AREA FOR DBAQUART                       
SVDBFIL  DS    CL3                 SAVE AREA FOR DBFILE                         
SVDBABK  DS    CL2                 SAVE AREA FOR DBACTBK                        
SVDBSBK  DS    CL2                 SAVE AREA FOR DBSELBK                        
SVDBINTF DS    CL2                 SAVE AREA FOR DBINTFIL                       
SVMODIFY DS    CL1                 SAVE AREA FOR DEMO MODIFIER                  
SVGAA    DS    CL1                 SAVE AREA FOR GAA SW                         
SVRAWV   DS    F                   SAVE AREA FOR RAW VALUE(TVQ)                 
SAVETVQ  DS    C                   SAVE AREA FOR TVQ CAT CODE                   
AENDFTAB DS    A                   A(END OF FUSION TABLE IN DATASPACE)          
AFSTFTAB DS    A                   A(FIRST ENTRY OF FUSION TAB)                 
ADSPTBL  DS    A                   A(DEMDISP TABLE FOR F/M/S/BOOK)              
ABBLIST  DS    A                   A(BUILDING BLOCK LIST FROM FORM TBL)         
ACURFORM DS    A                   A(CURRENT FORMULA)                           
ADEMMAC  DS    A                   A(GENERAL MACROS TABLE)                      
ADEMOVF  DS    A                   A(SPECIFIC MACROS TABLE)                     
ADEMOCON DS    A                   A(DEMOCON)                                   
*                                                                               
         DS    0V                                                               
APLDTABS DS    0XL(6*4)            6 ADDRESSES                                  
APLMODTB DS    V                   A(MODIFIER TABLE)                            
APLPLDTB DS    V                   A(PERSONAL LANG. ATTRIBUTE TABLE)            
APLENCTB DS    V                   A(MODIFIER BYTE ENCODING TABLE)              
APLPRNTB DS    V                   A(PERSONAL LANG. PRINTABLE DESCRIPS)         
         DS    V                   SPARE                                        
         DS    V                   SPARE                                        
*                                                                               
SVFMS    DS    CL3                 FILE/MEDIA/SOURCE TO LOOK-UP                 
SVDEMOEL DS    X                   SAVED DEMO ELEMENT FROM DEMDISP              
SVDEMINX DS    AL1                 SAVED DEMO INDEX FROM DEMDISP                
NEGSUM   DS    AL4                 ADD VALUES TO BE SUBTRACTED HERE             
ENGEXP   DS    0XL(L'DEMOEXP)      DEMO EXP FOR DEMO ENGINE                     
ENGMODC  DS    CL(L'DEMMODC)       MODIFIER                                     
ENGDEMO  DS    XL(L'DEMDEMO)       DEMO NUMBER TYPE4 (2 BYTE)                   
SVEXP    DS    XL(L'ENGEXP)        SAVED DEMO EXPRESSION                        
EXTOCOMP DS    CL(L'DEMFGEXP)                                                   
MOPERAND DS    CL20                NEXT MACRO OPERAND                           
MOPERTOR DS    C                   NEXT MACRO OPERATOR (*,/,+,-)                
EXRESULT DS    D                   RESULT OF MACRO EXPRESSION                   
ROUNDIND DS    HL1                 NO OF DIGITS TO ROUND TO                     
*                                                                               
AMSTTVQ  DS    F                   TVQ AMSTHDR                                  
SVMSTHDR DS    F                   SAVE AMSTHDR                                 
SVBKEF   DS    X                   SAVE EFFECTIVE BOOK SEQUENCE NUMBER          
SVBKNO   DS    X                   SAVE BOOK SEQUENCE NUMBER                    
BKNOTVQ  DS    X                   TVQ BOOK SEQUENCE NUMBER                     
BKEFTVQ  DS    X                   TVQ EFFECTIVE BOOK SEQUENCE NUMBER           
PREVOVR  DS    C                   PREV DEMO WAS AN OVERRIDE                    
TVQID    DS    CL3                 ID FOR TVQ/OPI                               
*                                                                               
DUMMYLST DS    CL5                 FOR SINGLE ENTRY DEMO LIST                   
TABLISTW DS    XL(TABLISTL)        DEMADDR WORK                                 
DEMOFLAG DS    X                   GLOBAL LOOKUP FLAG                           
OVERPRES EQU   X'80'               OVERRIDE ELEMENT(S) PRESENT                  
INDXPRES EQU   X'40'               DEMO NDX ELEMENT(S) PRESENT                  
BOOKPRES EQU   X'20'               BOOK ELEMENT FOUND                           
DEMOPRES EQU   X'10'               ADEMOEL LIST BUILT                           
MINVPRES EQU   X'08'               MINIMUM VALUE FOR UPGRADE PRESENT            
EVENPRES EQU   X'04'               EVEN ELEMENTS USED FOR DEMOS                 
FASTPRES EQU   X'02'               FAST DEMO CALCS PRESENT                      
LISTTYPE DS    C                   TYPE OF DEMO LIST PASSED -                   
LISTPREC EQU   C'P'                PRECISION ADJUSTMENT LIST                    
LISTMAST EQU   C'M'                MASTER DISPLACEMENT TABLE LIST               
LISTMULT EQU   C'L'                MULITIPLE DEMO LIST                          
LISTMULP EQU   C'N'                MULITIPLE DEMO LIST W/ PLD ARRAY             
LISTDEMO EQU   C'D'                SINGLE DEMO LOOKUP                           
LISTORIG EQU   C'*'                AS LISTMAST - ORIGIN MAP REQUESTED           
LISTOPTI EQU   C'O'                AS LISTMAST - ORIGIN MAP PASSED              
CALC2TRU DS    XL1                 CALC TO 2 DEC PLACES & TRUNCATE FLGS         
C2TRTG   EQU    X'80'               CALC RTG TO 2 PLACES & TRUNCATE             
C2TSHR   EQU    X'40'               CALC SHR TO 2         "                     
C2TPUT   EQU    X'20'               CALC PUT TO 2         "                     
C2TYES   EQU    X'08'               CALC CURR DEMO TO 2 PLACE & TRUNC           
C2T2DEC  EQU    X'04'               CALC'D CURR DEMO TO 2 PLACES                
C2TDONE  EQU    X'02'               CALC'D CURR DEMO TO 2 PL. & TRUNC'D         
*                                                                               
CBLUDSP  EQU   150                 DISP TO CBL UNIVS IN DBSPANAD                
CHNUDSP  EQU   320                 DISP TO HISP CABLE UNIVS (150+170)           
NAD170Q  EQU   170                 EVN: COREDEMO CTGY=USA*DD OVR INDEX          
NADTVQQ  EQU   171                 TVQ: USER DEFINED FILE                       
NADOPIQ  EQU   172                 OPI: USER DEFINED FILE                       
NADIGOQ  EQU   175                 IAG: USER DEFINED FILE (ORIGINAL)            
NADIGRQ  EQU   176                 IAG: USER DEFINED FILE (REPEAT)              
TCAR181  EQU   181                 TCAR LOOK UP PREFIX 181                      
TCAR189  EQU   189                 TCAR LOOK UP PREFIX 189                      
*                                   (WB1=181-185 TCAR=185-189)                  
*                                                                               
TVQBOOK  DS    XL2                 TVQBOOK (BINARY YYMM)                        
BOOK     DS    XL2                 BOOK (BINARY YYMM)                           
BKNO     DS    X                   BOOK SEQUENCE NUMBER                         
BKEF     DS    X                   EFFECTIVE BOOK SEQUENCE NUMBER               
OVERELEM DS    X                   OVERRIDE DEMO ELEMENT CODE                   
BOOKELEM DS    X                   BOOK ELEMENT CODE                            
QTHRELEM DS    X                   QUARTER HOUR DELIMITER ELEMENT               
NEIDELEM DS    X                   NETWORK FILE ID ELEMENT                      
FCOVELEM DS    X                   FULL COVERAGE ELEMENT                        
GAASW    DS    C                   GAA DEMO LOOKUP                              
UNCOUNTM EQU   22                  MAXIMUM RECURSION LEVEL                      
UNCOUNT  DS    X                   CURRENT RECURSION LEVEL                      
UNKNOWNS DS    (UNCOUNTM)XL3       LIST OF UNRESOLVED DEMOS                     
ANEIDELM DS    F                   SAVE A(NETWORK 41 ELEMENT)                   
LASTNAD  DS    C                   LAST NAD CATEGORY PROCESSED                  
ZAPZERO  DS    C                   SUPPRESS CALCS. W/ZERO RECORD VALS.          
ZEROVAL  DS    C                   CURR CALC HAS ZERO VALUES                    
MININV   DS    C                   INV RECD AND MIN VALUE PRESENT               
IPREC    DS    C                   INPUT PRECISION                              
OPREC    DS    C                   OUTPUT PRECISION                             
PRECSAV  DS    C                   PREC ADJUST SAVE AREA                        
RTGADJ   DS    C                   VARIABLE RATING FLAG                         
BASEMOD  DS    C                   CURR. BASE MODIFIER FOR VAR.CALC             
SPNETFLG DS    X                   INDIC NET RECD BUILT FOR SPOT                
SPNHTQ   EQU   X'01'               NHT FILE                                     
SPNADQ   EQU   X'02'               NAD                                          
SPNTIQ   EQU   X'04'               NTI                                          
SPCBLQ   EQU   X'10'               CABLE                                        
SPNETMST DS    CL3                 MASTER DISP TABLE TO USE                     
DEM2BYT  DS    XL2                 2 BYTE DEMO#                                 
DEM3BYT  DS    XL3                 3 BYTE DEMO#                                 
SVINTSM  DS    CL2                 DBINTSRC/DBINTMED ON ENTRY                   
SVACTBK  DS    CL2                 SAVED DBACTBK                                
SVACTSRC DS    CL2                 SAVED DBACTSRC                               
DUMSECT  DS    CL8                                                              
NETWPREC DS    CL16                                                             
SYSOPT   DS    A                   A(SYSOPT LIST)                               
PRECLIST DS    A                   OVERRIDE PRECISION LIST                      
AMSTHDR  DS    A                   A(START OF MASTER DEMO TABLE)                
ARECORD  DS    A                   A(DEMO RECORD)                               
AQTHREL  DS    A                   A(FIRST DEMO ELEMENT)                        
AOVEREL  DS    A                   A(FIRST OVERRIDE ELEMENT)                    
AINDXEL  DS    A                   A(FIRST DEMO NDX ELEMENT)                    
AUPGREL  DS    A                   A(1ST DEMO UPGRADE ELEM W/MIN VAL)           
ADEMOEL  DS    24A                 A(DEMO ELEMENTS) X'30' THRU X'5F'            
ADEMOELL EQU   *-ADEMOEL                                                        
MAPARAM  DS    4F                                                               
MADEMTYP DS    C                   SAVE FOR DEMO TYPE                           
MEANAGEI DS    XL61                MEAN AGE INPUT DEMOS 20*3+1                  
MEANAGEO DS    20F                 MEAN AGE VALUES                              
DENGINE  DS    X                   DEMO ENGINE OPTION                           
DENGOFFQ EQU   X'00'                 OFF                                        
DENGONQ  EQU   X'01'                 ON                                         
MYREC    DS    XL2000              TEMP STORAGE AREA FOR DEMO RECORD            
SVELEMTB DS    XL2000                                                           
DEMOUTL  EQU   *-DEMOUTD                                                        
         EJECT                                                                  
***********************************************************************         
* LOCAL DSECTS - DEMCALCD                                             *         
***********************************************************************         
         SPACE 1                                                                
DEMCALCD DSECT                     ** DEMCALC S/R LOCAL W/S **                  
DEMODUB  DS    D                   DEMCALC WORK AREA                            
DEMODUB2 DS    D                   DITTO ABOVE                                  
DEMAOUT  DS    A                   A(OUTPUT VALUE)                              
FULL2    DS    F                                                                
OVREXP   DS    XL2                 REAL DEMO FOR OVERRIDES                      
OVRALI   DS    XL2                 ALIAS DEMO FOR OVERRIDES                     
OVRALI2  DS    XL2                 ALIAS 2 DEMO FOR OVERRIDES                   
DEMOEXP  DS    0XL3                DEMO MODIFIER/NUMBER                         
DEMMODC  DS    C                   REQUIRED DEMO (MODIFIER/NUMBER)              
DEMDEMO  DS    XL2                 DEMO NUMBER                                  
DEMPREC  DS    X                   REQUIRED OUTPUT FIELD PRECISION              
DEMMODQ  DS    C                   REAL DEMO MODIFIER (IF EQUATED DEMO)         
EQUELCD  DS    C                   EQUATED ELEMENT CODE                         
SAVPREC  DS    X                   SAVED FIELD PRECISION                        
SAVELCD  DS    X                   SAVED ELEMENT CODE                           
SAVFLDN  DS    X                   SAVED FIELD NUMBER                           
SAVBKNO  DS    X                   SAVED BOOK NUMBER                            
SAVMODC  DS    X                   SAVED MODIFIER CODE                          
SAVINDS  DS    X                   SAVED INDICATORS                             
DEMIPRC  DS    X                   INPUT PRECISION                              
DEMOPRC  DS    X                   OUTPUT PRECISION                             
ADJIPRC  DS    X                   DEMADJP S/R INPUT PRECISION                  
ADJOPRC  DS    X                   DEMADJP S/R OUTPUT PRECISION                 
DEMDTAW  DS    XL(MSTDTALN)        DEMO DEFINITION (FROM DEMO TABLE)            
DEMVALS  DS    44F                 FORMULA VALUES & CALCULATION STACK           
DEMCALCL EQU   *-DEMCALCD                                                       
*** FORCE AN ASSEMBLY ERROR IF DEMCALCD GROWS LARGER THAN 256 BYTES.            
*** IT'S OKAY IF THAT HAPPENS, BUT THEN DEMCALCD MUST BE CLEARED USING          
*** MULTIPLE MVC INSTRUCTIONS, OR ELSE AN MVCL.                                 
         DS    (256-DEMCALCL)X     SEE COMMENT JUST ABOVE                       
*                                                                               
         SPACE 1                                                                
GETOWRKD DSECT                     ** DEMGETO S/R LOCAL W/S **                  
DMCB3    DS    10F                                                              
FULL3    DS    F                                                                
SAV_EF01 DS    4F                  SAVED REGISTERS (RE, RF, R0, R1)             
GETOWRKL EQU   *-GETOWRKD                                                       
         SPACE 3                                                                
DEMOUT   RSECT                                                                  
         ORG   DEMOUT+(((*-DEMOUT)/1024)+1)*1024                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'209DEDEMOUT  05/26/20'                                      
         END                                                                    
