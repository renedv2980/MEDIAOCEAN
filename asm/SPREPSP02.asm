*          DATA SET SPREPSP02  AT LEVEL 053 AS OF 05/01/02                      
*PHASE SPSP02T,*,NOAUTO                                                         
*INCLUDE HEXOUT                                                                 
         SPACE 1                                                                
* QOPT1 = Y MEANS DELETE THE RECORDS                                            
* QOPT5 = Y MEANS THAT YOU MAY DELETE STATIONS WHEN THERE IS NO                 
*  ACTIVITY FOR THE ENTIRE REQUEST, OTHERWISE THE REQUEST IS ILLEGAL            
*  LEV 32    FEB08/89 FIX MSPACK AT SP710 CHANGE FROM SMKT TO =C'0000'          
*  LEV 33-47 FEB13/89 BY UNKNOWN FOR UNKNOWN                                    
*  LEV 48    JUL10/92 ACCOMADATE NUMERIC CLIENT CODES                           
*  LEV 49    AUG18/92 ENLARGE STATION TABLE                                     
*  LEV 50    JUL06/94 ADD STRAFFIC                                              
*  LEV 51    OCT04/94 BYPASS BLANK STATIONS                                     
*  LEV 52    MAR04/96 PACK MKT # AFTER CHECKING IF MKT REC                      
         TITLE 'STATION FILE PURGE PROGRAM - APPLICATION'                       
SPSP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSP02,R4                                                      
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         STM   R9,RB,SPSPR9                                                     
         MVI   RCSUBPRG,1                                                       
         SPACE 2                                                                
* CONTROL SECTION *                                                             
         SPACE 1                                                                
         CLI   MODE,PROCBUY                                                     
         BE    SP300               CHECK FOR STATION ACTIVITY                   
         BH    CNTRL10             DO 'LAST' BREAKS                             
         SPACE 1                                                                
* 'FRST' BREAKS *                                                               
         SPACE 1                                                                
         CLI   MODE,STAFRST                                                     
         BE    SP200               ADD STATION TO STATION TABLE                 
         CLI   MODE,REQFRST                                                     
         BE    SP100               INITIALIZE TABLES                            
         B     EXIT                                                             
         SPACE 1                                                                
* 'LAST' BREAKS *                                                               
         SPACE 1                                                                
CNTRL10  DS    0H                                                               
         CLI   MODE,PROCGOAL                                                    
         BE    SP400               FLAG ALL MKTS WITH GOALS AS ACTIVE           
         CLI   MODE,REQLAST                                                     
         BE    SP500               DELETE INACTIVE STATIONS AND PRINT           
*                                   LIST                                        
EXIT     XIT1                      EXIT                                         
         EJECT                                                                  
* SP100 - AT REQFRST, INITIALIZE TABLES AND COUNTERS *                          
         SPACE 1                                                                
SP100    DC    0H'0'                                                            
         MVI   ERRCD,1                                                          
         CLC   =C'ALL',QCLT                                                     
         BNE   BADREQ                                                           
         MVI   ERRCD,2                                                          
         CLC   =C'ALL',QPRD                                                     
         BNE   BADREQ                                                           
         MVI   ERRCD,3                                                          
         CLC   =C'ALL',QMKT                                                     
         BNE   BADREQ                                                           
         MVI   ERRCD,4                                                          
         CLC   =C'ALL',QSTA                                                     
         BNE   BADREQ                                                           
         MVI   ERRCD,5                                                          
         CLC   =C'NO',QEST                                                      
         BE    BADREQ                                                           
         SPACE 1                                                                
         RELOC RELO                                                             
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         SPACE                                                                  
*                                  INITIALIZE THE STATION TABLE                 
         L     RE,=A(STATBL)                                                    
         A     RE,RELO                                                          
         ST    RE,ASTATBL                                                       
         L     RF,=A(L'STATBL)                                                  
         XCEF                                                                   
         SPACE                                                                  
         L     RF,=A(MKTTBL)                                                    
         A     RF,RELO                                                          
         ST    RF,AMKTTBL                                                       
         LA    R0,120                                                           
         XC    0(250,RF),0(RF)     INITIALIZE THE MARKET TABLE                  
         LA    RF,250(RF)                                                       
         BCT   R0,*-10                                                          
         SPACE 1                                                                
         MVC   PSTA2,ASTATBL       POINT PSTA2 TO THE STATION TABLE             
         XC    PSTA3(12),PSTA3     NUMBER OF STATION RECORDS IS 0               
         MVI   PSTA4+3,7           LENGTH OF STATION RECORD IS 7                
         MVI   PSTA5+3,6           LENGTH OF KEY IS 6                           
         MVC   PSTA6,=F'20000'     MAXIMUM OF 20,000 STATIONS                   
         SPACE 1                                                                
         LA    R0,9                                                             
         LA    RF,ADDRCTR                                                       
         ZAP   0(8,RF),=P'0'                                                    
         LA    RF,8(RF)                                                         
         BCT   R0,*-10                                                          
         B     EXIT                EXIT                                         
         EJECT                                                                  
* SP200 - AT STAFRST, ADD A STATION TO STATBL & MARK THE MKT ACTIVE *           
         SPACE                                                                  
SP200    DC    0H'0'                                                            
         L     R6,ADSTAT                                                        
         USING STARECD,R6                                                       
         XC    BINREC,BINREC                                                    
         GOTO1 MSPACK,DMCB,SMKT,STAKCALL,DUB                                    
         MVC   BINSTA,DUB+2        PACKED STATION                               
         CLC   =C'000',STAKCLT                                                  
         BE    SP210                                                            
         MVC   BINCLT,STAKCLT                                                   
SP210    DS    0H                                                               
         GOTO1 BINSRCH,PSTA,(1,BINREC)                                          
         LH    R7,DUB              GET MARKET NUMBER (=DISPLACEMENT)            
         A     R7,AMKTTBL                                                       
         MVI   0(R7),1             SET MARKET ACTIVITY FLAG                     
         MVC   TBLENGTH,PSTA3                                                   
         B     EXIT                EXIT                                         
         SPACE 2                                                                
* SP300 - AT PROCBUY, SET THE STATION ACTIVITY SWITCHES *                       
         SPACE 1                                                                
SP300    DS    0H                                                               
         LA    R8,KEY                                                           
         USING BUYRECD,R8                                                       
         L     RF,PSTA1                                                         
         MVC   BINREC,0(RF)                                                     
         MVC   DUB(3),BUYMSTA+2                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,DUB+3                                        
         MVI   DUB+6,0                                                          
         CLC   BINREC(6),DUB       DO WE ALREADY HAVE THE STATION?              
         BE    SP310                YES.                                        
         MVC   BINREC,DUB                                                       
         GOTO1 BINSRCH,PSTA,(2,BINREC)                                          
         L     RF,PSTA1                                                         
         CLC   0(6,RF),BINREC      IS THIS THE RIGHT STATION?                   
         BNE   SP320                NO.                                         
SP310    DS    0H                                                               
         MVI   6(RF),1             SET THE STATION ACTIVITY FLAG                
         MVI   BUYSW,C'Y'                                                       
SP320    DS    0H                                                               
         CLC   BINCLT,=H'0'                                                     
         BE    SP330                                                            
         XC    BINCLT,BINCLT                                                    
         GOTO1 BINSRCH,PSTA,(2,BINREC)                                          
         L     RF,PSTA1                                                         
         CLC   0(6,RF),BINREC      IS THIS THE RIGHT STATION?                   
         BNE   SP330                NO.                                         
         MVI   6(RF),1             SET THE STATION ACTIVITY FLAG                
         MVI   BUYSW,C'Y'                                                       
* SET MARKET ACTIVITY SWITCH ALSO                                               
         XR    R7,R7                                                            
         ICM   R7,3,BUYMSTA                                                     
         A     R7,AMKTTBL                                                       
         CLI   0(R7),1                                                          
         BE    SP330                                                            
         MVI   0(R7),1                                                          
******* PRINT OUT KEYS FOR DEBUGGING PURPOSES                                   
*******  LA    R5,P                                                             
*******  GOTO1 HEXOUT,DMCB,KEY,(R5),18,=C'TOG'                                  
*******  MVI   SPACING,3                                                        
*******  GOTO1 REPORT                                                           
*                                                                               
SP330    DS    0H                                                               
         B     EXIT                EXIT                                         
         EJECT                                                                  
* SP400 - AT PROCGOAL, FLAG ALL MARKETS WITH GOALS AS ACTIVE *                  
         SPACE 1                                                                
SP400    DS    0H                                                               
         XR    R7,R7                                                            
         ICM   R7,3,KEY+5          GET THE MARKET NUMBER                        
         A     R7,AMKTTBL                                                       
         MVI   0(R7),1             SET THE MARKET ACTIVITY FLAG                 
         B     EXIT                                                             
         SPACE 2                                                                
* SP500 - AT REQLAST, READS TRAFFIC BUY RECORDS & SETS ACTIVE SWITCH            
         SPACE 1                                                                
SP500    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AB2'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVI   KEY+5,X'FF'                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'TRFDIR',KEYSAVE,KEY                   
*                                                                               
SP501    CLC   KEY(3),KEYSAVE      ANY MORE TRAFFIC BUY RECS                    
         BNE   SP505                                                            
*                                                                               
         L     R2,TBLENGTH                                                      
         L     R3,ASTATBL                                                       
*                                                                               
SP502    CLC   KEY+8(3),0(R3)      STATION KEY IN TABLE?                        
         BE    SP503                                                            
         LA    R3,7(,R3)           NEXT STATION IN TABLE                        
         BCT   R2,SP502                                                         
         DC    H'0'                STATION NOT IN TABLE - MUST BE               
*                                                                               
SP503    MVI   6(R3),1             MAKE STATION ACTIVE                          
         MVI   BUYSW,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'TRFDIR',KEYSAVE,KEY                   
         B     SP501                                                            
*                                                                               
         SPACE 2                                                                
* SP505 - PRINT ACTIVITY LISTING & DELETE INACTIVE STATIONS                     
         SPACE 1                                                                
SP505    CLI   BUYSW,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,NOACTIV          NO STATIONS ARE ACTIVE                       
         L     R2,TBLENGTH                                                      
         L     R3,ASTATBL                                                       
SP510    DS    0H                                                               
         LA    R0,10                                                            
         LA    R5,P                                                             
SP520    DS    0H                                                               
         MVC   9(3,R5),3(R3)                                                    
         MVC   WORK(2),=H'0'                                                    
         MVC   WORK+2(3),0(R3)                                                  
         GOTO1 MSUNPK,DMCB,WORK,DOUBLE,DUB                                      
         MVC   1(4,R5),DUB                                                      
         MVI   5(R5),C'-'                                                       
         MVC   6(1,R5),DUB+4                                                    
         MVI   7(R5),C'M'                                                       
         CLI   QMED,C'T'                                                        
         BNE   *+10                                                             
         MVC   6(2,R5),=C'TV'                                                   
         CLI   6(R3),1                                                          
         BE    *+8                                                              
         MVI   0(R5),C'*'                                                       
         LA    R3,7(,R3)                                                        
         LA    R5,13(R5)                                                        
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    *+8                                                              
         BCT   R0,SP520                                                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LTR   R2,R2                                                            
         BP    SP510                                                            
         CLI   QOPT1,C'Y'          SHOULD WE DELETE THE RECORDS?                
         BNE   SP560                NO.                                         
         EJECT                                                                  
* THIS SECTION OF CODE DELETES THE APPROPRIATE *                                
*  STATION, MARKET AND ADDRESS RECORDS         *                                
         SPACE 1                                                                
         L     R2,TBLENGTH                                                      
         L     R3,ASTATBL                                                       
SP530    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'            LOOK FOR STATION RECORD - BUILD KEY          
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+7(2),QAGY                                                    
         CLI   6(R3),1             IS THIS STATION ACTIVE?                      
         BE    SP550                YES. DON'T DELETE                           
         MVC   DUB+2(3),0(R3)                                                   
         GOTO1 MSUNPK,DMCB,DUB,DOUBLE,KEY+2                                     
         CLI   KEY+6,C' '                                                       
         BNE   *+8                                                              
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+9(3),=C'000'                                                 
         OC    3(3,R3),3(R3)       IS THERE A CLIENT EXCEPTION CODE?            
         BZ    SP540                NO.                                         
         MVC   KEY+9(3),3(R3)                                                   
SP540    DS    0H                                                               
         GOTO1 HIGHSTA             GET THE STATION RECORD                       
         L     R6,ADSTAT                                                        
         CLC   KEY(12),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                PICKED UP WRONG RECORD -ABORT                
         XC    DMCB,DMCB                                                        
         L     R6,ADSTAT                                                        
         ST    R6,DM4                                                           
         BAS   RE,DELREC           DELETE STATION RECORD                        
         AP    STACTR,=P'1'                                                     
         CLC   9(3,R6),=C'000'     IS THERE A CLIENT EXCEPTION CODE?            
         BNE   SP550                YES. DON'T DELETE ADDRESS RECORD            
         MVI   KEY,C'A'            LOOK FOR STATION ADDRESS RECORD              
         MVC   KEY+9(3),=C'000'                                                 
         GOTO1 HIGHSTAD                                                         
         L     R6,ADSTATAD                                                      
         CLC   KEY(9),0(R6)                                                     
         BNE   SP550                                                            
         XC    DMCB,DMCB                                                        
         ST    R6,DM4                                                           
         BAS   RE,DELREC           DELETE STATION ADDRESS RECORD                
         AP    ADDRCTR,=P'1'                                                    
SP550    DS    0H                                                               
         LA    R3,7(,R3)                                                        
         BCT   R2,SP530                                                         
         EJECT                                                                  
* THIS SECTION OF CODE PRINTS A LIST OF THE INACTIVE  *                         
*  MARKETS AND DELETES THE APPROPRIATE MARKET RECORDS *                         
         SPACE 1                                                                
SP560    DS    0H                                                               
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   SPACING,2                                                        
         LA    R0,10                                                            
         L     R3,AMKTTBL                                                       
         LA    R5,P+1                                                           
         L     R6,ADMARKET                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'            BUILD KEY FOR MARKET RECORD                  
         MVC   KEY+1(1),QMED                                                    
         GOTO1 HIGHMKT                                                          
         B     SP580                                                            
         SPACE                                                                  
SP570    DS    0H                                                               
         GOTO1 SEQMKT                                                           
SP580    DS    0H                                                               
         CLC   KEY(2),0(R6)                                                     
         BNE   SP600               NO MORE MARKETS TO DO                        
         CLC   6(2,R6),QAGY                                                     
         BNE   SP570                WRONG AGENCY                                
         SPACE                                                                  
         PACK  DUB,2(4,R6)         GET THE MARKET NUMBER                        
         CVB   R2,DUB                                                           
         STH   R2,BINMKT           SAVE THE MARKET NUMBER (IN BINARY)           
         AR    R2,R3                                                            
         CLI   0(R2),1                                                          
         BE    SP570               ACTIVE MARKET, DON'T DELETE                  
         SPACE                                                                  
         MVC   KEY(6),0(R6)                                                     
         CLI   QOPT1,C'Y'          SHOULD WE DELETE THE RECORDS?                
         BNE   SP590                NO.                                         
         XC    DMCB,DMCB                                                        
         ST    R6,DM4                                                           
         BAS   RE,DELREC           DELETE MARKET RECORD                         
         AP    MKTCTR,=P'1'                                                     
         BAS   RE,MKTASSGN         DELETE MARKET ASSIGNMENT RECORDS             
         MVC   KEY(17),SAVEKEY     RESTORE KEY                                  
SP590    DS    0H                                                               
         MVC   0(4,R5),2(R6)       LIST THE INACTIVE MARKETS                    
         LA    R5,13(R5)                                                        
         BCT   R0,SP570                                                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LA    R0,10                                                            
         LA    R5,P+1                                                           
         B     SP570                                                            
         EJECT                                                                  
SP600    DS    0H                                                               
         CP    MKTCTR,=P'0'                                                     
         BNE   SP610                                                            
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         XC    P,P                                                              
         MVC   P+49(28),=CL28'NO MARKETS HAVE BEEN DELETED'                     
SP610    DS    0H                                                               
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         EJECT                                                                  
* THIS SECTION OF CODE DELETES THE APPROPRIATE *                                
* TRAFFIC STATION ADDRESS RECORD               *                                
SP700    DS    0H                                                               
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,10               # STATIONS PER LINE ON REPORT                
         LA    R5,P                PRINT LINE                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'TRFDIR',KEYSAVE,KEY                   
*                                                                               
SP710    CLC   KEY(3),KEYSAVE                                                   
         BNE   SP780                                                            
         SPACE                                                                  
         CLI   KEY+3,C'0'          IF A CABLE STATION, END                      
         BNL   SP780                                                            
         SPACE                                                                  
         L     R2,TBLENGTH                                                      
         L     R3,ASTATBL                                                       
         GOTO1 MSPACK,DMCB,=C'0000',KEY+3,DUB                                   
*                                                                               
SP720    CLC   0(3,R3),DUB+2                                                    
         BNE   SP750               IS STATION IN TABLE?                         
         CLI   6(R3),1             YES- IS STATION ACTIVE?                      
         BE    SP742               ACTIVE - DON'T DELETE                        
*                                                                               
SP725    DS    0H                                                               
         CLI   QOPT1,C'Y'          SHOULD WE DELETE                             
         BNE   SP740               NO- BUT INCLUDE IN COUNT                     
         CLI   RCWRITE,C'Y'                                                     
         BNE   SP730                                                            
         OI    KEY+13,X'80'                                                     
         GOTO1 DATAMGR,DMCB,DMWRT,=C'TRFDIR',KEY                                
SP730    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'TRFFIL',KEY+14,ADEST,DMWORK           
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RCWRITE,C'Y'                                                     
         BNE   SP740                                                            
         L     RE,ADEST                                                         
         OI    15(RE),X'80'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'TRFFIL',KEY+14,ADEST,DMWORK           
SP740    AP    ADDRCTR2,=P'1'                                                   
         MVI   0(R5),C'*'          DENOTES INACTIVITY                           
SP742    MVC   1(4,R5),KEY+3                                                    
         MVI   5(R5),C'-'                                                       
         MVC   6(1,R5),KEY+7                                                    
         MVI   7(R5),C'M'                                                       
         CLI   QMED,C'T'                                                        
         BNE   *+10                                                             
         MVC   6(2,R5),=C'TV'                                                   
         LA    R5,13(R5)                                                        
         BCTR  R0,0                                                             
         LTR   R0,R0               PRINTED ONE FULL LINE?                       
         BNZ   SP745                                                            
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         LA    R0,10                                                            
         LA    R5,P                                                             
*                                                                               
SP745    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'TRFDIR',KEYSAVE,KEY                   
         B     SP710               CHECK NEXT TRAFFIC REC.                      
*                                                                               
SP750    LA    R3,7(,R3)           NEXT STATION IN TABLE                        
         BCT   R2,SP720                                                         
*              STATION NOT IN TABLE -TREAT AS INACTIVE                          
         B     SP725                                                            
*                                                                               
SP780    CP    ADDRCTR2,=P'0'                                                   
         BNE   SP790                                                            
         MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         XC    P,P                                                              
         MVC   P+40(46),=CL46'NO TRAFFIC STATION ADDRESSES HAVE BEEN DEX        
               LETED'                                                           
SP790    MVI   SPACING,3                                                        
         GOTO1 REPORT                                                           
         EJECT                                                                  
* DELETES NSID, MKT/STA, AND DETAIL RECORDS OF                                  
* INACTIVE STATIONS *                                                           
         SPACE                                                                  
SP800    DS    0H                                                               
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
         LA    R5,P                                                             
*                                                                               
         L     R2,TBLENGTH                                                      
         L     R3,ASTATBL                                                       
SP810    CLI   6(R3),1             IS STATION ACTIVE?                           
         BE    SP870               YES - DON'T DELETE                           
*                                                                               
SP820    XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                   
*                                                                               
SP825    CLC   KEY(2),KEYSAVE                                                   
         BNE   SP890               NO X'0C' RECS                                
*                                                                               
         CLC   0(3,R3),KEY+6       CHECK STATION?                               
         BNE   SP850               NOT RIGHT STA/ OR SCHEME REC                 
*                                                                               
         CLI   QOPT1,C'Y'          SHOULD WE DELETE                             
         BNE   SP840               NO BUT INCLUDE IN COUNT                      
         CLI   RCWRITE,C'Y'                                                     
         BNE   SP830                                                            
         OI    KEY+13,X'80'                                                     
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY                                    
SP830    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,ADEST,DMWORK           
         CLI   DMCB+8,X'00'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   RCWRITE,C'Y'                                                     
         BNE   SP840                                                            
         L     RE,ADEST                                                         
         OI    15(RE),X'80'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',KEY+14,ADEST,DMWORK           
SP840    DS    0H                                                               
         CLC   KEY+9(3),=XL3'00'   IS THIS MKT/STA REC                          
         BNE   SP845                                                            
         AP    MSTACTR,=P'1'                                                    
         B     SP848                                                            
*                                                                               
SP845    CLI   KEY+10,0            IS THIS NSID REC                             
         BNE   SP847                                                            
         AP    NSIDCTR,=P'1'                                                    
         B     SP848                                                            
*                                                                               
SP847    AP    DTLCTR,=P'1'                                                     
* TEMPORARY PRINTING KEYS FOR DEBUGGING                                         
SP848    DS    0H                                                               
*******  GOTO1 HEXOUT,DMCB,KEY,(R5),18,=C'TOG'                                  
*******  MVI   SPACING,2                                                        
*******  GOTO1 REPORT                                                           
*******  LA    R5,P                                                             
*                                                                               
SP850    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',KEYSAVE,KEY                   
         B     SP825                                                            
*                                                                               
SP870    LA    R3,7(,R3)           NEXT STATION IN TABLE                        
         BCT   R2,SP810                                                         
*                                                                               
SP890    DS   0H                                                                
         MVI   SPACING,4                                                        
         GOTO1 REPORT                                                           
         MVC   P+18(29),=CL29'NUMBER OF STATIONS DELETED = '                    
         OI    STACTR+7,X'0F'                                                   
         EDIT  (P8,STACTR),(4,P+47),ALIGN=LEFT,ZERO=NOBLANK                     
         SPACE 1                                                                
         MVC   P+70(36),=CL36'NUMBER OF ADDRESS RECORDS DELETED = '             
         OI    ADDRCTR+7,X'0F'                                                  
         EDIT  (P8,ADDRCTR),(4,P+106),ALIGN=LEFT,ZERO=NOBLANK                   
         SPACE 1                                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+18(28),=CL28'NUMBER OF MARKETS DELETED = '                     
         OI    MKTCTR+7,X'0F'                                                   
         EDIT  (P8,MKTCTR),(4,P+46),ALIGN=LEFT,ZERO=NOBLANK                     
         SPACE 1                                                                
         MVC   P+70(46),=CL46'NUMBER OF MARKET ASSIGNMENT RECORDS DELETX        
               ED = '                                                           
         OI    MASCTR+7,X'0F'                                                   
         EDIT  (P8,MASCTR),(4,P+116),ALIGN=LEFT,ZERO=NOBLANK                    
         SPACE 1                                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE 1                                                                
         MVC   P+18(44),=CL44'NUMBER OF TRAFFIC ADDRESS RECORDS DELETEDX        
                = '                                                             
         OI    ADDRCTR2+7,X'0F'                                                 
         EDIT  (P8,ADDRCTR2),(4,P+62),ALIGN=LEFT,ZERO=NOBLANK                   
         SPACE 1                                                                
         MVC   P+70(44),=CL44'NUMBER OF STATION UPGRADE RECORDS DELETEDX        
                = '                                                             
         OI    MSTACTR+7,X'0F'                                                  
         EDIT  (P8,MSTACTR),(4,P+116),ALIGN=LEFT,ZERO=NOBLANK                   
         SPACE 1                                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P+18(33),=CL33'NUMBER OF NSID RECORDS DELETED = '                
         OI    NSIDCTR+7,X'0F'                                                  
         EDIT  (P8,NSIDCTR),(4,P+62),ALIGN=LEFT,ZERO=NOBLANK                    
         SPACE 1                                                                
         MVC   P+70(35),=CL35'NUMBER OF DETAIL RECORDS DELETED = '              
         OI    DTLCTR+7,X'0F'                                                   
         EDIT  (P8,DTLCTR),(4,P+116),ALIGN=LEFT,ZERO=NOBLANK                    
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P+41(34),=CL34'TOTAL NUMBER OF RECORDS DELETED = '               
         AP    TOTALCTR,MASCTR                                                  
         AP    TOTALCTR,ADDRCTR                                                 
         AP    TOTALCTR,MKTCTR                                                  
         AP    TOTALCTR,STACTR                                                  
         AP    TOTALCTR,ADDRCTR2                                                
         AP    TOTALCTR,MSTACTR                                                 
         AP    TOTALCTR,NSIDCTR                                                 
         AP    TOTALCTR,DTLCTR                                                  
         OI    TOTALCTR+7,X'0F'                                                 
         EDIT  (P8,TOTALCTR),(4,P+75),ALIGN=LEFT,ZERO=NOBLANK                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
* THIS SUBROUTINE DELETES MARKET ASSIGNMENT RECORDS *                           
         SPACE 1                                                                
MKTASSGN NTR1                                                                   
         MVC   SAVEKEY(17),KEY                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D03'                                                  
         MVC   KEY+8(1),SVAGYMD                                                 
MKT10    DC    0H'0'                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE      IS THIS SAME TYPE/A-M?                       
         BNE   EXIT                                                             
         MVC   KEY+11(2),BINMKT    GET THE MARKET CODE IN BINARY                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   MKT30                                                            
         MVI   KEY+13,C'D'         MARK THE RECORD AS 'TO BE DELETED'           
         AP    MASCTR,=P'1'                                                     
         B     MKT20               *** PATCH OUT TRACE ***                      
         MVC   SAVEPRT(132),P                                                   
         XC    P,P                                                              
         GOTO1 =V(HEXOUT),DMCB,KEY,P+5,20,=C'MIX',0,RR=RELO                     
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
MKT20    CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY                                    
         TM    DM3,X'FD'                                                        
         BNZ   DMGRERR                                                          
         B     EXIT                                                             
MKT30    MVC   KEY+11(2),=X'FFFF'                                               
         B     MKT10                                                            
         EJECT                                                                  
* THIS SUBROUTINE DELETES THE RECORDS ON THE STATION FILE *                     
         SPACE 1                                                                
DELREC   NTR1                                                                   
         MVI   17(R6),C'D'         MARK RECORD AS 'TO BE DELETED'               
         B     DEL10               *** PATCH OUT TRACE ***                      
         MVC   SAVEPRT(132),P                                                   
         XC    P,P                                                              
         MVC   P+5(117),0(R6)      TRACE - PRINT RECORD                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
DEL10    CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD                   
         BNE   EXIT                                                             
         GOTO1 DATAMGR,DMCB,DMWRT,STATION                                       
         TM    DM3,X'FD'                                                        
         BNZ   DMGRERR                                                          
         B     EXIT                                                             
         SPACE 1                                                                
BADREQ   DS    0H                                                               
         MVC   P+2(23),=C'ERROR - XXX MUST BE ALL'                              
         ZIC   RF,ERRCD                                                         
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RE,ERRMSG                                                        
         AR    RE,RF                                                            
         MVC   P+10(3),0(RE)                                                    
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         SPACE 1                                                                
* NOACTIV - PRINT ERROR MSG *                                                   
         SPACE 1                                                                
NOACTIV  NTR1                                                                   
         XC    P,P                                                              
         MVC   P+2(30),=C'*** NO STATIONS ARE ACTIVE ***'                       
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         CLI   QOPT5,C'Y'                                                       
         BNE   EXIT                                                             
         GOTO1 AENDREQ                                                          
         SPACE 1                                                                
DMGRERR  DC    H'0'                                                             
         SPACE 1                                                                
         USING *,RF                                                             
SPSPHDHK DC    0H'0'                                                            
         LM    R9,RB,SPSPR9                                                     
         B     EXIT                                                             
         DROP  RF                                                               
SPSPR9   DC    3F'0'                                                            
         EJECT                                                                  
* BINREC - BINSRCH RECORD FORMAT *                                              
         SPACE 1                                                                
BINREC   DS    0CL7                                                             
BINSTA   DS    XL3                 PACKED STATION                               
BINCLT   DS    CL3                 CLIENT                                       
BINACT   DS    CL1                 ACTIVITY SWITCH                              
         SPACE 2                                                                
         DS    0D                                                               
MKTNUM   DS    D                                                                
ADDRCTR  DS    PL8                                                              
MASCTR   DS    PL8                                                              
MKTCTR   DS    PL8                                                              
STACTR   DS    PL8                                                              
TOTALCTR DS    PL8                                                              
ADDRCTR2 DS    PL8                                                              
MSTACTR  DS    PL8                                                              
NSIDCTR  DS    PL8                                                              
DTLCTR   DS    PL8                                                              
TBLENGTH DS    F                                                                
PSTA     DS    0FL6                PARAMETER LIST FOR BINSRCH                   
PSTA1    DS    F                                                                
PSTA2    DS    F                                                                
PSTA3    DS    F                                                                
PSTA4    DS    F                                                                
PSTA5    DS    F                                                                
PSTA6    DS    F                                                                
RELO     DS    F                                                                
ASTATBL  DS    A                                                                
AMKTTBL  DS    A                                                                
BINMKT   DS    H                                                                
SAVEKEY  DS    CL17                                                             
SAVEPRT  DS    CL132                                                            
BUYSW    DS    C                                                                
ERRCD    DS    C                                                                
ERRMSG   DS    0H                                                               
         DC    C'CLT'                                                           
         DC    C'PRD'                                                           
         DC    C'MKT'                                                           
         DC    C'STA'                                                           
         DC    C'EST'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
STATBL   CSECT                                                                  
         DS    210000C                                                          
         SPACE 3                                                                
MKTTBL   CSECT                                                                  
         DS    30000C                                                           
         PRINT OFF                                                              
         SPACE 3                                                                
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053SPREPSP02 05/01/02'                                      
         END                                                                    
