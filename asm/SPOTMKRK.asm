*          DATA SET SPOTMKRK   AT LEVEL 022 AS OF 09/26/08                      
*PHASE T00A4EB,*                                                                
*INCLUDE XSORT                                                                  
         SPACE 2                                                                
********************************************************************            
*                                                                  *            
*       SPOTMKRK (T00A4E) - GET MARKETS RANK                       *            
*                                                                  *            
*------------------------------------------------------------------*            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
* 08SEP08 22 AKAT SBQBOOK NOW 24 BYTES                             *            
* 16MAY07 21 EJOR CHANGE GR LABELS TO GRK TO NOT BE SAME AS REQUS  *            
* 11JUL03 20 EJOR PASS DBSELAGY TO DEMAND                          *            
* 12NOV98 19 EJOR Y2K COMPLIANCE                                   *            
* 12NOV98 ?? ???? HISTORY LOST                                     *            
*                                                                  *            
********************************************************************            
*                                                                               
********************************************************************            
*                                                                  *            
* SPOTMKRK - T00A4E                                                *            
* --------                                                         *            
*                                                                  *            
* ROUTINE TO GET A MARKET'S RANK                                   *            
*                                                                  *            
* INPUT  : PARM1    = A(SPOTBLOCK)                                 *            
*          SBQAGY   = AGENCY ALPHA CODE                            *            
*          SBMKT    = AGENCY MARKET NUMBER                         *            
*          SBBMKT   = AGENCY MARKET NUMBER IN BINARY               *            
*          SBCPROF  = CLIENT'S PROFILE                             *            
*          SBBPRD   = BINARY PRODUCT CODE                          *            
*          SBBEST   = BINARY ESTIMATE NUMBER                       *            
*          SBQBOOK  = LOOKUP BOOK (OPTIONAL)                       *            
*          SBMKTREC = MARKET RECORD (OPTIONAL)                     *            
*                                                                  *            
* OUTPUT : SBMRNUM  = MARKET'S ABSOLUTE RANK NUMBER (1,2,3,...)    *            
*          SBMKTRNK = MARKET'S RANK  10=1-10                       *            
*                                    20=11-20                      *            
*                                    30=21-30                      *            
*                                    40=31-40                      *            
*                                    50=41-50                      *            
*                                    60=51-100                     *            
*                                    70=101+                       *            
*                                                                  *            
********************************************************************            
         TITLE 'SPOTMKRK - SPOTPAK GET MARKET RANK ROUTINE'                     
SPOTMKRK CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**SPMR**,RR=RE,CLEAR=YES                             
         USING WORKD,RC            RC = A(LOCAL WORKING STORAGE)                
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING SBLOCKD,RA          RA = A(SPOTBLOCK)                            
         L     R9,SBCOMFAC         R9 = A(COMFACS)                              
         USING COMFACSD,R9                                                      
         STM   R9,RC,SAVER9C                                                    
*                                                                               
         L     R1,=A(BSMKTS)       RE-SET ADDRESS CONSTANTS                     
         A     R1,RELO                                                          
         LA    R0,6                                                             
         LA    RE,BKSVCTAB                                                      
         STCM  R1,7,3(RE)                                                       
         LA    R1,NMKT*2(R1)                                                    
         LA    RE,6(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
         L     R1,=A(DEMKTS)                                                    
         A     R1,RELO                                                          
         LA    R1,0(R1)                                                         
         LA    R0,6                                                             
         LA    RE,DEMKTAB                                                       
         ST    R1,6(RE)                                                         
         AH    R1,=Y(NMKT*6)                                                    
         LA    RE,10(RE)                                                        
         BCT   R0,*-12                                                          
*                                                                               
         B     GM2                                                              
*                                                                               
EQXIT    CR    RB,RB               SET CC =                                     
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT =                                 
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
GM2      BAS   RE,GETSMKT          GET THE SERVICE MARKET                       
         LH    R4,=H'3000'         DEFAULT RANK TO LAST                         
         OC    SVCMKT,SVCMKT       TEST FOR ZERO MARKET                         
         BZ    GM8                                                              
         BAS   RE,GETRKTAB         GET THE RANK TABLE                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,THISBSDL                                                      
*                                                                               
GM6      OC    0(6,R5),0(R5)       FIND RANK NUMBER FOR MARKET                  
         BZ    GM8                                                              
         CLC   SVCMKT,4(R5)                                                     
         BE    *+12                                                             
         LA    R5,6(R5)                                                         
         B     GM6                                                              
         L     R4,0(R5)                                                         
*                                                                               
GM8      STCM  R4,3,SBMRNUM        MARKET RANK                                  
         LA    R1,70                                                            
         LTR   R4,R4               1-10   = 10                                  
         BZ    GM10                11-20  = 20                                  
         CH    R4,=H'100'          21-30  = 30                                  
         BH    GM10                31-40  = 40                                  
         LA    R1,60               41-50  = 50                                  
         CH    R4,=H'50'           51-100 = 60                                  
         BH    GM10                101+   = 70                                  
         LA    R1,9(R4)                                                         
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MH    R1,=H'10'                                                        
*                                                                               
GM10     STC   R1,SBMKTRNK                                                      
*                                                                               
GMX      B     EQXIT                                                            
         EJECT                                                                  
* ROUTINE TO GET THE SERVICE MARKET NUMBER FOR AN AGENCY MARKET                 
* MARKET TABLE ENTRY IS AGYMKT(2)/ARBMKT(2)/NSIMKT(2)                           
* INPUT :  SBMKT  = AGENCY MARKET NUM                                           
*          SBBMKT = BINARY AGENCY MARKET NUM                                    
* OUTPUT:  SVCMKT = SERVICE MARKET NUM                                          
*                                                                               
GETSMKT  NTR1                                                                   
         LA    R2,MKTAB            SEARCH FOR MARKET IN MARKET TABLE            
*                                                                               
GS2      OC    0(6,R2),0(R2)       TEST FOR END OF TABLE                        
         BZ    GS4                 YES - CREATE NEW ENTRY                       
         CLC   SBBMKT,0(R2)        COMPARE MARKETS                              
         BE    GS8                 FOUND                                        
         LA    R2,6(R2)            NEXT MARKET IN TABLE                         
         C     R2,AMKTABX          TEST FOR ROOM IN TABLE                       
         BL    GS2                                                              
         DC    H'0'                                                             
*                                                                               
GS4      LA    R3,KEY              BUILD MARKET RECORD KEY                      
         USING MKTRECD,R3                                                       
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(16),MKTKEY                                              
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,SBMED                                                    
         MVC   MKTKMKT,SBMKT                                                    
         MVC   MKTKAGY,SBAGY                                                    
         LA    R3,SBMKTREC                                                      
         CLC   MKTKEY,KEY          TEST RECORD ALREADY IN CORE                  
         BE    GS6                 YES                                          
*                                  NO-READ THE MARKET RECORD                    
         GOTO1 CDATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,MARKETRC                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,MARKETRC                                                      
*                                                                               
GS6      MVC   0(2,R2),SBBMKT      BUILD MARKET TABLE ENTRY                     
         CLI   MKTRS1,C'1'                                                      
         BNE   *+14                                                             
         MVC   2(2,R2),MKTRSM1     ARB MARKET                                   
         B     *+18                                                             
         CLI   MKTRS1,C'0'                                                      
         BNE   *+10                                                             
         MVC   4(2,R2),MKTRSM1     NSI MARKET                                   
         CLI   MKTRS2,C'1'                                                      
         BNE   *+14                                                             
         MVC   2(2,R2),MKTRSM2     ARB MARKET                                   
         B     *+18                                                             
         CLI   MKTRS2,C'0'                                                      
         BNE   *+10                                                             
         MVC   4(2,R2),MKTRSM2     NSI MARKET                                   
*                                                                               
GS8      MVC   SVCMKT,4(R2)        SET SERVICE MARKET NUM                       
         CLI   SBCPROF+3,C'0'                                                   
         BE    GSX                                                              
         MVC   SVCMKT,2(R2)                                                     
*                                                                               
GSX      B     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO GET THE RANK TABLE                                                 
* OUTPUT:   THISBSDL = A(RANK TABLE)                                            
* COMMENTS: THISBSE = A(BOOK/SERVICE ENTRY TABLE)                               
*                     ENTRY = BOOK(2)/SERVICE(1)/AL3(MARKET LIST)               
*           THISBSL = A(BOOK/SERVICE MARKET LIST)                               
*                     ENTRY = SVCMKT(2)                                         
*                                                                               
         SPACE                                                                  
GETRKTAB NTR1                                                                   
         MVC   FULL,SBQBOOK        IF BOOK SPECIFIED, USE IT                    
         CLC   SBQBOOK(3),=C'ACT'                                               
         BH    *+14                                                             
         MVC   FULL,SBQEND         ELSE USE NOVEMBER OF YEAR PRIOR              
         MVI   WORK+1,11           TO REQUEST END                               
         MVC   DUB2(4),FULL                                                     
         MVC   DUB2+4(2),=C'01'                                                 
         GOTO1 CDATCON,DMCB,(0,DUB2),(3,DUB)                                    
         LLC   R2,DUB                                                           
         CLC   SBQBOOK(3),=C'ACT'                                               
         BH    GRK2                                                             
         GOTO1 CDATCON,DMCB,(5,0),(3,FULL)   BOOK MUST BE LESS THAN             
*                                            CURRENT MONTH                      
GRK1     BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BNM   *+8                                                              
         LA    R2,99                                                            
         STC   R2,WORK                                                          
         CLI   FULL,51                                                          
         BNL   *+12                                                             
         CLI   WORK,50                                                          
         BH    GRK3                                                             
         CLC   WORK(2),FULL                                                     
         BL    GRK3                                                             
         B     GRK1                                                             
*                                                                               
GRK2     STC   R2,WORK                                                          
         PACK  DUB,FULL+2(2)                                                    
         CVB   RF,DUB                                                           
         STC   RF,WORK+1                                                        
*                                                                               
GRK3     MVI   WORK+2,C'N'         SERVICE IN CLIENT HEADER                     
         CLI   SBCPROF+3,C'0'                                                   
         BE    *+8                                                              
         MVI   WORK+2,C'A'                                                      
         CLI   WORK,94             NO MORE ARB AFTER 12/31/93                   
         BL    *+8                                                              
         MVI   WORK+2,C'N'         FORCE NSI                                    
         LA    R2,BKSVCTAB        LOOK FOR BOOK/SERVICE IN BK/SVC TABLE         
         LA    R0,6                                                             
*                                                                               
GRK4     CLI   0(R2),0             TEST FOR EMPTY SLOT                          
         BE    GRK8                                                             
         CLC   0(3,R2),WORK        TEST FOR MATCH BK/SVC                        
         BNE   GRK6                                                             
         ST    R2,THISBSE          THIS BK/SVC ENTRY                            
         SR    RE,RE                                                            
         ICM   RE,7,3(R2)                                                       
         ST    RE,THISBSL          THIS BK/SVC LIST                             
         B     GRK10                                                            
*                                                                               
GRK6     LA    R2,6(R2)                                                         
         BCT   R0,GRK4                                                          
         L     R2,ANXTBS           TABLE FULL - GET A(NEXT ENTRY)               
         EJECT                                                                  
* BUILD LIST OF MARKETS FOR REQUEST BOOK / SERVICE                              
*                                                                               
GRK8     MVC   0(3,R2),WORK        BUILD NEW BK/SVC ENTRY                       
         SR    R3,R3                                                            
         ICM   R3,7,3(R2)                                                       
         LR    RE,R3                                                            
         LA    RF,1000                                                          
         XCEFL ,                                                                
         ST    R2,THISBSE          THIS BK/SVC ENTRY                            
         ST    R3,THISBSL          THIS BK/SVC LIST                             
         LA    RE,6(R2)            SET UP ADDRESS OF NEXT ENTRY                 
         C     RE,=A(BKSVCTBX)                                                  
         BL    *+8                                                              
         LA    RE,BKSVCTAB                                                      
         ST    RE,ANXTBS                                                        
*                                                                               
         LA    R4,DBLOCK           BUILD MARKET LIST FOR THIS YR/SVC            
         XC    0(256,R4),0(R4)                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBGETMKB                                                 
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC(1),2(R2)                                                
         MVC   DBSELAGY,SBQAGY                                                  
         MVC   DBSELBK,0(R2)                                                    
         MVC   DBCOMFCS,SBCOMFAC                                                
         MVC   DBAREC,ADEMREC                                                   
         GOTO1 CDEMAND,DMCB,(R4),SVMRKT                                         
         EJECT                                                                  
*                                                                               
* BUILD LIST OF MARKET UNIVERSES FOR BOOK/SERVICE/DEMO AND SORT.                
* ENTRY IS BOOK(2)/SERVICE(1)/DEMO(3)/AL4(UNIVERSE LIST)                        
* UNIVERSE LIST ENTRY IS UNIVERSE(4)/SVCMKT(2)                                  
*                                                                               
         SPACE                                                                  
GRK10    MVC   WORK(3),0(R2)       BK/SVC                                       
         OC    SBEDEMOS,SBEDEMOS   TEST OVERRIDE DEMO LIST                      
         BZ    *+14                                                             
         MVC   WORK+3(3),SBEDEMOS  YES                                          
         B     GRK12                                                            
         OC    SBPDEMOS,SBPDEMOS   TEST DEMO MENU OR DEMOS OPTION SET           
         BZ    *+14                                                             
         MVC   WORK+3(3),SBPDEMOS  YES                                          
         B     GRK12                                                            
         OC    SBAESTTB,SBAESTTB   NO-GET DEMOS FROM ESTIMATE BUFFER            
         BZ    GRK12                                                            
         OC    SBAESTBF,SBAESTBF                                                
         BZ    GRK12                                                            
         LLC   RE,SBBPRD                                                        
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         LLC   RF,SBBEST                                                        
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
         MVC   WORK+3(3),EBDEMOS                                                
*                                                                               
GRK12    MVC   DEMO,WORK+3         SAVE THE DEMO CODE                           
         MVI   DEMO+1,C'U'         UNIVERSE                                     
         LA    R2,DEMKTAB          LOOK FOR YR/SVC/DEMO IN TABLE                
         LA    R0,6                                                             
*                                                                               
GRK14    CLI   0(R2),0             TEST FOR EMPTY SLOT                          
         BE    GRK16                                                            
         CLC   0(6,R2),WORK        TEST FOR MATCH YR/SVC/DEMO                   
         BNE   *+12                                                             
         L     R3,6(R2)            RANK LIST FOUND - EXIT                       
         B     GRK26                                                            
         LA    R2,10(R2)                                                        
         BCT   R0,GRK14                                                         
         L     R2,ANXTDEM          TABLE FULL - GET A(NEXT ENTRY)               
*                                                                               
GRK16    MVC   0(6,R2),WORK        BUILD NEW YR/SVC ENTRY                       
         L     R3,6(R2)                                                         
         XCEFL (R3),3000           CLEAR THE MRKT LIST                          
         ST    R3,THISBSDL         THIS YR/SVC/DEMO LIST                        
         LA    RE,10(R2)           SET UP ADDRESS OF NEXT ENTRY                 
         C     RE,=A(DEMKTABX)                                                  
         BL    *+8                                                              
         LA    RE,DEMKTAB                                                       
         ST    RE,ANXTDEM                                                       
*                                                                               
         L     R5,THISBSL          READ THE UNIVERSES                           
*                                                                               
GRK18    OC    0(2,R5),0(R5)       TEST FOR END OF MARKET LIST                  
         BZ    GRK20                                                            
         LA    R7,DBLOCK                                                        
         XC    0(256,R7),0(R7)                                                  
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBGETTOT                                                 
         MVI   DBSELMED,C'T'                                                    
         MVC   DBSELSRC(1),2(R2)                                                
         MVC   DBSELAGY,SBQAGY                                                  
         MVC   DBSELBK,0(R2)                                                    
         MVC   DBCOMFCS,SBCOMFAC                                                
         MVC   DBAREC,ADEMREC                                                   
         MVC   DBSELRMK,0(R5)                                                   
         MVI   DBSELDAY,X'40'      SET TO READ MON 5-515P                       
         MVC   DBSELTIM(2),=H'1700'                                             
         MVC   DBSELTIM+2(2),=H'1715'                                           
         GOTO1 CDEMAND,DMCB,(R7),SVUNIV                                         
         OC    0(4,R3),0(R3)       TEST FOR ZERO UNIVERSE                       
         BNZ   *+10                                                             
         MVC   0(4,R3),=F'-1'      YES - FORCE TO 1,000                         
         MVC   4(2,R3),0(R5)       MOVE MARKET TO TABLE                         
         LA    R3,6(R3)                                                         
         ST    R3,THISBSDL                                                      
         LA    R5,2(R5)            NEXT MARKET                                  
         B     GRK18                                                            
         EJECT                                                                  
* COUNT THE MARKETS IN UNIVERSE LIST, SORT AND INSERT RANK NUMBERS              
* TO PRODUCE THE RANK LIST                                                      
*                                                                               
         SPACE                                                                  
GRK20    L     R3,6(R2)            COUNT THE MARKETS                            
         LR    RE,R3                                                            
         SR    R5,R5                                                            
*                                                                               
GRK22    OC    0(6,RE),0(RE)                                                    
         BZ    *+12                                                             
         LA    RE,6(RE)                                                         
         BCT   R5,GRK22                                                         
         LPR   R5,R5                                                            
         BZ    NEQXIT                                                           
         GOTO1 =V(XSORT),DMCB,(R3),(R5),6,6,0 SORT IN REVERSE UNIVERSE          
         LA    R1,1                                                             
         LR    RE,R3               REPLACE UNIVERSES WITH RANK NUMBERS          
         LA    R5,500                                                           
*                                                                               
GRK24    OC    0(6,RE),0(RE)                                                    
         BZ    GRK26                                                            
         ST    R1,0(RE)                                                         
         LA    R1,1(R1)                                                         
         LA    RE,6(RE)                                                         
         BCT   R5,GRK24                                                         
*                                                                               
GRK26    ST    R3,THISBSDL                                                      
         B     EQXIT                                                            
         EJECT                                                                  
* HOOK TO SAVE MARKET NUMBER FROM RATING SERVICE MARKET RECORD                  
*                                                                               
         DS    0D                                                               
         USING *,RF                                                             
SVMRKT   NTR1  BASE=SAVERB                                                      
         LM    R9,RC,SAVER9C                                                    
         DROP  RF                                                               
         L     R3,DBAREC                                                        
         USING DMKEY,R3                                                         
         OC    DMRMKT,DMRMKT                                                    
         BZ    SMX                                                              
         L     R7,THISBSL          A(THIS BK/SVC MARKET LIST)                   
         LA    R0,250                                                           
*                                                                               
SM2      OC    0(2,R7),0(R7)                                                    
         BZ    SM4                                                              
         CLC   DMRMKT,0(R7)        CHECK FOR DUPLICATE                          
         BE    SMX                                                              
         LA    R7,2(R7)                                                         
         BCT   R0,SM2                                                           
         DC    H'0'                                                             
*                                                                               
SM4      MVC   0(2,R7),DMRMKT      SAVE SERVICE MARKET NUM                      
*                                                                               
SMX      B     EXIT                                                             
*                                                                               
         DROP  R9                                                               
         EJECT                                                                  
* HOOK TO SAVE MARKET UNIVERSE IN MARKET TABLE                                  
*                                                                               
         DS    0D                                                               
         USING *,RF                                                             
SVUNIV   NTR1  BASE=SAVERB                                                      
         LM    R9,RC,SAVER9C                                                    
         DROP  RF                                                               
         L     R8,SBCOMFAC                                                      
         USING COMFACSD,R8                                                      
         GOTO1 CDEMOUT,DMCB,(C'D',DEMO),DBLOCK,FULL                             
         DROP  R8                                                               
         L     RE,THISBSDL                                                      
         L     R1,FULL                                                          
         LNR   R1,R1                                                            
         ST    R1,0(RE)            SAVE UNIVERSE IN MARKET TABLE                
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SAVER9C  DS    0F                                                               
SAVER9   DS    F                                                                
SAVERA   DS    F                                                                
SAVERB   DS    F                                                                
SAVERC   DS    F                                                                
AMKTABX  DC    A(MKTABX)                                                        
ADEMREC  DC    A(DEMREC)                                                        
ANXTBS   DC    A(BKSVCTAB)                                                      
ANXTDEM  DC    A(DEMKTAB)                                                       
THISBSE  DS    A                                                                
THISBSL  DS    A                                                                
THISBSDL DS    A                                                                
SVCMKT   DS    XL2                                                              
DEMO     DS    XL3                                                              
*                                                                               
MARKETRC DS    CL(L'MKTREC)                                                     
*                                                                               
NMKT     EQU   1000                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'BKSVCTAB'                                                    
BKSVCTAB DC    XL3'0000',AL3(BSMKTS)                                            
         DC    XL3'0000',AL3(BSMKTS+NMKT*2)                                     
         DC    XL3'0000',AL3(BSMKTS+NMKT*4)                                     
         DC    XL3'0000',AL3(BSMKTS+NMKT*6)                                     
         DC    XL3'0000',AL3(BSMKTS+NMKT*8)                                     
         DC    XL3'0000',AL3(BSMKTS+NMKT*10)                                    
BKSVCTBX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*DEMKTAB'                                                    
DEMKTAB  DC    XL6'00',AL4(DEMKTS)                                              
         DC    XL6'00',AL4(DEMKTS+NMKT*6)                                       
         DC    XL6'00',AL4(DEMKTS+NMKT*12)                                      
         DC    XL6'00',AL4(DEMKTS+NMKT*18)                                      
         DC    XL6'00',AL4(DEMKTS+NMKT*24)                                      
         DC    XL6'00',AL4(DEMKTS+NMKT*30)                                      
DEMKTABX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'**MKTAB*'                                                    
MKTAB    DC    (NMKT)XL6'00'                                                    
MKTABX   EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'*BSMKTS*'                                                    
BSMKTS   DS    6XL(NMKT*2)'00'                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'*DEMKTS*'                                                    
DEMKTS   DS    6XL(NMKT*6)'00'                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'*DEMREC*'                                                    
DEMREC   DS    1500X                                                            
         EJECT                                                                  
* WORKING STORAGE                                                               
*                                                                               
WORKD    DSECT                                                                  
*                                                                               
DUB      DS    D                                                                
DUB2     DS    D                                                                
DMCB     DS    8F                                                               
FULL     DS    F                                                                
RELO     DS    F                                                                
KEY      DS    CL48                                                             
WORK     DS    CL64                                                             
*                                                                               
* DEDBLOCK                                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* SPOTBLOCK                                                                     
         PRINT OFF                                                              
SBLOCKD  DSECT                                                                  
       ++INCLUDE SPOTBLOCK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SPGENMKT                                                                      
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPOTMKRK  09/26/08'                                      
         END                                                                    
