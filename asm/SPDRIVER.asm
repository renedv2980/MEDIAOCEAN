*          DATA SET SPDRIVER   AT LEVEL 054 AS OF 05/01/02                      
*PHASE SPDRIVEA SPDRIVER                                                        
         TITLE 'SPDRIVER - DRIVER FOR SPOT SYSTEM'                              
SPDRIVER CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPDV**,R9                                                    
         L     R8,0(R1)                                                         
         USING GLOBALD,R8                                                       
         L     RA,GLAWORKD                                                      
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
*                                                                               
         LA    R7,SPDRVWKC         SPOT DRIVER WORK AREA                        
         USING SPDRVWKD,R7                                                      
*                                                                               
         CLI   GLHOOK,GLINIT       TEST FOR INITIALIZATION                      
         BNE   SD005                                                            
         BAS   RE,INIT                                                          
         B     EXIT                                                             
*                                                                               
SD005    CLI   GLHOOK,GLRESOLV     TEST TO RESOLVE LABELS                       
         BNE   SD010                                                            
         BAS   RE,RESOLVE                                                       
         B     EXIT                                                             
*                                                                               
SD010    CLI   GLHOOK,GLROUT       TEST TO EXECUTE USER ROUTINE                 
         BNE   EXIT                                                             
         CLI   GLMODE,GLINPUT      YES - INPUT PHASE                            
         BNE   SD020                                                            
         BAS   RE,INPUT                                                         
         B     EXIT                                                             
*                                                                               
SD020    CLI   GLMODE,GLOUTPUT           OUTPUT PHASE                           
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,OUTPUT                                                        
         B     EXIT                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
INIT     NTR1                                                                   
*                                                                               
*        ROUTINE TO EXECUTE INITIALIZATION CODE                                 
*                                                                               
         LA    RE,SDPLIST          SET SPOT DRIVER PARAMETER LIST               
         ST    R7,0(RE)                                                         
         ST    RE,GLAPLIST                                                      
*                                                                               
         MVI   SDNMGRPS,0          FIGURE OUT MGR AND PGR MASKS                 
         XC    MGR1MASK,MGR1MASK                                                
         XC    MGR2MASK,MGR2MASK                                                
         CLI   QMGR,C' '                                                        
         BE    IN025                                                            
         MVI   SDNMGRPS,3                                                       
         CLC   MGR3LEN,MGR2LEN                                                  
         BE    IN010                                                            
         ZIC   R0,MGR2LEN                                                       
         BAS   RE,SETMASK                                                       
         STH   R0,MGR2MASK                                                      
         B     *+8                                                              
*                                                                               
IN010    MVI   SDNMGRPS,2                                                       
         CLC   MGR2LEN,MGR1LEN                                                  
         BE    IN020                                                            
         ZIC   R0,MGR1LEN                                                       
         BAS   RE,SETMASK                                                       
         STH   R0,MGR1MASK                                                      
         B     IN025                                                            
*                                                                               
IN020    MVI   SDNMGRPS,1                                                       
*                                                                               
IN025    MVI   SDNPGRPS,0                                                       
         XC    PGR1MASK,PGR1MASK                                                
         CLI   QPGR,C' '                                                        
         BE    IN030                                                            
         MVI   SDNPGRPS,1                                                       
         CLC   PGR2LEN,PGR1LEN                                                  
         BE    IN030                                                            
         ZIC   R0,PGR1LEN                                                       
         BAS   RE,SETMASK                                                       
         STH   R0,PGR1MASK                                                      
         MVI   SDNPGRPS,2                                                       
*                                                                               
IN030    MVI   SDTAX,0             DETERMINE TAX OPTION                         
         L     R6,ADAGY                                                         
         USING AGYHDRD,R6                                                       
         CLI   AGYPROF+7,C'C'      IF COUNTRY IS CANADA, TAX                    
         BE    IN040                                                            
         CLI   PROGPROF+5,C'Y'     TEST A3 PROFILE FOR TAX OPTION               
         BNE   IN050                                                            
*                                                                               
IN040    MVI   SDTAX,C'T'                                                       
*                                                                               
IN050    L     R3,MEDBUFF          FIND NUMBER OF MONTHS IN REQUEST             
         USING MEDBLOCK,R3                                                      
         L     R0,MEDNUMMO                                                      
         LA    R1,MEDMON01                                                      
         OC    0(2,R1),0(R1)                                                    
         BZ    *+12                                                             
         LA    R1,12(R1)                                                        
         BCT   R0,*-14                                                          
         L     R1,MEDNUMMO                                                      
         SR    R1,R0                                                            
         STC   R1,SDNMNTHS                                                      
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
SETMASK  LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RF,GRPMASKS-2                                                    
         LA    RF,2(RF)                                                         
         BCT   R0,*-4                                                           
         LH    R0,0(RF)                                                         
         BR    RE                                                               
*                                                                               
GRPMASKS DC    X'0FFF00FF000F'                                                  
         EJECT                                                                  
*                                                                               
*        ROUTINE TO RESOLVE ROUTINE LABELS                                      
*                                                                               
RESOLVE  LA    R1,ROUTLIST                                                      
         LA    R2,GLLABEL                                                       
*                                                                               
RV010    CLI   0(R1),X'FF'                                                      
         BER   RE                                                               
         CLC   0(8,R1),0(R2)                                                    
         BE    RV020                                                            
         LA    R1,12(R1)                                                        
         B     RV010                                                            
*                                                                               
RV020    MVC   GLAROUT,8(R1)                                                    
         BR    RE                                                               
         SPACE 2                                                                
ROUTLIST DS    0F                                                               
         DC    C'CLIENT  ',A(CLIENT)                                            
         DC    C'PRODUCT ',A(PRODI)                                             
         DC    C'PRODO   ',A(PRODO)                                             
         DC    C'ESTIMI  ',A(ESTIMI)                                            
         DC    C'ESTIMO  ',A(ESTIMO)                                            
         DC    C'MARKETI ',A(MKTI)                                              
         DC    C'MARKETO ',A(MKTO)                                              
         DC    C'STATIONI',A(STAI)                                              
         DC    C'MKTGRP1I',A(MKTGRP1I)                                          
         DC    C'MKTGRP2I',A(MKTGRP2I)                                          
         DC    C'MKTGRP3I',A(MKTGRP3I)                                          
         DC    C'MKTGRP1O',A(MKTGRP1O)                                          
         DC    C'MKTGRP2O',A(MKTGRP2O)                                          
         DC    C'MKTGRP3O',A(MKTGRP3O)                                          
         DC    C'MGRPFRST',A(RSETMODE)                                          
         DC    C'MGRPLAST',A(MGLAST)                                            
         DC    C'PRDGRP1I',A(PRDGRP1I)                                          
         DC    C'PRDGRP2I',A(PRDGRP2I)                                          
         DC    C'PRDGRP1O',A(PRDGRP1O)                                          
         DC    C'PRDGRP2O',A(PRDGRP2O)                                          
         DC    C'PGRPFRST',A(RSETMODE)                                          
         DC    C'PGRPLAST',A(PGLAST)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
INPUT    NTR1                                                                   
*                                                                               
*        ROUTINE TO EXECUTE USER ROUTINE DURING INPUT PHASE                     
*                                                                               
         L     RF,GLAROUT                                                       
         L     R3,GLAIFLD                                                       
         BR    RF                                                               
         EJECT                                                                  
OUTPUT   NTR1                                                                   
*                                                                               
*        ROUTINE TO EXECUTE USER ROUTINE DURING OUTPUT PHASE                    
*                                                                               
         L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                NO RETURN EXPECTED HERE                      
         EJECT                                                                  
*                                                                               
*        USER INPUT ROUTINES                                                    
*        R3 = A(OUTPUT AREA)                                                    
*                                                                               
CLIENT   MVC   0(3,R3),CLT                                                      
         B     EXIT                                                             
*                                                                               
PRODI    MVC   0(3,R3),=C'POL'                                                  
         CLI   BPRD,219                                                         
         BNL   EXIT                                                             
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)                                             
PRD2     CLC   BPRD,3(RF)                                                       
         BE    PRDX                                                             
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNL   PRD2                                                             
         DC    H'0'                                                             
PRDX     MVC   0(3,R3),0(RF)           ALPHA PRODUCT CODE                       
         B     EXIT                                                             
*                                                                               
MKTGRP1I MVC   0(2,R3),BMGR+1          MARKET GROUP 1                           
         OC    0(2,R3),MGR1MASK                                                 
         B     EXIT                                                             
*                                                                               
MKTGRP2I MVC   0(2,R3),BMGR+1          MARKET GROUP 2                           
         OC    0(2,R3),MGR2MASK                                                 
         B     EXIT                                                             
*                                                                               
MKTGRP3I MVC   0(2,R3),BMGR+1          MARKET GROUP 3                           
         B     EXIT                                                             
*                                                                               
PRDGRP1I MVC   0(2,R3),SVPGRKEY+6      PRODUCT GROUP 1                          
         OC    0(2,R3),PGR1MASK                                                 
         B     EXIT                                                             
*                                                                               
PRDGRP2I MVC   0(2,R3),SVPGRKEY+6      PRODUCT GROUP 2                          
         B     EXIT                                                             
*                                                                               
MKTI     MVC   0(4,R3),MKT         MARKET NUMBER                                
         B     EXIT                                                             
*                                                                               
STAI     MVC   0(5,R3),STA         STATION                                      
         B     EXIT                                                             
*                                                                               
ESTIMI   ZIC   RE,BEST             ESTIMATE NUMBER                              
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        USER OUTPUT ROUTINES                                                   
*        R2 = A(INPUT AREA)                                                     
*        R3 = A(OUTPUT AREA)                                                    
         SPACE 2                                                                
* PRODUCT OUTPUT   GLARHS=H - PRODUCT IN HEADLINE                               
*                  GLARGS=P - PRODUCT IN PRINTLINE                              
*                                                                               
PRODO    MVC   0(3,R3),0(R2)       PRODUCT                                      
         CLI   GLARGS,C'P'         PRINTLINE                                    
         BE    EXIT                                                             
         CLI   GLARGS,C'H'         HEADLINE                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(3,R3),SPACES                                                   
         MVC   PRODUCT,0(R2)                                                    
         CLC   PRODUCT,SVPROD                                                   
         BE    PRODO2                                                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),PRODUCT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETPRD                                                           
         L     R6,ADPRD                                                         
         MVC   SVPRDNM(L'PNAME),PNAME-PRDHDR(R6)                                
*                                                                               
PRODO2   MVC   PRDNM,SVPRDNM                                                    
         B     EXIT                                                             
         EJECT                                                                  
* ESTIMATE OUPUT   GLARHS=H - EST IN HEADLINE                                   
*                  GLARGS=P - EST IN PRINTLINE                                  
*                                                                               
ESTIMO   MVC   0(3,R3),SPACES                                                   
         CLC   0(3,R2),=C'000'     DON'T PRINT ESTIMATE 0                       
         BE    EXIT                                                             
         CLI   GLARGS,C'H'                                                      
         BE    ESTIMO2                                                          
         MVC   0(3,R3),0(R2)                                                    
         CLI   GLARGS,C'P'                                                      
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
ESTIMO2  MVC   ESTIMATE,0(R2)                                                   
         CLC   ESTIMATE,SVESTIM    TEST FOR CHANGE IN EST                       
         BE    ESTIMO4             NO - ALREADY HAVE NAME                       
         MVC   SVESTIM,ESTIMATE                                                 
         XC    KEY,KEY             GET ESTIMATE NAME FOR PROD POL               
         MVC   KEY+1(3),BAGYMD                                                  
         MVC   KEY+4(3),=C'POL'                                                 
         PACK  DUB,ESTIMATE                                                     
         CVB   RE,DUB                                                           
         STC   RE,KEY+7                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES ESTIMATE HDR FOR POL EXIST ?            
         BE    *+14                                                             
         MVC   SVESTNM,SPACES      NO - SET EST NAME TO SPACES                  
         B     ESTIMO4                                                          
         GOTO1 GETEST                                                           
         L     R6,ADEST                                                         
         MVC   SVESTNM(L'EDESC),EDESC-ESTHDR(R6)                                
*                                                                               
ESTIMO4  MVC   ESTNM,SVESTNM                                                    
         B     EXIT                                                             
         EJECT                                                                  
MKTGRP1O CLC   MGR1LEN,MGR2LEN     MKTGRP1 OUTPUT                               
         BNE   EXIT                TEST FOR ONE LEVEL                           
         BAS   RE,GETMGRP          YES - GET MKTGRP RECORD                      
         B     EXIT                                                             
*                                                                               
MKTGRP2O CLC   MGR2LEN,MGR3LEN     MKTGRP2 OUTPUT                               
         BNE   EXIT                TEST FOR TWO LEVELS                          
         BAS   RE,GETMGRP          YES - GET MKTGRP RECORD                      
         B     EXIT                                                             
*                                                                               
MKTGRP3O BAS   RE,GETMGRP          MKTGRP3 OUTPUT                               
         B     EXIT                GET MKTGRP RECORD                            
*                                                                               
PRDGRP1O CLC   PGR1LEN,PGR2LEN     PRDGRP1 OUTPUT                               
         BNE   EXIT                TEST FOR ONE LEVEL                           
         BAS   RE,GETPGRP          YES - GET MKTGRP RECORD                      
         B     EXIT                                                             
*                                                                               
PRDGRP2O BAS   RE,GETPGRP          PRDGRP2 OUTPUT                               
         B     EXIT                GET PRDGRP RECORD                            
*                                                                               
*                                                                               
MKTO     CLC   SVMARKET,0(R2)      MARKET OUTPUT                                
         BE    MKTO2                                                            
         MVC   SVMARKET,0(R2)      NEW MARKET - GET THE MARKET RECORD           
         CLC   SVMARKET,XFF                                                     
         BE    MKTO2                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),0(R2)                                                   
         MVC   KEY+6(2),QAGY                                                    
         MVI   KEY+8,C'0'                                                       
         MVC   KEY+9(8),KEY+8                                                   
         GOTO1 READMKT                                                          
*                                                                               
MKTO2    CLI   GLARGS,C'H'         HEADLINE - LET SPONSOR DO REST               
         BE    EXIT                                                             
         CLI   GLARGS,C'P'         PRINTLINE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SVMARKET,XFF                                                     
         BNE   *+14                                                             
         MVC   0(5,R3),=C'*ALL*'                                                
         B     EXIT                                                             
         MVC   0(4,R3),MKT                                                      
         MVC   5(19,R3),MKTNM                                                   
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* MISC ROUTINES                                                                 
*                                                                               
MGLAST   CLI   GLARGS,1            MKTGRP1 LAST                                 
         BNE   MGLAST2                                                          
         MVI   MODE,MGR1LAST       SUPPRESSES MKTGRP2 IN HEADLINE               
         B     EXIT                                                             
*                                                                               
MGLAST2  CLI   GLARGS,2            MKTGRP2 LAST                                 
         BNE   MGLAST3                                                          
         MVI   MODE,MGR2LAST       SUPPRESSES MKTGRP3 IN HEADLINE               
         B     EXIT                                                             
*                                                                               
MGLAST3  CLI   GLARGS,3            MKTGRP3 LAST                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MODE,MGR3LAST       SUPPRESSES MARKET IN HEADLINE                
         B     EXIT                                                             
         SPACE 2                                                                
PGLAST   MVI   MODE,PGR1LAST       SUPPRESSES PRDGRP2 IN HEADLINE               
         B     EXIT                                                             
         SPACE 2                                                                
RSETMODE MVI   MODE,REQFRST        RESET MODE - MAKES SURE ALL                  
         B     EXIT                             HEADLINES PRINT                 
         EJECT                                                                  
*                                                                               
*        DATA MANAGER CALL ROUTINES                                             
*                                                                               
         SPACE                                                                  
GETMGRP  NTR1                      GET MARKET GROUP RECORD                      
         CLC   0(2,R2),XFF                                                      
         BE    EXIT                                                             
         CLC   0(2,R2),=X'9999'                                                 
         BNE   GM010                                                            
         MVC   WORK(1),QMGR                                                     
         MVC   WORK+1(4),=C'9999'                                               
         MVC   MGR1,WORK                                                        
         MVC   MGR2,WORK                                                        
         MVC   MGR3,WORK                                                        
         MVC   MGR1NM,=CL24'*** UNKNOWN ***'                                    
         MVC   MGR2NM,=CL24'*** UNKNOWN ***'                                    
         MVC   MGR3NM,=CL24'*** UNKNOWN ***'                                    
         B     EXIT                                                             
*                                                                               
GM010    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         CLI   QPGR,C' '                                                        
         BNH   GM012                                                            
         MVC   KEY+5(1),QPGR                                                    
         MVC   KEY+6(2),SVPGRP                                                  
*                                                                               
GM012    MVC   KEY+8(1),QMGR                                                    
         MVC   KEY+9(2),0(R2)                                                   
*                                                                               
GM014    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GM020                                                            
         OC    KEYSAVE+6(2),KEYSAVE+6                                           
         BZ    GM015                                                            
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+6(2),KEY+6                                                   
         B     GM014                                                            
*                                                                               
GM015    CLI   KEYSAVE+5,0                                                      
         BE    GM016                                                            
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+5,0                                                          
         B     GM014                                                            
*                                                                               
GM016    CLI   QMGR,C'F'                                                        
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GM020                                                            
         DC    H'0'                                                             
*                                                                               
GM020    GOTO1 GETMKTGR                                                         
         B     EXIT                                                             
         SPACE                                                                  
GETPGRP  NTR1                      GET PRODUCT GROUP RECORD                     
         XC    SVPGRP,SVPGRP                                                    
         CLC   0(2,R2),=X'9999'                                                 
         BNE   GP010                                                            
         MVC   WORK(1),QPGR                                                     
         MVC   WORK+1(3),=C'999'                                                
         MVI   WORK+4,C' '                                                      
         MVC   PGR1,WORK                                                        
         MVC   PGR2,WORK                                                        
         MVC   PGR3,WORK                                                        
         MVC   PGR1NM,=CL24'*** UNKNOWN ***'                                    
         MVC   PGR2NM,=CL24'*** UNKNOWN ***'                                    
         MVC   PGR3NM,=CL24'*** UNKNOWN ***'                                    
         B     EXIT                                                             
*                                                                               
GP010    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),QPGR                                                    
         MVC   KEY+6(2),0(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETPRDGR                                                         
         MVC   SVPGRP,0(R2)        SAVE THE PRODUCT GROUP                       
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SDPLIST  DS    0D                                                               
         DS    4A                                                               
*                                                                               
SVPROD   DC    XL3'00'                                                          
SVESTIM  DC    XL3'00'                                                          
SVPRDNM  DS    CL24                                                             
SVESTNM  DS    CL24                                                             
SVMARKET DC    CL4'0'                                                           
SVPGRP   DS    XL2                                                              
XFF      DC    XL8'FFFFFFFFFFFFFFFF'                                            
         EJECT                                                                  
*                                                                               
*        SPOT DRIVER WORK AREA                                                  
*                                                                               
SPDRVWKC DS    0D                                                               
         DC    CL8'SPDRVWRK'                                                    
         DS    XL100                                                            
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
       ++INCLUDE SPDRVWRKD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054SPDRIVER  05/01/02'                                      
         END                                                                    
