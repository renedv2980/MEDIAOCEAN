*          DATA SET SPLDEXTAWI AT LEVEL 130 AS OF 06/16/00                      
*PHASE SPEXTAWI,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - PEEL AN AGY/CLT OFF THE SPOT FILE'                    
*                         FOR WESTERN                                           
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
*                                                                               
         CLC   =X'0E01',0(R3)      STA BILL REC                                 
         BE    RECTYP1                                                          
         CLC   =X'0C01',0(R3)      COMPETITION REC                              
         BE    RECTYP2                                                          
         BAS   RE,GETRECTP         TRY TO DEFINE REC TYPE                       
         BNE   DMXREC10                                                         
         CLI   TYPESW,1                                                         
         BE    RECTYP1                                                          
         CLI   TYPESW,2                                                         
         BE    RECTYP2                                                          
         CLI   TYPESW,3                                                         
         BE    RECTYP3                                                          
         CLI   TYPESW,4                                                         
         BE    RECTYP4                                                          
         CLI   TYPESW,5                                                         
         BE    RECTYP5                                                          
         DC    H'0'                SHOULD BE TYPE 1-5                           
DMXREC10 CLI   0(R3),X'00'         AGY/CLT/PRD/EST/BILL                         
         BE    RECTYP6                                                          
         CLI   0(R3),X'02'         GOAL RECORD                                  
         BE    RECTYP6                                                          
         CLI   0(R3),X'37'         PROD EQUIVOA REC                             
         BE    RECTYP6                                                          
         CLI   0(R3),X'B7'         EQUIV PRODOA REC                             
         BE    RECTYP6                                                          
         CLI   0(R3),X'10'         BUY RECORD                                   
         BH    RECTYP7                                                          
         CLI   0(R3),X'03'         SYND RECORD                                  
         BE    RECTYP8                                                          
         CLI   0(R3),X'05'         ADVH RECORD                                  
         BE    RECTYP9                                                          
         CLI   0(R3),X'06'         AGYH RECORD                                  
         BE    RECTYP10                                                         
         CLI   0(R3),X'08'         DPTH RECORD                                  
         BE    RECTYP10                                                         
         CLI   0(R3),X'07'         PROFH RECORD                                 
         BE    RECTYP11                                                         
         CLI   0(R3),X'09'         EQUH RECORD                                  
         BE    RECTYP12                                                         
         CLI   0(R3),X'0B'         INVC RECORD                                  
         BE    RECTYP13                                                         
         CLC   =X'0D03',0(R3)      MKT ASGNMT REC                               
         BE    RECTYP14                                                         
         CLC   =X'0D11',0(R3)      NETW CAN REC                                 
         BE    RECTYP15                                                         
         CLC   =X'0D13',0(R3)      SPIL CAN REC                                 
         BE    RECTYP16                                                         
         CLC   =X'0D31',0(R3)      ADDS RECORD                                  
         BE    RECTYP17                                                         
         CLC   =X'0D34',0(R3)      DARE RECORD                                  
         BE    RECTYP18                                                         
         CLC   =X'0D37',0(R3)      DARE RECORD                                  
         BE    RECTYP19                                                         
         CLC   =X'0A2B',0(R3)      FEED RECORD                                  
         BE    RECTYP19                                                         
         CLC   =X'0D3A',0(R3)      AUTOPAY REC                                  
         BE    RECTYP20                                                         
         CLC   =X'0D43',0(R3)      NV LETR REC                                  
         BE    RECTYP21                                                         
         CLC   =X'0DE3',0(R3)      CLT/BUYER REC                                
         BE    RECTYP22                                                         
         CLC   =X'0A41',0(R3)      CLT LIST REC                                 
         BE    RECTYP23                                                         
         CLI   0(R3),X'0C'         0C RECORDS                                   
         BE    RECTYP24                                                         
         B     DMXKX                                                            
*                                                                               
RECTYP1  DS    0H                  XX XX A/M CLT CLT                            
         MVC   BYTE,2(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    3(2,R3),3(R3)       CLT                                          
         BZ    CHANGE1                                                          
         LA    R5,3(R3)                                                         
CHANGE1  BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
         B     DMXKX                                                            
*                                                                               
RECTYP2  DS    0H                  XX XX A/M                                    
         MVC   BYTE,2(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         B     DMXKX                                                            
*                                                                               
RECTYP3  DS    0H                  XX XX A/M XX CLT CLT                         
         MVC   BYTE,2(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    4(2,R3),4(R3)       CLT                                          
         BZ    CHANGE3                                                          
         LA    R5,4(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE3  B     DMXKX                                                            
*                                                                               
RECTYP4  DS    0H                  XX XX AGY AGY                                
         CLC   2(2,R3),=C'WI'      AGY=WI                                       
         BNE   DMXPURGE                                                         
         MVC   2(2,R3),=C'WI'      CHANGE TO 'WI'                               
         B     DMXKX                                                            
*                                                                               
RECTYP5  DS    0H                  XX XX A/M 5XX'S CLT CLT                      
         MVC   BYTE,2(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    8(2,R3),8(R3)       CLT                                          
         BZ    CHANGE5                                                          
         LA    R5,8(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE5  B     DMXKX                                                            
*                                                                               
RECTYP6  DS    0H                  XX A/M CLT CLT                               
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVC   BYTE,1(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    2(2,R3),2(R3)       CLT                                          
         BZ    CHANGE6                                                          
         LA    R5,2(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE6  B     DMXKX                                                            
*                                                                               
RECTYP7  DS    0H                  A/M CLT CLT                                  
         MVC   BYTE,0(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    1(2,R3),1(R3)       CLT                                          
         BZ    CHANGE7                                                          
         LA    R5,1(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE7  B     DMXKX                                                            
*                                                                               
RECTYP8  DS    0H                  AGY ?????                                    
         B     DMXKX                                                            
*                                                                               
RECTYP9  DS    0H                  XX CLT CLT                                   
         B     DMXKX                                                            
*                                                                               
RECTYP10 DS    0H                  XX AGY AGY                                   
         CLC   1(2,R3),=C'WI'      AGY=WI                                       
         BNE   DMXPURGE                                                         
         MVC   1(2,R3),=C'WI'                                                   
         B     DMXKX                                                            
*                                                                               
RECTYP11 DS    0H                  XX XX XX AGY AGY XX CLT CLT CLT              
         CLC   3(2,R3),=C'WI'      AGY=WI                                       
         BNE   DMXPURGE                                                         
         OC    6(3,R3),6(R3)                                                    
         BZ    CHANGE11                                                         
         LA    R5,6(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE11 MVC   3(2,R3),=C'WI'      CHANGE WI FOR WI                             
         B     DMXKX                                                            
*                                                                               
RECTYP12 DS    0H                  XX AGY AGY XX CLT CLT CLT                    
         CLC   1(2,R3),=C'WI'      AGY=WI                                       
         BNE   DMXPURGE                                                         
         OC    4(3,R3),4(R3)                                                    
         BZ    CHANGE12                                                         
         LA    R5,4(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE12 MVC   1(2,R3),=C'WI'      CHANGE WI FOR WI                             
         B     DMXKX                                                            
*                                                                               
RECTYP13 DS    0H                  XX A/M XX XX XX CLT CLT                      
         MVC   BYTE,1(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    5(2,R3),5(R3)       CLT                                          
         BZ    CHANGE13                                                         
         LA    R5,5(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE13 B     DMXKX                                                            
*                                                                               
RECTYP14 DS    0H                  8XX'S A/M CLT CLT                            
         MVC   BYTE,8(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    9(2,R3),9(R3)       CLT                                          
         BZ    CHANGE14                                                         
         LA    R5,9(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE14 B     DMXKX                                                            
*                                                                               
RECTYP15 DS    0H                  XX XX AGY AGY 4XX'S CLT CLT                  
         CLC   2(2,R3),=C'WI'      AGY=WI                                       
         BNE   DMXPURGE                                                         
         OC    8(2,R3),8(R3)       CLT                                          
         BZ    CHANGE15                                                         
         LA    R5,8(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE15 MVC   2(2,R3),=C'WI'                                                   
         B     DMXKX                                                            
*                                                                               
RECTYP16 DS    0H                  XX XX AGY AGY 6XX'S CLT CLT                  
         CLC   2(2,R3),=C'WI'      AGY=WI                                       
         BNE   DMXPURGE                                                         
         OC    10(2,R3),10(R3)     CLT                                          
         BZ    CHANGE16                                                         
         LA    R5,10(R3)                                                        
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE16 MVC   2(2,R3),=C'WI'                                                   
         B     DMXKX                                                            
*                                                                               
RECTYP17 DS    0H                  XX XX A/M XX XX XX CLT CLT                   
         MVC   BYTE,2(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    6(2,R3),6(R3)       CLT                                          
         BZ    CHANGE17                                                         
         LA    R5,6(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE17 B     DMXKX                                                            
*                                                                               
RECTYP18 DS    0H                  XX XX XX XX A/M                              
         MVC   BYTE,4(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         B     DMXKX                                                            
*                                                                               
RECTYP19 DS    0H                  XX XX A/M 4XX'S CLT CLT                      
         MVC   BYTE,2(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    7(2,R3),7(R3)       CLT                                          
         BZ    CHANGE19                                                         
         LA    R5,7(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE19 B     DMXKX                                                            
*                                                                               
RECTYP20 DS    0H                  XX XX XX XX A/M CLT CLT                      
         MVC   BYTE,4(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    5(2,R3),5(R3)       CLT                                          
         BZ    CHANGE20                                                         
         LA    R5,5(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE20 B     DMXKX                                                            
*                                                                               
RECTYP21 DS    0H                  XX XX AGY AGY 3XX'S CLT CLT                  
         CLC   2(2,R3),=C'WI'      AGY=WI                                       
         BNE   DMXPURGE                                                         
         OC    7(2,R3),7(R3)       CLT                                          
         BZ    CHANGE21                                                         
         LA    R5,7(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE21 MVC   2(2,R3),=C'WI'                                                   
         B     DMXKX                                                            
*                                                                               
RECTYP22 DS    0H                  XX XX A/M XX XX CLT CLT                      
         MVC   BYTE,2(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    5(2,R3),5(R3)       CLT                                          
         BZ    CHANGE22                                                         
         LA    R5,7(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE22 B     DMXKX                                                            
*                                                                               
RECTYP23 DS    0H                  XX XX A/M CLT CLT CLT                        
         MVC   BYTE,2(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         OC    3(3,R3),3(R3)                                                    
         BZ    CHANGE23                                                         
         LA    R5,3(R3)                                                         
         BAS   RE,CKHEXCLT                                                      
         BNE   DMXPURGE                                                         
CHANGE23 B     DMXKX                                                            
*                                                                               
RECTYP24 DS    0H                  0C A/M                                       
         MVC   BYTE,1(R3)          AGY HEX                                      
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'10'          WI = X'10'                                   
         BNE   DMXPURGE                                                         
         B     DMXKX                                                            
*                                                                               
*                                                                               
DMXKX    DS    0H                  CHANGE AGY TO WI                             
*        GOTO1 =V(HEXOUT),DMCB,0(R3),P,12                                       
*        GOTO1 VPRINTER                                                         
*        CLC   =C'WI',20(R3)       CHANGE AGY TO WI                             
*        BNE   *+10                                                             
*        MVC   20(2,R3),=C'WI'                                                  
*        B     DMXKEEP                                                          
*                                                                               
*                                                                               
DMXR30   DS    0H                                                               
         AP    NUMRECS,=P'1'                                                    
         B     DMXKX                                                            
*                                                                               
RECPRNT  NTR1                                                                   
*        LA    R4,=CL20'GOAL RECORD'                                            
*        SR    R5,R5               PRINT OUT RECORD                             
*        ICM   R5,3,13(R3)                                                      
*        GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
*        GOTO1 VPRINTER                                                         
*        GOTO1 VPRINTER                                                         
*        LA    R4,=CL20'HEADER REC'                                             
*        SR    R5,R5               PRINT OUT RECORD                             
*        ICM   R5,3,13(R3)                                                      
*        GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
*        GOTO1 VPRINTER                                                         
*        GOTO1 VPRINTER                                                         
*        XIT1                                                                   
*        EJECT                                                                  
*                                                                               
** SUB TRYES TO DEFINE REC TYPE USING RECTBL **                                 
** IF SUCCESFUL XIT WITH CC =, OTHERWISE =/= **                                 
GETRECTP NTR1                                                                   
         XC    TYPESW,TYPESW       TYPE INDICATOR                               
         LHI   R0,1                INITIALIZE TYPE COUNTER                      
         LA    R2,RECTBL           SET DEFAULT TABLE ADDR                       
         CLI   0(R3),X'0D'                                                      
         BE    GETREC10                                                         
         CLI   0(R3),X'0A'                                                      
         BNE   GETREC50            NOT 0D, 0A, EXIT WITH CC =/=                 
         LA    R2,OATYP1                                                        
*                                                                               
GETREC10 CLC   1(1,R3),0(R2)       LOOK FOR MATCH IN TAB                        
         BE    GETREC30                                                         
         LA    R2,1(R2)                                                         
         CLI   0(R2),0             CK EOT                                       
         BE    GETREC50                                                         
         CLI   0(R2),X'FF'         CK END OF TYPE                               
         BNE   GETREC10                                                         
         AHI   R0,1                YES, INCREMENT TO NXT TYPE                   
         B     GETREC10                                                         
*                                                                               
GETREC30 STC   R0,TYPESW                                                        
         CR    RB,RB               SET CC =                                     
         B     GETRECX                                                          
*                                                                               
GETREC50 CR    RA,RB               SET CC =/=                                   
*                                                                               
GETRECX  XIT1                                                                   
*                                                                               
*                                                                               
CKHEXCLT NTR1                                                                   
         LA    R2,CLTTBLM                                                       
CKCLT10  CLC   3(2,R2),0(R5)                                                    
         BE    XITEQ                                                            
         LA    R2,5(R2)                                                         
         CLC   =X'FFFF',0(R2)                                                   
         BE    XITNEQ                                                           
         B     CKCLT10                                                          
*                                                                               
XITEQ    CR    RB,RB                                                            
         B     *+6                                                              
XITNEQ   CR    RA,RB                                                            
*                                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
CKCHARCL NTR1                                                                   
         LA    R2,CLTTBLM                                                       
CKCHAR10 CLC   0(3,R2),0(R5)                                                    
         BE    EQ                                                               
         LA    R2,5(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BE    NEQ                                                              
         B     CKCHAR10                                                         
*                                                                               
EQ       CR    RB,RB                                                            
         B     *+6                                                              
NEQ      CR    RA,RB                                                            
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(16),=C'NUMBER OF RECS ='                                       
         EDIT  (P6,NUMRECS),(12,P+25)                                           
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
DATADISP DC    H'0024'                                                          
*                                                                               
*                                                                               
NUMRECS  DC    PL6'0'                                                           
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
*                                                                               
***      TAB OF REC TYPES ACCORDING TO AGY AND CLT POSITION IN RECORD           
*                                                                               
RECTBL   DS    0H                                                               
*                                                                               
ODTYP1   DC    X'0102810D152728383940'    ODXX A/M CLT CLT                      
         DC    X'444548494A4E5357585C'                                          
         DC    X'696F7071727678797A7C'                                          
         DC    X'9091'                                                          
         DC    X'FF'                                                            
ODTYP1LQ EQU   *-ODTYP1                                                         
*                                                                               
ODTYP2   DC    X'040506141620264F3233'       0DXX A/M                           
         DC    X'363B3C464750525A5B5E'                                          
         DC    X'606162E265E56667686B'                                          
         DC    X'6C737B7D7EFEFD'                                                
         DC    X'FF'                                                            
ODTYP2LQ EQU   *-ODTYP2                                                         
*                                                                               
ODTYP3   DC    X'0C17'                       ODXX A/M XX CLT CLT                
         DC    X'FF'                                                            
ODTYP3LQ EQU   *-ODTYP3                                                         
*                                                                               
ODTYP4   DC    X'12224B77'                   ODXX AGY AGY                       
         DC    X'FF'                                                            
ODTYP4LQ EQU   ODTYP4                                                           
*                                                                               
ODTYP5   DC    X'3575'                       ODXX A/M,5X,CLT CLT                
         DC    X'00'                                                            
ODTYP5LQ EQU   *-ODTYP5                                                         
*                                                                               
******                                                                          
OATYP1   DC    X'2122232425272A2E2F34'                                          
         DC    X'353637442C2D313242'                                            
         DC    X'FF'                                                            
OATYP1LQ EQU   *-OATYP1                                                         
*                                                                               
OATYP2   DC    X'26282943'                                                      
         DC    X'00'                                                            
OATYP2LQ EQU   *-OATYP2                                                         
***                                                                             
*                                                                               
*        THESE ARE THE CLIENTS TO KEEP !!!                                      
*                                                                               
CLTTBLM  DS    0H                                                               
         DC    C'BVT',X'86B3'                                                   
         DC    C'CHE',X'88E4'                                                   
         DC    C'CJR',X'8931'                                                   
         DC    C'CTS',X'8A72'                                                   
         DC    C'ELA',X'9160'                                                   
         DC    C'EUR',X'9291'                                                   
         DC    C'GTW',X'9A76'                                                   
         DC    C'HDE',X'9C64'                                                   
         DC    C'LGE',X'ACC4'                                                   
         DC    C'MBL',X'B02B'                                                   
         DC    C'MRX',X'B237'                                                   
         DC    C'PKB',X'BD41'                                                   
         DC    C'WDP',X'D86F'                                                   
         DC    C'WDW',X'D876'                                                   
         DC    C'WHV',X'D8F5'                                                   
         DC    X'FFFF'                                                          
*                                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
BYTE     DS    XL1                                                              
FLAG     DS    X                                                                
TYPESW   DS    X                                                                
NUMBUYS  DS    PL6                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
*                                                                               
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
*SPGENBUY                                                                       
SPGENBUYD      DSECT                                                            
       ++INCLUDE SPGENBUY                                                       
*SPGENEST                                                                       
SPGENESTD      DSECT                                                            
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'130SPLDEXTAWI06/16/00'                                      
         END                                                                    
