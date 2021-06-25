*          DATA SET MZEIEXT    AT LEVEL 103 AS OF 03/29/00                      
*PHASE SPEXTSD1                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
         TITLE 'DMLDEXT - PEEL AN AGY/CLT OFF THE SPOT FILE'                    
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALIZE                           
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
* INITIALIZE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   DS    0H                                                               
         L     R3,AREC                                                          
*                                                                               
         BAS   RE,GETRECTP         TRY TO DEFINE REC TYPE                       
*                                                                               
DMXREC10 DS    0H                                                               
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
         MVC   BAMCLT,2(R3)        A/M & CLT                                    
         B     DMXKX                                                            
*                                                                               
RECTYP3  DS    0H                  XX XX A/M XX CLT CLT                         
         MVC   BAGYMD,2(R3)        A/M                                          
         MVC   BCLT,4(R3)                                                       
         B     DMXKX                                                            
*                                                                               
RECTYP5  DS    0H                  XX XX A/M 5XX'S CLT CLT                      
         MVC   BAGYMD,2(R3)        AGY HEX                                      
         B     DMXKX                                                            
*                                                                               
RECTYP6  DS    0H                  XX A/M CLT CLT                               
         MVC   BAMCLT,1(R3)        A/M & CLT                                    
         B     DMXKX                                                            
*                                                                               
RECTYP7  DS    0H                  A/M CLT CLT                                  
         MVC   BAMCLT,0(R3)        A/M & CLT                                    
         B     DMXKX                                                            
*                                                                               
RECTYP8  DS    0H                  ?????????                                    
         B     DMXPURGE                                                         
*                                                                               
RECTYP9  DS    0H                  XX CLT CLT                                   
         B     DMXPURGE            ??????????                                   
*                                                                               
RECTYP10 DS    0H                  NO CLIENT                                    
         B     DMXPURGE                                                         
*                                                                               
RECTYP11 DS    0H                  XX XX XX AGY AGY MED CLT CLT                 
* !!!! CONVERT AGY AGY MED TO A/M                                               
         B     DMXKX               ?????????                                    
*                                                                               
RECTYP12 DS    0H                  XX AGY AGY XX CLT CLT CLT                    
         B     DMXKX               ?????????                                    
*                                                                               
RECTYP13 DS    0H                  XX A/M XX XX XX CLT CLT                      
         MVC   BAGYMD,1(R3)        A/M                                          
         B     DMXKX                                                            
*                                                                               
RECTYP14 DS    0H                  8XX'S A/M CLT CLT                            
         MVC   BAMCLT,8(R3)        A/M & CLT                                    
         B     DMXKX                                                            
*                                                                               
RECTYP15 DS    0H                  XX XX AGY AGY 4XX'S CLT CLT                  
         B     DMXKX                                                            
*                                                                               
RECTYP16 DS    0H                  XX XX AGY AGY 6XX'S CLT CLT                  
         MVC   BCLT,10(R3)         CLT                                          
         B     DMXKX                                                            
*                                                                               
RECTYP17 DS    0H                  XX XX A/M XX XX XX CLT CLT                   
         B     DMXKX                                                            
*                                                                               
RECTYP18 DS    0H                  XX XX XX XX A/M                              
         MVC   BAGYMD,4(R3)        AGY HEX                                      
* !!! CLIENT IN RECORD - 0D34                                                   
         B     DMXKX                                                            
*                                                                               
RECTYP19 DS    0H                  XX XX A/M 4XX'S CLT CLT                      
         MVC   BAGYMD,2(R3)        A/M                                          
         B     DMXKX                                                            
*                                                                               
RECTYP20 DS    0H                  XX XX XX XX A/M CLT CLT                      
         MVC   BAMCLT,4(R3)        A/M & CLT                                    
         B     DMXKX                                                            
*                                                                               
RECTYP21 DS    0H                  XX XX AGY AGY 3XX'S CLT CLT                  
         MVC   BCLT,7(R3)          CLT                                          
         B     DMXKX                                                            
*                                                                               
RECTYP22 DS    0H                  XX XX A/M XX XX CLT CLT                      
         MVC   BAGYMD,2(R3)        A/M                                          
         MVC   BCLT,5(R3)          CLT                                          
         B     DMXKX                                                            
*                                                                               
RECTYP23 DS    0H                  XX XX A/M CLT CLT CLT                        
         MVC   BAGYMD,2(R3)        A/M                                          
* !!!! CLIENT IS NOT BINARY  !!!                                                
         B     DMXKX                                                            
*                                                                               
RECTYP24 DS    0H                  NO CLIENT                                    
         B     DMXPURGE                                                         
*                                                                               
DMXKX    DS    0H                                                               
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
DMXR30   DS    0H                                                               
         AP    NUMRECS,=P'1'                                                    
         B     DMXKX                                                            
*                                                                               
RECPRNT  NTR1                                                                   
         LA    R4,=CL20'GOAL RECORD'                                            
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         LA    R4,=CL20'HEADER REC'                                             
         SR    R5,R5               PRINT OUT RECORD                             
         ICM   R5,3,13(R3)                                                      
         GOTO1 =V(PRNTBL),DMCB,(20,(R4)),(R3),C'DUMP',(R5),=C'2D'               
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
** SUB TRIES TO DEFINE REC TYPE USING RECTBL **                                 
** IF SUCCESFUL XIT WITH CC =, OTHERWISE =/= **                                 
GETRECTP NTR1                                                                   
         LA    R2,RECTBL           SET DEFAULT TABLE ADDR                       
         CLI   0(R3),X'0D'                                                      
         BE    GETREC05                                                         
         LA    R2,OATYP1                                                        
         CLI   0(R3),X'0A'                                                      
         BE    GETREC05                                                         
         LA    R2,OETYP1                                                        
         CLI   0(R3),X'0E'                                                      
         BNE   GETREC07                                                         
GETREC05 MVC   BYTE,1(R3)          0D,0A,0E RECORDS                             
         B     GETREC15                                                         
GETREC07 LA    R2,TYPTAB           1 BYTE KEY TYPE RECORDS                      
         MVC   BYTE,0(R3)                                                       
         B     GETREC15                                                         
*                                                                               
GETREC10 CLC   0(1,R2),BYTE        LOOK FOR MATCH IN TAB                        
         BE    GETREC30                                                         
         LA    R2,1(R2)                                                         
         CLC   =X'FFFF',0(R2)      CK EOT                                       
         BE    GETREC50                                                         
         CLI   0(R2),X'FF'         CK END OF TYPE                               
         BNE   GETREC10                                                         
         LA    R2,1(R2)                                                         
GETREC15 ICM   RF,15,0(R2)         ADDRESS OF ROUTINE                           
         LA    R2,4(R2)            ADVANCE TO TYPES                             
         B     GETREC10                                                         
*                                                                               
GETREC30 A     RF,RELO                                                          
         BASR  RE,RF               BRANCH TO ROUTINE                            
*                                                                               
GETREC50 DC    0H                  NO MATCH PURGE???                            
GETRECX  XIT1                                                                   
*                                                                               
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
ODTYP1   DC    AL4(RECTYP1)                                                     
         DC    X'0102810D152728383940'    ODXX A/M CLT CLT                      
         DC    X'444548494A4E5357585C'                                          
         DC    X'696F7071727678797A7C'                                          
         DC    X'9091'                                                          
         DC    X'FF'                                                            
*                                                                               
ODTYP2   DC    AL4(RECTYP2)        PURGE                                        
         DC    X'04050614162026313233'       0DXX A/M                           
         DC    X'363B3C464750525A5B5E'                                          
         DC    X'606162E265E56667686B'                                          
         DC    X'6C737B7D7EFEFD4F'                                              
         DC    X'FF'                                                            
*                                                                               
ODTYP3   DC    AL4(RECTYP3)                                                     
         DC    X'0C17'                       ODXX A/M XX CLT CLT                
         DC    X'FF'                                                            
*                                                                               
ODTYP4   DC    AL4(RECTYP4)        PURGE                                        
         DC    X'12224B77'                   ODXX AGY AGY                       
         DC    X'FF'                                                            
*                                                                               
ODTYP5   DC    AL4(RECTYP5)                                                     
         DC    X'3575'                       ODXX A/M,5X,CLT CLT                
         DC    X'FF'                                                            
*                                                                               
ODTYP14  DC    AL4(RECTYP14)                                                    
         DC    X'03FF'                                                          
*                                                                               
ODTYP15  DC    AL4(RECTYP15)                                                    
         DC    X'11FF'                                                          
*                                                                               
ODTYP16  DC    AL4(RECTYP16)                                                    
         DC    X'13FF'                                                          
*                                                                               
ODTYP17  DC    AL4(RECTYP17)                                                    
         DC    X'31FF'                                                          
*                                                                               
ODTYP18  DC    AL4(RECTYP18)                                                    
         DC    X'34FF'                                                          
*                                                                               
ODTYP19  DC    AL4(RECTYP19)                                                    
         DC    X'37FF'                                                          
*                                                                               
ODTYP20  DC    AL4(RECTYP20)                                                    
         DC    X'3AFF'                                                          
*                                                                               
ODTYP21  DC    AL4(RECTYP21)                                                    
         DC    X'43FF'                                                          
*                                                                               
ODTYP22  DC    AL4(RECTYP22)                                                    
         DC    X'E3FFFF'                                                        
*                                                                               
*                                                                               
******                                                                          
OATYP1   DC    AL4(RECTYP1)                                                     
         DC    X'2122232425272A2E2F34'                                          
         DC    X'353637442C2D313242'                                            
         DC    X'FF'                                                            
*                                                                               
OATYP2   DC    AL4(RECTYP2)        PURGE                                        
         DC    X'26282943FF'                                                    
*                                                                               
OATYP19  DC    AL4(RECTYP19)                                                    
         DC    X'2BFF'                                                          
*                                                                               
OATYP23  DC    AL4(RECTYP23)                                                    
         DC    X'41FFFF'                                                        
*                                                                               
OCTYP24  DC    AL4(RECTYP24)                                                    
         DC    X'41FFFF'                                                        
         CLI   0(R3),X'0C'         0C RECORDS                                   
         BE    RECTYP24                                                         
***                                                                             
OETYP1   DC    AL4(RECTYP1)                                                     
         DC    X'01'                                                            
         DC    X'FFFF'                                                          
*                                                                               
TYPTAB   DC    AL4(RECTYP6)                                                     
         DC    X'000237B7FF'                                                    
         DC    AL4(RECTYP7)                                                     
         DC    X'10FF'                                                          
         DC    AL4(RECTYP8)                                                     
         DC    X'03FF'                                                          
         DC    AL4(RECTYP9)                                                     
         DC    X'05FF'                                                          
         DC    AL4(RECTYP10)                                                    
         DC    X'0608FF'                                                        
         DC    AL4(RECTYP11)                                                    
         DC    X'07FF'                                                          
         DC    AL4(RECTYP12)                                                    
         DC    X'09FF'                                                          
         DC    AL4(RECTYP13)                                                    
         DC    X'0BFFFF'                                                        
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
*                                                                               
BAMCLT   DS    0XL3                                                             
BAGYMD   DS    XL1                                                              
BCLT     DS    XL2                                                              
*                                                                               
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
**PAN#1  DC    CL21'103MZEIEXT   03/29/00'                                      
         END                                                                    
