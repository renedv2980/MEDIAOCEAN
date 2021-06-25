*          DATA SET PZSPXTNE   AT LEVEL 032 AS OF 05/01/02                      
*PHASE PZSPXTNA PZSPXTNE                                                        
*INCLUDE CLPACK                                                                 
*INCLUDE BINSRCH                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'PZSPXTNE - CHANG NE AGENCY TO RP AGENCY'                        
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
PZSPXTNE CSECT                                                                  
         NMOD1 20,DMLDEXT,RR=R2                                                 
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     R4,=V(HEXOUT)                                                    
         AR    R4,R2                                                            
         ST    R4,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    PROCREC             PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
PUTPURGE L     R3,AREC                                                          
         PUT   FILEOUT,(R3)        WRITE TO SECOND OUTPUT TAPE                  
         B     DMXPURGE                                                         
*                                                                               
PUTKEEP AP     0(4,R5),=P'1'       ADD TO REC ACCUMULATOR                       
         NI    0(R8),X'0F'         CHANGE AGENCY CODE                           
         OI    0(R8),X'D0'                                                      
         L     R5,AREC                                                          
         SH    R5,=H'4'                                                         
         PUT   FILEOUT,(R5)        WRITE TO SECOND OUTPUT TAPE                  
         NI    0(R8),X'0F'         CHANGE BACK AGENCY CODE                      
         OI    0(R8),X'90'                                                      
         B     DMXKEEP                                                          
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
* SET UP CLIENT TABLES                                                          
         LA    R5,CLIST1                                                        
         LA    R6,33                                                            
*                                                                               
INIT20   EQU   *                                                                
         GOTO1 =V(CLPACK),DMCB,0(R5),3(R5)                                      
         LA    R5,5(R5)                                                         
         BCT   R6,INIT20                                                        
* OPEN THE TAPE FILE                                                            
         OPEN  (FILEOUT,(OUTPUT))                                               
* EXIT INITIAL ROUTINE                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
* PROCESS RECORD LOGIC                                                          
*                                                                               
PROCREC  DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
*                                                                               
* 0 IN KEY CODE                                                                 
         LA    R5,RECCOUNT                                                      
         LA    R8,1(R3)            A/M CODE                                     
         LA    R9,2(R3)            CLIENT CODE                                  
         CLI   0(R3),0                                                          
         BE    ALTREC                                                           
*                                                                               
* 2 IN KEY CODE                                                                 
         LA    R5,RECCOUNT+24                                                   
         LA    R8,1(R3)            A/M CODE                                     
         LA    R9,2(R3)            CLIENT CODE                                  
         CLI   0(R3),2                                                          
         BE    ALTREC                                                           
*                                                                               
* TRAFFIC RECORD                                                                
         LA    R5,RECCOUNT+48                                                   
         LA    R8,2(R3)            A/M CODE                                     
         LA    R9,3(R3)            CLIENT CODE                                  
         CLI   0(R3),10            TEST FOR TRAFFIC                             
         BE    ALTREC                                                           
*                                                                               
* BUY RECORD                                                                    
         LA    R5,RECCOUNT+72                                                   
         LA    R8,0(R3)            A/M CODE                                     
         LA    R9,1(R3)            CLIENT CODE                                  
         TM    0(R3),X'F0'         PROCESS BUY RECORD                           
         BNZ   ALTREC                                                           
*                                                                               
* INVOICE RECORD                                                                
         LA    R5,RECCOUNT+96                                                   
         LA    R8,1(R3)            A/M CODE                                     
         LA    R9,5(R3)            CLIENT CODE                                  
         CLI   0(R3),11            PROCESS INVOICE RECORD                       
         BE    ALTREC                                                           
*                                                                               
* PRODUCT GROUP RECORD                                                          
         LA    R5,RECCOUNT+120                                                  
         LA    R8,2(R3)            A/M CODE                                     
         LA    R9,3(R3)            CLIENT CODE                                  
         CLC   0(2,R3),=X'0D01'    PROCESS PRODUCT GROUP RECORD                 
         BE    ALTREC                                                           
         CLC   0(2,R3),=X'0D81'    PROCESS PRODUCT GROUP RECORD                 
         BE    ALTREC                                                           
*                                                                               
* STATION BILL RECORD                                                           
         LA    R5,RECCOUNT+144                                                  
         LA    R8,2(R3)            A/M CODE                                     
         LA    R9,3(R3)            CLIENT CODE                                  
         CLI   0(R3),14            STATION BILL RECORD                          
         BE    ALTREC                                                           
*                                                                               
* DAYPART HEADER RECORD                                                         
         LA    R5,RECCOUNT+168                                                  
         CLI   0(R3),8             DAYPART HEADER RECORD                        
         BNE   STAREC                                                           
         AP    0(4,R5),=P'1'                                                    
         BE    DMXKEEP                                                          
         EJECT                                                                  
*                                                                               
* STATION ADDRESS RECORD                                                        
STAREC   LA    R5,RECCOUNT+192                                                  
         CLC   0(2,R3),=X'0A28'      STATION ADDRESS RECORD                     
         BE    ALTR80                                                           
*                                                                               
* MARKET GROUP RECORD                                                           
         LA    R5,RECCOUNT+216                                                  
         LA    R8,2(R3)            A/M CODE                                     
         LA    R9,3(R3)            CLIENT CODE                                  
         CLC   0(2,R3),=X'0D02'    TEST FOR MARKET GROUP                        
         BE    TYPMKG                                                           
*                                                                               
* MARKET GROUP RECORD                                                           
         LA    R5,RECCOUNT+216                                                  
         LA    R8,2(R3)            A/M CODE                                     
         LA    R9,3(R3)            CLIENT CODE                                  
         CLC   0(2,R3),=X'0D82'    TEST FOR MARKET GROUP                        
         BE    TYPMKG                                                           
*                                                                               
* MARKET ASSIGNMENT RECORD                                                      
         LA    R5,RECCOUNT+240                                                  
         LA    R8,8(R3)            A/M CODE                                     
         LA    R9,9(R3)            CLIENT CODE                                  
         CLC   0(2,R3),=X'0D03'    TEST FOR MARKET ASSIGNMENT RECORD            
         BE    TYPMKG                                                           
*                                                                               
* EQUIVALENCY HEADER RECORD                                                     
         LA    R5,RECCOUNT+264                                                  
         LA    R8,1(R3)            AGENCY MEDIA CODE                            
         LA    R9,4(R3)            CLIENT CODE                                  
         CLI   0(R3),X'09'         TEST FOR EQUIVALENCY HEADER RECORD           
         BE    TYPEQU                                                           
*                                                                               
* PROGRAM RECORD                                                                
         LA    R5,RECCOUNT+288                                                  
         LA    R8,2(R3)                                                         
         CLC   0(3,R3),=X'0D2093'   TEST FOR PROGRAM RECORD/NEEDHAM             
         BE    PUTKEEP                                                          
*                                                                               
* UNIVERSE RECORD                                                               
UNIVREC  LA    R5,RECCOUNT+312                                                  
         CLC   0(4,R3),=X'0D22D5C5'  TEST FOR UNIVERSE RECORD/NEEDHAM           
         BNE   HUTREC                                                           
         AP    0(4,R5),=P'1'                                                    
         MVC   2(2,R3),=C'RP'                                                   
         LR    R5,R3                                                            
         SH    R5,=H'4'                                                         
         PUT   FILEOUT,(R5)                                                     
         MVC   2(2,R3),=C'NE'                                                   
         B     DMXKEEP                                                          
*                                                                               
* HUT RECORD                                                                    
HUTREC   LA    R5,RECCOUNT+336                                                  
         LA    R8,2(R3)            AGENCY MEDIA CODE                            
         CLC   0(3,R3),=X'0D5093'     TEST FOR HUT RECORD                       
         BE    PUTKEEP                                                          
*                                                                               
         B     DMXKEEP                                                          
         EJECT                                                                  
*  HANDLES TYPE 0 OR TYPE 2 RECORDS                                             
*  R8=A(A/M CODE),  R9=A(CLIENT CODE)                                           
ALTREC   EQU   *                                                                
         LA    R6,CLIST1                                                        
         CLI   0(R8),X'91'                                                      
         BNE   ALTR20                                                           
         BAS   R7,CKCLT                                                         
*                                                                               
         CLI   DMCB,X'01'                                                       
         BE    DMXKEEP                                                          
         B     ALTR80                                                           
*                                                                               
ALTR20   LA    R6,CLIST2                                                        
         CLI   0(R8),X'92'                                                      
         BNE   ALTR40                                                           
         BAS   R7,CKCLT                                                         
*                                                                               
         CLI   DMCB,X'01'                                                       
         BE    DMXKEEP                                                          
         B     ALTR80                                                           
*                                                                               
ALTR40   LA    R6,CLIST4                                                        
         CLI   0(R8),X'94'                                                      
         BNE   ALTR50                                                           
*                                                                               
         CLC   0(2,R9),3(R6)                                                    
         BE    ALTR80                                                           
         LA    R6,5(R6)                                                         
         CLC   0(2,R9),3(R6)                                                    
         BE    ALTR80                                                           
*                                                                               
ALTR50   LA    R6,CLIST5                                                        
         CLI   0(R8),X'93'                                                      
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   0(2,R9),3(R6)                                                    
         BE    ALTR80                                                           
         LA    R6,5(R6)                                                         
         CLC   0(2,R9),3(R6)                                                    
         BE    ALTR80                                                           
         LA    R6,5(R6)                                                         
         CLC   0(2,R9),3(R6)                                                    
         BNE   DMXKEEP                                                          
*                                                                               
ALTR80   DS    0H                                                               
         CLC   20(2,R3),=C'NE'                                                  
         BNE   PUTKEEP                                                          
         AP    0(4,R5),=P'1'         ADD TO REC ACCUMULATOR                     
         NI    0(R8),X'0F'                                                      
         OI    0(R8),X'D0'                                                      
         MVC   20(2,R3),=C'RP'                                                  
         L     R5,AREC                                                          
         SH    R5,=H'4'                                                         
         PUT   FILEOUT,(R5)                                                     
         NI    0(R8),X'0F'                                                      
         OI    0(R8),X'90'                                                      
         MVC   20(2,R3),=C'NE'                                                  
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
*  HANDLES MARKET GROUP RECORDS                                                 
TYPMKG   EQU   *                                                                
         CLC   0(2,R9),=X'0000'                                                 
         BNE   ALTREC                                                           
*                                                                               
         CLI   0(R8),X'91'                                                      
         BE    ALTR80                                                           
*                                                                               
         CLI   0(R8),X'92'                                                      
         BE    ALTR80                                                           
*                                                                               
         CLI   0(R8),X'94'                                                      
         BE    ALTR80                                                           
*                                                                               
         CLI   0(R8),X'93'                                                      
         BE    ALTR80                                                           
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
*  HANDLES EQUIVALENCY HEADER RECORDS                                           
TYPEQU   EQU   *                                                                
         CLC   0(2,R8),=C'NE'      IS IT A NEEDHAM AGENCY                       
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   0(3,R9),=C'   '                                                  
         BH    EQUHD20                                                          
         B     EQUHD80                                                          
*                                                                               
EQUHD20  LA    R6,CLIST1                                                        
         CLI   2(R8),C'T'                                                       
         BNE   EQUHD40                                                          
         BAS   R7,EQUCLT                                                        
         CLI   DMCB,X'01'                                                       
         BE    DMXKEEP                                                          
         B     EQUHD80                                                          
*                                                                               
EQUHD40  LA    R6,CLIST2                                                        
         CLI   0(R8),C'R'                                                       
         BNE   EQUHD60                                                          
         BAS   R7,EQUCLT                                                        
         CLI   DMCB,X'01'                                                       
         BE    DMXKEEP                                                          
         B     EQUHD80                                                          
*                                                                               
EQUHD60  LA    R6,CLIST4                                                        
         CLI   0(R8),C'X'                                                       
         BNE   EQUHD70                                                          
*                                                                               
         CLC   0(3,R9),0(R6)                                                    
         BE    EQUHD80                                                          
         LA    R6,5(R6)                                                         
         CLC   0(3,R9),0(R6)                                                    
         BE    EQUHD80                                                          
*                                                                               
EQUHD70  LA    R6,CLIST5                                                        
         CLI   0(R8),C'N'                                                       
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   0(3,R9),0(R6)                                                    
         BE    EQUHD80                                                          
         LA    R6,5(R6)                                                         
         CLC   0(3,R9),0(R6)                                                    
         BE    EQUHD80                                                          
         LA    R6,5(R6)                                                         
         CLC   0(3,R9),0(R6)                                                    
         BNE   DMXKEEP                                                          
*                                                                               
*                                                                               
EQUHD80  DS    0H                                                               
*                                                                               
         AP    0(4,R5),=P'1'       ADD TO REC ACCUMULATOR                       
         MVC   0(2,R8),=C'RP'                                                   
         L     R5,AREC                                                          
         SH    R5,=H'4'                                                         
         PUT   FILEOUT,(R5)        WRITE TO SECOND OUTPUT TAPE                  
         MVC   0(2,R8),=C'NE'                                                   
         B     DMXKEEP                                                          
         EJECT                                                                  
*  R9 HAS THE ADDRESS OF THE INPUT RECORDS CLIENT CODE                          
*  R6 HAS TABLE TO CHECK AGAINST                                                
*                                                                               
CKCLT    XC    DUB,DUB                                                          
         MVC   DUB+3(2),0(R9)                                                   
         GOTO1 =V(BINSRCH),DMCB,DUB,(R6),14,5,(3,2),14                          
         BR    R7                                                               
         SPACE 2                                                                
*  R9 HAS THE ADDRESS OF THE INPUT RECORDS CLIENT CODE                          
*  R6 HAS TABLE TO CHECK AGAINST                                                
*                                                                               
EQUCLT   XC    DUB,DUB                                                          
         MVC   DUB(2),0(R9)                                                     
         GOTO1 =V(BINSRCH),DMCB,DUB,(R6),14,5,(0,3),14                          
         BR    R7                                                               
         SPACE 2                                                                
*                                                                               
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
         CLOSE FILEOUT                                                          
*  PRINT TOTALS                                                                 
         LA    R6,15                                                            
         LA    R7,RECCOUNT                                                      
PRTLOOP  DS    0H                                                               
         EDIT  (P4,0(R7)),(8,P+30),ALIGN=LEFT                                   
         MVC   P+40(20),4(R7)                                                   
         LA    R7,24(R7)                                                        
         GOTO1 VPRINTER                                                         
         BCT   R6,PRTLOOP                                                       
*                                                                               
         B     DMXIT                                                            
*                                                                               
HEXIT    NTR1                                                                   
         XC    DMCB,DMCB                                                        
         GOTO1 VHEXOUT,DMCB,(R3),P,50                                           
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*  OUTPUT DTF                                                                   
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,MACRF=(PM),                     X        
               RECFM=VB,LRECL=4004,BUFNO=2,BLKSIZE=32760                        
*                                                                               
CLIST1   DC    CL3'BAY',AL2(0)                                                  
         DC    CL3'CLO',AL2(0)                                                  
         DC    CL3'EP ',AL2(0)                                                  
         DC    CL3'HF ',AL2(0)                                                  
         DC    CL3'HO ',AL2(0)                                                  
         DC    CL3'KNB',AL2(0)                                                  
         DC    CL3'LP ',AL2(0)                                                  
         DC    CL3'MLV',AL2(0)                                                  
         DC    CL3'MNA',AL2(0)                                                  
         DC    CL3'MSA',AL2(0)                                                  
         DC    CL3'MSD',AL2(0)                                                  
         DC    CL3'PNB',AL2(0)                                                  
         DC    CL3'PR ',AL2(0)                                                  
         DC    CL3'SH ',AL2(0)                                                  
         SPACE 2                                                                
CLIST2   DC    CL3'BAY',AL2(0)                                                  
         DC    CL3'CLO',AL2(0)                                                  
         DC    CL3'EP ',AL2(0)                                                  
         DC    CL3'HF ',AL2(0)                                                  
         DC    CL3'HO ',AL2(0)                                                  
         DC    CL3'LP ',AL2(0)                                                  
         DC    CL3'MLV',AL2(0)                                                  
         DC    CL3'MNA',AL2(0)                                                  
         DC    CL3'MSA',AL2(0)                                                  
         DC    CL3'MSD',AL2(0)                                                  
         DC    CL3'PNB',AL2(0)                                                  
         DC    CL3'PR ',AL2(0)                                                  
         DC    CL3'SH ',AL2(0)                                                  
         DC    CL3'SH ',AL2(0)                                                  
         SPACE 2                                                                
CLIST4   DC    CL3'EP ',AL2(0)                                                  
         DC    CL3'HON',AL2(0)                                                  
         SPACE 2                                                                
CLIST5   DC    CL3'EP ',AL2(0)     FOR NETWORK                                  
         DC    CL3'HON',AL2(0)                                                  
         DC    CL3'HC ',AL2(0)                                                  
         SPACE 2                                                                
* RECORD COUNTERS                                                               
RECCOUNT DS    0H                                                               
         DC    PL4'0',CL20'0 TYPE RECORDS  '                                    
         DC    PL4'0',CL20'2 TYPE RECORDS  '                                    
         DC    PL4'0',CL20'TRAFFIC RECORDS '                                    
         DC    PL4'0',CL20'BUY RECORDS     '                                    
         DC    PL4'0',CL20'INVOICE RECORDS '                                    
         DC    PL4'0',CL20'PROD GRP RECORDS'                                    
         DC    PL4'0',CL20'STA BILL RECORDS'                                    
         DC    PL4'0',CL20'DAYPT H RECORDS '                                    
         DC    PL4'0',CL20'STA ADD RECORDS '                                    
         DC    PL4'0',CL20'MKT GP RECORDS  '                                    
         DC    PL4'0',CL20'MKT ASGN RECORDS'                                    
         DC    PL4'0',CL20'EQUIV H RECORDS '                                    
         DC    PL4'0',CL20'PROGRAM RECORDS '                                    
         DC    PL4'0',CL20'UNIVERSE RECORDS'                                    
         DC    PL4'0',CL20'HUT RECORDS     '                                    
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
VHEXOUT  DS    A                                                                
WORK     DS    CL24                                                             
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032PZSPXTNE  05/01/02'                                      
         END                                                                    
