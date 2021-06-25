*          DATA SET PZNEXTNE   AT LEVEL 040 AS OF 05/01/02                      
*PHASE PZNEEXTA                                                                 
*INCLUDE HEXOUT                                                                 
         TITLE 'PZNEXTNE - CHANGE NE TO RP '                                    
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
PZNEXTNE CSECT                                                                  
         NMOD1 40,DMLDUMP,RR=R2                                                 
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         ST    R2,RELO                                                          
         L     R3,=V(HEXOUT)                                                    
         AR    R3,R2                                                            
         ST    R3,VHEXOUT                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
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
         L     RF,CNTR             KEEP COUNT OF PURGES                         
         LA    RF,1(RF)                                                         
         ST    RF,CNTR                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
*                                                                               
         XC    CNTR(20),CNTR           CLEAR COUNTER                            
         ZAP   PAKACT,=P'0'                                                     
*                                                                               
         LA    R2,FILEOUT          OPEN TAPE                                    
         OPEN  ((R2),(OUTPUT))                                                  
         B     DMXIT                                                            
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               BUFNO=2,                                                X        
               MACRF=PM                                                         
         SPACE 2                                                                
* END-OF-FILE LOGIC                                                             
*                                                                               
DMXEOF   DS    0H                                                               
*                                                                               
         LA    R2,FILEOUT          CLOSE TAPE                                   
         CLOSE ((R2),)                                                          
*                                                                               
         MVC   P(13),=C'DELETED RECS='                                          
         L     R4,CNTR                                                          
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+16(6),DUB                                                      
         GOTO1 VPRINTER                                                         
         MVC   P(15),=C'CONVERTED PACK='                                        
         L     R4,PAKCNTR                                                       
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+16(6),DUB                                                      
         GOTO1 VPRINTER                                                         
         MVC   P(15),=C'CONVERTED UNIT='                                        
         L     R4,UNTCNTR                                                       
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+16(6),DUB                                                      
         GOTO1 VPRINTER                                                         
         MVC   P(15),=C'COMMENT/INTEG ='                                        
         L     R4,COMINTG                                                       
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+16(6),DUB                                                      
         GOTO1 VPRINTER                                                         
         MVC   P(15),=C'CONVERTED TRAF='                                        
         L     R4,TRAFCNTR                                                      
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+16(6),DUB                                                      
         GOTO1 VPRINTER                                                         
         MVC   P(9),=C'ORDERED ='                                               
         LA    R2,P+11                                                          
         EDIT  (P8,PAKACT),(15,0(R2)),2                                         
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
*                                                                               
CNTR     DS    F                                                                
PAKCNTR  DS    F                                                                
UNTCNTR  DS    F                                                                
COMINTG  DS    F                                                                
TRAFCNTR DS    F                                                                
         EJECT                                                                  
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
*                                                                               
         L     R3,AREC             POINT TO RECORD                              
         USING NPRECD,R3                                                        
         CLI   0(R3),X'02'         PACKAGE                                      
         BNE   DMXRECU                                                          
         CLI   NPKAM,X'93'         TEST NEEDHAM PACKAGE                         
         BNE   DMXKEEP                                                          
         CLC   NPKCLT,=X'91FF'     TEST EP                                      
         BE    DMX5                                                             
         CLC   NPKCLT,=X'9C5F'     TEST HC                                      
         BE    DMX5                                                             
         CLC   NPKCLT,=X'9DCD'     TEST HON                                     
         BNE   DMXKEEP                                                          
DMX5     MVI   NPKAM,X'D3'         CHANGE TO  RP                                
         BAS   RE,PUTOUT                                                        
         L     R4,PAKCNTR                                                       
         LA    R4,1(R4)                                                         
         ST    R4,PAKCNTR                                                       
         MVI   NPKAM,X'93'         RESET TO NE                                  
*        CLI   CNTR,X'A0'                                                       
*        BH    DMXKEEP                                                          
*        GOTO1 VHEXOUT,DMCB,(R3),P,50                                           
*        GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXRECU  CLC   0(2,R3),=X'0493'        TEST NEEDHAM UNIT                        
         BNE   DMXCOMNT                                                         
         CLC   0(4,R3),=X'049391FF'     NE/EP UNIT                              
         BE    DMXCVRT                                                          
         CLC   0(4,R3),=X'04939C5F'     NE/HC UNIT                              
         BE    DMXCVRT                                                          
         CLC   0(4,R3),=X'04939DCD'     NE/HON UNIT                             
         BE    DMXCVRT                                                          
         B     DMXKEEP                                                          
*                                                                               
DMXCOMNT DS    0H                  COMMENT RECORDS                              
         CLI   0(R3),X'0C'                                                      
         BNE   DMXINTG                                                          
         CLI   1(R3),X'93'         IS IT NEEDHEM                                
         BE    DMXCIX                                                           
DMXINTG  DS    0H                  INTEGRATION RECORDS                          
         CLI   0(R3),X'0A'                                                      
         BNE   DMXTRAFC                                                         
         CLI   1(R3),X'93'         IS IT NEEDHAM                                
         BNE   DMXTRAFC                                                         
*                                                                               
DMXCIX   L     R4,COMINTG                                                       
         LA    R4,1(R4)                                                         
         ST    R4,COMINTG                                                       
         B     PUTSAVE                                                          
*                                                                               
DMXTRAFC DS    0H                  IS IT TRAFFIC REVISION REC                   
         CLI   0(R3),X'21'                                                      
         BNE   DMXKEEP                                                          
         CLI   1(R3),X'93'         IS IT NEEDHAM                                
         BNE   DMXKEEP                                                          
         CLC   2(2,R3),=X'91FF'    EP                                           
         BE    DMXTR5                                                           
         CLC   2(2,R3),=X'9C5F'    HC                                           
         BE    DMXTR5                                                           
         CLC   2(2,R3),=X'9DCD'    HON                                          
         BNE   DMXKEEP                                                          
DMXTR5   DS    0H                                                               
         L     R4,TRAFCNTR                                                      
         LA    R4,1(R4)                                                         
         ST    R4,TRAFCNTR                                                      
*                                                                               
PUTSAVE  DS    0H                                                               
         MVI   1(R3),X'D3'                                                      
         BAS   RE,PUTOUT                                                        
         MVI   1(R3),X'93'                                                      
*        CLI   CNTR,X'A0'                                                       
*        BH    DMXKEEP                                                          
*        GOTO1 VHEXOUT,DMCB,(R3),P,50                                           
*        GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
DMXCVRT  DS    0H                                                               
         CLI   0(R3),X'04'         CONVERT UNIT                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,UNTCNTR                                                       
         LA    R4,1(R4)                                                         
         ST    R4,UNTCNTR                                                       
         MVI   1(R3),X'D3'         CHANGE TO AGENCY RP                          
         LR    R6,R3                                                            
         USING NURECD,R6                                                        
         TM    NUUNITST,X'42'      TEST PRE-EMPT/MISSED                         
         BNZ   PUTSAVE                                                          
         TM    NURSTAT,X'80'       IS IT DELETED                                
         BNZ   PUTSAVE                                                          
         USING NUMAINEL,R6                                                      
DMXCV5   LA    R6,27(R6)                                                        
         CLI   0(R6),X'01'        GET MAIN ELEM                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FULL,NUACTUAL                                                    
         L     R1,FULL                                                          
         TM    NUUNITST,X'80'      TEST IF MINUSSED                             
         BZ    *+6                                                              
         LNR   R1,R1               IF MINUSSED/MAKE SURE ITS MINUS              
         CVD   R1,PAKFLD                                                        
         AP    PAKACT,PAKFLD        ADD TO ORDERED TOTAL                        
*                                                                               
         MVC   FULL,NUINTEG                                                     
         L     R1,FULL                                                          
         TM    NUUNITST,X'80'      TEST IF MINUSSED                             
         BZ    *+6                                                              
         LNR   R1,R1               IF MINUSSED/MAKE SURE ITS MINUS              
         CVD   R1,PAKFLD                                                        
         AP    PAKACT,PAKFLD        ADD TO ORDERED TOTAL                        
         B     PUTSAVE                                                          
*                                                                               
*                                                                               
PUTOUT   NTR1                                                                   
*        USING NURECD,R3                                                        
*        MOVE  (WORKAREA+4,2000),0(R3)                                          
***      MVC   WORKAREA+4(2000),0(R3)                                           
*        MVC   FULL+2(2),NURLEN                                                 
*        LH    R1,FULL+2                                                        
*        LA    R1,4(R1)                                                         
*        STH   R1,FULL+2                                                        
*        MVC   WORKAREA(2),FULL+2                                               
*        MVC   WORKAREA+2(2),=X'0000'                                           
         LR    R5,R3                                                            
         SH    R5,=H'4'                                                         
         PUT   FILEOUT,(R5)                                                     
*        CLI   CNTR,X'A0'                                                       
*        BH    PTOUTX                                                           
*        GOTO1 VHEXOUT,DMCB,(R5),P,50                                           
*        GOTO1 VPRINTER                                                         
PTOUTX   XIT1                                                                   
         EJECT                                                                  
PAKACT   DS    CL8                 PACKED TOTALS                                
         SPACE                                                                  
         LTORG                                                                  
*                                                                               
WORKAREA DS    CL2004                                                           
*                                                                               
         EJECT                                                                  
WORKD    DSECT                                                                  
FULL     DS    F                                                                
RELO     DS    F                                                                
DUB      DS    D                                                                
PAKFLD   DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
WORK     DS    CL32                                                             
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
VHEXOUT  DS    A                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPACK                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040PZNEXTNE  05/01/02'                                      
         END                                                                    
