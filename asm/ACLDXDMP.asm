*          DATA SET ACLDXDMP   AT LEVEL 030 AS OF 01/14/99                      
*PHASE ACLDXDMP,*                                                               
*INCLUDE DATCON                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE ACRECTYP                                                               
         TITLE 'GENERAL ACLD EXTERNAL'                                          
*                                                                               
*                  DUMP OUT RECORDS                                             
*                                                                               
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
*                                                                               
         PRINT NOGEN                                                            
ACLDDMP  CSECT                                                                  
DMX      DC    10000X'00'                                                       
         ORG   DMX                                                              
         NMOD1 WORKX-WORKD,DMLDELS                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
         USING TRNRECD,R3                                                       
DMXREC   L     R3,AREC                                                          
         GOTO1 VRECTYP,DMCB,(C'D',0(R3))                                        
         MVC   RECTYP,0(R1)        RECORD TYPE                                  
         CLC   COMP,1(R1)                                                       
         BE    DMXRECX1                                                         
         MVC   BYTE,1(R1)                                                       
         NI    BYTE,X'F0'                                                       
         SR    RF,RF                                                            
         IC    RF,BYTE                                                          
         SRL   RF,4                SHIFT NIBBLE                                 
         LA    RF,HEXTAB(RF)                                                    
         MVC   P(9),=C'HEX COMP='                                               
         MVC   P+10(1),0(RF)                                                    
         MVC   BYTE,1(R1)                                                       
         NI    BYTE,X'0F'                                                       
         SR    RF,RF                                                            
         IC    RF,BYTE                                                          
         LA    RF,HEXTAB(RF)                                                    
         MVC   P+11(1),0(RF)                                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         CLI   COMP,0                                                           
         BE    DMXRECX1                                                         
         MVC   P(3),=C'DR='                                                     
         EDIT  (P8,DEB),(13,P+3)                                                
         MVC   P+20(3),=C'CR='                                                  
         EDIT  (P8,CRD),(13,P+23)                                               
         GOTO1 VPRINTER                                                         
*                                                                               
DMXRECX1 MVC   COMP,1(R1)          COMPANY                                      
         MVC   DSPCMP,2(1)         DISP. TO COMPANY CODE                        
*                                                                               
         CLI   COMP,X'4A'          YNRO ONLY                                    
         BNE   DMXKEEP                                                          
         CLI   RECTYP,ACRTTRN                                                   
         BNE   DMXKEEP                                                          
         CLI   TRNRSMOS,X'99'                                                   
         BNH   DMXKEEP                                                          
         CLI   TRNKDATE,X'90'                                                   
         BL    DMXKEEP             TRANSACTION BEFORE 1990                      
         CLI   TRNKDATE,X'97'                                                   
         BH    DMXKEEP             TRANSACTION AFTER  1997                      
         AP    POS#,=P'1'          ADD ONE TO POSSIBLE RECORDS                  
         SR    R1,R1                                                            
         IC    R1,TRNRSMOS         GET YEAR                                     
         SRL   R1,4                SHIFT OFF LOWER NIBBLE                       
         SR    RF,RF                                                            
         IC    RF,TRNKDATE         GET YEAR                                     
         SRL   RF,4                                                             
         CR    R1,RF                                                            
         BE    DMXKEEP             SAME YEAR                                    
         MVC   DATEBAD(2),TRNRSMOS                                              
         AP    REC#,=P'1'          ADD ONE TO RECORD FIXED COUNT                
         SLL   RF,4                MOVE NIBBLE BACK TO HIGH HALF                
         STC   RF,BYTE                                                          
         NI    TRNRSMOS,X'0F'                                                   
         OC    TRNRSMOS(1),BYTE    FIX MOS                                      
         MVC   DATEOK(2),TRNRSMOS                                               
*                                                                               
         USING TRNELD,R1                                                        
         LA    R1,TRNRFST          POINT TO FIRST ELEMENT                       
DMXR00   CLI   0(R1),0                                                          
         BE    DMXR30                                                           
         CLI   0(R1),TRNELQ        X'44'                                        
         BNE   DMXR10                                                           
         LR    R6,R1               SAVE OFF ADDRESS                             
         LA    RE,CRD                                                           
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+8                                                              
         LA    RE,DEB                                                           
         AP    0(L'DEB,RE),TRNAMNT                                              
*                                                                               
         TM    TRNSTAT,TRNSREV     REVERSAL ?                                   
         BZ    DMXR10                                                           
         AP    REV#,=P'1'          ADD ONE TO REVERSAL     COUNT                
*                                                                               
         USING TRSELD,R1                                                        
DMXR10   CLI   0(R1),TRSELQ        X'60'                                        
         BNE   DMXR20                                                           
         NI    TRSPMOS,X'0F'                                                    
         OC    TRSPMOS(1),BYTE                                                  
*                                                                               
DMXR20   SR    RF,RF                                                            
         IC    RF,1(,R1)                                                        
         AR    R1,RF                                                            
         B     DMXR00                                                           
*                                                                               
DMXR30   BAS   RE,DMPPUT                                                        
         B     DMXKEEP                                                          
         DROP  R1                                                               
         EJECT                                                                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
DMXRET   DS    0H                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         ZAP   LINE,=P'99' '       PRINT RECORD COUNTS                          
         GOTO1 VPRINTER                                                         
         MVC   P+00(7),=C'RECORD='                                              
         EDIT  (P8,POS#),(12,P+8)                                               
         MVC   P+25(6),=C'FIXED='                                               
         EDIT  (P8,REC#),(13,P+32)                                              
         MVC   P+50(5),=C'REVS='                                                
         EDIT  (P8,REV#),(13,P+56)                                              
         MVC   P+72(3),=C'DR='                                                  
         EDIT  (P8,DEB),(13,P+75)                                               
         MVC   P+90(3),=C'CR='                                                  
         EDIT  (P8,CRD),(13,P+93)                                               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         USING TRNELD,R6                                                        
DMPPUT   NTR1  ,                                                                
         MVC   WORK,SPACES                                                      
         MVC   WORK(4),=C'BAD='                                                 
         MVI   DATEBAD+2,1                                                      
         GOTO1 VDATCON,DMCB,(1,DATEBAD),(6,WORK+4)                              
         MVC   WORK+12(5),=C'GOOD='                                             
         MVI   DATEOK+2,1                                                       
         GOTO1 VDATCON,DMCB,(1,DATEOK),(6,WORK+17)                              
         MVC   WORK+28(5),=C'TRAN='                                             
         GOTO1 VDATCON,DMCB,(1,TRNKDATE),(5,WORK+33)                            
         CLI   0(R6),X'44'                                                      
         BE    DMPPUT01                                                         
         SR    R0,R0                                                            
         LA    RF,X'FF'                                                         
         DC    H'00'                                                            
*                                                                               
DMPPUT01 MVC   WORK+24(2),TRNMOS   CHARACTER YM, Y IS DECADE                    
         MVC   WORK+45(3),=C'TY='                                               
         SR    RF,RF                                                            
         IC    RF,TRNTYPE                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+48(3),DUB                                                   
         MVC   WORK+54(14),TRNKULA                                              
         MVC   WORK+78(2),TRNKOFF                                               
         MVC   WORK+82(14),TRNKULC                                              
         MVC   WORK+98(6),TRNKREF                                               
*                                                                               
         CP    COUNT,=P'10'                                                     
         BNH   DMPPUT10                                                         
         MVC   P(L'WORK),WORK                                                   
         GOTO1 VPRINTER                                                         
         B     XIT                                                              
         DROP  R6                                                               
*                                                                               
DMPPUT10 AP    COUNT,=P'1'                                                      
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(R3)                                         
         GOTO1 VPRNTBL,DMCB,(L'WORK,WORK),(R3),C'DUMP',(RF),=C'2D'              
*                                                                               
XIT      DS    0H                                                               
DUMPX    XIT1  ,                                                                
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
VDATCON  DC    V(DATCON)                                                        
VPRNTBL  DC    V(PRNTBL)                                                        
VRECTYP  DC    V(ACRECTYP)                                                      
DMPMAX   DC    PL3'100'                                                         
*                                                                               
POS#     DC    PL8'0'                                                           
REC#     DC    PL8'0'                                                           
REV#     DC    PL8'0'                                                           
DEB      DC    PL8'0'                                                           
CRD      DC    PL8'0'                                                           
COUNT    DC    PL8'0'                                                           
COUNTS   DS    0XL15                                                            
FNDCNT   DC    PL3'0',CL12'FOUND'                                               
XX       DC    PL3'0',CL12'XX   '                                               
         DC    X'FF'                                                            
COMP     DC    X'00'               COMPANY CODE                                 
HEXTAB   DC    C'0123456789ABCDEF'                                              
*                                                                               
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
HALF     DS    H                                                                
WORK     DS    CL110                                                            
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
RECTYP   DS    XL1                 RECORD TYPE                                  
DSPCMP   DS    XL1                 DISPLACEMENT TO COMPANY                      
*                                                                               
BYTE     DS    XL1                                                              
*                                                                               
TMSDATE  DS    XL2                 DATE START TMS                               
DATEOK   DS    XL3                 PACKED DATE                                  
DATEBAD  DS    XL3                                                              
WORKX    EQU   *                                                                
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACLDXDMP  01/14/99'                                      
         END                                                                    
