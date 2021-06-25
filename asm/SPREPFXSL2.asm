*          DATA SET SPREPFXSL2 AT LEVEL 028 AS OF 09/29/00                      
*          DATA SET SPREPFX02  AT LEVEL 028 AS OF 09/28/99                      
*PHASE SPFX02X                                                                  
         TITLE 'SPFX02 - GENERATE XSPFILE STATION LOCKIN RECORDS'               
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX10                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX00                                                             
*                                                                               
         CLI   MODE,REQLAST                                                     
         BE    FXX                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
FX00     DS    0H                                                               
         CLI   QUESTOR,C' '                                                     
         BE    *+10                                                             
         PACK  COUNT,QUESTOR(6)                                                 
*                                                                               
         OPEN  (FILEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
                                                                                
         EJECT                                                                  
* CLTFRST                                                                       
*                                                                               
FX10     BAS   RE,BLDTAB                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D72'                                                  
         L     R6,ADCLT                                                         
         MVC   KEY+2(3),1(R6)      A-M/CLT                                      
         GOTO1 HIGH                                                             
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLC   KEY(5),KEYSAVE                                                   
         BNE   EXIT                                                             
*                                                                               
         MVC   SAVEKEY,KEY         SAVE 0D72 KEY                                
*                                                                               
K        USING SLHRECD,SAVEKEY                                                  
*                                                                               
FX20     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D73'                                                  
         MVC   KEY+2(1),K.SLHKAGMD A-M                                          
         MVC   KEY+3(3),K.SLHKSEQ  SEQ                                          
         DROP  K                                                                
*                                                                               
         GOTO1 HIGH                                                             
         B     FX26                                                             
*                                                                               
FX24     GOTO1 SEQ                                                              
*                                                                               
FX26     CLC   KEY(6),KEYSAVE                                                   
         BNE   FX40                                                             
* TEST ESTIMATE RECORD ON FILE                                                  
         SR    R4,R4                                                            
         IC    R4,KEY+6            GET PRODUCT NUMBER                           
         MHI   R4,256                                                           
         A     R4,=A(ESTTAB)                                                    
         SR    R5,R5                                                            
         IC    R5,KEY+8                                                         
         AR    R4,R5                                                            
*                                                                               
         TM    0(R4),X'01'                                                      
         BO    FX28                                                             
         TM    0(R4),X'80'         TEST ERROR PRINTED YET                       
         BO    FX24                YES - CONTINUE                               
         MVI   0(R4),X'80'         SET ERROR PRINTED                            
         BAS   RE,ESTERR                                                        
         B     FX24                                                             
*                                                                               
FX28     GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         USING SLKRECD,R6                                                       
*                                                                               
         LA    R7,XREC                                                          
         MVC   0(2,R7),=X'0D73'    TYP/SUBTYP                                   
         MVC   17(8,R7),SAVEKEY+2  A-M/CLT/MKT/STA                              
         MVC   25(7,R7),SLKKPRD    PRD/EST/DPT/...                              
         LA    R7,42(R7)                                                        
         LHI   R8,42               SET INITIAL LENGTH                           
*                                                                               
         LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
         CLI   0(R6),1                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
* MOVE 01 ELEMENT                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R6)                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R6)                                                    
         LA    R8,1(R8,RE)         UPDATE LENGTH                                
*                                                                               
         LA    R7,1(R7,RE)         POINT PAST ELEMENT JUST ADDED                
*                                                                               
FX29     SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    FX30                                                             
         CLI   0(R6),3                                                          
         BNE   FX29                                                             
*                                                                               
         XC    0(38,R7),0(R7)      CLEAR SPACE FOR NEW ELEMENT                  
         SR    RE,RE                                                            
         ICM   RE,1,1(R6)                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),0(R6)                                                    
         MVI   1(R7),38            SET NEW ELEMENT LENGTH                       
         AHI   R7,38               POINT TO NEXT ELEMENT AREA                   
         AHI   R8,38               ADJUST LENGTH                                
         B     FX29                                                             
*                                                                               
FX30     STCM  R8,3,XREC+32        SET LENGTH IN NEW RECORD                     
         AHI   R8,4                                                             
         SLL   R8,16                                                            
         ST    R8,XRECLEN                                                       
         LA    R0,XRECLEN                                                       
         PUT   FILEOUT,(0)                                                      
*                                                                               
         SP    COUNT,=P'1'                                                      
         BP    FX24                                                             
         B     FXX                                                              
         EJECT                                                                  
* NO MORE SLK RECORDS - RESTORE FOR NEXT SLH RECORD                             
*                                                                               
FX40     MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     FX12                                                             
*                                                                               
FXX      CLOSE FILEOUT                                                          
*                                                                               
         GOTO1 AENDREQ                                                          
         B     EXIT                                                             
         EJECT                                                                  
*=================================================*                             
* BUILD A TABLE OF  ESTIMATES FOR THIS CLIENT     *                             
*=================================================*                             
         SPACE 1                                                                
BLDTAB   NTR1                                                                   
         L     R0,=A(ESTTAB)                                                    
         L     R1,=A(ESTTABX-ESTTAB)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,ADCLT                                                         
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   BLDTABX                                                          
         B     BLDTAB22                                                         
*                                                                               
BLDTAB2  L     R6,ADCLT                                                         
         LA    R6,CLIST-CLTHDRD(R6)                                             
*                                                                               
BLDTAB4  CLC   0(3,R6),KEY+4                                                    
         BE    BLDTAB10                                                         
         AHI   R6,4                                                             
         CLI   0(R6),C'A'                                                       
         BNL   BLDTAB4                                                          
         DC    H'0'                                                             
*                                                                               
BLDTAB10 SR    RE,RE                                                            
         IC    RE,3(R6)                                                         
         MHI   RE,256                                                           
         A     RE,=A(ESTTAB)                                                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,KEY+7                                                         
         AR    RE,RF                                                            
         MVI   0(RE),X'01'         SET FLAG IN TABLE                            
*                                                                               
BLDTAB20 GOTO1 SEQ                                                              
*                                                                               
BLDTAB22 CLC   KEY(4),KEYSAVE                                                   
         BNE   BLDTABX                                                          
         CLI   KEY+7,0             TEST ESTIMATE PRESENT                        
         BE    BLDTAB20            NO                                           
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BZ    BLDTAB2             NO - THIS IS AN ESTIMATE                     
         B     BLDTAB20                                                         
*                                                                               
BLDTABX  XIT1                                                                   
         EJECT                                                                  
ESTERR   NTR1                                                                   
         MVC   P(18),=C'NO ESTIMATE HEADER'                                     
         MVC   P+20(3),CLT                                                      
*                                                                               
         L     R6,ADCLT                                                         
         LA    R6,CLIST-CLTHDRD(R6)                                             
*                                                                               
ESTERR2  CLC   KEY+6(1),3(R6)                                                   
         BE    ESTERR4                                                          
         AHI   R6,4                                                             
         CLI   0(R6),C'A'                                                       
         BNL   ESTERR2                                                          
         LA    R6,=C'***'                                                       
*                                                                               
ESTERR4  MVC   P+25(3),0(R6)                                                    
*                                                                               
         SR    R0,R0                                                            
         IC    R0,KEY+8                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+30(3),DUB                                                      
         GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
SAVEKEY  DS    XL13                                                             
ELEM     DS    XL64                                                             
COUNT    DC    PL8'99999999'                                                    
         DS    0D                                                               
         DC    C'**XREC**'                                                      
XRECLEN  DS    A                                                                
XREC     DS    4000C                                                            
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,LRECL=2008,            X        
               MACRF=PM                                                         
*                                                                               
ESTTAB   DS    256XL256                                                         
ESTTABX  EQU   *                                                                
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSLH                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSLK                                                       
         EJECT                                                                  
*PREFIX=$                                                                       
       ++INCLUDE SPGENXLK                                                       
*PREFIX=                                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028SPREPFXSL209/29/00'                                      
         END                                                                    
