*          DATA SET PPREP0302O AT LEVEL 013 AS OF 05/01/02                      
*PHASE PP0302O,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM WILL COPY ALL RELEVANT RECORDS FOR CLIENT FX                   
*   IN AGENCY BD (BDNY SIGN-ON FOR BBDO)                                        
*   OUTPUT IS TO A VARIABLE LENGTH "FLAT" FILE                                  
*                                                                               
*      QOPT5     Y= TEST RUN (DON'T MARK FILE)                                  
*      QOPT6     Y= DUMP FIRST 10 RECORDS (BEFORE AND AFTER)                    
*                                                                               
PP0302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0302                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   CPYCNT,=P'0'                                                     
         ZAP   OUTCNT,=P'0'                                                     
         ZAP   CNT01,=P'0'                                                      
         ZAP   CNT02,=P'0'                                                      
         ZAP   CNT03,=P'0'                                                      
         ZAP   CNT04,=P'0'                                                      
         ZAP   CNT05,=P'0'                                                      
         ZAP   CNT06,=P'0'                                                      
         ZAP   CNT07,=P'0'                                                      
         ZAP   CNT08,=P'0'                                                      
         ZAP   CNT09,=P'0'                                                      
         ZAP   CNT10,=P'0'                                                      
         ZAP   CNT11,=P'0'                                                      
         ZAP   CNT14,=P'0'                                                      
         ZAP   CNT15,=P'0'                                                      
         ZAP   CNT17,=P'0'                                                      
         ZAP   CNT18,=P'0'                                                      
         ZAP   CNT20,=P'0'                                                      
         ZAP   CNT25,=P'0'                                                      
         ZAP   CNT27,=P'0'                                                      
         ZAP   CNT40,=P'0'                                                      
         ZAP   CNT41,=P'0'                                                      
         ZAP   CNT50,=P'0'                                                      
         ZAP   DUMPCNT,=P'0'                                                    
*                                                                               
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                  FIRST FOR REQUEST                            
         MVI   RCWRITE,C'Y'                                                     
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         OPEN  (OUT,OUTPUT)                                                     
*                                                                               
         LA    R0,PBUYREC          READ EVERYTHING INTO BUYREC                  
         ST    R0,AREC                                                          
*                                                                               
         LA    R6,RCTBL            TABLE OF DESIRED RECORD CODES                
         LA    R7,MEDTBL           TABLE OF MEDIA                               
         B     PROC10                                                           
*                                                                               
MEDUP    DS    0H                                                               
         ZAP   DUMPCNT,=P'0'                                                    
         LA    R7,1(R7)            NEXT MEDIA                                   
         CLI   0(R7),X'FF'         END OF TABLE ?                               
         BNE   PROC10              NO                                           
         MVI   MODE,RUNLAST        YES - END OF COPY                            
         B     EXIT                                                             
*                                                                               
RCUP     DS    0H                                                               
         ZAP   DUMPCNT,=P'0'                                                    
         LA    R6,1(R6)            NEXT RECORD CODE                             
         CLI   0(R6),X'FF'         END OF RECORD CODES ?                        
         BNE   PROC10              NO                                           
         LA    R6,RCTBL            YES - START OVER FOR NEW MEDIA               
         B     MEDUP               NEXT MEDIA                                   
*                                                                               
PROC10   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=C'BD'       AGENCY                                       
         MVC   KEY+2(1),0(R7)      MEDIA                                        
         MVC   KEY+3(1),0(R6)      RECORD CODE                                  
         CLI   0(R6),X'01'         AGENCY REC ?                                 
         BE    PROC20              YES - COPY ALL                               
         CLI   0(R6),X'11'         REP REC ?                                    
         BE    PROC20              YES - COPY ALL                               
         CLI   0(R6),X'40'         STD COMM REC ?                               
         BE    PROC20              YES - COPY ALL                               
         CLI   0(R6),X'27'         FSI REC ?                                    
         BE    PROC20              YES - TEST ALL                               
         CLI   0(R6),X'41'         NV TEXT REC ?                                
         BE    PROC20              YES - TEST ALL                               
         MVC   KEY+4(3),=C'FX '    CLIENT WANTED                                
*                                                                               
PROC20   GOTO1 HIGH                                                             
         B     PROC50                                                           
*                                                                               
PROC30   GOTO1 SEQ                                                              
*                                                                               
PROC50   DS    0H                                                               
         AP    INCNT,=P'1'                                                      
         CLC   KEY(4),KEYSAVE      CHECK THRU RECORD CODE                       
         BNE   RCUP                GO DO NEXT RECORD CODE                       
         CLI   0(R6),X'01'         AGENCY REC ?                                 
         BE    PROC56              YES - COPY ALL                               
         CLI   0(R6),X'11'         REP REC ?                                    
         BE    PROC56              YES - COPY ALL                               
         CLI   0(R6),X'40'         STD COMM REC ?                               
         BE    PROC56              YES - COPY ALL                               
         CLI   0(R6),X'27'         FSI REC ?                                    
         BE    PROC52              YES - TEST ALL                               
         CLC   KEY+4(3),=C'FX '    CLIENT WANTED ?                              
         BE    PROC56              YES - COPY                                   
         CLI   0(R6),X'41'         NV TEXT REC ?                                
         BNE   RCUP                NO - GO DO NEXT RECORD CODE                  
         CLC   KEY+4(3),=X'FFFFFF'    ALL CLIENTS ?                             
         BNE   PROC30                  NO - TEST NEXT                           
         B     PROC56              YES - COPY                                   
*                                                                               
PROC52   DS    0H                                                               
         CLC   KEY+10(3),=C'FX '   CLIENT WANTED ?                              
         BE    PROC56              YES - COPY                                   
         CLC   KEY+10(3),=X'FFFFFF'    ALL CLIENTS ?                            
         BNE   PROC30                  NO - TEST NEXT                           
*                                                                               
PROC56   DS    0H                                                               
*                                                                               
         CLI   KEY+3,X'01'                                                      
         BNE   *+10                                                             
         AP    CNT01,=P'1'                                                      
         CLI   KEY+3,X'02'                                                      
         BNE   *+10                                                             
         AP    CNT02,=P'1'                                                      
         CLI   KEY+3,X'03'                                                      
         BNE   *+10                                                             
         AP    CNT03,=P'1'                                                      
         CLI   KEY+3,X'04'                                                      
         BNE   *+10                                                             
         AP    CNT04,=P'1'                                                      
         CLI   KEY+3,X'05'                                                      
         BNE   *+10                                                             
         AP    CNT05,=P'1'                                                      
         CLI   KEY+3,X'06'                                                      
         BNE   *+10                                                             
         AP    CNT06,=P'1'                                                      
         CLI   KEY+3,X'07'                                                      
         BNE   *+10                                                             
         AP    CNT07,=P'1'                                                      
         CLI   KEY+3,X'08'                                                      
         BNE   *+10                                                             
         AP    CNT08,=P'1'                                                      
         CLI   KEY+3,X'09'                                                      
         BNE   *+10                                                             
         AP    CNT09,=P'1'                                                      
         CLI   KEY+3,X'10'                                                      
         BNE   *+10                                                             
         AP    CNT10,=P'1'                                                      
         CLI   KEY+3,X'11'                                                      
         BNE   *+10                                                             
         AP    CNT11,=P'1'                                                      
         CLI   KEY+3,X'14'                                                      
         BNE   *+10                                                             
         AP    CNT14,=P'1'                                                      
         CLI   KEY+3,X'15'                                                      
         BNE   *+10                                                             
         AP    CNT15,=P'1'                                                      
         CLI   KEY+3,X'17'                                                      
         BNE   *+10                                                             
         AP    CNT17,=P'1'                                                      
         CLI   KEY+3,X'18'                                                      
         BNE   *+10                                                             
         AP    CNT18,=P'1'                                                      
         CLI   KEY+3,X'20'                                                      
         BNE   *+10                                                             
         AP    CNT20,=P'1'                                                      
         CLI   KEY+3,X'25'                                                      
         BNE   *+10                                                             
         AP    CNT25,=P'1'                                                      
         CLI   KEY+3,X'27'                                                      
         BNE   *+10                                                             
         AP    CNT27,=P'1'                                                      
         CLI   KEY+3,X'40'                                                      
         BNE   *+10                                                             
         AP    CNT40,=P'1'                                                      
         CLI   KEY+3,X'41'                                                      
         BNE   *+10                                                             
         AP    CNT41,=P'1'                                                      
         CLI   KEY+3,X'50'                                                      
         BNE   *+10                                                             
         AP    CNT50,=P'1'                                                      
*                                                                               
         GOTO1 GETPRT              GET THE RECORD                               
*                                                                               
PROC60   DS    0H                  *****  COPY THE RECORD                       
*                                                                               
         AP    CPYCNT,=P'1'                                                     
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   PROC70              NO                                           
         AP    DUMPCNT,=P'1'                                                    
         CP    DUMPCNT,=P'01'      01 RECORDS DUMPED ?                          
         BH    PROC70              YES - NO DUMP                                
*                                                                               
PROC65   DS    0H                                                               
         MVC   P+55(12),=C'** BEFORE **'                                        
*****    BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC70   DS    0H                                                               
*                                                                               
*                                                                               
         XCEFL REC-4,4004          CLEAR OUTPUT AREA                            
         MVC   HALF,PBUYREC+25     REC LENGTH                                   
         LH    R1,HALF                                                          
         LA    RF,REC                                                           
         LA    RE,PBUYREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE PBUYREC TO REC                          
         LH    R1,HALF                                                          
         LA    R1,4(R1)            TOTAL REC LENGTH                             
         STH   R1,REC-4                                                         
*                                                                               
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC80              NO                                           
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
*                                                                               
PROC80   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC30              NO - NEXT RECORD                             
         CP    DUMPCNT,=P'01'      01 RECORDS DUMPED ?                          
         BH    PROC30              YES                                          
*                                                                               
*****    MVC   P+55(12),=C'** AFTER ***'                                        
*****    BAS   RE,DMPKEY                                                        
*****    BAS   RE,RPRT                                                          
*****    BAS   RE,DMPREC                                                        
         MVC   P+55(12),=C'** REC-4 ***'                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPRECA                                                       
*                                                                               
*****    MVC   KEY(64),SKEY        RESTORE CONTRACT KEYS                        
*****    GOTO1 HIGH                AND SEQUENCE                                 
*                                                                               
         B     PROC30              NEXT SEQ REC                                 
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
         CLOSE OUT                                                              
*                                                                               
RUNL10   DS    0H                                                               
*                                                                               
RUNL40   GOTO1 REPORT                                                           
         LA    R4,COUNTS                                                        
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNL90                                                           
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL50                                                           
*                                                                               
RUNL90   DS    0H                                                               
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
*****    MVI   RCSUBPRG,10                                                      
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
*                                                                               
DMPRECA  NTR1                                                                   
         LA    R5,REC-4                                                         
         MVC   HALF,29(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R2,4(R2)           REC-4   RECORD LENGTH                         
         LA    R3,0(R5,R2)                                                      
         B     DMPREC2                                                          
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
*                                                                               
         SPACE 2                                                                
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'RECORDS READ'                                               
OUTCNT   DS    PL8                                                              
         DC    CL15'RECORDS OUTPUT'                                             
CPYCNT   DS    PL8                                                              
         DC    CL15'RECORDS COPIED'                                             
CNT01    DS    PL8                                                              
         DC    CL15'AGENCY      '                                               
CNT02    DS    PL8                                                              
         DC    CL15'CLIENTS     '                                               
CNT03    DS    PL8                                                              
         DC    CL15'DIV         '                                               
CNT04    DS    PL8                                                              
         DC    CL15'REG         '                                               
CNT05    DS    PL8                                                              
         DC    CL15'DST         '                                               
CNT06    DS    PL8                                                              
         DC    CL15'PRD         '                                               
CNT07    DS    PL8                                                              
         DC    CL15'EST         '                                               
CNT08    DS    PL8                                                              
         DC    CL15'BILL        '                                               
CNT09    DS    PL8                                                              
         DC    CL15'BUCKET      '                                               
CNT10    DS    PL8                                                              
         DC    CL15'CONTRACT    '                                               
CNT11    DS    PL8                                                              
         DC    CL15'REPS        '                                               
CNT14    DS    PL8                                                              
         DC    CL15'AOR         '                                               
CNT15    DS    PL8                                                              
         DC    CL15'JOB         '                                               
CNT17    DS    PL8                                                              
         DC    CL15'LIST        '                                               
CNT18    DS    PL8                                                              
         DC    CL15'BUDGET      '                                               
CNT20    DS    PL8                                                              
         DC    CL15'BUY         '                                               
CNT25    DS    PL8                                                              
         DC    CL15'CLR/STATUS  '                                               
CNT27    DS    PL8                                                              
         DC    CL15'FSI         '                                               
CNT40    DS    PL8                                                              
         DC    CL15'STD COMM    '                                               
CNT41    DS    PL8                                                              
         DC    CL15'NVTEXT      '                                               
CNT50    DS    PL8                                                              
         DC    CL15'INVMAT      '                                               
         DC    X'FF'                                                            
DUMPCNT  DS    PL8                                                              
*                                                                               
PRTFILES DS    0D                                                               
         DC    CL8' PRTDIR'                                                     
         DC    CL8' PRTFILE'                                                    
         DC    CL8' PUBDIR'                                                     
         DC    CL8' PUBFILE'                                                    
         DC    C'X'                                                             
*                                                                               
MEDTBL   DC    C'MNOST',X'FF'      PRINT MEDIA CODES                            
RCTBL    DS    0H                  RECORD TYPES TO BE COPIED                    
         DC    X'0102030405060708091011'                                        
         DC    X'14151718202527404150FF'                                        
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
PUBSW    DS    CL1                                                              
CHKGST   DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
PUBNET   DS    PL8                                                              
PUBCASH  DS    PL8                                                              
PUBMYTAX DS    PL8                                                              
PUBBASIS DS    PL8                                                              
PUBGSTT  DS    PL8                                                              
PUBGSTTP DS    PL8                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
*                                                                               
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
PPBYOWRK DS    600C                                                             
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREP0302O05/01/02'                                      
         END                                                                    
