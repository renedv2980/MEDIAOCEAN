*          DATA SET PPREPYR02D AT LEVEL 010 AS OF 08/22/97                      
*PHASE PPYR02D,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE CASHVAL                                                                
         TITLE 'PPYR02 - SPECIAL PUB FILE UPDATE'                               
*                                                                               
*                                                                               
*        QOPT1 N= TEST RUN - DON'T MARK FILE                                    
*        QOPT2 Y= DUMP RECORD                                                   
*                                                                               
PPYR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPYR02,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPYRWRKD,R8                                                      
*                                                                               
**                                                                              
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   PUBCNT,=P'0'                                                     
         ZAP   NAMCNT,=P'0'                                                     
         ZAP   MISCNT,=P'0'                                                     
         ZAP   ZONCNT,=P'0'                                                     
         ZAP   REPCNT,=P'0'                                                     
         ZAP   COMCNT,=P'0'                                                     
         MVI   RCSUBPRG,10                                                      
*                                                                               
         L     RF,=V(CASHVAL)                                                   
         A     RF,RELO                                                          
         ST    RF,ACASHVAL                                                      
*                                                                               
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT1,C'N'           MEANS DON'T MARK FILE                       
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   DMINBTS,X'08'   ??   SET TO PASS DELETES                         
         MVI   DMOUTBTS,X'FD'  ??   SET TO PASS DELETES                         
*                                                                               
*****    OPEN  (IN,(INPUT))                                                     
         OPEN  IN                                                               
GET      DS    0H                                                               
         BAS   RE,TAPEGET                                                       
         CLI   EOFSW,C'Y'                                                       
         BE    EXIT                                                             
         AP    INCNT,=P'1'                                                      
         MVI   DUMPSW,C' '         CLEAR "FORCE DUMP" SWITCH                    
********                           BUILD PUB RECORD KEY                         
PUBKEY1  LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
*                                                                               
         LA    R7,REC                                                           
         USING YRRECD,R7                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),QMEDIA       MEDIA                                        
*                                                                               
*********                          SET PUB NUMBER                               
         XC    NEWNUM,NEWNUM                                                    
*                                  SPECIAL FOR YSHNY                            
*                                  THEY ONLY HAD 6 DIGITS                       
         MVC   WORK(2),=C'00'                                                   
         MVC   WORK+2(6),YRPBCOD                                                
*****    MVC   WORK(8),YRPBCOD                                                  
         LA    R6,8                INPUT FIELD LENGTH                           
*                                                                               
         MVI   WORK+8,C','                                                      
         MVC   WORK+9(2),YRZONCOD                                               
         CLI   YRZONCOD,C' '                                                    
         BNE   YR05                                                             
         CLI   YREDCOD,C' '                                                     
         BE    YR10                                                             
         MVC   WORK+9(3),YREDCOD                                                
         LA    R6,4(R6)            INC LENGTH FOR ED AND COMMA                  
         B     YR10                                                             
*                                                                               
YR05     LA    R6,3(R6)            INC LENGTH FOR ZONE AND COMMA                
         CLI   YREDCOD,C' '                                                     
         BE    YR10                                                             
         MVI   WORK+11,C','                                                     
         MVC   WORK+12(3),YREDCOD                                               
         LA    R6,4(R6)            INC LENGTH FOR ED AND COMMA                  
*                                                                               
YR10     GOTO1 PUBVAL,DMCB,((R6),WORK),(0,NEWNUM)                               
         MVC   KEY+1(6),NEWNUM                                                  
*                                                                               
         MVC   KEY+7(2),QAGENCY    AGENCY                                       
         MVI   KEY+9,X'81'         RECORD CODE                                  
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE         RECORD FOUND ?                           
         BE    YR20                YES                                          
*                           PRINT "NO RECORD" MESSAGE                           
*****    MVC   P+2(1),QMEDIA                                                    
         GOTO1 PUBEDIT,DMCB,KEYSAVE+1,P+7                                       
         MVC   P+46(20),YRPUBNAM                                                
         MVC   P+68(20),YRPUBZNM                                                
         MVC   P+90(39),=C'*** PUB NUMBER NOT FOUND OR MISSING ***'             
         AP    MISCNT,=P'1'                                                     
         BAS   RE,RPRT                                                          
         B     GET                 NEXT REC                                     
*                                                                               
YR20     DS    0H                  CHECK NAME + ADDR ELEM                       
*                                                                               
         GOTO1 GETNAME             GET PUB REC                                  
*                                                                               
         LA    R2,PUBREC+33        FIRST ELEM (PUBNAMEL)                        
*                                                                               
         USING PUBNAMEL,R2                                                      
         MVC   PUBZNAME,YRPUBZNM   PUB ZONE NAME                                
         OC    PUBZNAME,SPACES                                                  
*                           PRINT "RESULTS" AND MESSAGES                        
         MVC   P+2(1),QMEDIA                                                    
         GOTO1 PUBEDIT,DMCB,PUBREC+1,P+7                                        
         MVC   P+24(20),PUBNAME                                                 
         MVC   P+46(20),YRPUBNAM                                                
         MVC   P+68(20),YRPUBZNM                                                
*                                                                               
         CLC   PUBZNAME(20),SPACES   ZONE NAME PRESENT ?                        
         BH    YR24                  YES                                        
         MVC   P+90(16),=C'*** NO ZONE NAME'                                    
         AP    NAMCNT,=P'1'                                                     
YR24     MVC   WORK(20),YRPUBNAM                                                
         OC    WORK(20),SPACES                                                  
         CLC   PUBNAME(20),WORK    NAMES MATCH ?                                
         BE    YR30                YES                                          
         AP    NAMCNT,=P'1'        NO                                           
         CLI   P+90,C' '           ANY MESSAGE HERE ?                           
         BNE   YR25                YES                                          
         MVC   P+90(20),=C'*** PUB NAME CHANGED'                                
         B     YR28                                                             
YR25     MVC   P+109(20),=C'*** PUB NAME CHANGED'                               
*                                                                               
YR28     DS    0H                  REPLACE DDS NAME (PUBNAME)                   
         MVC   PUBNAME(20),YRPUBNAM                                             
         OC    PUBNAME(20),SPACES                                               
         MVC   PSECOND+05(16),=C'NEW PUB NAME IS:'                              
         MVC   PSECOND+24(20),PUBNAME                                           
         CLI   QOPT2,C'Y'          DUMP RECORDS ?                               
         BNE   YR30                NO                                           
         MVI   DUMPSW,C'Y'         FORCE DUMP REGARDLESS OF PUBCNT              
*                                                                               
YR30     DS    0H                                                               
*                                                                               
*                                                                               
REP      CLI   YRPAYREP,C' '       ALL FIELDS ARE OPTIONAL                      
         BE    GEN                 IF NO FIELD, NO ELEMENT                      
*                                                                               
         DROP  R2                                                               
*                                                                               
REP1     LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'14'        REP ELEMENT                                  
         BAS   RE,NEXTEL                                                        
         BNE   GEN                                                              
         USING PUBREPEL,R2                                                      
*                                                                               
         CLC   PUBPAREP,YRPAYREP                                                
         BE    GEN                 IF NO CHANGE, DO NOTHING                     
*                                                                               
         MVC   PSECOND+47(37),=C'** PAY REP CHANGED FROM  ZZZZ TO ZZZZ'         
         MVC   PSECOND+72(4),PUBPAREP                                           
         MVC   PSECOND+80(4),YRPAYREP                                           
         MVC   PUBPAREP,YRPAYREP                                                
         AP    REPCNT,=P'1'                                                     
         CLI   QOPT2,C'Y'          DUMP RECORDS ?                               
         BNE   GEN                 NO                                           
         MVI   DUMPSW,C'Y'         FORCE DUMP REGARDLESS OF PUBCNT              
*                                                                               
*****    MVC   REPS,PUBPAREP                                                    
*****    BAS   RE,REPCHECK                                                      
*                                                                               
         DROP  R2                                                               
*                                                                               
GEN      CLI   YRAGYCMS,C' '       ALL FIELDS ARE OPTIONAL                      
         BE    YRPUBX              DONE                                         
*                                                                               
GEN1     LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'20'        GENERAL INFO ELEMENT                         
         BAS   RE,NEXTEL                                                        
         BNE   YRPUBX                                                           
         USING PUBGENEL,R2                                                      
*                                                                               
*        ANOTHER SPECIAL FOR YSHNY                                              
*        SINCE THEY ADDED DECIMALS                                              
*                                                                               
         LA    R3,YRAGYCMS                                                      
         LA    R6,YRAGYCMS+4                                                    
GEN1B    CLI   0(R6),C' '                                                       
         BH    GEN1C                                                            
         BCTR  R6,0                                                             
         B     GEN1B                                                            
GEN1C    SR    R6,R3                                                            
         AH    R6,=H'1'                                                         
         BP    *+6                                                              
         DC    H'0'          CAN'T BE NON POSITIVE                              
         GOTO1 ACASHVAL,DMCB,(3,YRAGYCMS),(R6)                                  
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
*****    ZAP   PUBAC,DUB                                                        
         ZAP   FIELD,DUB                                                        
*                                                                               
         CLC   PUBAC,FIELD                                                      
         BE    YRPUBX              IF NO CHANGE, DO NOTHING                     
*                                                                               
         MVC   PSECOND+90(23),=C'COMMISSION CHANGED FROM'                       
         EDIT  (P3,PUBAC),(6,PSECOND+115),3                                     
         MVC   PSECOND+122(2),=C'TO'                                            
         EDIT  (P3,FIELD),(6,PSECOND+125),3,ALIGN=LEFT                          
         MVC   PUBAC,FIELD         REPLACE PUBAC                                
         AP    COMCNT,=P'1'                                                     
         CLI   QOPT2,C'Y'          DUMP RECORDS ?                               
         BNE   YRPUBX              NO                                           
         MVI   DUMPSW,C'Y'         FORCE DUMP REGARDLESS OF PUBCNT              
*                                                                               
         DROP  R2                                                               
*                                                                               
YRPUBX   BAS   RE,RPRT                                                          
*                                                                               
*****    CLC   PUBZNAME(20),SPACES   ZONE NAME PRESENT ?                        
*****    BNE   *+8                   YES                                        
*****    B     GET                   NO - SKIP OUTPUT                           
*                                                                               
PUTPUB1  DS    0H                                                               
         AP    PUBCNT,=P'1'                                                     
         CLI   RCWRITE,C'N'                                                     
         BE    PUTPUB5                                                          
*                                                                               
         GOTO1 PUTPUB                                                           
*                                                                               
PUTPUB5  DS    0H                                                               
         CLI   QOPT2,C'Y'          DUMP RECORDS ?                               
         BNE   GET                 NO - NEXT RECORD                             
         CLI   DUMPSW,C'Y'         ALWAYS DUMP ?                                
         BE    PUTPUB7             YES                                          
         CP    PUBCNT,=P'25'       DUMP FIRST 25 PUBS                           
         BH    GET                 NEXT RECORD                                  
PUTPUB7  MVI   FORCEHED,C'Y'       EACH ON A NEW PAGE                           
         BAS   RE,DMPREC                                                        
         B     GET                                                              
         SPACE 2                                                                
*                                                                               
**********    PUBUP NOT IN USE IN THIS PROGRAM     ***********                  
PUBUP    DS    0H                                                               
         ST    RE,SAVERE                                                        
         GOTO1 RECUP,DMCB,(1,PUBREC),ELEM,0(R2),0                               
*                                                                               
         LA    R2,PUBREC                                                        
         ZICM  R1,PUBREC+25,2      RECORD LEN                                   
         AR    R2,R1               BUMP R2 TO END OF RECORD                     
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
TAPEGET  NTR1                                                                   
         GET   IN,REC-4                                                         
         XIT1                                                                   
*                                                                               
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
*                                                                               
         EJECT                                                                  
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DC    PL8'0'                                                           
         DC    CL20'TAPE RECS READ'                                             
PUBCNT   DC    PL8'0'                                                           
         DC    CL20'TOTAL PUBS UPDATED'                                         
MISCNT   DC    PL8'0'                                                           
         DC    CL20'PUB NO. NOT FOUND'                                          
NAMCNT   DC    PL8'0'                                                           
         DC    CL20'PUB NAMES CHANGED'                                          
REPCNT   DC    PL8'0'                                                           
         DC    CL20'PAY REPS CHANGED'                                           
COMCNT   DC    PL8'0'                                                           
         DC    CL20'COMMISSION CHANGED'                                         
ZONCNT   DC    PL8'0'                                                           
         DC    CL20'ZONE NAME MISSING'                                          
         DC    X'FF'                                                            
*                                                                               
EOFSW    DC    C'N'                                                             
*                                                                               
NEWNUM   DS    XL6                                                              
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL10   CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(20),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,28(R4)                                                        
         B     RUNL10                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
**********************    BELOW NOT USED IN THIS PROGRAM   **********           
REPCHECK NTR1                 SUBRTINE TO VALIDATE REPS                         
         LA    R3,KEY2                                                          
         USING PREPREC,R3                                                       
         XC    KEY2,KEY2                                                        
         MVC   PREPKAGY,=C'AR'                                                  
         MVI   PREPKMED,C'N'                                                    
         MVI   PREPKRCD,X'11'                                                   
         MVC   PREPKREP,REPS                                                    
         GOTO1 DATAMGR,DMCB,(0,DMREAD),PRTDIR,KEY2,IO                           
         TM    8(R1),X'10'                                                      
         BZ    REPCKX                                                           
         MVC   P(4),REPS                                                        
         MVC   P+5(34),=C'ERROR-THIS REP NEEDS TO BE ENTERED'                   
         GOTO1 REPORT                                                           
REPCKX   XIT1                                                                   
**********************    ABOVE NOT USED IN THIS PROGRAM   **********           
*                                                                               
EOF      CLOSE (IN,)                                                            
         MVI   EOFSW,C'Y'                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
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
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)                                                      
         LH    R2,HALF              USE RECORD LENGTH                           
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
         LTORG                                                                  
ACASHVAL DS    F                                                                
*                                                                               
IN       DCB   DDNAME=TAPEIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=00255,                                            X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
PPYRWRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    CL200                                                            
FRSTSW   DS    XL1                                                              
DUMPSW   DS    CL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ITOT     DS    F                                                                
SAVERE   DS    F                                                                
WPUBNAME DS    CL35                                                             
DPUBNAME DS    CL35                                                             
WADDR1   DS    CL45                                                             
DADDR1   DS    CL45                                                             
WADDR2   DS    CL45                                                             
DADDR2   DS    CL45                                                             
WCITY    DS    CL30                                                             
DCITY    DS    CL30                                                             
FIELD    DS    XL3                 FOR PACKING COMMISSION                       
KEY2     DS    CL25                                                             
IO       DS    1000X                                                            
REPS     DS    CL4                                                              
*                                                                               
YRRECD   DSECT                                                                  
*                                                                               
YRPBCOD  DS    CL8                                                              
YRZONCOD DS    CL2                                                              
YREDCOD  DS    CL3                                                              
YRPUBNAM DS    CL20                                                             
YRPUBZNM DS    CL20                                                             
YRADRES  DS    CL30                                                             
YRCITY   DS    CL16                                                             
YRSTATE  DS    CL2                                                              
YRZIP    DS    CL10                                                             
YRATTN   DS    CL24                                                             
YRPHONE  DS    CL12                                                             
YRFAX    DS    CL12                                                             
YRAGYCMS DS    CL5                                                              
YRCSHDSC DS    CL4                                                              
YRPAYREP DS    CL4                                                              
YRTRFREP DS    CL4                                                              
YRCONREP DS    CL4                                                              
YRCRLF   DS    CL2                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBUFFALOD                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPREPYR02D08/22/97'                                      
         END                                                                    
