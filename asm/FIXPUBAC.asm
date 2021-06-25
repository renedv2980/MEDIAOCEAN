*          DATA SET FIXPUBAC   AT LEVEL 002 AS OF 08/10/99                      
*PHASE FIXPUBAC,*,NOAUTO                                                        
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE RECUP                                                                  
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'FIXPUBAC - PRT/PUB FILE AGENCY COMMISSION CHANGE'               
*                                                                               
*   THIS PROGRAM CHANGES AGENCY COMMISSION (PUBAC) IN THE GENERAL INFO          
*   ELEMENT (PUBGENEL) OF THE X'81' PUB RECORD. THE DATA FOR THE CHANGE         
*   IS SUPPLIED VIA CARD INPUT AS FOLLOWS:                                      
*                                                                               
*   CC 01-02: ALWAYS FP  (FOR "FIX PROGRAM")                                    
*   CC 03-04: AGENCY                                                            
*   CC 05   : MEDIA                                                             
*   CC 06-10: AGENCY COMMISSION: 5-DIGIT NUMERIC (3 DECIMAL PLACES)             
*                                                                               
*                                                                               
         PRINT NOGEN                                                            
FIXPUBAC CSECT                                                                  
         NBASE 0,FIXPUBAC,=V(REGSAVE),R9      *** NOTE R9 AS 2ND BASE           
         SPACE 2                                                                
         BAS   RE,PRNT             "INITIALIZE" PRINT                           
*                                                                               
*                                  BUILD TABLE OF AGENCIES/PUBAC                
*                                                                               
         XC    BINPARMS(24),BINPARMS                                            
         XCEFL BINTBL,1800                                                      
*                                                                               
         SR    R0,R0               ADDRS OF INSERT                              
         LA    R1,BINTBL           ADDRS OF TBL                                 
         SR    R2,R2               NUM OF RECS SO FAR                           
         LA    R3,LAGYTAB          L'REC                                        
         LA    R4,3                BYTE 0=KEY DISP,1-3=L'KEY                    
         LA    R5,300              MAX NUM OF RECS                              
         STM   R0,R5,BINPARMS                                                   
*                                                                               
*                                  GET AGENCY/MEDIA/PUBAC CARDS                 
GC10     BAS   RE,CARDS                                                         
         CLC   CARD(2),=C'/*'      END OF FILE ?                                
         BE    GCDONE              YES - DONE                                   
         CLC   CARD(2),=C'FP'      RIGHT "KIND" OF CARD ?                       
         BNE   GCERR               NO - REJECT                                  
*                                                                               
         MVC   WORK(5),=5X'F0'     TEST FOR A NUMERIC                           
         MVZ   WORK(5),CARD+5        AGENCY COMMISSION FIELD                    
         CLC   WORK(5),=5X'F0'         IN THE CARD                              
         BNE   GCERR               NOT NUMERIC - REJECT                         
*                                                                               
*                                  ADD "CARD" TO TABLE                          
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         USING AGYTAB,R3                                                        
         MVC   AGYTAGY(3),CARD+2      AGENCY/MEDIA                              
         PACK  FULL,CARD+5(5)      AGENCY COMMISSION                            
         ZAP   AGYTAC(3),FULL+1(3)   AGENCY COMMISSION                          
*                                                                               
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   GC10                NEXT AGENCY/MEDIA CARD                       
         DC    H'0'                TABLE IS FULL                                
*                                                                               
*                                                                               
GCERR    DS    0H                  PRINT REJECTED CARDS                         
*                                                                               
         MVC   P+1(18),=C'*** INVALID CARD -'                                   
         MVC   P+20(80),CARD       REJECT CARD                                  
         BAS   RE,PRNT                                                          
         B     GC10                NEXT CARD                                    
*                                                                               
GCDONE   DS    0H                  TABLE OF CARDS IS COMPLETE                   
*                                  NOW PRINT ACCEPTED CARDS FROM TABLE          
         XC    WORK,WORK                                                        
         GOTO1 =V(BINSRCH),BINPARMS,(X'02',WORK)         READ HIGH              
         CLI   0(R1),1             RECORD FOUND ?                               
         BNE   GCDPRT              YES                                          
*                                                                               
         BAS   RE,PRNT                                                          
         MVC   P+1(30),=C'*** FIXPUBAC NOT PROCESSED ***'                       
         MVC   P+35(40),=C'NO CHANGE CARDS WERE INPUT (OR ACCEPTED)'            
         BAS   RE,PRNT                                                          
         B     EOJ                 END                                          
*                                                                               
GCDPRT   DS    0H                                                               
         BAS   RE,PRNT                                                          
         ZICM  R3,1(R1),3          POINT R3 TO AG/M/AGYCOMM RECORD              
         USING AGYTAB,R3                                                        
GCDPRT5  MVC   P+1(3),AGYTAGY         AGENCY/MEDIA                              
         MVC   P+7(07),=C'NEW AC:'                                              
         EDIT  (P3,AGYTAC),(6,P+15),3                                           
         BAS   RE,PRNT                                                          
         LA    R3,LAGYTAB(R3)      NEXT TABLE ENTRY                             
         CLI   0(R3),0             END OF TABLE ?                               
         BH    GCDPRT5             NO - PRINT NEXT ENTRY                        
         BAS   RE,PRNT                                                          
*                                                                               
         DROP  R3                                                               
*                                                                               
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*                                                                               
GET      DS    0H                                                               
         BAS   RE,GETREC                                                        
         CLI   EOFSW,C'Y'                                                       
         BE    EOJ                                                              
*                                                                               
         MVI   CHGSW,C'N'          TURN OFF CHANGED ELEM INDICATOR              
         MVI   DMPSW,C'N'            AND "DUMP" INDICATOR                       
         CLI   REC+9,X'81'         IF NOT X'81' PUB RECORD                      
         BNE   PUT                   JUST WRITE IT                              
*                                                                               
         LA    R2,REC+33           X'81' PUB RECORD                             
         MVI   ELCODE,X'20'        GENERAL INFO ELEM CODE                       
         BAS   RE,NEXTEL           ELEM FOUND ?                                 
         BE    GET3F               YES                                          
*                                  NO - NOTHING TO CHANGE                       
         AP    NOELCNT,=P'1'                                                    
*NOP*    MVC   P+90(32),=C'** NO GENERAL INFO ELEM FOUND **'                    
*NOP*    BAS   RE,DMPKEY           WRITE "NO ELEM" LINE                         
*NOP*    BAS   RE,PRNT                                                          
         B     PUT                 WRITE RECORD (WILL RETURN TO GET)            
*                                                                               
GET3F    DS    0H                  GENERAL INFO ELEMENT FOUND                   
*                                                                               
         XC    WORK,WORK                                                        
         LA    R3,REC                                                           
         USING PUBKEY,R3                                                        
         MVC   WORK(2),PUBKAGY     AGENCY                                       
         MVC   WORK+2(1),PUBKMED   MEDIA                                        
*                                                                               
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BINPARMS,(X'00',WORK)                                
         CLI   0(R1),X'01'         AGENCY/MEDIA FOUND ?                         
         BE    PUT                 NO - NO CHANGE TO RECORD                     
         ZICM  R3,1(R1),3          POINT R3 TO AG/M/AGYCOMM RECORD              
*                                                                               
         AP    NOCHACNT,=P'1'                                                   
         USING PUBGEND,R2          ESTABLISH GENERAL INFO ELEMENT               
         CP    PUBAC(3),3(3,R3)    AGENCY COMMISSION CHANGED ?                  
         BE    PUT                 NO - NO CHANGE TO RECORD                     
         MVI   CHGSW,C'Y'          TURN ON CHANGED ELEM INDICATOR               
         SP    NOCHACNT,=P'1'                                                   
         AP    CHACNT,=P'1'                                                     
*                                                                               
         SP    DMPCNT,=P'1'                                                     
         BNP   GET4A               25 RECS DUMPED ALREADY                       
         MVI   DMPSW,C'Y'                                                       
         MVC   P+50(30),=C'***  BEFORE PUBAC CHANGES  ***'                      
         BAS   RE,PRNT                                                          
         BAS   RE,DMPREC           DUMP THE "BEFORE" RECORD                     
         BAS   RE,PRNT                                                          
*                                                                               
GET4A    DS    0H                  DO THE CHANGE TO PUBAC                       
         USING PUBGEND,R2          ESTABLISH GENERAL INFO ELEMENT               
*                                                                               
         MVC   P+85(07),=C'OLD AC:'                                             
         EDIT  (P3,PUBAC),(6,P+93),3,ALIGN=LEFT                                 
         MVC   P+102(07),=C'NEW AC:'                                            
         MVC   PUBAC,3(R3)         REPLACE AGENCY COMMISSION                    
         EDIT  (P3,PUBAC),(6,P+110),3,ALIGN=LEFT                                
         BAS   RE,DMPKEY                                                        
         BAS   RE,PRNT             WRITE "CHANGE" LINE                          
*                                                                               
         DROP  R2                                                               
*                                                                               
PUT      DS    0H                                                               
         CLI   CHGSW,C'Y'          HAS AN ELEMENT BEEN CHANGED ?                
         BNE   PUTEND              NO                                           
         CP    DMPCNT,=P'0'        25 RECS DUMPED ALREADY ?                     
         BNH   PUTEND              YES                                          
         MVI   DMPSW,C'Y'          NO - DUMP                                    
         MVC   P+50(30),=C'***   AFTER PUBAC CHANGES  ***'                      
         BAS   RE,PRNT                                                          
*                                                                               
PUTEND   BAS   RE,PUTREC                                                        
*                                                                               
         B     GET                                                              
         SPACE 2                                                                
*                                                                               
EOF      CLOSE (IN,,OUT,)                                                       
*                                                                               
         BAS   RE,PRNT                                                          
         BAS   RE,SKIP                                                          
         LA    R3,COUNTS                                                        
         LA    R4,25                                                            
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(20),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+22(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
*                                                                               
         MVI   EOFSW,C'Y'                                                       
         XIT1                      RETURN TO GETREC CALL                        
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         SPACE 2                                                                
SKIP     MVC   PCOM,=C'BC01'                                                    
         ZAP   LNCNT,=P'0'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT3    MVC   PCOM,=C'BL03'                                                    
         AP    LNCNT,=P'3'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT2    MVC   PCOM,=C'BL02'                                                    
         AP    LNCNT,=P'2'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT     MVC   PCOM,=C'BL01'                                                    
         AP    LNCNT,=P'1'                                                      
*                                                                               
PRNTR    NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
*                                                                               
         CP    LNCNT,=P'54'                                                     
         BH    SKIP                                                             
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
DMPREC   NTR1                                                                   
*                                                                               
         LA    R5,REC-4                                                         
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R2,4(R2)                                                         
         LA    R3,0(R5,R2)         EOR                                          
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   XIT                                                              
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 =V(HEXOUT),DMCB,(R5),WORK,(R4),=C'N'                             
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,PRNT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         B     XIT                                                              
         SPACE 3                                                                
DMPKEY   NTR1                                                                   
*                                                                               
         LA    R5,REC                                                           
         LA    R2,KLEN                                                          
         GOTO1 =V(HEXOUT),DMCB,(R5),P+01,(R2),=C'N'                             
*                                                                               
         MVC   WORK(KLEN),0(R5)                                                 
         TR    WORK(KLEN),TRTAB                                                 
         MVC   P+55(KLEN),WORK                                                  
         B     XIT                                                              
         SPACE 3                                                                
GETREC   NTR1                                                                   
         GET   IN,REC-4                                                         
*                                                                               
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0             EOR                                          
         AP    INCNT,=P'1'                                                      
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
PUTREC   NTR1                                                                   
*                                                                               
         MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
*                                                                               
         CLI   DMPSW,C'Y'                                                       
         BNE   XIT                                                              
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         BNP   XIT                                                              
         BAS   RE,DMPREC                                                        
         B     XIT                                                              
         SPACE 3                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                                                               
         SPACE 3                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
IN       DCB   DDNAME=TAPEIN,          DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
*                                                                               
OUT      DCB   DDNAME=TAPEOUT,         DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4D4E4B'     40-4F                    
         DC    X'504B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
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
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
ELCODE   DS    X                                                                
UPSI     DS    XL1                                                              
         DS    0F                                                               
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
DMPSW    DC    C'N'                                                             
EOFSW    DC    C'N'                                                             
CHGSW    DC    C' '                                                             
*                                                                               
DMPCNT   DC    PL5'25'                                                          
DUMPADD  DC    PL5'0'                                                           
CARD     DS    CL80                                                             
*                                                                               
BINPARMS DS    6F                                                               
         DS    0D                                                               
         DC    C'*AGYTAB*'                                                      
BINTBL   DS    300XL(LAGYTAB)                                                   
LBINTBL  EQU   *-BINTBL                                                         
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0'                                                           
         DC    CL20'RECORDS READ'                                               
OUTCNT   DC    PL5'0'                                                           
         DC    CL20'RECORDS WRITTEN'                                            
CHACNT   DC    PL5'0'                                                           
         DC    CL20'RECORDS CHANGED'                                            
NOCHACNT DC    PL5'0'                                                           
         DC    CL20'SAME AC VALUES'                                             
NOELCNT  DC    PL5'0'                                                           
         DC    CL20'NO GENERAL INFO ELEM'                                       
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    4100C                                                            
         SPACE 3                                                                
         ORG   REC                                                              
       ++INCLUDE PUBREC                                                         
PUBNAMED DSECT                                                                  
       ++INCLUDE PUBNAMEL                                                       
PUBGEND  DSECT                                                                  
       ++INCLUDE PUBGENEL                                                       
*                                                                               
* DSECT FOR AGENCY TABLE (BINTBL)                                               
AGYTAB   DSECT                                                                  
*                                                                               
AGYTAGY  DS    CL2                 AGENCY CODE (KEY)                            
AGYTMED  DS    CL1                 AGENCY MEDIA (KEY)                           
AGYTAC   DS    PL3                 AGENCY COMMISSION                            
LAGYTAB  EQU   *-AGYTAGY                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002FIXPUBAC  08/10/99'                                      
         END                                                                    
