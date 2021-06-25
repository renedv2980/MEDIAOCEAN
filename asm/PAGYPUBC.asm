*          DATA SET PAGYPUBC   AT LEVEL 040 AS OF 05/01/02                      
*PHASE PAGYPUBC                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE BINSRCH                                                                
*INCLUDE RECUP                                                                  
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'PAGYPUB - CHANGE AGENCY CODE FOR ALL AGENCY PUBS'               
PAGYPUB  CSECT                                                                  
         NBASE 0,PAGYPUB,=V(REGSAVE),R7                                         
         SPACE 2                                                                
         BAS   RE,PRNT                                                          
*&&DO                                                                           
         OPEN  IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*&&                                                                             
*                                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   OUTCNT,=P'0'                                                     
         ZAP   CHANGCNT,=P'0'                                                   
*                                                                               
GET      DS    0H                                                               
         BAS   RE,GETREC                                                        
         CLC   REC(3),=3X'FF'                                                   
         BE    EOF                                                              
*                                                                               
         BAS   RE,PUTREC        PUT ORIGINAL RECORD                             
         CLC   REC+7(2),=C'RH'                                                  
         BNE   GET                                                              
         MVC   REC+7(2),=C'MY'                                                  
*                                                                               
         CLI   REC+9,X'81'                                                      
         BNE   CHK85                                                            
         LA    R2,REC+33    FIRST ELEMENT                                       
ONCEAGN  CLI   0(R2),X'14'                                                      
         BE    OKEL                                                             
*                                                                               
         CLI   0(R2),X'08'                                                      
         BE    OKEL                                                             
*                                                                               
         CLI   0(R2),X'09'                                                      
         BE    OKEL                                                             
*                                                                               
         CLI   0(R2),X'0A'                                                      
         BE    OKEL                                                             
*                                                                               
BALREG   BAS   RE,GETNXTL                                                       
         BNE   ONCEAGN                                                          
         B     GET5         PDUMP OUTPUT CHANGE                                 
****                                                                            
OKEL     DS    0H          START SELECTING                                      
         CLC   2(3,R2),=3X'FF'                                                  
         BE    BALREG                                                           
         CLC   2(3,R2),=X'FFD540'     OFFICE N                                  
         BE    BALREG                                                           
*   MUST LOOKUP CLIENT FOR OFFICE N                                             
         MVC   MEDCDE,REC                                                       
         MVC   CLICDE,2(R2)                                                     
         BAS   RE,LOOKUP                                                        
         B     *+8        NOT FOUND                                             
         B     BALREG                                                           
***8*   DELETER   ELEMENT                                                       
         GOTO1 =V(RECUP),DMCB,(C'P',REC),(R2),(R2),DATA                         
         B     BALREG                                                           
DATA     DS    CL8                                                              
RTESW    DC    X'0'                                                             
CHK85    CLI   REC+9,X'85'                                                      
         BNE   GET                                                              
         AP    JUST85,=P'1'                                                     
         CP    JUST85,=P'200'                                                   
         BH    *+8                                                              
         BAS   RE,DMPREC                                                        
         LA    R2,REC+33    FIRST ELEMENT                                       
         MVI   RTESW,0                                                          
CHKF71   CLI   0(R2),X'71'                                                      
         BE    HITON71                                                          
NXTL     BAS   RE,GETNXTL                                                       
         BNE   CHKF71                                                           
         CLI   RTESW,255                                                        
         BNE   GET                                                              
         B     GET5                                                             
*                                                                               
HITON71  MVC   MEDCDE,REC                                                       
         MVC   CLICDE,2(R2)                                                     
         BAS   RE,LOOKUP                                                        
         B     DELTHIS CLIENT NOT IN LIST                                       
         MVI   RTESW,255                                                        
         B     NXTL                                                             
DELTHIS  GOTO1 =V(RECUP),DMCB,(C'P',REC),(R2),(R2),DATA                         
         B     NXTL                                                             
GET5     DS    0H            '                                                  
         CLI   REC+9,X'81'                                                      
         BNE   NOT81R                                                           
         AP    CHANGCNT,=P'1'                                                   
         CP    CHANGCNT,=P'300'                                                 
         BH    PUT                                                              
DDD      BAS   RE,DMPREC                                                        
         B     PUT                                                              
*                                                                               
NOT81R   CLI   REC+9,X'85'                                                      
         BNE   *-2                                                              
         AP    CHANGC85,=P'1'                                                   
         CP    CHANGC85,=P'100'                                                 
         BH    PUT                                                              
         BAS   RE,DMPREC                                                        
PUT      DS    0H                                                               
         BAS   RE,PUTREC                                                        
         B     GET                                                              
GETNXTL  ZIC   R3,1(R2)     R2 MUST POINT TO REC                                
         AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BR    RE           CHECK FOR BE AFTER CALL FOR EOR                     
**********8                                                                     
LOOKUP   LA    RF,MEDIAM     CLIENT CODE IN CLICDE                              
         CLI   MEDCDE,C'M'   MEDIA IN MEDCDE                                    
         BE    STARTL                                                           
         LA    RF,MEDIAN     CLIENT CODE IN CLICDE                              
         CLI   MEDCDE,C'N'   MEDIA IN MEDCDE                                    
         BE    STARTL                                                           
         LA    RF,MEDIAO     CLIENT CODE IN CLICDE                              
         CLI   MEDCDE,C'O'   MEDIA IN MEDCDE                                    
         BE    STARTL                                                           
         LA    RF,MEDIAS     CLIENT CODE IN CLICDE                              
         CLI   MEDCDE,C'S'   MEDIA IN MEDCDE                                    
         BE    STARTL                                                           
         LA    RF,MEDIAT     CLIENT CODE IN CLICDE                              
         CLI   MEDCDE,C'T'   MEDIA IN MEDCDE                                    
         BE    STARTL                                                           
         DC    H'0'                                                             
STARTL   CLI   0(RF),255                                                        
         BER   RE                                                               
         CLC   0(3,RF),CLICDE                                                   
         BE    4(RE)                                                            
         LA    RF,3(RF)                                                         
         B     STARTL                                                           
MEDCDE   DS    CL1                                                              
CLICDE   DS    CL3                                                              
         SPACE 2                                                                
*&&DO                                                                           
EOF      CLOSE IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
EOF      CLOSE (IN,,OUT,)                                                       
*&&                                                                             
         BAS   RE,PRNT                                                          
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
         B     XIT                                                              
         SPACE 3                                                                
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
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
         MVC   P+75(KLEN),WORK                                                  
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
*        CLI   DMPSW,C'Y'                                                       
*        BNE   PUTREC2                                                          
*        MVI   DMPSW,C'N'                                                       
*        SP    DMPCNT,=P'1'                                                     
*        BNP   PUTREC2                                                          
*        BAS   RE,DMPREC                                                        
PUTREC2  DS    0H                                                               
         MVC   HALF,REC+25                                                      
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
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
*   MAGAZINE OFFICE N  CHANGED AC TO DP                                         
MEDIAM   DC    C'AC AP BA BC BE BF BI BL BM BT BW CG CP ED FB FC FW GN X        
               HC HO HS IA IC IL IR JD JH JO JR KM LH MI NE NO NS NT OTX        
                RC RG RM RU SA SB SN SW TO TP UN WC WH WL WS YH '               
         DC    X'FFFFFFFF'                                                      
*   NEWSPAPER   OFFICE N                                                        
MEDIAN   DC    C'AC AL AP BA BC BE BF BI BL BM BW CP DI ED FB FC FW GN X        
               HO HS IA IC IL IR JD JH JO JR KM LH MI NE NO NS NT NV OTX        
                RC RG RM RU SA SB SN SW TO UN WC WD WH WI WS YH '               
         DC    X'FFFFFFFF'                                                      
*   OUTDOOR     OFFICE N                                                        
MEDIAO   DC    C'AC AP BC BE BF BI BL BW ED FB FC FW GN HO HS IA IC IL X        
               IR JD JH JO JR KM LH MI NE NO NS NT OT PD RG RM RU SA SBX        
                SN SW TO UN WC WH WS YH '                                       
         DC    X'FFFFFFFF'                                                      
*   SUPPLEMENT  OFFICE N                                                        
MEDIAS   DC    C'AC AP BC BE BF BI BL BW CP ED FC FW HO HS IA IC IR JR X        
               KM LH MI NE NO NS NT OT RG RU SA SN UN WS YH'                    
         DC    X'FFFFFFFF'                                                      
*   TRADE  OFFICE N                                                             
MEDIAT   DC    C'AC AH AL AP BC BE BF BI BL BW CG CP ED FC FW GN HC HO X        
               HS IA IC IR JR KM LH MI NE NG NS NT OT RC RG RU SA SN TOX        
                UN WI WS YH '                                                   
         DC    X'FFFFFFFF'                                                      
*****                                                                           
*                                                                               
*                                                                               
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
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
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
AGMLST   DC    1200X'00'                                                        
AGMLSTX  EQU   *-1                                                              
         DC    6X'00'                                                           
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT         '                                
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT        '                                
CHANGCNT DC    PL5'0',CL20'CHANGED 81 RCD COUNT'                                
CHANGC85 DC    PL5'0',CL20'CHANGED 85 RCD COUNT'                                
JUST85   DC    PL5'0',CL20'85 REC W RH AS CLINT'                                
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    4100C                                                            
         DS    D                                                                
*&&DO                                                                           
IN1      DS    8500C                                                            
*&&                                                                             
*&&DO                                                                           
OUT1     DS    8500C                                                            
*&&                                                                             
         SPACE 3                                                                
         ORG   REC                                                              
       ++INCLUDE PBUYREC                                                        
       ++INCLUDE PBDELEM                                                        
       ++INCLUDE PUBREC                                                         
       ++INCLUDE LTLREC                                                         
       ++INCLUDE PUBDSTEL                                                       
       ++INCLUDE PUBREPL                                                        
       ++INCLUDE PUBAOVEL                                                       
*                                                                               
BSTAB    CSECT                                                                  
         DS    1000C                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040PAGYPUBC  05/01/02'                                      
         END                                                                    
