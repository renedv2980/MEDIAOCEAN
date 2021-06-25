*          DATA SET PUBCBIG    AT LEVEL 050 AS OF 05/01/02                      
*PHASE PUBCBIG,*,NOAUTO                                                         
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE IJDFYZZZ                                                               
*INCLUDE IJFVZZWZ                                                               
*INCLUDE REGSAVE                                                                
         TITLE 'PUBCBIG- EXPAND PUBNAMELS'                                      
*                                                                               
*              EXPANDS PUBNAMELS ON PRINTPAK PUBRECS                            
*                                                                               
PUBCBIG CSECT                                                                   
         NBASE 0,PUBCBIG,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,PUBCBIG+4095                                                  
         LA    R6,1(R6)                                                         
         USING PUBCBIG+4096,R6                                                  
*                                  NOTE USE OF R6 AS BASE REGISTER              
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*&&DO                                                                           
         OPEN  IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*&&                                                                             
*                                                                               
*                                                                               
START1   DS    0H                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    START10                                                          
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
START2   DS    0H                                                               
         CLC   =C'PRINT',CARD                                                   
         BNE   START3                                                           
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
START3   DS    0H                                                               
         B     START1                                                           
START10  DS    0H                                                               
*                                                                               
GET      DS    0H                                                               
         BAS   RE,GETREC                                                        
*                                                                               
*              ADD CODE FOR THIS RUN HERE                                       
*                                                                               
         CLC   REC(3),=3X'FF'      EOF RECORD                                   
         BE    EOF                                                              
         CLI   REC+9,X'81'         PUBREC                                       
         BE    PUB                                                              
         BNE   PUT                 COPY LITTLE PUBRECS                          
*                                                                               
         SPACE 3                                                                
PUB      DS    0H                                                               
         MVI   BIGSW,0                                                          
         CLC   REC+25(2),=X'0F78'                                               
         BNH   GET                                                              
         BAS   RE,DMPREC                                                        
         B     GET                                                              
*                                                                               
*                                                                               
PUB4     LA    R2,PUBREC+33                                                     
         CLI   0(R2),X'10'                                                      
         BE    PUB5                                                             
         MVI   ELCODE,X'10'               CNG 12/28/87  X'20' TO X'10'          
         BAS   RE,NEXTEL                                                        
         BE    PUB5                                                             
         AP    BADCNT,=P'1'                                                     
         MVC   P+1(15),=C'NO NAME ELEMENT'                                      
         GOTO1 =V(HEXOUT),DMCB,PUBREC,P+20,33                        *          
         BAS   RE,PRNT                                                          
         B     PUT                                                              
*                                                                               
PUB5     DS    0H                                                               
         USING PUBNAMEL,R2                ****************************          
         CLI   1(R2),160                              12/28/87       *          
         BE    EXPAND                                                *          
         CLI   1(R2),198                                             *          
         BE    *+6                                                   *          
         DC    H'0'                                                  *          
         AP    NEWCNT,=P'1'                198 NAMEL COUNT           *          
         MVI   BIGSW,C'Y'                                                       
         MVC   P+1(16),=C'ALREADY EXPANDED'                                     
         GOTO1 =V(HEXOUT),DMCB,PUBREC,P+20,33                        *          
         MVC   P+90(12),PUBNWZIP    PRINT WHAT WAS THERE             *          
         BAS   RE,PRNT                                               *          
         XC    ELEM,ELEM                                             *          
         MVC   ELEM(198),0(R2)            SAVE OLD 198 NAMEL         *          
         B     EXPAND2                   ACTUALLY CONTRACTING                   
*                                                                               
                                                                                
MOVEZIP1 MVC   PUBNWZIP(5),PUBZIP                                    *          
         XC    PUBZIP,PUBZIP                CLEAR OLD FIELD                     
         DROP  R2                                                    *          
         AP    NAMECNT,=P'1'                                         *          
         MVI   DMPSW,C'Y'                                            *          
         B     PUT                                                   *          
EXPAND   EQU   *                                                     *          
         AP    OLDCNT,=P'1'               160 NAMEL COUNT            *          
         XC    ELEM,ELEM                                             *          
         MVC   ELEM(160),0(R2)            SAVE OLD 160 NAMEL         *          
EXPAND2  MVI   ELEM+1,196                 NEW LENGTH                 *          
         XC    DMCB(24),DMCB                                         *          
         GOTO1 =V(RECUP),DMCB,(1,PUBREC),(R2)      DELETE 160 NAMEL  *          
         XC    DMCB(24),DMCB                                         *          
         GOTO1 =V(RECUP),DMCB,(1,PUBREC),ELEM,PUBREC+33    INSERT196 *          
         LA    R2,PUBREC+33                                          *          
         B     MOVEZIP1                   ****************************          
         SPACE 3                                                                
*                                                                               
PUT      DS    0H                                                               
         BAS   RE,PUTREC                                                        
         B     GET                                                              
         SPACE 2                                                                
*&&DO                                                                           
EOF      CLOSE IN,OUT                                                           
*&&                                                                             
*&&OS                                                                           
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
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
         B     EOJ                                                              
         B     EOJ                                                              
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
         CLI   BIGSW,C'Y'                                                       
         BE    PUTREC1         DUMP ALWAYS                                      
         CLI   DMPSW,C'Y'                                                       
         BNE   PUTREC2                                                          
         MVI   DMPSW,C'N'                                                       
         SP    DMPCNT,=P'1'                                                     
         BNP   PUTREC2                                                          
PUTREC1  BAS   RE,DMPREC                                                        
*                                                                               
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
*&&DO                                                                           
IN       DTFMT BLKSIZE=32760,RECFORM=VARBLK,TYPEFLE=INPUT,             X        
               IOAREA1=IN1,DEVADDR=SYS010,FILABL=NO,WORKA=YES,         X        
               EOFADDR=EOF                                                      
*&&                                                                             
*&&OS                                                                           
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*&&                                                                             
         EJECT                                                                  
*&&DO                                                                           
OUT      DTFMT BLKSIZE=32760,RECFORM=VARBLK,TYPEFLE=OUTPUT,            X        
               IOAREA1=OUT1,DEVADDR=SYS011,FILABL=NO,WORKA=YES                  
*&&                                                                             
*&&OS                                                                           
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*&&                                                                             
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
BIGSW    DS    CL1                                                              
DMPCNT   DC    PL5'100'                                                         
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
LKPARS   DS    6F                                                               
ESTPARS  DS    6F                                                               
TODAY    DS    CL8                                                              
PRTSW    DS    CL1                                                              
RTOTS    DS    11PL8               RUN TOTALS                                   
MTOTS    DS    11PL8               AGY MEDIA TOTALS                             
CTOTS    DS    11PL8               CLT TOTALS                                   
SAVAMCL  DS    CL6                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
*                                                                               
ELEM     DS    CL250                                                            
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
INCNT    DC    PL5'0',CL20'INPUT COUNT'                                         
OUTCNT   DC    PL5'0',CL20'OUTPUT COUNT'                                        
NAMECNT  DC    PL5'0',CL20'NAME ELEM COUNT'       CNG 12/28/87                  
OLDCNT   DC    PL5'0',CL20'OLD 160 ELEM COUNT'    ADD 12/28/87                  
NEWCNT   DC    PL5'0',CL20'NEW 198 ELEM COUNT'    ADD 12/28/87                  
NOTX0CNT DC    PL5'0',CL20'PUBNWZIP NOT 10HX0'    ADD 12/28/87                  
BADCNT   DC    PL5'0',CL20'NO NAME ELEMS'                                       
*              OTHER COUNTERS ADDED HERE WILL                                   
*              AUTOMATICALLY PRINT AT EOJ                                       
*                                                                               
COUNTSX  EQU   *-1                                                              
P        DC    CL133' '                                                         
         DS    F                                                                
REC      DS    4000C                                                            
         DS    D                                                                
*&&DO                                                                           
IN1      DS    8500C                                                            
*&&                                                                             
*&&DO                                                                           
OUT1     DS    8500C                                                            
*&&                                                                             
         SPACE 3                                                                
         ORG   REC                                                              
       ++INCLUDE PUBREC                                                         
*                                                                               
       ++INCLUDE PUBNAMEL                ADD 12/28/87                           
PUBPROD  DSECT                                                                  
       ++INCLUDE PUBGENEL                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050PUBCBIG   05/01/02'                                      
         END                                                                    
