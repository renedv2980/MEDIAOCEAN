*          DATA SET DDDSPPRT   AT LEVEL 001 AS OF 12/18/14                      
*PHASE DSPPRTA                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'DDDSPACE - PRINT OUT DATASPACE'                                 
         PRINT NOGEN                                                            
DDDSPACE CSECT                                                                  
         NBASE WORKX-WORKD,DDDSPACE,=A(WORKAREA),RA,R9                          
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,INIT             READ CARDS ETC                               
         BNE   XBASE                                                            
         BAS   RE,MAIN             MAIN LOOP FOR MODE=INIT                      
*                                                                               
         BAS   RE,PRINTX                                                        
         B     XBASE                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         LA    R2,CARD                                                          
         L     RF,=V(CARDS)                                                     
*                                                                               
INIT02   GOTO1 (RF),DMCB,(R2),=C'RE00'                                          
         CLC   =C'/*',0(R2)        END OF CARDS?                                
         BE    INIT04              YES                                          
         MVC   PLINE+1(L'CARD),CARD                                             
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R2               VALIDATE KEYWORD=VALUE                       
         BAS   RE,CARDVAL          CARDVAL SETS CC                              
         BNE   EXITL                                                            
         B     INIT02                                                           
*                                                                               
INIT04   BAS   RE,GETSPC                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAIN CODE FOR MODE=INIT                                             *         
***********************************************************************         
         SPACE 1                                                                
MAIN     NTR1  ,                                                                
         MVC   PLINE,SPACES                                                     
*                                                                               
         XR    R2,R2                                                            
         LAM   AR0,ARF,ARZERO                                                   
         LAM   AR2,AR2,DMALET                                                   
*                                                                               
* OBTAIN DATASPACE LENGTH                                                       
         SAC   512                                                              
         MVC   DSPCLEN,DHALAST-DMDSHDR(R2)                                      
         SAC   0                                                                
*                                                                               
         L     R3,DSPCLEN                                                       
*                                                                               
         MVC   HEXSAVE,=C'*HEXSAVEHEXSAVE*'                                     
         MVI   SAMEFLAG,C'N'                                                    
*                                                                               
MAIN02   CR    R3,R2                                                            
         BNH   MAINX                                                            
*                                                                               
         SAC   512                                                              
         MVC   HEX,0(R2)                                                        
         SAC   0                                                                
*                                                                               
         MVC   PLINE,SPACES                                                     
*                                                                               
         STCM  R2,15,FULL                                                       
         GOTO1 =V(HEXOUT),DMCB,FULL,PLADDR,L'FULL,0                             
*                                                                               
         CLC   HEXSAVE,HEX         SAME DATA AS BEFORE?                         
         BNE   MAIN10              NO - PRINT IT                                
         CLI   SAMEFLAG,C'Y'       PRINTED "SAME" ALREADY?                      
         BE    MAIN30              YES - DON'T REPEAT IT                        
*                                                                               
         MVC   PLHEX1,=C'--SAME--'                                              
         MVI   SAMEFLAG,C'Y'                                                    
         B     MAIN20                                                           
*                                                                               
MAIN10   DS    0H                                                               
         MVC   HEXSAVE,HEX                                                      
         MVI   SAMEFLAG,C'N'                                                    
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,HEX,NUM,L'HEX,0                                  
*                                                                               
         MVC   PLHEX1,NUM                                                       
         MVC   PLHEX2,NUM+8                                                     
         MVC   PLHEX3,NUM+16                                                    
         MVC   PLHEX4,NUM+24                                                    
*                                                                               
         TR    HEX,TRTAB                                                        
         MVC   PLALPH,HEX                                                       
*                                                                               
MAIN20   DS    0H                                                               
         BAS   RE,PRINTL                                                        
*                                                                               
MAIN30   DS    0H                                                               
         AHI   R2,L'HEX                                                         
         B     MAIN02                                                           
*                                                                               
MAINX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATASPACE ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
FREESPC  ST    RE,SAVERE           DELETE DATASPACE                             
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'DEL '                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                 NB - IGNORE CONDITON CODE                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
MAKESPC  ST    RE,SAVERE           CREATE NEW DATASPACE                         
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'MAKE'                                                 
         MVC   WORK+4(12),DSPACE                                                
         ICM   RF,15,PAGES         NUMBER OF 4K PAGES                           
         STCM  RF,15,WORK+16                                                    
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GETSPC   ST    RE,SAVERE           GET DMALET                                   
         LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+4(12),DSPACE                                                
         SVC   247                                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DMOFFS,WORK+20      EXTRACT VALUES                               
         MVC   DMALET,WORK+24                                                   
         MVC   DMTOKN,WORK+28                                                   
         OC    DMALET,DMALET                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRINTER                                                  *         
***********************************************************************         
         SPACE 1                                                                
PRINTI   NTR1  ,                                                                
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PRINT TITLES                                                        *         
***********************************************************************         
         SPACE 1                                                                
PRINTT   NTR1  ,                                                                
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PRINT LINE                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRINTL   NTR1  ,                                                                
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,T1         PRINT TITLES                                 
         PUT   SYSPRINT,T2                                                      
         PUT   SYSPRINT,T3                                                      
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         B     EXITOK              EXIT                                         
         SPACE 2                                                                
***********************************************************************         
* CLOSE PRINTER                                                       *         
***********************************************************************         
         SPACE 1                                                                
PRINTX   NTR1  ,                                                                
         CLOSE SYSPRINT                                                         
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE INPUT CARDS                                     *         
***********************************************************************         
         SPACE 1                                                                
CARDVAL  NTR1  ,                                                                
         ST    RD,CARDRD                                                        
         LR    R2,R1               R2=A(CARD START)                             
         LA    R1,79(R2)                                                        
         ST    R1,CARDEND          SAVE A(LAST CHAR)                            
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
         GOTO1 =V(SCANNER),DMCB,(C'C',(R2)),(1,SCNBLK)                          
         CLI   4(R1),0                                                          
         BE    CEINVLIN            INVALID LINE                                 
*                                                                               
         LA    R2,SCNBLK                                                        
         USING SCANBLKD,R2                                                      
         LA    R3,CARDTAB                                                       
         USING CARDTABD,R3                                                      
         XR    RF,RF                                                            
*                                                                               
CARDV02  CLI   CNAME,CARDEOT       END OF TABLE                                 
         BE    CEINVKEY            INVALID KEYWORD                              
         ICM   RF,1,CXLEN                                                       
         EX    RF,*+8                                                           
         BE    CARDV06                                                          
         CLC   SC1STFLD(0),CNAME                                                
CARDV04  LA    R3,CARDTABL(R3)                                                  
         B     CARDV02                                                          
*                                                                               
CARDV06  CLI   CTYPE,CTNUM         NUMERIC INPUT?                               
         BNE   CARDV08             NO                                           
         TM    SC2NDVAL,SCNUMQ                                                  
         BNO   CENOTNUM                                                         
         CLC   SC2NDNUM,CMIN       SCOPE FOR MAX/MIN VALUES                     
         BL    CETOOLOW                                                         
         CLC   SC2NDNUM,CMAX                                                    
         BH    CETOOBIG                                                         
         ICM   RF,15,COUT                                                       
         MVC   0(4,RF),SC2NDNUM    SET NUMERIC VALUE INTO OUTPUT                
         B     EXITOK                                                           
*                                                                               
CARDV08  CLI   CTYPE,CTCHR         CHARACTER INPUT                              
         BNE   CARDV10             NO                                           
         XR    RF,RF                                                            
         ICM   RF,1,SC2NDLEN                                                    
         BZ    CENOINP                                                          
         C     RF,CMIN             SCOPE FOR LENGTH                             
         BL    CETOOSHT                                                         
         C     RF,CMAX                                                          
         BH    CETOOLNG                                                         
         ICM   RE,15,COUT          MOVE IN FIELD                                
         ICM   RF,1,CLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,RE),SC2NDFLD                                                 
         B     EXITOK                                                           
*                                                                               
CARDV10  DC    H'0'                EXTRA TYPES HERE                             
*                                                                               
CEINVLIN LA    R1,=CL40'Invalid Line Format'                                    
         B     CERR                                                             
*                                                                               
CEINVKEY LA    R1,=CL40'Invalid Keyword'                                        
         B     CERR                                                             
*                                                                               
CENOTNUM LA    R1,=CL40'Value not a valid number'                               
         B     CERR                                                             
*                                                                               
CENOTCHR LA    R1,=CL40'Value not a valid character string'                     
         B     CERR                                                             
*                                                                               
CETOOSHT LA    R1,=CL40'Length of input string too short'                       
         B     CERR                                                             
*                                                                               
CETOOLNG LA    R1,=CL40'Length of input string too long'                        
         B     CERR                                                             
*                                                                               
CETOOLOW LA    R1,=CL40'Numeric value too small'                                
         B     CERR                                                             
*                                                                               
CETOOBIG LA    R1,=CL40'Numeric value too large'                                
         B     CERR                                                             
*                                                                               
CENOINP  LA    R1,=CL40'Invalid/missing value'                                  
         B     CERR                                                             
*                                                                               
CERR     L     RD,CARDRD                                                        
         MVC   PLINE,SPACES                                                     
         MVC   PLINE(15),=CL15' *** ERROR ***'                                  
         MVC   PLINE+15(40),0(R1)                                               
         BAS   RE,PRINTL                                                        
         B     EXITL                                                            
*                                                                               
***********************************************************************         
* INPUT CARDS TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         ORG   DDDSPACE+((((*-DDDSPACE)/16)+1)*16)                              
         SPACE 1                                                                
CARDTAB  DS    0D                                                               
         DC    CL8'DSPACE  ',F'001',F'012'                                      
         DC    X'05',AL1(CTCHR),AL1(L'DSPACE),AL1(0),AL4(DSPACE)                
         DC    X'FFFFFFFF'                                                      
         SPACE 2                                                                
***********************************************************************         
* CARD TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
CARDTABD DSECT                                                                  
CNAME    DS    CL8                 INPUT CARD                                   
CMIN     DS    F                   MINIMUM (VALUE OR LENGTH)                    
CMAX     DS    F                   MAXIMUM (VALUE OR LENGTH)                    
CXLEN    DS    XL1                 LEN-1 OF CNAME VALUE FOR COMPARE             
CTYPE    DS    AL1                 INPUT TYPE                                   
CTNUM    EQU   1                   NUMERIC                                      
CTCHR    EQU   2                   CHARACTER                                    
CLEN     DS    X                   OUTPUT AREA LENGTH (CHAR ONLY)               
         DS    XL1                 N/D                                          
COUT     DS    AL4                 A(OUTPUT AREA)                               
CARDTABL EQU   *-CARDTABD                                                       
*                                                                               
CARDEOT  EQU   X'FF'                                                            
*                                                                               
DDDSPACE CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
ARZERO   DC    16F'0'                                                           
*                                                                               
DSPCLEN  DC    A(0)                                                             
*                                                                               
SPACES   DC    CL166' '                                                         
MAXLINE  DC    PL3'60'                                                          
         SPACE 2                                                                
T1       DC    166C' '                                                          
T2       DC    166C' '                                                          
T3       DC    166C' '                                                          
         SPACE 2                                                                
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
DSPACE   DC    CL12' '                                                          
*                                                                               
TRTAB    DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  00-0F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  10-1F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  20-2F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  30-3F                    
         DC    XL16'406F6F6F6F6F6F6F6F6F4A4B4C4D4E4F'  40-4F                    
         DC    XL16'506F6F6F6F6F6F6F6F6F5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60616F6F6F6F6F6F6F6F6A6B6C6D6E6F'  60-6F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F797A7B7C7D7E7F'  70-7F                    
         DC    XL16'6F8182838485868788896F6F6F6F6F6F'  80-8F                    
         DC    XL16'6F9192939495969798996F6F6F6F6F6F'  90-9F                    
         DC    XL16'6FA1A2A3A4A5A6A7A8A96F6F6F6F6F6F'  A0-AF                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C96F6F6F6F6F6F'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D96F6F6F6F6F6F'  D0-D1                    
         DC    XL16'E06FE2E3E4E5E6E7E8E96F6F6F6F6F6F'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F96F6F6F6F6F6F'  F0-FF                    
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE DC                                 *                   
*************************************************************                   
         SPACE 1                                                                
         DS    0D                                                               
BUFFER   DS    60000C                                                           
         DS    0D                                                               
WORKAREA DC    60000X'00'                                                       
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 2                                                                
WORKD    DSECT                                                                  
HEXSAVE  DS    XL16                                                             
HEX      DS    XL16                                                             
NUM      DS    XL32                                                             
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
LONG     DS    L                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
SAMEFLAG DS    C                                                                
*                                                                               
DMCB     DS    6F                                                               
CARDEND  DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
GETLEN   DS    F                   LENGTH RETURNED IF GETMAIN                   
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
*                                                                               
DMOFFS   DS    A                   DATASPACE OFFSET                             
DMALET   DS    A                   DMALET                                       
DMTOKN   DS    CL8                 TOKEN                                        
*                                                                               
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
*                                                                               
PLINE    DS    CL166                                                            
*                                                                               
         ORG   PLINE                                                            
PLADDR   DS    CL8                                                              
         DS    CL4                                                              
PLHEX1   DS    CL8                                                              
         DS    CL2                                                              
PLHEX2   DS    CL8                                                              
         DS    CL2                                                              
PLHEX3   DS    CL8                                                              
         DS    CL2                                                              
PLHEX4   DS    CL8                                                              
         DS    CL4                                                              
PLALPH   DS    CL(L'HEX)                                                        
         ORG                                                                    
                                                                                
*                                                                               
*                                                                               
TITLE    DS    CL166                                                            
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
SCNBLK   DS    3CL(SCBLKLQ)                                                     
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
         TITLE 'VARIABLE SCAN MODULE'                                           
SCANNER  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SWORKX-SWORKD,**SCAN**                                           
         USING SWORKD,RC                                                        
         LR    R9,R1               R9=A(PARAMETER LIST)                         
         LM    R2,R3,0(R9)         R2=A(DATA STRING) R3=A(BLOCK)                
         MVC   MAXLINES,4(R9)                                                   
         XC    SDISP,SDISP                                                      
         SR    R4,R4                                                            
         IC    R4,5(R2)            L'DATA IF SCREEN FIELD                       
         LA    R2,8(R2)                                                         
         MVC   LROW,=H'42'         PRESET DEFAULT LENGTHS                       
         MVC   LRIGHT,=H'20'                                                    
         MVC   LBOTH,=H'30'                                                     
         CLI   0(R9),C'C'                                                       
         BE    SCAN1                                                            
*                                                                               
SCAN1    SH    R2,=H'8'                                                         
         LA    R4,80                                                            
         CLC   0(80,R2),SSPACES                                                 
         BE    ERROR2                                                           
         LA    R5,79(R2)                                                        
         SPACE 2                                                                
SCAN2    CLI   0(R5),C' '                                                       
         BNE   SCAN4                                                            
         BCTR  R5,0                                                             
         BCT   R4,SCAN2                                                         
         SPACE 2                                                                
SCAN4    LA    R5,0(R2,R4)         L'DATA IN R4                                 
         MVC   BORROW,0(R5)        SAVE THE NEXT CHARACTER                      
         MVC   0(1,R5),COMMA       AND POP IN A COMMA TO SIMPLIFY               
         SR    R6,R6               R6=NUMBER OF LINES USED                      
         EJECT                                                                  
*HANDLE LINES OF DATA                                                           
*                                                                               
SCAN6    XC    0(12,R3),0(R3)      PRESET A LINE                                
         LH    RF,LBOTH                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),SSPACES                                                 
         MVC   2(2,R3),=X'E0E0'                                                 
         BAS   RE,GETL                                                          
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         MVI   2(R3),0                                                          
         CLI   1(R3),0                                                          
         BNE   *+8                                                              
         MVI   3(R3),0                                                          
         CLC   0(1,R3),LBOTH+1                                                  
         BH    ERROR                                                            
         CLC   1(1,R3),LRIGHT+1                                                 
         BH    ERROR                                                            
         CLI   1(R3),0                                                          
         BE    SCAN8                                                            
         CLI   0(R3),10                                                         
         BH    ERROR                                                            
         SPACE 2                                                                
SCAN8    SR    R7,R7                                                            
         IC    R7,0(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN18                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R2)                                                   
         TM    2(R3),X'80'                                                      
         BZ    SCAN10                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN10                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,5(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING DISPS.               
         BO    SCAN10                                                           
         ST    R8,4(R3)                                                         
         SPACE 2                                                                
SCAN10   LA    R2,2(R2,R7)                                                      
         IC    R7,1(R3)                                                         
         LTR   R7,R7                                                            
         BZ    SCAN20                                                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),0(R2)                                                   
         TM    3(R3),X'80'                                                      
         BZ    SCAN12                                                           
         CH    R7,=H'8'                                                         
         BH    SCAN12                                                           
         EX    R7,VARPAK                                                        
         CVB   R8,SDUB                                                          
         STCM  R8,7,9(R3)          STORE AT LEAST 3 BYTES BINARY                
         TM    4(R9),X'80'         THAT'S ALL IF RETURNING SDISPS.              
         BO    SCAN12                                                           
         ST    R8,8(R3)                                                         
         SPACE 2                                                                
SCAN12   LA    R2,2(R2,R7)                                                      
         B     SCAN20                                                           
         SPACE 2                                                                
VARPAK   PACK  SDUB,0(0,R2)                                                     
         SPACE 2                                                                
SCAN18   LA    R2,1(R2)                                                         
         CLI   1(R3),0                                                          
         BNE   ERROR                                                            
         SPACE 2                                                                
SCAN20   LA    R6,1(R6)            BUMP N'LINES                                 
         AH    R3,LROW             BUMP TO NEXT LINE IN BLOCK                   
         CR    R2,R5               ARE WE NOW PAST LAST 'COMMA'                 
         BH    OK                                                               
         IC    R7,MAXLINES                                                      
         LTR   R7,R7                                                            
         BZ    SCAN6                                                            
         CR    R6,R7               HAVE WE REACHED MAX N'LINES                  
         BNE   SCAN6                                                            
         SPACE 2                                                                
OK       MVC   0(1,R5),BORROW      RETURN THE BYTE                              
         STC   R6,4(R9)            SET NUMBER OF LINES USED                     
         B     XIT                                                              
         SPACE 2                                                                
ERROR    MVI   4(R9),0                                                          
         MVC   0(1,R5),BORROW                                                   
         MVC   2(2,R3),=X'FFFF'                                                 
         B     XIT                                                              
         SPACE 2                                                                
ERROR2   MVI   4(R9),0                                                          
         MVC   2(2,R3),=X'FFFF'                                                 
         SPACE 2                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
*VALIDATE AND GET LENGTHS                                                       
*                                                                               
GETL     NTR1                                                                   
         LR    R4,R3                                                            
         SR    R5,R5                                                            
         TM    4(R9),X'80'                                                      
         BZ    GETL2                                                            
         MVC   4(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
         SPACE 2                                                                
GETL2    CLC   0(1,R2),COMMA       TEST FIELD SEPERATOR                         
         BE    GETL12                                                           
         CLC   0(1,R2),EQUAL                                                    
         BE    GETL14                                                           
         SPACE 2                                                                
GETL3    LA    R5,1(R5)                                                         
         CLI   0(R2),C'9'                                                       
         BNH   *+8                                                              
         MVI   2(R4),0             (ALL INVALID)                                
         CLI   0(R2),C'0'                                                       
         BL    GETL4                                                            
         NI    2(R4),X'BF'         (INVALID ALPHA)                              
         B     GETL10                                                           
         SPACE 2                                                                
GETL4    NI    2(R4),X'7F'         (INVALID NUM)                                
         CLI   0(R2),C'Z'                                                       
         BNH   GETL6                                                            
         MVI   2(R4),0             Z-0 = ALL INVALID                            
         B     GETL10                                                           
         SPACE 2                                                                
GETL6    CLI   0(R2),C'A'          LESS THAN A = ALL INVALID                    
         BNL   GETL8                                                            
         MVI   2(R4),0                                                          
         B     GETL10                                                           
         SPACE 2                                                                
GETL8    CLI   0(R2),C'F'          OK FOR ALPHA                                 
         BNH   GETL10                                                           
         NI    2(R4),X'DF'         G-Z = INVALID HEX                            
         SPACE 2                                                                
GETL10   LA    R2,1(R2)                                                         
         B     GETL2                                                            
         SPACE 2                                                                
GETL12   STC   R5,0(R4)            COMMA FOUND                                  
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         B     XIT                                                              
         SPACE 2                                                                
GETL14   CR    R4,R3               EQUAL FOUND - IS THIS THE FIRST ONE?         
         BNE   GETL3               TREAT AS NORMAL CHARACTER IF NOT             
         STC   R5,0(R4)            NOW STORE L1                                 
         LA    R5,1(R5)                                                         
         AH    R5,SDISP                                                         
         STH   R5,SDISP                                                         
         TM    4(R9),X'80'                                                      
         BZ    GETL16                                                           
         MVC   8(1,R4),SDISP+1     DISPLACEMENT INTO FIELD                      
         SPACE 2                                                                
GETL16   LA    R4,1(R4)            POINT TO FIELD2 DATA                         
         SR    R5,R5               CLEAR L2                                     
         LA    R2,1(R2)            POINT PAST EQUAL SIGN                        
         B     GETL2                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
COMMA    DC    C','                                                             
EQUAL    DC    C'='                                                             
SSPACES  DC    CL80' '                                                          
         SPACE 2                                                                
SWORKD   DSECT                                                                  
SDUB     DS    D                                                                
SWORK    DS    CL32                                                             
LASTSTOP DS    F                                                                
BORROW   DS    CL1                                                              
MAXLINES DS    CL1                                                              
LROW     DS    H                                                                
LRIGHT   DS    H                                                                
LBOTH    DS    H                                                                
SDISP    DS    H                                                                
*                                                                               
SWORKX   DS    0C                                                               
         SPACE 1                                                                
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 2                                                                
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* FATABSD                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATABSD                                                        
         PRINT ON                                                               
* FATABSDMP                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDMP                                                      
         PRINT ON                                                               
* FATABSZIP                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSZIP                                                      
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* FASCTTAB                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASCTTAB                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDDSPPRT  12/18/14'                                      
         END                                                                    
