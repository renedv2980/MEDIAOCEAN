*          DATA SET DDPANFILT  AT LEVEL 153 AS OF 08/13/00                      
*PHASE PANFILTA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE CARDS                                                                  
*INCLUDE HEXIN                                                                  
         SPACE 1                                                                
         TITLE 'PANFILT - FILTER PAN2 INDEX LISTING'                            
         PRINT NOGEN                                                            
         EJECT                                                                  
PANFILT  CSECT                                                                  
         NBASE WORKX-WORKD,**PANF**,R9,WORK=V(REGSAVE),CLEAR=YES                
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,INIT             READ CARDS ECT                               
         BAS   RE,OPENFILS         OPEN FILES                                   
         BAS   RE,MAIN             MAIN LOOP                                    
*                                                                               
XBASE    L     RD,SAVERD           GET HERE FROM ANYWHERE                       
         XBASE 1                                                                
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
         LA    R1,TITLE1           PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         LA    R3,IOAREA                                                        
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    EXIT                                                             
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BNE   XBASE               NEQ MEANS INVALID KEYWORD                    
         B     INIT010                                                          
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        OPEN INPUT                                         *                   
*************************************************************                   
         SPACE 1                                                                
OPENFILS NTR1                                                                   
         OPEN  (PANIN,INPUT)                                                    
         OPEN  (PANEX,INPUT)                                                    
         OPEN  (PANOUT,OUTPUT)                                                  
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        MAIN                                               *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
*                                                                               
MAIN010  MVC   PLINE,SPACES                                                     
         GET   PANIN                                                            
         LR    R6,R1                                                            
*                                                                               
         BAS   RE,FILTER                                                        
         BNE   MAIN010                                                          
         BAS   RE,PRINT                                                         
         BAS   RE,PRINTL                                                        
         BAS   RE,PUTPAN                                                        
         B     MAIN010                                                          
*                                                                               
PANEND   B     EXIT                                                             
         EJECT                                                                  
*****************************************************************               
*        FILTER                                                 *               
*****************************************************************               
         SPACE 1                                                                
FILTER   NTR1                                                                   
         USING PANLINED,R6                                                      
*                                                                               
FILT010  CLC   PANSTAT2,=C'ACTV'   ACTIVE BOOKS ONLY                            
         BNE   EXITNEQ                                                          
*                                                                               
FILT020  CLI   EXLEN,0             TEST NO CARDS READ YET                       
         BNE   *+8                                                              
         BAS   RE,GETCARD          GET FIRST CARD                               
         CLI   EXLEN,X'FF'         TEST ALL CARDS READ                          
         BE    EXITEQU                                                          
*                                                                               
FILT030  SR    R1,R1               TEST EXCLUDE BOOK                            
         IC    R1,EXLEN                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PANNAME(0),EXCARD                                                
         BE    EXITNEQ             IF MATCH THEN EXCLUDE                        
         BL    EXITEQU             IF LOWER THEN OK                             
         BAS   RE,GETCARD                                                       
         B     FILT020             GO BACK WITH NEXT CARD                       
*                                                                               
FILT110  B     EXITEQU                                                          
*                                                                               
GETCARD  ST    RE,SAVERE           GET NEXT EXCLUDE CARD                        
         GET   PANEX,EXCARD                                                     
         LA    RF,EXCARD                                                        
         LA    R1,EXCARD+10                                                     
GETC010  CLI   0(R1),C'*'          LOOK FOR A *                                 
         BE    GETC020                                                          
         BCTR  R1,0                TRACK BACKWARDS                              
         CR    R1,RF                                                            
         BH    GETC010                                                          
         MVI   EXLEN,9             NO * SO LENGTH IS 9                          
         B     GETC990                                                          
*                                                                               
GETC020  SR    R1,RF                                                            
         SH    R1,=H'1'            SUB 1 FOR EX LEN                             
         STC   R1,EXLEN                                                         
         B     GETC990                                                          
*                                                                               
PANEXX   MVI   EXLEN,X'FF'         FLAG EOF                                     
                                                                                
GETC990  L     RE,SAVERE           RETURN                                       
         BR    RE                                                               
         DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*        PUT A LINE TO PANOUT                               *                   
*************************************************************                   
         SPACE 1                                                                
PUTPAN   NTR1                                                                   
         MVC   IOAREA(132),0(R6)                                                
         PUT   PANOUT,IOAREA                                                    
         B     EXITEQU                                                          
         EJECT                                                                  
*************************************************************                   
*        BUILD A PRINT LINE FROM ADRREC @ R6                *                   
*************************************************************                   
         SPACE 1                                                                
PRINT    NTR1                                                                   
         LA    R7,PLINE            R7=A(PLINE)                                  
         USING PANLINED,R7                                                      
*                                                                               
         MVC   PLINE,0(R6)                                                      
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***************************************************                             
*        CLOSE FILES AND EXIT                     *                             
***************************************************                             
         SPACE 1                                                                
DISKEND  EQU   *                                                                
*                                                                               
TSTEND   CLOSE (SYSPRINT)                                                       
         CLOSE (PANIN)                                                          
         CLOSE (PANOUT)                                                         
         CLOSE (PANEX)                                                          
         XBASE                                                                  
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN-1),X'FLAGS',AL3(OUTPUT)           
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*                                                                               
         SPACE 1                                                                
CARDTAB  DS    0F                                                               
         DC    C'INPUT  ',AL1(4,0),X'0000',AL3(IPTYP)                           
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
IPTYP    DC    C'T'                INPUT=TAPE                                   
*                                                                               
         EJECT                                                                  
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC090                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC090                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC090                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC090                                                          
*                                                                               
VALC080  CLI   8(R4),0             DONT CARE                                    
         BE    *+12                                                             
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC090  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC090                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU  *                   
*************************************************************                   
         SPACE 1                                                                
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALF                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BAS   RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BAS   RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BAS   RE,VALTADD                                                       
         B     EXITEQU                                                          
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
CERRSYS  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
CERRPRG  LA    R1,=C'INVALID PROGRAM '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SESYS   '                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BAS   RE,PRINTL                                                        
*                                                                               
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
         SPACE 1                                                                
PRINTI   ST    RE,SAVERE                                                        
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'1'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        PRINT TITLE                                        *                   
*************************************************************                   
         SPACE 1                                                                
TITLE    DC    CL166' '                                                         
         ORG   TITLE                                                            
         DC    C'1',X'40'                                                       
         DC    C'END TIME       TERMINAL  SYS  SE SYS   PROGRAM  TK'            
         DC    C'    SIN     START TIME       CPU     QUE    ET    I/P'         
         DC    C'   O/P   #IO   #OV'                                            
         ORG                                                                    
TITLE1   DC    CL166' '                                                         
         ORG   TITLE1                                                           
         DC    C'1',X'40'                                                       
         DC    C'-----------------------------------------------------'         
         DC    C'------------------ PARAMETER CARDS ------------------'         
         DC    C'-----------------------------------------------------'         
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        DCBS                                               *                   
*************************************************************                   
*                                                                               
*        LRECL=(166) USE CHARS=(BX15)                                           
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
PANIN    DCB   DSORG=PS,MACRF=GL,DDNAME=PANIN,EODAD=PANEND                      
PANEX    DCB   DSORG=PS,MACRF=GM,DDNAME=PANEX,EODAD=PANEXX                      
PANOUT   DCB   DSORG=PS,MACRF=PM,DDNAME=PANOUT                                  
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
         SPACE 1                                                                
MAXLINE  DC    P'60'                                                            
SPACES   DC    CL166' '                                                         
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        TABLES                                             *                   
*************************************************************                   
         SPACE 1                                                                
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
*                                                                               
SAVERD   DS    F                                                                
SAVERE   DS    F                                                                
CARDRD   DS    F                                                                
CARDR2   DS    F                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
EXLEN    DS    X                                                                
DMCB     DS    6F                                                               
WORK     DS    CL64                                                             
WORK1    DS    CL64                                                             
*                                                                               
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
CARDEND  DS    A                                                                
*                                                                               
EXCARD   DS    CL80                                                             
*                                                                               
PLINE    DS    CL166                                                            
*                                                                               
IOAREA   DS    4096C               4K BUFFER                                    
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 1                                                                
PANLINED DSECT                                                                  
         DS    0CL132                                                           
         DS    CL1                                                              
PANNAME  DS    CL10                                                             
         DS    CL2                                                              
PANLEVEL DS    CL3                                                              
         DS    CL2                                                              
PANUSER  DS    CL4                                                              
         DS    CL4                                                              
PANTYPE  DS    CL9                                                              
         DS    CL3                                                              
PANSTAT1 DS    CL4                                                              
         DS    CL1                                                              
PANSTAT2 DS    CL4                                                              
         DS    CL1                                                              
PANSTAT3 DS    CL5                                                              
         DS    CL1                                                              
PANDATEM DS    CL8                                                              
         DS    CL1                                                              
PANDATEA DS    CL8                                                              
         DS    CL1                                                              
PANBLKS  DS    CL6                                                              
         DS    CL2                                                              
PANLINES DS    CL10                                                             
         DS    CL1                                                              
PANLAST  DS    CL6                                                              
         DS    CL1                                                              
PANAVG   DS    CL5                                                              
         DS    CL1                                                              
PANPCT   DS    CL4                                                              
         DS    CL1                                                              
PANSUBS  DS    CL7                                                              
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'153DDPANFILT 08/13/00'                                      
         END                                                                    
