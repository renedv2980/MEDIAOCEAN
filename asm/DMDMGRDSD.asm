*          DATA SET DMDMGRDSD  AT LEVEL 056 AS OF 03/23/06                      
*PHASE DMGRDSD                                                                  
*INCLUDE FATABOFF                                                               
*INCLUDE CARDS                                                                  
*INCLUDE PERVAL                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE DDWTO                                                                  
         TITLE 'DMDMGRDSD - TAKE A DUMP OF DATASPACES '                         
         PRINT NOGEN                                                            
DMGRDSD  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         NBASE WORKX-WORKD,**DMDS**,=A(WORKAREA),RA,R9                          
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         BRAS  RE,PRINTI           INIT PRINTING                                
         BRAS  RE,INIT             READ CARDS ECT                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=XL4'20000000'                                           
         GOTO1 ALOCKSPC,DMCB                                                    
*                                                                               
         MVC   DMCB(4),=XL4'20008000'                                           
         GOTO1 ALOCKSPC,DMCB                                                    
*                                                                               
         L     RF,=A(SSB)                                                       
         MVC   ALET,SSOALET-SSOOFF(RF)                                          
         MVC   TBLET,SSOTBLET-SSOOFF(RF)                                        
*                                                                               
         SR    R2,R2                                                            
         LAM   R2,R2,ALET                                                       
         SAC   512                                                              
         OC    60(4,R2),60(R2)                                                  
         BZ    *+10                                                             
         MVC   DSEND(4),60(R2)                                                  
         LAM   R2,R2,TBLET                                                      
         OC    60(4,R2),60(R2)                                                  
         BZ    *+10                                                             
         MVC   DSENDT(4),60(R2)                                                 
         SAC   0                                                                
*                                                                               
ENDITALL ALESERV EXTRACT,STOKEN=DSPCSTOK,ALET=ALET                              
         ALESERV EXTRACT,STOKEN=DSPCSTOT,ALET=TBLET                             
         ABEND 901,DUMP,DUMPOPX=SNAPCAN                                         
*                                                                               
SNAPCAN  SNAPX DSPSTOR=DSSTART,MF=L                                             
*                                                                               
         DS    0F                                                               
DSSTART  DC    X'00000000'                                                      
DSEND    DC    X'00080000'                                                      
DSPCSTOK DC    X'0000000000000000'                                              
*                                                                               
DSSTARTT DC    X'00000000'                                                      
DSENDT   DC    X'80080000'                                                      
DSPCSTOT DC    X'0000000000000000'                                              
*                                                                               
*************************************************************                   
*        END OF JOB                                         *                   
*************************************************************                   
         SPACE 1                                                                
ENDOFJOB EQU   *                                                                
         WTO   'END OF JOB ',MCSFLAG=HRDCPY                                     
         B     XBASE                                                            
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
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,T1         PRINT TITLE                                  
         PUT   SYSPRINT,T2                                                      
         PUT   SYSPRINT,T3                                                      
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
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN),X'FLAGS',AL3(OUTPUT)             
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*                                                                               
         SPACE 1                                                                
CARDTAB  DS    0F                                                               
         DC    C'DDSIO  ',AL1(4,09),X'0000',AL3(DDSIO)                          
         DC    C'MODE   ',AL1(3,09),X'0000',AL3(MODE)                           
         DC    C'DSPACE ',AL1(5,00),X'0000',AL3(SSB+SSODSPAC-SSOOFF)            
         DC    X'0000'                                                          
*                                                                               
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
DDSIO    DC    CL10'DDSIO'                                                      
MODE     DC    CL10'INIT'          DEFAULT TO INIT                              
*                                                                               
         EJECT                                                                  
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
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
         B     VALC500                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         CLC   0(2,RF),=C'  '      EMPTY ENTRY                                  
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
         B     VALC500                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BRAS  RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC500                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BRAS  RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC500                                                          
*                                                                               
VALC080  TM    9(R4),X'01'         DATE INPUT                                   
         BZ    VALC400                                                          
         LA    R0,1(R1)            SET R0 INPUT LEN                             
         ST    RF,FULL                                                          
         GOTO1 =V(PERVAL),DMCB,((R0),(R2)),(X'60',WORK)                         
         L     RF,FULL                                                          
         CLI   4(R1),X'04'                                                      
         BNE   CERRDAT                                                          
         MVC   0(2,RF),WORK+PVALCSTA-PERVALD                                    
         B     VALC500                                                          
*                                                                               
VALC400  CLI   8(R4),0             DONT CARE                                    
         BE    VALC410                                                          
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         SR    RE,RE                                                            
         IC    RE,8(R4)            PAD OUT TO SPACES                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SPACES                                                   
VALC410  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC500  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC500                                                          
         B     EXITOK                                                           
         EJECT                                                                  
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
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDAT  LA    R1,=C'INVALID DATE    '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SYSTEM  '                                          
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
         BRAS  RE,PRINTL                                                        
         B     EXITL                                                            
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
         BRAS  RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BRAS  RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BRAS  RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BRAS  RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITOK                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BRAS  RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITOK                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BRAS  RE,VALTADD                                                       
         B     EXITOK                                                           
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
*************************************************************                   
*        GET FSESYS FROM 0(R2) (R1)=EX LEN                  *                   
*************************************************************                   
         SPACE 1                                                                
VALSYSI  NTR1                                                                   
         LR    RF,R1                                                            
         L     R4,=V(SELIST)       MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
VALSYSI0 EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SENAME      TEST NAME                                    
         BE    VALSYSI1                                                         
         BXLE  R4,R0,VALSYSI0      NEXT                                         
         B     EXITL               ERROR SE SYS NOT FOUND                       
VALSYSI1 LR    R1,R4                                                            
         B     EXITOK                                                           
*                                                                               
VALSYS   NTR1                                                                   
         LR    RF,R1                                                            
         L     R4,=V(SELIST)       MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
VALSYS0  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SENAME      TEST NAME                                    
         BE    VALSYS1                                                          
         BXLE  R4,R0,VALSYS0       NEXT                                         
         B     CERRSES             ERROR SE SYS NOT FOUND                       
*                                                                               
VALSYS1  LA    R1,SEFILN           FIND A ZERO ENTRY                            
VALSYS1A CLI   0(R1),0                                                          
         BE    VALSYS2                                                          
         LA    R1,1(R1)                                                         
         B     VALSYS1A                                                         
*                                                                               
VALSYS2  MVC   0(1,R1),SESYS       SET NUMBER IN LIST                           
         B     EXITOK                                                           
*                                                                               
SETBXLE  LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE                                                          *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
*                                                                               
         LA    R3,CARD                                                          
*                                                                               
INIT02   GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT06                                                           
*                                                                               
         L     RF,=V(DDSIO)        SET UP DDSIO FIRST                           
         MVC   0(8,RF),DDSIO                                                    
*                                                                               
         MVC   PLINE+1(80),0(R3)                                                
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
INIT03   LR    R1,R3               PASS TO VALCARD                              
         BRAS  RE,VALCARD          READ KEYWORD=VALUES                          
         BE    INIT02                                                           
         B     XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
         L     RF,=A(SSB)                                                       
         MVC   ALET,SSOALET-SSOOFF(RF)                                          
         MVC   TBLET,SSOTBLET-SSOOFF(RF)                                        
*                                                                               
         LAM   R2,R2,ALET                                                       
         ICM   R2,15,DMCB+16                                                    
         BZ    INIT06                                                           
*                                                                               
         BRAS  RE,ARSOFF                                                        
*                                                                               
INIT06   LHI   R0,4                MAKE JOB NON SWAPPABLE                       
         LNR   R0,R0                                                            
         SVC   247                                                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* USEFUL ROUTINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
ARSOFF   SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         BR    RE                                                               
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
*                                                                               
CVDR0    CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         BR    RE                                                               
         EJECT                                                                  
ARZERO   DC    16F'0'                                                           
READ     DC    CL8'READ   '                                                     
BUFFER   DC    CL8'BUFFER '                                                     
DMREAD   DC    CL8'DMREAD '                                                     
DMRSEQ   DC    CL8'DMRSEQ '                                                     
DMRDHI   DC    CL8'DMRDHI '                                                     
OPEN     DC    CL8'OPEN'                                                        
CTFILE   DC    CL8'CTFILE'                                                      
LOCKSPC  DC    CL8'LOCKSPC'                                                     
SPACES   DC    CL166' '                                                         
STARS    DC    16C'*'                                                           
EFFS     DC    16X'FF'                                                          
MAXLINE  DC    PL3'60'                                                          
LWAIT    DC    AL4(30*100)                                                      
         SPACE 2                                                                
T1       DC    166C' '                                                          
T2       DC    166C' '                                                          
T3       DC    166C' '                                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DCBS & ADCONS                                      *                   
*************************************************************                   
         SPACE 2                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
ADATAMGR DC    V(DATAMGR)                                                       
ALOCKSPC DC    V(LOCKSPC)                                                       
*                                                                               
ACOMM    DC    A(0)                                                             
*                                                                               
       ++INCLUDE FACIDTAB                                                       
*                                                                               
UTL      DC    F'0',AL1(10),XL250'00'                                           
*                                                                               
SSB      DC    H'0',X'FF',X'14',1022X'00'                                       
         EJECT                                                                  
*************************************************************                   
*        ECBS AND TIMER FIELDS                              *                   
*************************************************************                   
         DS    0F                                                               
ECBLST2  EQU   *                                                                
ADSPECB  DC    A(DSPACECB)                                                      
AOPERECB DC    A(0)                                                             
*                                                                               
STIMERID DS    XL4                                                              
TIME     DS    XL4                                                              
DSPACECB DS    XL4                                                              
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE DC                                 *                   
*************************************************************                   
         SPACE 1                                                                
         DS    0D                                                               
SORTBLK  DS    1024CL16                                                         
*                                                                               
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 2                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAGS    DS    X                                                                
*                                                                               
SAVEF01  DS    4A                                                               
*                                                                               
DMCB     DS    6F                                                               
CARDEND  DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
*                                                                               
ALET     DS    A                   DMGR ALET                                    
TBLET    DS    A                   TABS ALET                                    
*                                                                               
WORK     DS    CL64                                                             
WORK1    DS    CL64                                                             
MYWORK   DS    CL64                                                             
*                                                                               
SEFILN   DS    CL256               FILENAMES FOR OPEN                           
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
*                                                                               
DMOFFS   DS    A                   DATASPACE OFFSET                             
DMALET   DS    A                   ALET                                         
DMTOKN   DS    CL8                 TOKEN                                        
*                                                                               
ASYSFLES DS    A                                                                
*                                                                               
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
CARD     DS    CL80                                                             
CARD1    DS    CL80                                                             
CARD2    DS    CL80                                                             
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         SPACE 1                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* FACIDTAB                                                                      
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FAD                                                                           
       ++INCLUDE FAD                                                            
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056DMDMGRDSD 03/23/06'                                      
         END                                                                    
