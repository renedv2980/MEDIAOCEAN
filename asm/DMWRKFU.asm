*          DATA SET DMWRKFU    AT LEVEL 002 AS OF 10/08/09                      
*PHASE WRKFUA                                                                   
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE DDINFO                                                                 
*INCLUDE REGSAVE                                                                
                                                                                
* THIS CODE OVERWROTE AN EXISTING DMWRKFT.                                      
* A NEW DMWRKFT HAS BEEN MADE BASED ON DMPRTQT.                                 
* THIS CODE IS NOW CALLED DMWRKFU. APPROPRIATE.                                 
                                                                                
         TITLE 'WRKFU - TRANSFER MVS FILES TO WRKF FILES'                       
         PRINT NOGEN                                                            
WRKFU    CSECT                                                                  
*                                                                               
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         NBASE 0,**WRKF**,=V(REGSAVE),RA,R9                                     
         ST    RD,SAVERD                                                        
*                                                                               
         LA    R8,IOAREA           R8=IOAREA                                    
         USING WLHDRD,R8                                                        
*                                                                               
         BAS   RE,PRINTI           INIT PRINTING                                
         BAS   RE,INIT             READ CARDS ECT                               
*                                                                               
         CLI   PARMIN,C' '         MUST HAVE VALID PARM TO COPY FILE            
         BE    XBASE                                                            
*                                                                               
         BAS   RE,OPENALL          OPEN FILES                                   
         BNE   EXIT                                                             
         BAS   RE,MAIN             MAIN LOOP                                    
         BAS   RE,CLOSEALL         CLOSE FILES                                  
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
EXITEQ   CR    RB,RB                                                            
         B     EXIT                                                             
EXITNE   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(1,TODAY)                                  
*                                                                               
         LA    R1,TITLE1           PRINT PARAMETER CARDS TITLE                  
         BAS   RE,PRINTT                                                        
         LA    R3,CARD                                                          
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT020                                                          
         MVC   PLINE+1(80),0(R3)                                                
         BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BNE   XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
INIT011  CLI   DDSIOV,C' '         SET DDSIO VALUE IF INPUT THIS TIME           
         BE    INIT012                                                          
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),DDSIOV                                                   
         MVC   DDSIOV,SPACES                                                    
         B     INIT010                                                          
*                                                                               
INIT012  CLI   DSPACEV,C' '        SET DSPACE VALUE IF INPUT THIS TIME          
         BE    INIT013                                                          
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),DSPACEV                                    
         MVI   DSPACEV,C' '                                                     
         B     INIT010                                                          
*                                                                               
INIT013  CLI   CLEANUPV,C' '       CLEANUP=WRKFX INPUT                          
         BE    INIT014                                                          
         L     RF,=V(DATAMGR)                                                   
         GOTO1 (RF),DMCB,(X'00',CLEAN),CLEANUPV,KEY,IOAREA,AWKBUFF              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CLEANUPV,SPACES                                                  
         B     INIT010                                                          
*                                                                               
INIT014  MVI   PARMIN,C'Y'         SET VALID PARAMETER CARD INPUT               
         B     INIT010                                                          
*                                                                               
INIT020  EQU   *                                                                
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        OPEN FILES                                         *                   
*************************************************************                   
         SPACE 1                                                                
OPENALL  NTR1                                                                   
*                                                                               
         MVI   BYTE,C'N'           HAVE NOT PROCESSED STRATA                    
*                                                                               
         MVC   TUKEY,=X'C017'      TEXT UNIT FOR HFS FILENAME                   
         GOTO1 =V(DDINFO),DMCB,(5,DDNAME),TUKEY,0                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,DMCB+8                                                        
         CLC   =C'/u/',0(RE)       HFS PATHNAME?                                
         BNE   OPENIN                                                           
*                                                                               
         MVC   TUKEY,=X'C01D'      TEXT UNIT FOR FILEDATA                       
         GOTO1 =V(DDINFO),DMCB,(5,DDNAME),TUKEY,0                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,DMCB+8                                                        
         ICM   RF,1,DMCB+8                                                      
         BNZ   *+6                                                              
         DC    H'0'                NO FILEDATA PARAM                            
*                                                                               
         CLI   0(RE),X'40'                                                      
         BE    *+6                                                              
         DC    H'0'                FILEDATA != TEXT                             
*                                                                               
         LA    R1,MVSIN            R1=INPUT DCB                                 
         USING IHADCB,R1                                                        
         MVI   DCBRECFM,DCBRECV+DCBRECBR      RECFM=VB                          
         MVC   DCBBLKSI,=H'25500'             BLKSIZE=25500                     
         MVC   DCBLRECL,=H'2550'              LRECL=2550                        
         DROP  R1                                                               
*                                                                               
OPENIN   OPEN  (MVSIN,INPUT)       OPEN INPUT                                   
         LA    R1,MVSIN            RA=INPUT DCB                                 
         USING IHADCB,R1                                                        
         MVI   FIXED,C'N'                                                       
         TM    DCBRECFM,DCBRECF    TEST FOR FIXED LEN FILE                      
         BZ    OPENIN2                                                          
         MVI   FIXED,C'Y'                                                       
         MVC   FIXLEN,DCBLRECL     GET LEN FROM DCBLRECL                        
         SR    R1,R1                                                            
         ICM   R1,3,FIXLEN         SET RECORD LEN FOR WRITE                     
         LA    R1,4(R1)                                                         
         SLL   R1,16                                                            
         STCM  R1,15,IOAREAF                                                    
OPENIN2  EQU   *                                                                
         DROP  R1                                                               
         CLC   DATATP,=CL20'STRATA'   STRATA DEMO DATA?                         
         BNE   OPENIN3             NO - DON'T READ DESCRIPTION                  
         CLI   BYTE,C'Y'           PROCESSED STRATA?                            
         BE    OPENIN3             YES                                          
*                                                                               
         GET   MVSIN,IOAREA                                                     
         LA    R1,IOAREAF          FOR FIXED                                    
         CLI   FIXED,C'Y'                                                       
         BE    *+8                                                              
         LA    R1,IOAREA           FOR VARIABLE OR HEADER                       
*                                                                               
         XC    DESC,DESC                                                        
*                                                                               
         MVC   DESC(12),4+30(R1)   TRY SRZ POSITION                             
         LA    RF,EBCDIC                                                        
         TR    DESC,0(RF)          TRANSLATE CHARS                              
         CLC   =C'STR',DESC+9                                                   
         BE    OPENIN2A                                                         
         CLC   =C'ST2',DESC+9                                                   
         BE    OPENIN2A                                                         
         CLC   =C'RTG',DESC+9                                                   
         BE    OPENIN2A                                                         
         CLC   =C'RT2',DESC+9                                                   
         BE    OPENIN2A                                                         
         CLC   =C'ARB',DESC+9                                                   
         BNE   OPENIN2C                                                         
OPENIN2A DS    0H                                                               
         MVC   DESC+9(3),=C'SRZ'                                                
         B     OPENIN2G            YES IT WAS SRZ                               
*                                                                               
OPENIN2C MVC   DESC(12),4+02(R1)   TRY SRV POSITION                             
         LA    RF,EBCDIC                                                        
         TR    DESC,0(RF)          TRANSLATE CHARS                              
         CLC   =C'STR',DESC+9                                                   
         BE    OPENIN2E                                                         
         CLC   =C'ST2',DESC+9                                                   
         BE    OPENIN2E                                                         
         CLC   =C'RTG',DESC+9                                                   
         BE    OPENIN2E                                                         
         CLC   =C'RT2',DESC+9                                                   
         BE    OPENIN2E                                                         
         CLC   =C'ARB',DESC+9                                                   
         BNE   FNERR               NO GOOD                                      
OPENIN2E DS    0H                                                               
         MVC   DESC+9(3),=C'SRV'                                                
*                                                                               
OPENIN2G MVC   WRKFN(3),DESC       SET WORKER FILE ID                           
         MVC   WRKFN+3(1),DESC+5                                                
         MVC   CLASS,DESC+6                                                     
*                                                                               
         LA    RF,DESC+3           POINT RF TO ENCODED "MONTH"/YEAR             
         CLI   0(RF),C'1'          CHECK FOR SEASON 1-4                         
         BL    OPENIN2H                                                         
         CLI   0(RF),C'4'                                                       
         BH    FNERR                                                            
         MVC   DUB+4(1),0(RF)      MONTH POSITION IS SEASON NUMBER              
         B     OPENIN2F                                                         
*                                                                               
OPENIN2H DS    0H                                                               
*                                  LETTERS INDICATE MONTHS:                     
*                                   "A" THRU "L" = JAN THRU DEC                 
*                                   "M"          = HOLIDAY BOOK                 
         CLI   0(RF),C'A'          CHECK FOR MONTH A-M                          
         BL    FNERR                                                            
         CLI   0(RF),C'M'                                                       
         BH    FNERR                                                            
         MVI   DUB+4,C'1'          FORCE A "1" IN THE MONTH POSITION            
*                                  (BART SAYS THIS IS OKAY)                     
*                                                                               
OPENIN2F MVC   DUB+5(1),1(RF)      TRANSLATE YEAR CHARACTER                     
         TR    DUB+5(1),STRATAYR                                                
*                                                                               
         CLI   DUB+5,C'0'          CHECK FOR YEAR 0-9                           
         BL    FNERR                                                            
         CLI   DUB+5,C'9'                                                       
         BH    FNERR                                                            
*                                                                               
         PACK  DUB(2),DUB+4(2)                                                  
         XC    DUB+4(2),DUB+4                                                   
         MVO   DUB+4(2),DUB(2)                                                  
*                                                                               
         MVC   TODAY+2(1),DUB+4     FUDGE WLDAY                                 
*                                                                               
         MVI   BYTE,C'Y'                                                        
         CLOSE MVSIN                                                            
         B     OPENIN                                                           
*                                                                               
OPENIN3  EQU   *                                                                
*                                                                               
         XC    0(255,R8),0(R8)     BUILD HEADER                                 
         MVC   WLSOFLAB,=C'*SOFSOF*'                                            
         MVC   WLUSRID,USERID                                                   
         MVC   WLSYSPRG(4),WRKFN                                                
         MVC   WLDAY,TODAY+2                                                    
         MVC   WLCLASS,CLASS                                                    
         MVC   WLTYPE,TYPE                                                      
*                                                                               
         CLI   REPLACE,C'Y'        TEST REPLACE OPTION                          
         BNE   *+24                                                             
         OC    REFNO,REFNO                                                      
         BZ    CERRREF                                                          
         MVC   WLFILENO,REFNO                                                   
         OI    WLFLAG,WLFLREFN                                                  
         EJECT                                                                  
*                                                                               
         CLI   OBJ,C'Y'            TEST FOR OBJECT CODED                        
         BNE   *+8                                                              
         OI    WLATTB,WLATOBJ                                                   
*                                                                               
         MVC   WLRETNL,RETAIN                                                   
*                                                                               
         MVC   WLDESC,DESC                                                      
         MVC   WLPSWD,SPACES                                                    
         EJECT                                                                  
*************************************************************                   
*        OPEN WKRF FILE AND EXTRACT RETURNED VALUES         *                   
*************************************************************                   
         SPACE 1                                                                
GEN000   BAS   RE,WRITEH           SET OPEN FILE PARAMS                         
         MVC   PLINE,SPACES                                                     
         MVC   PLINE+1(10),=CL10'WKFILE    '                                    
         LA    RE,PLINE+10                                                      
         MVC   0(3,RE),WLSYSPRG                                                 
         MVC   3(1,RE),WLSUBPRG                                                 
         SR    RF,RF                                                            
         IC    RF,WLDAY                                                         
         SLL   RF,4                                                             
         O     RF,=F'12'                                                        
         ST    RF,FULL                                                          
         EDIT  (P4,FULL),(2,4(RE)),FILL=0                                       
         MVC   6(1,RE),WLCLASS                                                  
         MVI   7(RE),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,WLREPRNO       GET ASSIGNED REPORT NUMBER                   
         STH   R0,FILENUM                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,RE),DUB                                                      
*                                                                               
         MVC   13(17,RE),=C'HAS BEEN CREATED '                                  
         BAS   RE,PRINTL                                                        
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        READ DATA LINES AND WRITE TO WRKF                  *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
*                                                                               
READWK   GET   MVSIN,IOAREA                                                     
         BAS   RE,WRITE            WRITE THE RECORD                             
         B     READWK                                                           
*                                                                               
MVSEND   EQU   *                                                                
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        CLOSE ALL AND EXIT                                 *                   
*************************************************************                   
         SPACE 1                                                                
CLOSEALL NTR1                                                                   
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
         BAS   RE,WRITEH           WRITE EOF                                    
*                                                                               
         CLC   DATATP,=CL20'STRATA'   STRATA DEMO DATA?                         
         BNE   CLOSEA4             NO - DON'T READ DESCRIPTION                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY              GET FILENUM FOR THIS USERID                  
         USING UKRECD,R5                                                        
*                                                                               
         MVC   UKUSRID,USERID                                                   
         GOTO1 (RF),DMCB,(X'00',=CL8'GFILE'),WRKFILE,KEY,IOAREA,AWKBUFF         
*                                                                               
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKUSRID,USERID                                                   
*                                                                               
         MVI   UKFLAG,X'80'                                                     
         MVC   UKFILENO,FILENUM                                                 
*                                                                               
         MVC   UKINFO,=X'FFFF'                                                  
         GOTO1 =V(DATAMGR),DMCB,=CL8'RETAIN',UKUSRINF,KEY,IOAREA,      +        
               AWKBUFF                                                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&DO                                                                           
         GOTO1 =V(DATAMGR),DMCB,=CL8'KEEP',UKUSRINF,KEY,IOAREA,AWKBUFF          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         DROP  R5                                                               
CLOSEA4  DS    0H                                                               
         CLOSE MVSIN               CLOSE MVS                                    
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        CALL WRKF WITH DMPRINT COMMAND                     *                   
*************************************************************                   
         SPACE 1                                                                
WRITE    LA    RF,IOAREAF          FOR FIXED                                    
         CLI   FIXED,C'Y'                                                       
         BE    *+8                                                              
WRITEH   LA    RF,IOAREA           FOR VARIABLE OR HEADER                       
*                                                                               
         ST    RE,SAVERE                                                        
         GOTO1 =V(DATAMGR),DMCB,DMPRINT,WRKFILE,0,(RF),AWKBUFF                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
*************************************************************                   
*        PRINT TITLE                                        *                   
*************************************************************                   
         SPACE 1                                                                
TITLE1   DC    CL133' '                                                         
         ORG   TITLE1                                                           
         DC    C'1',X'40'                                                       
         DC    C'--------------------- PARAMETER CARDS ---------------'         
         DC    C'-----------------------------------------------------'         
         DC    C'---------------------------'                                   
         ORG                                                                    
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
         PUT   SYSPRINT,TITLE1     PRINT TITLE                                  
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
         PUT   SYSPRINT,TITLE1     PRINT TITLE                                  
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
                                                                                
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN-1),X'FLAGS',AL3(OUTPUT)           
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
                                                                                
CARDTAB  DS    0F                                                               
         DC    C'DDSIO  ',AL1(4,07),X'0000',AL3(DDSIOV)                         
         DC    C'DSPACE ',AL1(5,00),X'0000',AL3(DSPACEV)                        
         DC    C'CLEANUP',AL1(6,04),X'0000',AL3(CLEANUPV)                       
*                                                                               
         DC    C'INPUT  ',AL1(4,03),X'0000',AL3(INPUT)                          
         DC    C'NAME   ',AL1(3,03),X'0000',AL3(WRKFN)                          
         DC    C'TYPE   ',AL1(3,00),X'0000',AL3(TYPE)                           
         DC    C'OBJ    ',AL1(2,00),X'0000',AL3(OBJ)                            
         DC    C'DESC   ',AL1(3,15),X'0000',AL3(DESC)                           
         DC    C'USERID ',AL1(5,02),X'8000',AL3(VALUSR)                         
         DC    C'DATA   ',AL1(3,09),X'0000',AL3(DATATP)                         
         DC    C'REFNO  ',AL1(4,02),X'0800',AL3(REFNO)                          
         DC    C'RETAIN ',AL1(5,02),X'0800',AL3(RETAIN)                         
         DC    C'REPLACE',AL1(6,01),X'0000',AL3(REPLACE)                        
         DC    C'CONSOLE',AL1(6,00),X'0000',AL3(CONMSG)                         
*                                                                               
         DC    X'0000'                                                          
                                                                                
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
DDSIOV   DC    C'        '         DDSIO=                                       
DSPACEV  DC    C' '                DSPACE=                                      
CLEANUPV DC    C'        '         CLEANUP=                                     
*                                                                               
PARMIN   DC    C' '                SET TO "Y" IF VALID PARM VALUE INPUT         
INPUT    DC    C'MVS '             INPUT=MVS/WRKF                               
WRKFN    DC    C'WRKF'             WRKF=WRKF                                    
TYPE     DC    C' '                TYPE=' '                                     
OBJ      DC    C'N'                OBJ=N                                        
DESC     DC    CL16'FROM MVS'                                                   
DATATP   DC    CL10' '             DATA TYPE                                    
CONMSG   DC    C'N'                CONSOLE MESSGES=N                            
USERID   EQU   *                                                                
*&&UK*&& DC    XL2'0026'           DEFAULT TO DDS1                              
*&&US*&& DC    XL2'0011'           DEFAULT TO SJR                               
CLASS    DC    C'T'                                                             
REFNO    DC    XL2'0000'           FILE REF NUM                                 
RETAIN   DC    XL2'0000'           RETAIN VALUE                                 
REPLACE  DC    C'N'                REPLACE OPTION                               
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
         BE    VALC082                                                          
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BH    CERRMAX                                                          
*                                                                               
         LR    R0,R1                                                            
         CLI   0(R4),0                                                          
         BE    VALC082                                                          
         ZIC   R1,8(R4)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),=CL16' '    CLEAR OUTPUT AREA                            
         LR    R1,R0                                                            
*                                                                               
VALC082  EX    R1,*+8                                                           
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
CERRUSR  LA    R1,=C'INVALID USERD   '                                          
         B     CERRX                                                            
CERRREF  LA    R1,=C'NO REF NUMBER   '                                          
         B     CERRX                                                            
FNERR    DS    0H                                                               
         MVC   PLINE+1(13),=C'*** ERROR ***'                                    
         MVC   PLINE+15(16),=C'BAD STRATA NAME '                                
         MVI   PLINE+32,C'"'                                                    
         MVC   PLINE+33(L'DESC),DESC                                            
         MVI   PLINE+33+L'DESC,C'"'                                             
         BAS   RE,PRINTL                                                        
         CLI   CONMSG,C'Y'         CONSOLE MESSAGE?                             
         BNE   EXITNEQ             NO                                           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG,OPMSG)                       
         B     EXITNEQ                                                          
*                                                                               
OPMSG    DC    C'BAD STRATA FILE ENCOUNTERED'                                   
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
VALT020  LA    R3,2                PREPARE FULL AND HALD                        
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
*************************************************************                   
*        VALIDATE USRID=                                    *                   
*************************************************************                   
         SPACE 1                                                                
VALUSR   NTR1                                                                   
         XC    KEY,KEY             FIND ID REC                                  
         MVI   KEY,CTIKTYPQ                                                     
         MVC   KEY+CTIKID-CTIREC,SPACES                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+CTIKID-CTIREC(0),0(R2)                                       
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'OPEN',=C'SER',=C'NCTFILE X'                  
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,DMREAD,CTFILE,KEY,IOAREA                        
         CLI   8(R1),0                                                          
         BNE   CERRUSR                                                          
         USING CTIREC,IOAREA                                                    
         LA    R1,IOAREA+CTIDATA-CTIREC                                         
VALUS010 CLI   0(R1),X'02'         LOOK FOR ID NUMBER ELEMENT                   
         BE    VALUS020                                                         
         SR    R0,R0                                                            
         ICM   R0,1,1(R1)          NEXT                                         
         BZ    CERRUSR                                                          
         AR    R1,R0                                                            
         B     VALUS010                                                         
VALUS020 MVC   USERID(2),2(R1)                                                  
         B     EXITEQU                                                          
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
         SPACE 1                                                                
DMREAD   DC    CL8'DMREAD '                                                     
CTFILE   DC    CL8'CTFILE '                                                     
DMPRINT  DC    CL8'DMPRINT'                                                     
CLEAN    DC    CL8'CLEANUP'                                                     
WRKFILE  DC    CL8'WRKF1  '                                                     
SPACES   DC    CL136' '                                                         
MAXLINE  DC    P'60'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DCBS & ADCONS                                      *                   
*************************************************************                   
         SPACE 2                                                                
MVSIN    DCB   DDNAME=MVSIN,DSORG=PS,MACRF=(GM),EODAD=MVSEND                    
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(133)          
*                                                                               
AWKBUFF  DC    A(WKBUFF)                                                        
*                                                                               
*************************************************************                   
EBCDIC   DC    XL16'40404040404040404040404040404040'  00-0F **TEMP**           
         DC    XL16'40404040404040404040404040404040'  10-1F **TEMP**           
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040'  30-3F                    
         DC    XL16'40C1C2C3C4C5C6C7C8C9D1D2D3D4D5D6'  40-4F                    
         DC    XL16'D7D8D9E2E3E4E5E6E7E8E94040404040'  50-5F                    
         DC    XL16'40C1C2C3C4C5C6C7C8C9D1D2D3D4D5D6'  60-6F                    
         DC    XL16'D7D8D9E2E3E4E5E6E7E8E94040404040'  70-7F                    
         DC    XL16'40404040404040404040404040404040'  80-8F                    
         DC    XL16'40404040404040404040404040404040'  90-9F                    
         DC    XL16'40404040404040404040404040404040'  A0-AF                    
         DC    XL16'40404040404040404040404040404040'  B0-BF                    
         DC    XL16'40404040404040404040404040404040'  C0-CF                    
         DC    XL16'40404040404040404040404040404040'  D0-D1                    
         DC    XL16'40404040404040404040404040404040'  E0-EF                    
         DC    XL16'40404040404040404040404040404040'  F0-FF                    
*************************************************************                   
STRATAYR DC    XL16'00000000000000000000000000000000'  00-0F                    
         DC    XL16'00000000000000000000000000000000'  10-1F                    
         DC    XL16'00000000000000000000000000000000'  20-2F                    
         DC    XL16'00000000000000000000000000000000'  30-3F                    
         DC    XL16'00000000000000000000000000000000'  40-4F                    
         DC    XL16'00000000000000000000000000000000'  50-5F                    
         DC    XL16'00000000000000000000000000000000'  60-6F                    
         DC    XL16'00000000000000000000000000000000'  70-7F                    
         DC    XL16'00000000000000000000000000000000'  80-8F                    
         DC    XL16'00000000000000000000000000000000'  90-9F                    
         DC    XL16'00000000000000000000000000000000'  A0-AF                    
         DC    XL16'00000000000000000000000000000000'  B0-BF                    
         DC    XL16'00000000000000000000000000000000'  C0-CF                    
         DC    XL16'0000F0F1F2F3F4F5F6F7000000000000'  D0-D1 2K-07              
         DC    XL16'0000F8F9F0F1F2F3F4F5000000000000'  E0-EF 08-15              
         DC    XL16'F0F1F2F3F4F5F6F7F8F9000000000000'  F0-FF 90-99              
*************************************************************                   
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 2                                                                
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
FILENUM  DS    H                                                                
BYTE     DS    X                                                                
FLAG     DS    X                                                                
DMCB     DS    6F                                                               
CARDEND  DS    A                                                                
WORK     DS    CL64                                                             
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
PLINE    DS    CL133                                                            
TODAY    DS    CL3                 YYMMDD PWOS                                  
FIXED    DS    X                                                                
FIXLEN   DS    XL2                                                              
*                                                                               
DDNAME   DC    C'MVSIN'                                                         
TUKEY    DS    XL2                                                              
*                                                                               
CARD     DS    CL80                                                             
KEY      DS    CL40                                                             
*                                                                               
IOAREAF  DS    XL4                                                              
IOAREA   DS    4096C                                                            
*                                                                               
WKBUFF   DS    14336C                                                           
*                                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(MASTC),204X'00'          
UTL      DC    F'0',X'01',XL3'00',XL248'00'                                     
MASTC    DC    1932X'00'                                                        
         EJECT                                                                  
         DCBD    DSORG=PS,DEVD=DA                                               
         SPACE 1                                                                
SSOD     DSECT                                                                  
*FASSBOFF                                                                       
       ++INCLUDE FASSBOFF                                                       
* FADSECTS                                                                      
* DMWRKFL                                                                       
* DMWRKFK                                                                       
* CTGENFILE                                                                     
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAD                                                            
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DMWRKFU   10/08/09'                                      
         END                                                                    
