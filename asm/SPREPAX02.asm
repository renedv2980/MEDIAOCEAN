*          DATA SET SPREPAX02  AT LEVEL 026 AS OF 12/28/17                      
*PHASE SPAX02B                                                                  
         TITLE 'SPAX02 - SPOT CLEARANCE UPDATE'                                 
         SPACE 1                                                                
SPAX02   CSECT                                                                  
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPAX02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         LA    R8,2048(RC)                                                      
         LA    R8,2048(R8)                                                      
         USING SPAX02,RB,RC,R8                                                  
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    FRST                                                             
         CLI   MODE,RUNLAST                                                     
         BE    LAST                                                             
*                                                                               
EQXIT    CR    RB,RB               SET CC EQUAL                                 
         B     XIT                                                              
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=========================================================*                     
* RUNFRST PROCESSING                                      *                     
*=========================================================*                     
         SPACE 1                                                                
FRST     DS    0H                                                               
         MVI   FIRST,C'Y'                                                       
         MVI   OPENPQ,C'N'                                                      
*                                                                               
         USING MASTD,R1                                                         
         ICM   R1,15,VMASTC                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   VREMOTEC,MCVREMOT                                                
         MVC   RCRUN,MCTSTRUN      SAVE TEST RUN INDICATORS                     
         DROP  R1                                                               
*                                                                               
         L     R1,=A(IO)                                                        
         ST    R1,AREC                                                          
         OPEN  (DATAFILE,(INPUT))  OPEN INPUT FILE                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,FINDSPOT         FIND OUT WHAT SPOT WE'RE RUNNING             
*                                                                               
FRST05   L     R1,=A(DATAFILE)                                                  
         LA    R2,REC                                                           
         GET   (1),(2)                                                          
         USING RECD,R2                                                          
         CLC   RSENUM,THISSE       LOOP THROUGH DATA TIL FIND START             
         BNE   FRST05                                                           
*                                                                               
FRST07   CLC   RALPHA,LASTALPH     IF NEW AGENCY                                
         BE    FRST09                                                           
         BAS   RE,NEWAGY                                                        
         B     FRST10                                                           
FRST09   CLC   RMED,LASTMED        OR NEW MEDIA                                 
         BE    FRST30              GET BINARY AGENCY MEDIA                      
*                                                                               
FRST10   GOTO1 MEDGET,DMCB,(RMED,RALPHA),DATAMGR,WORK                           
         CLI   8(R1),X'FF'         RETURN CODE                                  
         BNE   FRST20                                                           
         BAS   RE,MEDERR           PRINT ERROR MESSAGE & CONTINUE               
         MVC   LASTALPH,RALPHA     SET LAST ALPHA OR NEW PQ REPORT              
         B     FRST40              WILL BE GENERATED                            
*                                                                               
FRST20   MVC   QMED,RMED           SO STAPACK DOESN'T CHOKE ON DIGITAL          
         MVC   LASTAGMD,WORK       SET BINARY AGY/MED                           
         MVC   LASTALPH,RALPHA         LAST ALPHA ID READ                       
         MVC   LASTMED,RMED            LAST MEDIA READ                          
         BAS   RE,GETCTRY                                                       
*                                                                               
FRST30   BAS   RE,PROCESS          PROCESS THE RECORD                           
*                                                                               
FRST40   L     R1,=A(DATAFILE)                                                  
         GET   (1),(2)                                                          
         CLC   RSENUM,THISSE       IF NEW SE NUMBER                             
         BE    FRST07                                                           
*                                                                               
DATAEND  CLOSE (DATAFILE)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AENDREQ                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SET REMOTE INFO FOR NEW AGENCY                                         
*                                                                               
NEWAGY   NTR1                                                                   
         CLI   FIRST,C'Y'          FIRST AGENCY                                 
         BE    NEWAGY10                                                         
         GOTO1 REPORT                                                           
         MVC   P(27),=C'CLEARANCE RECORDS PROCESSED'                            
         EDIT  COUNTER,(9,P+30),COMMAS=YES,ALIGN=LEFT                           
         GOTO1 REPORT                                                           
         MVC   P(27),=CL27'RECORDS IN ERROR'                                    
         EDIT  ERRCNT,(9,P+30),COMMAS=YES,ALIGN=LEFT                            
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         XC    COUNTER,COUNTER                                                  
         XC    ERRCNT,ERRCNT                                                    
         CLI   OPENPQ,C'Y'                                                      
         BNE   NEWAGY10                                                         
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         MVC   PAGE,=X'0001'       RESET PAGE NUMBER                            
         MVI   OPENPQ,C'N'                                                      
*                                                                               
NEWAGY10 DS    0H                                                               
         CLI   RCRUN,RUNTST        IS THIS A TEST RUN?                          
         BE    NEWAGY20            - THEN NO REMOTE.                            
******** BAS   RE,GETPQNUM         GET NEXT AGENCY ID NUMBER                    
         MVI   FIRST,C'N'                                                       
*                                                                               
         CLC   =C'YN',RALPHA       SPECIAL CODE TO SEND YN TO PQ                
         BE    NEWAGY12                                                         
         CLC   =C'MC',RALPHA       SPECIAL CODE TO SEND MC TO PQ                
         BE    NEWAGY12                                                         
         CLC   =C'FR',RALPHA       SPECIAL CODE TO SEND FR TO PQ                
         BE    NEWAGY12                                                         
         CLC   =C'H7',RALPHA       SPECIAL CODE TO SEND H7 TO PQ                
         BE    NEWAGY12                                                         
         CLC   =C'JW',RALPHA       SPECIAL CODE TO SEND JW TO PQ                
         BNE   NEWAGY20                                                         
*                                                                               
         USING REMOTED,R1                                                       
NEWAGY12 L     R1,VREMOTEC                                                      
         XC    REMOTKEY,REMOTKEY                                                
         MVC   REMOTJID,=C'SAX'                                                 
         MVC   REMOTKEY(10),=C'SPOT CLRST'                                      
         CLI   CARD,C'S'                                                        
         BE    *+16                                                             
         MVC   REMOTJID,=C'NAX'                                                 
         MVC   REMOTKEY(10),=C'NET CLRST '                                      
         MVI   REMOTCLS,C'K'                                                    
         MVC   REMOTDST,=X'099F'   USER ID NUMBER FOR YNRO (YN)                 
         CLC   =C'YN',RALPHA       SPECIAL CODE TO SEND YN TO PQ                
         BE    NEWAGY18                                                         
         MVC   REMOTDST,=X'0011'   USER ID NUMBER FOR MCCAN (MC)                
         CLC   =C'MC',RALPHA       SPECIAL CODE TO SEND MC TO SJR PQ            
         BE    NEWAGY18                                                         
         MVC   REMOTDST,=X'1974'   USER ID NUMBER FOR FDMJW (FR)                
         CLC   =C'FR',RALPHA       SPECIAL CODE TO SEND FR TO PQ                
         BE    NEWAGY18                                                         
         MVC   REMOTDST,=X'2192'   USER ID NUMBER FOR MSNYA (H7)                
         CLC   =C'H7',RALPHA       SPECIAL CODE TO SEND H7 TO PQ                
         BE    NEWAGY18                                                         
         MVC   REMOTDST,=X'0865'   USER ID NUMBER FOR JWNYSH (JW)               
NEWAGY18 MVI   OPENPQ,C'Y'                                                      
NEWAGY20 B     XIT                                                              
         DROP  R1                                                               
*                                                                               
*        GET ID NUMBER (FOR PRINT QUEUE) FROM POWER CODE IN RALPHA              
*                                                                               
GETPQNUM NTR1                                                                   
         USING CT5REC,R1                                                        
         LA    R1,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   CT5KTYP,CT5KTYPQ    C'5'                                         
         MVC   CT5KALPH,RALPHA                                                  
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'CTFILE',KEY,AREC,0                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AREC                                                          
         CLC   0(L'CT5KEY,R1),KEY                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CT5DATA                                                       
GPQ10    CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'02'                                                      
         BE    GPQ20                                                            
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     GPQ10                                                            
GPQ20    MVC   PQIDNUM,2(R2)       SAVE USER NUMBER FOR PQ                      
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
*        READ AGY RECORD TO SEE IF CANADIAN OR NOT                              
*                                                                               
GETCTRY  NTR1                                                                   
         MVI   CANADIAN,C'N'                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),RALPHA                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETAGY                                                           
         L     R6,ADAGY                                                         
         USING AGYHDRD,R6                                                       
         CLI   AGYPROF+7,C'C'      IS THIS CANADIAN                             
         BNE   *+8                                                              
         MVI   CANADIAN,C'Y'                                                    
         MVC   CNTRYCD,AGYPROF+7   COUNTRY CODE                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        FIND OUT WHICH SPOT WE'RE RUNNING & THE CORRESPONDING SE #             
*                                                                               
FINDSPOT NTR1                                                                   
*                                                                               
FINDS10  GOTO1 CARDS,DMCB,CARD,=C'RE00'                                         
         CLC   =C'/*',CARD                                                      
         BE    FINDSX                                                           
         CLC   =C'SPOT',CARD       FIND CARD WITH SPOT/NET                      
         BNE   *+12                                                             
         MVI   BYTE,X'02'          SPOT CODE FOR CTWREC                         
         B     FINDS20                                                          
         CLC   =C'NET',CARD                                                     
         BNE   FINDS10                                                          
         MVI   BYTE,X'03'          NET CODE FOR CTWREC                          
*                                                                               
         USING CTWREC,R3                                                        
FINDS20  LA    R3,KEY                                                           
         XC    KEY,KEY             GET SYS LIST RECORD FROM CT FILE             
         MVI   CTWKTYP,CTWKTYPQ    RECORD TYPE C'W'                             
         MVI   CTWKREC,CTWKRSYS    SUB-REC TYPE C'S'-SYSTEM LIST                
         MVC   CTWKSUB,BYTE        SUB-NUMBER FOR SPOT OR NET                   
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'CTFILE',KEY,AREC,0                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AREC                                                          
         CLC   0(L'CTWKEY,R3),KEY                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,CTWDATA                                                       
*                                                                               
         USING CTLSTD,R2                                                        
FINDS30  CLI   CTLSTEL,0           END OF RECORD                                
         BNE   *+6                                                              
         DC    H'0'                ELEMENT MUST BE THERE                        
*                                                                               
         CLI   CTLSTEL,CTLSTELQ    X'A4' - LIST ELEMENTS                        
         BE    FINDS50                                                          
*                                                                               
FINDS40  SR    R1,R1               BUMP TO NEXT ELEMENT                         
         IC    R1,CTLSTLEN                                                      
         AR    R2,R1                                                            
         B     FINDS30                                                          
*                                                                               
FINDS50  LA    R3,5                FOR SPOT, USE LENGTH 6                       
         CLI   CARD,C'S'                                                        
         BE    *+8                                                              
         LA    R3,4                O/W NET USES LENGTH 5                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   3(0,R2),CARD                                                     
         BNE   FINDS40                                                          
*                                                                               
         MVC   THISSE,11(R2)       THIS IS THE SE NUMBER WE NEED                
         L     RE,UTL                                                           
         MVC   4(1,RE),THISSE      SET SYSTEM NUMBER                            
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',FLIST,AREC                      
*                                                                               
FINDSX   B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
*        PROCESS THE RECORD                                                     
*                                                                               
PROCESS  NTR1                                                                   
         LA    R2,REC                                                           
         USING RECD,R2                                                          
         BAS   RE,GETCLI           GET CLIENT                                   
         BNE   PROCX                                                            
         BAS   RE,GETSTA           GET PACKED STATION                           
         BNE   PROCX                                                            
         BAS   RE,GETMKT           GET MARKET NUMBER                            
         BNE   PROCX                                                            
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLRSTATD,R6                                                      
         MVC   CLSKEY(2),=X'0D76'                                               
         MVC   CLSKAGMD,LASTAGMD   BINARY AGENCY/MEDIA                          
         MVC   CLSKCLT,RCLT        CLIENT                                       
         MVC   CLSKMKT,MRKT        MARKET                                       
         MVC   CLSKSTA,PKSTA       STATION                                      
         CLI   CANADIAN,C'Y'       FOR CANADIAN                                 
         BNE   PROC05                                                           
         CLI   RMED,C'N'           NETWORK                                      
         BNE   PROC05                                                           
         NI    CLSKSTA+2,X'E0'     PEEL OFF NETWORK BITS                        
         XC    CLSKMKT,CLSKMKT     AND CLEAR MARKET                             
*                                                                               
PROC05   GOTO1 HIGH                                                             
         MVI   ERRSW,C'H'                                                       
         CLC   KEY(CLSKDATE-CLSKEY),KEYSAVE                                     
         BNE   PROCERR                                                          
*                                                                               
PROC10   MVC   SVDSKADD,KEY+14     SAVE RECORD JUST READ'S D/A                  
         GOTO1 SEQ                 SAME A/M/C/MKT/STA                           
         CLC   KEY(CLSKDATE-CLSKEY),KEYSAVE                                     
         BNE   PROC20                                                           
         CLC   CLSKDATE(3),RCKCDT  IF THE LOWEST CLEARANCE DATE IS              
         BNH   PROC10              LOWER THAN NEEDED DATE - GET NEXT            
*                                                                               
PROC20   XC    KEY,KEY             SET D/A OF RECORD THAT CONTAINS              
         MVI   ELEMFND,C'N'                                                     
         MVC   KEY+14(4),SVDSKADD  THE NEEDED CLEARANCE                         
         GOTO1 GET                                                              
*                                                                               
         L     R6,AREC                                                          
         USING CLSTEL01,R6                                                      
         MVI   ELCODE,X'01'        GET CLEARANCE DETAILS                        
         MVI   ERRSW,C'E'                                                       
         BAS   RE,GETEL                                                         
         BE    PROC41                                                           
         B     PROCERR                                                          
*                                                                               
PROC40   BAS   RE,NEXTEL                                                        
         BE    PROC41                                                           
         CLI   ELEMFND,C'Y'        & AT LEAST ONE ELEMENT WAS FOUND             
         BE    PROC60              WRITE BACK RECORD                            
         B     PROCERR             ELSE ERROR                                   
*-----------------------------------------------------------------              
*                                                                               
PROC41   CLC   CLSTCLRD,RCKCDT     HAVE WE FOUND CORRECT ELEMENT                
         BNE   PROC40              RIGHT CLEARANCE DATE                         
         CLC   CLSTCLSQ,RSEQNUM    & SEQUENCE NUMBER                            
         BNE   PROC40              JIM SAYS WE WILL ALWAYS HAVE                 
*                                                                               
         TM    CLSTSTAT,X'02'      NEW FORMAT WITH X'03' TO FOLLOW?             
         BZ    PROC42                                                           
*                                                                               
         BAS   RE,NEWCLRST         PROCESS NEW ELEMS FIRST                      
         BE    PROC42                                                           
         BAS   RE,NCLRERR          PRINT NEW CLEARANCE ERROR                    
*-----------------------------------------------------------------              
*                                  THEN CONTINUE TO UPDATE X'01' ELEM           
PROC42   CLC   RCKNUM,SPACES       IF THIS IS A VOID (NO CHECK NUMBER)          
         BNH   PROC50                                                           
         CLC   CLSTCHK(4),=C'VOID' OR THIS WAS A VOIDED CHECK                   
         BE    PROC50              THEN CHECK DATE & SEQ NUMBER                 
         CLC   CLSTCHK,SPACES      IF THERE ALREADY IS A CHECK NUMBER           
         BNH   PROC50                                                           
         CLC   RCKNUM,CLSTCHK      FIND ELEMENT WITH CHECK NUMBER               
         BNE   PROC40                                                           
*                                                                               
         TM    RSTATUS,TRNSBREC    AND THE RECORD IS NOT RECONCILED             
         BO    PROC45                                                           
         NI    CLSTSTAT,X'7F'      SET RECORD UN-RECONCILED                     
         B     *+8                                                              
PROC45   OI    CLSTSTAT,X'80'      SET RECORD RECONCILED                        
*                                                                               
         TM    RSTATUS,X'01'       IS THIS A BANK CLEARED DATE                  
         BNO   PROC49                                                           
         CLI   1(R6),CL01ELLN      ROOM FOR DATE IN ELEM?                       
         BL    PROC49                                                           
         MVC   CLSTBKDT,RBNKCLRD   THEN FILL IN BANK CLEARED DATE               
         B     PROC49                                                           
*                                                                               
PROC49   MVI   ELEMFND,C'Y'                                                     
         B     PROC40              AND LOOK FOR NEXT                            
*                                                                               
*-----------------------------------------------------------------              
PROC50   CLC   CLSTCLRD,RCKCDT     HAVE WE FOUND CORRECT ELEMENT                
         BNE   PROC40              RIGHT CLEARANCE DATE                         
         CLC   CLSTCLSQ,RSEQNUM    & SEQUENCE NUMBER                            
         BNE   PROC40                                                           
*                                                                               
         CLC   RCKNUM(6),=6C'*'      IF JIM SAYS TO UNVOID THIS ITEM            
         BNE   PROC51                                                           
         CLC   CLSTCHK(4),=C'VOID'   IF IT DOESN'T SAY VOID                     
         BNE   PROCX                 I HAVE NO CLUE - SO EXIT                   
         MVC   CLSTCHK,SPACES        THEN CLEAR THE VOID AND DATE               
         XC    CLSTCHDT,CLSTCHDT                                                
         B     PROC60                                                           
*                                                                               
PROC51   CLC   RCKNUM,SPACES       IF NO CHECK NUMBER FROM ACC                  
         BH    PROC52              THERE IS                                     
         CLC   CLSTCHK,SPACES      AND NO CHECK NUMBER IN CLRSTAT               
         BNH   PROCX               THEN IT'S OBVIOUSLY NOT A VOID!              
*                                                                               
PROC52   MVI   ELEMFND,C'Y'                                                     
         MVC   CLSTCHK,RCKNUM      SET CHECK NUMBER                             
         MVC   CLSTCHDT,RCKDATE         & DATE                                  
         CLC   RCKNUM,SPACES       IF THIS IS A VOID (NO CHECK NUMBER)          
         BH    *+10                                                             
         MVC   CLSTCHK(4),=C'VOID' PUT THE WORD 'VOID'                          
         TM    RSTATUS,TRNSBREC    IF RECORD WAS UN-RECONCILED NOW              
         BO    PROC55                                                           
         NI    CLSTSTAT,X'7F'      SET RECORD UN-RECONCILED                     
         B     PROC40              AND LOOK FOR NEXT                            
PROC55   OI    CLSTSTAT,X'80'      SET RECORD RECONCILED                        
         B     PROC40              AND LOOK FOR NEXT                            
*-----------------------------------------------------------------              
*                                                                               
PROC60   AF    COUNTER,=F'1'                                                    
         BAS   RE,PRREP                                                         
*                                                                               
         CLI   QOPT3,C'Y'          CLEARANCE RECORD TRACE                       
         BNE   PROC62                                                           
         GOTO1 MYTRACE,DMCB,AREC,0,=C'CLEARANCE RECORD',17                      
*                                                                               
PROC62   CLI   RCWRITE,C'Y'        TEST WRITE = NO                              
         BNE   PROCX                                                            
         GOTO1 PUT                                                              
         B     PROCX                                                            
*                                                                               
PROCERR  BAS   RE,CLRERR           CLEARANCE ERROR                              
         B     PROCX                                                            
*                                                                               
PROCX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
         DROP  R6                                                               
NEWCLRST NTR1                                                                   
         LR    R3,R6                                                            
         ZIC   RF,1(R3)                                                         
         AR    R3,RF               BUMP TO NEXT ELEMENT (SHOULD BE 03)          
*                                                                               
NCL10    CLI   0(R3),0             DIDN'T FIND IT?                              
         BE    NCLXN                                                            
         CLI   0(R3),X'01'         DIDN'T FIND IT?                              
         BE    NCLXN                                                            
         CLI   0(R3),X'03'                                                      
         BNE   NCL15                                                            
         USING CLSTEL03,R3                                                      
*                                                                               
         OC    CLS3INV(10),SPACES                                               
         CLC   CLS3INV(10),RINVOICE    FOUND THE INVOICE?                       
         BE    NCL20                                                            
NCL15    ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     NCL10                                                            
         DROP  R3                                                               
*                                                                               
NCL20    DS    0H                  FOUND INVOICE MATCH                          
         ZIC   RF,1(R3)                                                         
         AR    R3,RF               SHOULD BE AT AN X'05'                        
*                                                                               
NCL30    CLI   0(R3),X'05'                                                      
         BNE   NCLXCHK             NO MORE, FINISHED PROCESSING                 
         USING CLSTEL05,R3                                                      
*                                                                               
         CLC   CLS5PRD1,SPACES     MATCH ON PRODUCT1?                           
         BNH   NCL40                                                            
         CLC   CLS5PRD1,RPROD1                                                  
         BNE   NCL20                                                            
         CLC   CLS5PRD2,SPACES     MATCH ON PRODUCT2?                           
         BNH   NCL40                                                            
         CLC   CLS5PRD2,RPROD2                                                  
         BNE   NCL20                                                            
*-----------------------------------------------------------------              
*                                                                               
NCL40    CLC   RCKNUM,SPACES       YOU HAVE A CHECK NUMBER                      
         BNH   NCL50                                                            
         CLC   CLS5CHK(4),=C'VOID' RECORD HAS A VOID                            
         BE    NCL50               YES = GO FILL IN NEW CHECK DATA              
         CLC   CLS5CHK,SPACES      IS THERE A CHECK NUMBER                      
         BNH   NCL50               NO = GO FILL IT IN                           
         CLC   RCKNUM,CLS5CHK      FIND ELEMENT WITH CHECK NUMBER               
         BNE   NCL20               NOT THE SAME CHECK = NEXT ELEM               
*                                                                               
         TM    RSTATUS,TRNSBREC    SAME CHECK NUMBER                            
         BO    NCL45                                                            
         NI    CLS5STAT,X'7F'      SET RECORD UN-RECONCILED                     
         B     *+8                                                              
NCL45    OI    CLS5STAT,X'80'      SET RECORD RECONCILED                        
*                                                                               
         TM    RSTATUS,X'01'       IS THIS A BANK CLEARED DATE                  
         BNO   NCL49                                                            
         MVC   CLS5BKDT,RBNKCLRD   THEN FILL IN BANK CLEARED DATE               
*                                                                               
NCL49    MVI   ELEMFND,C'Y'                                                     
         B     NCL20               CHECK FOR MORE WITH THAT CHECK #             
*-----------------------------------------------------------------              
*                                                                               
NCL50    DS    0H                                                               
         CLC   RCKNUM(6),=6C'*'      IF JIM SAYS TO UNVOID THIS ITEM            
         BNE   NCL51                                                            
         CLC   CLS5CHK(4),=C'VOID'   IF IT DOESN'T SAY VOID                     
         BNE   NCL20                 CHECK FOR MORE ELEMS                       
         MVC   CLS5CHK,SPACES        THEN CLEAR THE VOID AND DATE               
         XC    CLS5CHDT,CLS5CHDT                                                
         MVI   ELEMFND,C'Y'                                                     
         B     NCLXCHK                                                          
*                                                                               
NCL51    CLC   RCKNUM,SPACES       IF NO CHECK NUMBER FROM ACC                  
         BH    NCL52               THERE IS = GO FILL IT IN                     
         CLC   CLS5CHK,SPACES      AND NO CHECK NUMBER IN CLRSTAT               
         BNH   NCLXCHK             THEN IT'S OBVIOUSLY NOT A VOID!              
*-----------------------------------------------------------------              
*                                                                               
NCL52    MVI   ELEMFND,C'Y'                                                     
         MVC   CLS5CHK,RCKNUM      SET CHECK NUMBER                             
         MVC   CLS5CHDT,RCKDATE         & DATE                                  
         CLC   RCKNUM,SPACES       IF THIS IS A VOID (NO CHECK NUMBER)          
         BH    *+10                                                             
         MVC   CLS5CHK(4),=C'VOID' PUT THE WORD 'VOID'                          
         TM    RSTATUS,TRNSBREC    IF RECORD WAS UN-RECONCILED NOW              
         BO    NCL55                                                            
         NI    CLS5STAT,X'7F'      SET RECORD UN-RECONCILED                     
         B     NCLXCHK                                                          
NCL55    OI    CLS5STAT,X'80'      SET RECORD RECONCILED                        
*                                                                               
NCLXCHK  CLI   ELEMFND,C'Y'                                                     
         BNE   NCLXN                                                            
*                                                                               
NCLXY    LR    R1,RB                                                            
NCLXN    CR    R1,RB                                                            
*                                                                               
NCLX     B     XIT                                                              
*                                                                               
*                                                                               
*        GET MARKET CODE                                                        
*                                                                               
GETMKT   NTR1                                                                   
         LA    R3,KEY                                                           
         USING STARECD,R3                                                       
         BAS   RE,SETKEY2                                                       
         GOTO1 CLUNPK,DMCB,(SVAAN,RCLT),STAKCLT                                 
         GOTO1 HIGHSTA                                                          
         DROP  R3                                                               
*                                                                               
         USING STARECD,R6                                                       
         L     R6,ADSTAT                                                        
         CLC   KEY(L'STAKEY),0(R6)                                              
         BE    GM10                                                             
         BAS   RE,SETKEY2                                                       
         GOTO1 HIGHSTA                                                          
         L     R6,ADSTAT                                                        
         CLC   KEY(L'STAKEY),0(R6)                                              
         BE    GM10                                                             
         MVC   MYSTAT,RSTA                                                      
         BAS   RE,STAERR                                                        
*        GOTO1 MYTRACE,DMCB,ADSTAT,0,=C'STATION RECORD',14                      
         B     GMNO                                                             
*                                                                               
GM10     MVC   MYMKT,SMKT          SAVE MARKET TO PRINT                         
                                                                                
         GOTO1 MSPACK,DMCB,SMKT,MYSTAT,MRKT                                     
         B     EQXIT                                                            
*                                                                               
GMNO     B     NEQXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        GET CLIENT                                                             
*                                                                               
GETCLI   NTR1                                                                   
         MVI   SVAAN,C'N'          SET AAN PROF TO N INITIALLY                  
*                                                                               
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),LASTAGMD                                                
         MVC   KEY+2(2),RCLT                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    GCLI10                                                           
         MVC   MYSTAT,RCLT                                                      
         BAS   RE,CLIERR                                                        
         B     GCLINO                                                           
*                                                                               
GCLI10   GOTO1 GETCLT                                                           
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
*                                                                               
         MVC   SVAAN,CPROF+6                                                    
*                                                                               
         B     EQXIT                                                            
GCLINO   B     NEQXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        GET PACKED STATION                                                     
*                                                                               
GETSTA   NTR1                                                                   
         USING STAPACKD,R3                                                      
         XC    WORK(30),WORK                                                    
         LA    R3,WORK                                                          
         MVI   STAPACT,C'P'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,RALPHA      AGENCY ALPHA                                 
         MVC   STAPCTRY,CNTRYCD    COUNTRY - FROM AGYPROF+7                     
         MVC   STAPMED,RMED        MEDIA                                        
         MVC   STAPQMKT,=C'0000'   MARKET                                       
         MVC   STAPQSTA,RSTA       STATION                                      
         GOTO1 VSTAPACK,(R3)                                                    
         CLI   STAPERR,0                                                        
         BE    GST20                                                            
         MVC   MYSTAT,RSTA                                                      
         BAS   RE,STAPKERR                                                      
         B     GSTNO                                                            
*                                                                               
GST20    MVC   PKSTA,STAPSTA       PACKED STATION                               
         B     EQXIT                                                            
GSTNO    B     NEQXIT                                                           
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        SET STATION KEY                                                        
*        R3 - KEY                                                               
*                                                                               
SETKEY   NTR1                                                                   
         USING STARECD,R3                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(L'STAKEY-1),KEY                                            
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,RMED        MEDIA                                        
         XC    WORK,WORK                                                        
         MVC   WORK+2(3),PKSTA                      STATION                     
         GOTO1 MSUNPK,DMCB,WORK,MYMKT,MYSTAT                                    
         MVC   MYNETWK,=C'   '     FOR CABLE                                    
         CLI   RMED,C'T'           IS THIS TV?                                  
         BNE   *+16                NO                                           
         CLI   MYSTAT+4,C' '       IS THIS A SPACE?                             
         BNE   *+8                 NO                                           
         MVI   MYSTAT+4,C'T'       SET LAST BYTE TO 'T'                         
         MVC   STAKCALL,MYSTAT                                                  
         MVC   STAKAGY,RALPHA      AGENCY CODE                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
SETKEY2  NTR1                                                                   
         USING STARECD,R3                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(L'STAKEY-1),KEY                                            
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,RMED        MEDIA                                        
*        XC    WORK,WORK                                                        
*        MVC   WORK+2(3),PKSTA                      STATION                     
*        GOTO1 MSUNPK,DMCB,WORK,MYMKT,MYSTAT                                    
         MVC   MYSTAT,RSTA                                                      
         MVC   MYNETWK,=C'   '     FOR CABLE                                    
         CLI   RMED,C'T'           IS THIS TV?                                  
         BNE   *+16                NO                                           
         CLI   MYSTAT+4,C' '       IS THIS A SPACE?                             
         BNE   *+8                                                              
         MVI   MYSTAT+4,C'T'       SET LAST BYTE TO 'T'                         
         MVC   STAKCALL,MYSTAT                                                  
         MVC   STAKAGY,RALPHA      AGENCY CODE                                  
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PRINT THE REPORT                                                       
*                                                                               
PRREP    NTR1                                                                   
         LA    R3,P                                                             
         USING LINED,R3                                                         
         MVC   LAGY,RALPHA         ALPHA ID                                     
         MVC   LMED,RMED           MEDIA                                        
         GOTO1 CLUNPK,DMCB,RCLT,LCLT   CLIENT                                   
         MVC   LMARKET,MYMKT       MARKET                                       
         MVC   LSTATION,MYSTAT     STATION                                      
         GOTO1 DATCON,DMCB,(2,RCKDATE),(5,LDATE)                                
         MVC   LCKNUM,RCKNUM       CHECK NUMBER                                 
         EDIT  RSEQNUM,(3,LSEQNUM) SEQUENCE NUMBER                              
         TM    RSTATUS,TRNSBREC                                                 
         BNO   *+8                                                              
         MVI   LSTATUS,C'R'                                                     
***                                                                             
* BANK CLEARED DATE ADDED 02/14/02                                              
***                                                                             
         TM    RSTATUS,X'01'                                                    
         BNO   NOCLRDT                                                          
         OC    RBNKCLRD,RBNKCLRD                                                
         BZ    NOCLRDT                                                          
         GOTO1 DATCON,DMCB,(2,RBNKCLRD),(5,LBNKDT)                              
NOCLRDT  GOTO1 REPORT                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        PROCESS RUN LAST                                                       
*                                                                               
LAST     DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(27),=C'CLEARANCE RECORDS PROCESSED'                            
         EDIT  COUNTER,(9,P+30),COMMAS=YES,ALIGN=LEFT                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(27),=CL27'RECORDS IN ERROR'                                    
         EDIT  ERRCNT,(9,P+30),COMMAS=YES,ALIGN=LEFT                            
         GOTO1 REPORT                                                           
         CLI   OPENPQ,C'Y'                                                      
         BNE   XIT                                                              
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         B     XIT                                                              
         EJECT                                                                  
*=================================================================*             
* TRACE DATA BLOCK                                                              
*                                                                               
*        PARAMETER 1 - A(DATA)                                                  
*        PARAMETER 2 - L(DATA) OR ZERO FOR RECORD                               
*        PARAMETER 3 - A(LABEL) OR ZERO FOR NO LABEL                            
*        PARAMETER 4 - L(LABEL) IF PARM 3 IS NOT ZERO                           
*                                                                               
*=================================================================*             
MYTRACE  NTR1                                                                   
*                                                                               
         LM    R2,R5,0(R1)         R2 = A(DATA)                                 
*                                  R3 = L(DATA)                                 
*                                  R4 = A(LABEL)                                
*                                  R5 = L(LABEL)                                
*                                                                               
         LTR   R4,R4               IF CALLER SUPPLIED A LABEL                   
         BZ    TR10                                                             
*                                                                               
         MVI   P,C'-'              THEN FILL PRINT LINE WITH '-'S               
         MVC   P+1(131),P                                                       
*                                                                               
         LR    RE,R5               RF = A(PLACE TO CENTER LABEL)                
         SRL   RE,1                                                             
         LA    RF,66                                                            
         SR    RF,RE                                                            
         LA    RF,P(RF)                                                         
*                                                                               
         BCTR  R5,0                MOVE LABEL TO CENTER OF PRINT LINE           
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
*                                                                               
         GOTO1 REPORT              PRINT LABEL LINE                             
*                                                                               
TR10     LTR   R3,R3               IF DATA IS A RECORD                          
         BNZ   TR50                                                             
         OC    DATADISP,DATADISP   IF THERE IS A KEY                            
         BZ    TR15                                                             
*                                  PRINT OUT ITS KEY                            
         LH    R3,DATADISP                                                      
         GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR15     LR    R6,R2               A(RECORD)                                    
         AH    R6,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R4,1(R6)            PRINT ELEMENT                                
         GOTO1 PRNTBL,DMCB,0,(R6),C'DUMP',(R4),=X'01C4'                         
*                                                                               
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
         B     TR100                                                            
*                                  ELSE PRINT ENTIRE DATA BLOCK                 
TR50     GOTO1 PRNTBL,DMCB,0,(R2),C'DUMP',(R3),=X'01C4'                         
*                                                                               
TR100    DS    0H                                                               
*                                                                               
TRX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
MEDERR   NTR1                                                                   
         XC    MYMKT,MYMKT         CLEAR FIELDS THAT HAVEN'T BEEN SET           
         XC    MYSTAT,MYSTAT                                                    
         BAS   RE,PRREP                                                         
         MVC   P(20),=CL20'*** MEDIA ERROR ***'                                 
         B     ERRPRNT                                                          
*                                                                               
CLIERR   NTR1                                                                   
         BAS   RE,PRREP                                                         
         MVC   P(20),=CL20'*** CLIENT  ERROR ***'                               
         B     ERRPRNT                                                          
STAERR   NTR1                                                                   
         BAS   RE,PRREP                                                         
         MVC   P(20),=CL20'*** STATION ERROR ***'                               
         B     ERRPRNT                                                          
STAPKERR NTR1                                                                   
         BAS   RE,PRREP                                                         
         MVC   P(20),=CL20'*** STAPACK ERROR ***'                               
         B     ERRPRNT                                                          
*                                                                               
CLRERR   NTR1                                                                   
         BAS   RE,PRREP                                                         
         MVC   P(22),=CL22'*** CLEARANCE ERROR ***'                             
         MVC   P+23(1),ERRSW                                                    
         B     ERRPRNT                                                          
*                                                                               
NCLRERR  NTR1                                                                   
         BAS   RE,PRREP                                                         
         MVC   P(22),=CL22'*** CLEARANCE ERROR ***'                             
         MVC   P+23(12),=C'NEW ELEMENTS'                                        
         B     ERRPRNT                                                          
*                                                                               
ERRPRNT  AF    ERRCNT,=F'1'                                                     
         GOTO1 REPORT                                                           
         B     XIT                                                              
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
DATAFILE DCB   DDNAME=DATAFILE,DSORG=PS,MACRF=GM,                      X        
               RECFM=FB,LRECL=49,BLKSIZE=490,EODAD=DATAEND                      
*                                                                               
         DS    0D                                                               
FLIST    DC    CL8' SPTFILE'                                                    
         DC    CL8' SPTDIR '                                                    
         DC    CL8' STAFILE'                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
         SPACE                                                                  
*                                                                               
*        TABLE OF SPOTS & SE #'S                                                
*                                                                               
SVDSKADD DS    A                   SAVED DISK ADDRESS                           
COUNTER  DS    F                   RECORD COUNTER                               
ERRCNT   DS    F                   ERROR RECORD COUNTER                         
*                                                                               
VREMOTEC DS    F                                                                
PQIDNUM  DS    XL2                 USER ID NUMBER FOR PQ DEST                   
FIRST    DS    CL1                                                              
OPENPQ   DS    XL1                                                              
RCRUN    DS    XL1                 SYSTEM RUN INDICATOR (MCTSTRUN)              
RUNTST   EQU   X'FF'                                                            
*                                                                               
ELCODE   DS    XL1                                                              
ERRSW    DS    CL1                                                              
*                                                                               
SVAAN    DS    CL1                 Y/N FOR AAN PROFILE                          
*                                                                               
CNTRYCD  DS    CL1                 COUNTRY CODE                                 
PKSTA    DS    XL3                 PACKED STATION                               
*                                                                               
ELEMFND  DS    XL1                 RECONCILED ELEMENT FOUND                     
*                                                                               
THISSE   DS    XL1                 THIS SPOT SYSTEM'S SE NUMBER                 
*                                                                               
LASTALPH DS    CL2                      ALPHA AGENCY ID                         
LASTMED  DS    CL1                      MEDIA                                   
LASTAGMD DS    XL1                      BINARY AGENCY MEDIA CODE                
CANADIAN DS    CL1                 Y - CANADIAN                                 
*                                                                               
MRKTSTAT DS    0XL5                                                             
MRKT     DS    XL2                 MARKET NUMBER                                
STAT     DS    XL3                                                              
*                                                                               
MYMKT    DS    CL4                 MARKET NUMBER                                
MYSTAT   DS    CL5                                                              
MYNETWK  DS    CL3                 THANKS LISA                                  
MYKEY    DS    CL17                                                             
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
         DS    0D                                                               
         DC    C'**REC***'                                                      
REC      DS    (RRECLNQ)C                                                       
         DS    0D                                                               
         DC    C'**I/O***'                                                      
IO       DS    6000C                                                            
         EJECT                                                                  
*                                                                               
LINED    DSECT                                                                  
         DS    CL4                                                              
LAGY     DS    CL2                 ALPHA AGENCY                                 
         DS    CL10                                                             
LMED     DS    CL1                 MEDIA                                        
         DS    CL6                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL6                                                              
LMARKET  DS    CL4                 MARKET                                       
         DS    CL5                                                              
LSTATION DS    CL5                 STATION                                      
         DS    CL3                                                              
LDATE    DS    CL8                 DATE                                         
         DS    CL5                                                              
LCKNUM   DS    CL6                 CHECK NUMBER                                 
         DS    CL9                                                              
LSEQNUM  DS    CL3                 SEQUENCE NUMBER                              
         DS    CL2                                                              
LSTATUS  DS    CL1                 STATUS                                       
         DS    CL2                                                              
LBNKDT   DS    CL8                 BANK CLEARED DATE                            
         SPACE 2                                                                
*                                                                               
RECD     DSECT                                                                  
RSENUM   DS    XL1                 SE NUMBER                                    
RALPHA   DS    CL2                 ALPHA ID                                     
RMED     DS    CL1                 MEDIA CODE                                   
RCLT     DS    XL2                 CLIENT                                       
RSTA     DS    XL5                 STATION                                      
         DS    XL2                 SPARE - BECAUSE OF PRINT                     
RCKCDT   DS    XL2                 CHECK CLEARANCE DATE                         
RSEQNUM  DS    XL1                 SEQUENCE NUM                                 
RCKNUM   DS    CL6                 CHECK NUMBER                                 
RCKDATE  DS    XL2                 CHECK DATE                                   
RSTATUS  DS    XL1                 STATUS                                       
*              X'02'               RECONCILED                                   
*              X'01'               BANK CLEARED                                 
RBNKCLRD DS    XL2                 BANK CLEARED DATE                            
RINVOICE DS    CL12                INVOICE NUMBER                               
RPROD1   DS    CL3                 PRODUCT 1                                    
RPROD2   DS    CL3                 PRODUCT 2                                    
REST     DS    XL2                 ESTIMATE NUMBER                              
RMKT     DS    XL2                 BINARY MARKET NUMBER                         
RRECLNQ  EQU   *-RECD              LENGTH OF RECORD                             
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPGENCLRST                                                     
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
*DDCNTRL                                                                        
*SPREPMODES                                                                     
*SPREPWORKD                                                                     
*ACGENFILE                                                                      
*CTGENFILE                                                                      
*DDMASTD                                                                        
*DDREMOTED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPREPAX02 12/28/17'                                      
         END                                                                    
