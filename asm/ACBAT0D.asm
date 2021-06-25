*          DATA SET ACBAT0D    AT LEVEL 009 AS OF 01/17/13                      
*PHASE T61B0DA                                                                  
         TITLE 'MEDIA CLEARANCE POSTINGS - TYPE 13'                             
T61B0D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,**BAT0D,CLEAR=YES                                   
         USING TWAD,RA                                                          
         L     R9,4(R1)                                                         
         USING GWS,R9                                                           
         USING PROGD,RC                                                         
                                                                                
VALINV   LA    R2,MSPINVH          INVOICE NUMBER IS REQ'D                      
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   EXIT                                                             
         MVC   INVNOS,FVIFLD                                                    
                                                                                
VALDAT   LA    R2,MSPDATH          DATE IS REQ'D                                
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   EXIT                                                             
         GOTO1 AVALDAT,MSPDATH                                                  
         BNE   EXIT                                                             
         GOTO1 DATECHK,BOPARM,BCWORK+2                                          
         CLI   BOPARM,X'FF'                                                     
         BE    EXIT                                                             
         MVC   INVDATP,BCWORK+2                                                 
         MVC   INVDAT,BCWORK+102                                                
                                                                                
VALMED   LA    R2,MSPMCAH          MEDIA CONTROL ACCOUNT IS REQ'D               
         MVC   MEDIA,SPACES        CLEAR MEDIA CODE                             
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   EXIT                                                             
         GOTO1 SCANNER,DMCB,(20,(R2)),(3,BLOCK)                                 
         MVI   ERRNUM,INVALID                                                   
         CLI   DMCB+4,0                                                         
         BE    EXIT                                                             
         LA    R4,BLOCK                                                         
         USING SCAND,R4                                                         
         CLI   SCLEN2,0            IS THERE A SECOND FIELD?                     
         BE    VALMED6             NO, MUST BE SZ ACCOUNT THEN                  
         CLC   SCDATA1(2),=C'MI'   YES, FIRST MUST BE MI                        
         BNE   INVERR                                                           
         TM    BCCPYST4,CPYSMINT   MUST USE MEDIA INTERFACE RECORDS             
         BZ    INVERR                                                           
         MVC   MEDIA,SCDATA2       OK, SAVE THE MEDIA                           
         USING MINRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   MINKEY,BCSPACES     READ THE MEDIA RECORD                        
         MVI   MINKTYP,MINKTYPQ                                                 
         MVC   MINKCPY,CUABIN                                                   
         MVC   MINKMED,MEDIA                                                    
         GOTO1 ARDHI,AIO1                                                       
         L     RE,AIO1                                                          
         CLC   KEYSAVE,0(RE)                                                    
         BNE   ERRXIT                                                           
                                                                                
         L     RF,AIO1                                                          
         AH    RF,DATADISP                                                      
         SR    R6,R6                                                            
                                                                                
         USING MDIELD,RF                                                        
VALMED2  CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RF),MDIELQ                                                     
         BE    VALMED4                                                          
         IC    R6,1(RF)                                                         
         AR    RF,R6                                                            
         B     VALMED2                                                          
                                                                                
VALMED4  MVC   MEDCNTL,MDICNTL                                                  
         MVC   MEDCOMM,MDICOMM                                                  
         DROP  RF                                                               
                                                                                
         USING ACTRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     VERIFY MEDIA CONTROL OR COMMISSION           
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'SZ'                                                
         MVC   ACTKACT,MEDCOMM+2   DEFAULT TO COMMISSION ACCOUNT                
         CLI   MEDCNTL+1,0         DO WE HAVE MEDCNTL FROM MI RECORD?           
         BE    *+10                NO                                           
         MVC   ACTKULA,MEDCNTL     YES, USE IT                                  
         B     VALMED8             FOUND, USE IT                                
                                                                                
         USING ACTRECD,R6                                                       
VALMED6  LA    R6,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     READ MEDIA CONTROL ACCOUNT RECORD            
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'SZ'                                                
         MVC   ACTKACT,SCDATA1     PLUG IN THE ACCOUNT                          
         CLC   SCDATA1(2),=C'SZ'                                                
         BNE   *+10                                                             
         MVC   ACTKULA,SCDATA1                                                  
         CLC   SCDATA1(3),=C'*SI'                                               
         BNE   VALMED8                                                          
         MVC   ACTKULA,SCDATA1+1                                                
                                                                                
VALMED8  GOTO1 AGETACT,0                                                        
         BNE   ERRXIT                                                           
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BZ    POSTER              NO                                           
         L     R6,AIO1                                                          
         CLC   ACTKUNT(2),=C'SI'                                                
         BNE   VALMEDC                                                          
         OC    ACASPA,ACASPA       ANY 2C ELEMENT?                              
         BZ    VALMEDC             NO, SO NO BILLING AND REVENUE                
                                                                                
         USING SPAELD,R6                                                        
         ICM   R6,15,ACASPA                                                     
                                                                                
VALMEDA  CLI   SPAEL,0             END OF RECORD?                               
         BE    VALMEDC             YES                                          
         CLI   SPAEL,SPAELQ                                                     
         BE    *+14                                                             
VALMEDB  IC    R1,SPALN                                                         
         AR    R6,R1                                                            
         B     VALMEDA                                                          
         CLI   SPATYPE,SPATANAL                                                 
         BNE   VALMEDB                                                          
         LA    R2,MSPMCAH                                                       
         B     INVERR                                                           
                                                                                
VALMEDC  MVC   MCA,ACCODE          SAVE ACCOUNT CODE AND NAME                   
         MVC   MCAN,ACNAME                                                      
         DROP  R4,R6                                                            
                                                                                
VALCSH   MVC   CSH,SPACES           CASH ACCOUNT IS OPT'L                       
         MVC   CSHN,SPACES                                                      
         LA    R2,MSPCSHH                                                       
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   VALVEN                                                           
         GOTO1 SCANNER,DMCB,(20,(R2)),(3,BLOCK)                                 
         MVI   ERRNUM,INVALID                                                   
         CLI   DMCB+4,0                                                         
         BE    EXIT                                                             
         LA    R4,BLOCK                                                         
         USING SCAND,R4                                                         
         CLI   SCLEN2,0            IS THERE A SECOND FIELD?                     
         BE    VALCSH6             NO, MUST BE SC ACCOUNT THEN                  
         CLC   SCDATA1(2),=C'MI'   YES, FIRST MUST BE MI                        
         BNE   INVERR                                                           
         TM    BCCPYST4,CPYSMINT   MUST USE MEDIA INTERFACE RECORDS             
         BZ    INVERR                                                           
         CLC   MEDIA,SPACES        DO WE ALREADY HAVE A MEDIA?                  
         BE    *+14                NO                                           
         CLC   MEDIA,SCDATA2       YES, THIS MUST BE THE SAME                   
         BNE   INVERR                                                           
                                                                                
         USING MINRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   MINKEY,BCSPACES     READ THE MEDIA RECORD                        
         MVI   MINKTYP,MINKTYPQ                                                 
         MVC   MINKCPY,CUABIN                                                   
         MVC   MINKMED,SCDATA2                                                  
         GOTO1 ARDHI,AIO1                                                       
         L     RE,AIO1                                                          
         CLC   KEYSAVE,0(RE)                                                    
         BNE   ERRXIT                                                           
                                                                                
         L     RF,AIO1                                                          
         AH    RF,DATADISP                                                      
         SR    R6,R6                                                            
                                                                                
         USING MDIELD,RF                                                        
VALCSH2  CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RF),MDIELQ                                                     
         BE    VALCSH4                                                          
         IC    R6,1(RF)                                                         
         AR    RF,R6                                                            
         B     VALCSH2                                                          
                                                                                
VALCSH4  MVC   MEDCASH,MDICSHR                                                  
         DROP  RF                                                               
                                                                                
         USING ACTRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     VERIFY MEDIA CONTROL OR COMMISSION           
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,MEDCASH                                                  
         B     VALCSH8             GO READ THE RECORD                           
                                                                                
         USING ACTRECD,R6                                                       
VALCSH6  LA    R6,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     READ CASH ACCOUNT RECORD                     
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'SC'                                                
         MVC   ACTKACT,SCDATA1     PLUG IN THE ACCOUNT                          
         CLC   SCDATA1(2),=C'SC'                                                
         BNE   *+10                                                             
         MVC   ACTKULA,SCDATA1                                                  
                                                                                
VALCSH8  GOTO1 AGETACT,0                                                        
         BNE   ERRXIT                                                           
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BZ    POSTER              NO                                           
                                                                                
         MVC   CSH,ACCODE          SAVE CASH ACCOUNT CODE AND NAME              
         MVC   CSHN,ACNAME                                                      
         DROP  R4,R6                                                            
                                                                                
VALVEN   LA    R2,MSPVENH          VENDOR IS REQ'D                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   EXIT                                                             
                                                                                
         USING ACTRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,FVIFLD                                                   
                                                                                
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         MVC   PRD,=C'ALL'         DEFAULT FOR PRD IF SP                        
         CLC   ACTKUNT(2),=C'SP'   VALIDATE UNIT/LEDGER                         
         BE    VALVEN2                                                          
         CLC   ACTKUNT(2),=C'SQ'                                                
         BE    VALVEN2                                                          
         MVC   PRD,=C'POL'         DEFAULT FOR PRD IF SS, ST OR SU              
         CLC   ACTKUNT(2),=C'SS'                                                
         BE    VALVEN2                                                          
         CLC   ACTKUNT(2),=C'ST'                                                
         BE    VALVEN2                                                          
         CLC   ACTKUNT(2),=C'SU'                                                
         BNE   EXIT                                                             
                                                                                
VALVEN2  GOTO1 AGETACT,0           READ FOR THE VENDOR                          
         BNE   VALVEN4             CHECK FOR PAYREP IF INVALID                  
                                                                                
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BZ    POSTER              NO                                           
         MVC   VEN,ACCODE          SAVE VENDOR CODE AND NAME                    
         MVC   VENN,ACNAME                                                      
         B     VALPAY                                                           
                                                                                
VALVEN4  CLI   MSPPAYH+5,0         IF VENDOR INVALID, MUST HAVE PAYREP          
         BE    ERRXIT                                                           
         MVC   VEN,IOKEYSAV        SAVE THE VENDOR KEY                          
                                                                                
VALPAY   MVC   PAY,SPACES          PAYING REP IS OPT'L                          
         MVC   PAYN,SPACES                                                      
         LA    R2,MSPPAYH                                                       
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   VALCLT                                                           
                                                                                
         USING ACTRECD,R6                                                       
         LA    R6,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,FVIFLD                                                   
                                                                                
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         CLC   ACTKUNT(2),=C'SP'   VALIDATE UNIT/LEDGER                         
         BE    VALPAY2                                                          
         CLC   ACTKUNT(2),=C'SS'                                                
         BE    VALPAY2                                                          
         CLC   ACTKUNT(2),=C'ST'                                                
         BE    VALPAY2                                                          
         CLC   ACTKUNT(2),=C'SU'                                                
         BE    VALPAY2                                                          
         CLC   ACTKUNT(2),=C'SQ'                                                
         BNE   EXIT                                                             
                                                                                
VALPAY2  GOTO1 AGETACT,0           READ FOR PAYING REP                          
         BNE   ERRXIT                                                           
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
         TM    ACBSTAT,ACBSABAL    VALID FOR POSTING?                           
         BZ    POSTER              NO                                           
                                                                                
         MVC   PAY,ACCODE          SAVE PAYING REP CODE AND NAME                
         MVC   PAYN,ACNAME                                                      
         CLC   PAY(3),VEN          PAY U/L MUST = VEN U/L                       
         BNE   INVERR                                                           
                                                                                
VALCLT   CLC   =CL2'SI',MCA+1      IF MEDIA IS SI                               
         BNE   VALCLT2                                                          
         LA    R2,MSPMCAH                                                       
         CLC   =CL2'ST',VEN+1                                                   
         BE    VALCLT1             VENDOR OR PAY MUST BE ST OR SS               
         CLC   =CL2'ST',PAY+1                                                   
         BE    VALCLT1                                                          
         CLC   =CL2'SS',VEN+1                                                   
         BE    VALCLT1                                                          
         CLC   =CL2'SS',PAY+1                                                   
         BNE   INVERR                                                           
                                                                                
VALCLT1  CLC   CSH,SPACES          AND CASH MUST BE BLANK                       
         BNE   INVERR                                                           
                                                                                
VALCLT2  LA    R2,MSPCLTH          CLIENT IS REQ'D                              
         MVI   FVMINL,2            SET MINIMUM LENGTH                           
         MVI   FVMAXL,3            SET MAXIMUM LENGTH                           
         MVI   BOFLAG1,ACIPRCLI    INDICATE - VALIDATE CLIENT                   
         XC    PSCLICOD,PSCLICOD   CLEAR OUT OLD CLIENT                         
         GOTO1 AVALCPJ,MSPCLTH                                                  
         BNE   EXIT                                                             
                                                                                
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
                                                                                
         MVC   CLT,FVIFLD          SAVE CLIENT CODE AND NAME                    
         MVC   CLTN,ACNAME                                                      
                                                                                
         MVC   MSPCLTN,ACNAME      PRINT THE NAME                               
         OI    MSPCLTNH+6,X'80'                                                 
                                                                                
VALPRD   MVC   PRDN,SPACES         PRODUCT IS OPT'L                             
         LA    R2,MSPPRDH                                                       
         MVI   FVMINL,0            SET MINIMUM LENGTH                           
         MVI   FVMAXL,3            SET MAXIMUM LENGTH                           
         MVI   BOFLAG1,ACIPRPRO    INDICATE - VALIDATE PRODUCT                  
         XC    PSPROCOD,PSPROCOD   CLEAR OUT OLD PRODUCT                        
         GOTO1 AVALCPJ,MSPPRDH                                                  
         BNE   VALPRD2                                                          
                                                                                
         TM    ACBSTAT,ACBSLOCK    ACCOUNT LOCKED?                              
         BO    LOCKER              YES                                          
                                                                                
         MVC   PRD,FVIFLD          SAVE PRODUCT CODE AND NAME                   
         MVC   PRDN,ACNAME                                                      
                                                                                
         MVC   MSPPRDN,ACNAME      PRINT THE NAME                               
         OI    MSPPRDNH+6,X'80'                                                 
         B     VALEST                                                           
                                                                                
VALPRD2  CLI   FVILEN,0            NOT VALID, WAS ANYTHNG ENTERED?              
         BNE   EXIT                YES                                          
                                                                                
VALEST   MVC   ESTNUM,=C'000'      ESTIMATE IS OPT'L                            
         LA    R2,MSPESTH                                                       
         MVI   FVMINL,0            SET MINIMUM LENGTH                           
         MVI   FVMAXL,3            SET MAXIMUM LENGTH                           
         GOTO1 AFVAL,(R2)                                                       
         BH    EXIT                MORE THAN 3                                  
         BNE   VALAOF              NOTHING ENTERED                              
         TM    FVIIND,FVINUM                                                    
         BNO   NUMERR                                                           
         SR    R1,R1                                                            
         IC    R1,FVILEN           LENGTH OF INPUT                              
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FVIFLD(0)                                                    
         ZAP   ESTP,DUB                                                         
         EDIT  ESTP,MSPEST,0,MINUS=NO,ALIGN=RIGHT,FILL=0                        
         OI    MSPESTH+6,X'80'                                                  
         MVC   ESTNUM,MSPEST       SAVE THE ESTIMATE                            
                                                                                
VALAOF   LA    R2,MSPAOFH          OFFICE IS REQ'D                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   EXIT                                                             
         GOTO1 AVALOFF,BOPARM,(X'80',FVIFLD)                                    
         BNE   EXIT                                                             
         MVC   OFFICE,FVIFLD                                                    
                                                                                
VALNAMT  LA    R2,MSPAMTH          NET AMOUNT IS REQ'D                          
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   EXIT                                                             
                                                                                
         SR    R0,R0                                                            
         IC    R0,FVILEN           LENGTH OF AMOUNT INPUT                       
         GOTOR CASHVAL,BOPARM,(X'82',FVIFLD),(R0)                               
         CLI   0(R1),0                                                          
         BNE   AMTERR              'INVALID AMOUNT'                             
         ZAP   NETAMT,4(8,R1)      SAVE AMOUNT                                  
         CP    NETAMT,=P'2100000000'                                            
         BH    AMTOOLGE                                                         
         CP    NETAMT,=P'-2100000000'                                           
         BL    AMTOOLGE                                                         
         CURED NETAMT,(12,MSPAMT),2,MINUS=YES,ALIGN=LEFT                        
         OI    MSPAMTH+6,X'80'                                                  
                                                                                
VALGAMT  ZAP   GRSAMT,=P'0'        GROSS AMOUNT IS OPT'L                        
         LA    R2,MSPGAMTH                                                      
         MVI   FVMINL,0                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   VALSUB                                                           
                                                                                
         SR    R0,R0                                                            
         IC    R0,FVILEN           LENGTH OF AMOUNT INPUT                       
         GOTOR CASHVAL,BOPARM,(X'82',FVIFLD),(R0)                               
         CLI   0(R1),0                                                          
         BNE   AMTERR              'INVALID AMOUNT'                             
         ZAP   GRSAMT,4(8,R1)      SAVE AMOUNT                                  
         CP    GRSAMT,=P'2100000000'                                            
         BH    AMTOOLGE                                                         
         CP    GRSAMT,=P'-2100000000'                                           
         BL    AMTOOLGE                                                         
         MVC   BOBYTE1,NETAMT+7    MAKE SURE BOTH SAME SIGN                     
         NI    BOBYTE1,X'0F'                                                    
         MVC   BOBYTE2,GRSAMT+7                                                 
         NI    BOBYTE2,X'0F'                                                    
         CLC   BOBYTE1,BOBYTE2                                                  
         BNE   AMTERR                                                           
         CLC   GRSAMT,NETAMT       MAKE SURE NOT LOWER THAN NET                 
         BL    AMTERR                                                           
         CURED GRSAMT,(12,MSPGAMT),2,MINUS=YES,ALIGN=LEFT                       
         OI    MSPGAMTH+6,X'80'                                                 
                                                                                
VALSUB   LA    R2,MSPSUBH          NET SUB-MEDIA MAY BE REQ'D                   
         MVI   SUBMED,C' '                                                      
         MVI   FVMINL,1                                                         
         CLC   VEN+1(2),=C'SU'     REQ'D IS VENDOR IS SU                        
         BE    VALSUB2                                                          
         CLC   VEN+1(3),=C'SSN'    OR SSN                                       
         BE    VALSUB2                                                          
         CLI   5(R2),0             NOT VALID FOR ANY OTHER VENDORS              
         BNE   INVERR                                                           
         MVI   FVMINL,0            SET OK IF NOT THERE                          
                                                                                
VALSUB2  GOTO1 AFVAL,(R2)                                                       
         BH    EXIT                NEEDED BUT NOT ENTERED                       
         BL    VALMOS              NOT NEEDED AND NOT ENTERED                   
         MVC   SUBMED,FVIFLD       ENTERED                                      
                                                                                
         CLI   SUBMED,C'N'         ENTRY MUST BE NETWORK                        
         BE    VALMOS                                                           
         CLI   SUBMED,C'S'         SYNDICATION                                  
         BE    VALMOS                                                           
         CLI   SUBMED,C'C'         CABLE                                        
         BE    VALMOS                                                           
         CLI   SUBMED,C'O'         OTHERS                                       
         BE    VALMOS                                                           
         CLI   SUBMED,C'D'         NETWORK RADIO                                
         BNE   INVERR                                                           
                                                                                
VALMOS   LA    R2,MSPMOSH          MOS IS REQ'D                                 
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         BNE   ERRXIT                                                           
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR              BAD DATE                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   MNSRV,WORK+6                                                     
                                                                                
VALNAR   MVI   NARRTVEL,0          NARRATIVE IS OPT'L                           
         LA    R2,MSPNAR1H                                                      
         GOTO1 AVALNAR,BOPARM,(2,(R2)),NARRTVE                                  
         STC   R6,NARRTVEL                                                      
                                                                                
         EJECT                                                                  
***********************************************************************         
*        BUILD POSTING HEADER                                         *         
***********************************************************************         
                                                                                
POSTBLD  LA   R8,IOAREA+2                                                       
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,DLDSELQ                                                   
         MVC   DLDSREF(3),PRD                                                   
         MVC   DLDSREF+3(3),INVNOS                                              
         MVC   TRANREF,DLDSREF     SAVE FOR POSTING                             
         MVC   DLDSDATE,INVDATP                                                 
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         XC    DLDSSTAT+1(6),DLDSSTAT+1                                         
         LA    R3,DLDSNARR                                                      
         XC    DLDSNARR,DLDSNARR                                                
         SR    R3,R3                                                            
         IC    R3,NARRTVEL                                                      
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DLDSNARR(0),NARRTVE                                              
         AHI   R3,1                                                             
         LA    R1,DLDSNARR-DLDESCD                                              
         AR    R1,R3                                                            
         STC   R1,DLDSLEN                                                       
         AR    R8,R1                                                            
         EJECT                                                                  
***********************************************************************         
*        BUILD POSTING DETAIL                                         *         
***********************************************************************         
                                                                                
         USING XPYELD,R8                                                        
         XC    XPYEL(XPYLN2Q),XPYEL                                             
         MVI   XPYEL,XPYELQ        EXTRA PAYMENT ELEMENT                        
         MVI   XPYLN,XPYLN2Q                                                    
         ZAP   XPYCD,=P'0'         CASH DISCOUNT                                
         MVC   XPYCLI,CLTN         CLIENT NAME                                  
         MVC   XPYPRO,PRDN         PRODUCT NAME                                 
         MVC   XPYSMED,SUBMED      SUB-MEDIA                                    
         MVC   XPYPRO,SPACES       IS WE HAVE A PRODUCT NAME, USE IT            
         CLC   PRDN,SPACES                                                      
         BE    *+10                                                             
         MVC   XPYPRO,PRDN                                                      
                                                                                
         MVC   WORK(L'PAY),PAY     SAVE THE PAYING REP                          
         CLI   WORK,C' '           DO WE HAVE ONE?                              
         BH    *+10                YES                                          
         MVC   WORK(L'VEN),VEN     NO, USE VENDOR THAN                          
                                                                                
         CLC   WORK+1(2),=C'SP'    SP OR SQ = PRINT                             
         BE    *+14                USE MOS FOR DATE                             
         CLC   WORK+1(2),=C'SQ'                                                 
         BNE   POSTB00                                                          
                                                                                
         MVC   WORK,SPACES                                                      
         MVC   WORK(2),MNSRV       USE MOS FOR PRINT DATE                       
         MVI   WORK+2,X'01'        SET START/END DATES                          
         GOTO1 DATCON,DMCB,(1,WORK),(X'20',XPYPER)                              
         GOTO1 DATCON,DMCB,(X'31',WORK),(X'20',XPYPER+6),(1,0)                  
         B     POSTB02                                                          
                                                                                
POSTB00  GOTO1 DATCON,DMCB,(1,INVDATP),(X'20',XPYPER)                           
         MVC   XPYPER+6(6),XPYPER                                               
         CLI   CSH,C' '            DO WE HAVE A CASH ACCOUNT?                   
         BH    POSTB02             YES, LEAVE WITH START/END DATES              
                                                                                
         CLC   WORK+1(3),=C'SSN'   SSN OR SU = NET                              
         BE    POSTB02             LEAVE WITH START/END DATE                    
         CLC   WORK+1(2),=C'SU'                                                 
         BE    POSTB02                                                          
                                                                                
         MVC   XPYPER+6(6),SPACES  NOT NET, END DATE=SPACES                     
         CLC   WORK+1(2),=C'SS'    SS OR ST = SPOT                              
         BE    POSTB02                                                          
         CLC   WORK+1(2),=C'ST'                                                 
         BE    POSTB02                                                          
         MVC   XPYPER+6(6),XPYPER  NOT SPOT, END DATE=START                     
                                                                                
POSTB02  MVC   XPYINV,SPACES                                                    
         MVC   XPYINV(L'INVNOS),INVNOS                                          
         MVI   XPYTYPE,C'1'                                                     
                                                                                
         PACK  DUB,ESTNUM(3)                                                    
         CVB   R1,DUB                                                           
         STCM  R1,3,XPYEST                                                      
                                                                                
         SR    R3,R3                                                            
         IC    R3,XPYLN                                                         
         AR    R8,R3                                                            
         DROP  R8                                                               
                                                                                
         USING GDAELD,R8           GENERAL DATE ELEMENT                         
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATCLR                                                  
         MVC   GDADATE,BCTODAYP                                                 
                                                                                
         SR    R3,R3                                                            
         IC    R3,GDALN                                                         
         AR    R8,R3                                                            
                                                                                
         XC    GDAEL(GDALNQ),GDAEL                                              
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDAMMOS     MEDIA MOS                                    
         MVC   GDAYYMM,MNSRV                                                    
         SR    R3,R3                                                            
         IC    R3,GDALN                                                         
         AR    R8,R3                                                            
         DROP  R8                                                               
                                                                                
         CP    GRSAMT,=P'0'        ANY GROSS AMOUNT?                            
         BE    POSTB04             NO, SKIP THIS ELEMENT                        
                                                                                
         USING SCIELD,R8                                                        
         XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ        X'50' ELEMENT                                
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITGRSS    GROSS                                        
         ZAP   SCIAMNT,GRSAMT                                                   
         ZAP   SCINET,=P'0'                                                     
         SR    R3,R3                                                            
         IC    R3,SCILN                                                         
         AR    R8,R3                                                            
         DROP  R8                                                               
                                                                                
         USING DLPOSTD,R8                                                       
POSTB04  MVI   DLPSEL,DLPSECRQ     CREDIT VENDOR/PAYING REP/CASH                
         MVI   DLPSLEN,DLPSLNQ                                                  
                                                                                
         CLI   CSH,C' '            DO WE HAVE A CASH ACCOUNT?                   
         BNH   POSTB06             NO, LOOK FOR PAYREP                          
         MVC   DLPSCRAC,CSH        YES, BUILD ACCOUNT AND CONTRA                
         MVC   DLPSCRNM,CSHN                                                    
         MVC   DLPSDBAC,SPACES                                                  
         MVC   DLPSDBA(1),PAY+3    CASH CONTRA IS MEDIA/CLIENT/PRODUCT          
         CLI   PAY,C' '            DO WE HAVE A PAYREP?                         
         BNE   *+10                NO, USE VENDOR MEDIA THEN                    
         MVC   DLPSDBA(1),VEN+3    CASH CONTRA IS MEDIA/CLIENT/PRODUCT          
         MVI   DLPSDBA+1,C'/'                                                   
         MVC   DLPSDBA+2(L'CLT),CLT                                             
         MVI   DLPSDBA+5,C'/'                                                   
         MVC   DLPSDBA+6(L'PRD),PRD                                             
         MVC   DLPSDBNM,CLTN                                                    
         B     POSTB09                                                          
                                                                                
POSTB06  MVC   DLPSCRAC,PAY        SET UP FOR PAYREP                            
         MVC   DLPSCRNM,PAYN                                                    
         MVC   DLPSDBA,VEN+3       CONTRA FOR PAYREP IS MED/VEN/CLT             
         MVC   DLPSDBNM,VENN                                                    
         CLI   PAY,C' '            USING PAYING REP?                            
         BNH   POSTB07             NO, SET UP FOR VENDOR                        
         CLC   DLPSCRAC+1(2),=C'SP'                                             
         BE    *+14                IS IT SP OR SQ?                              
         CLC   DLPSCRAC+1(2),=C'SQ'                                             
         BNE   POSTB08             NO, JUST ADD CLIENT                          
         MVC   DLPSDBAC(12),VEN+3  YES, SKIP OVER C/U/L                         
         B     POSTB08                                                          
                                                                                
POSTB07  MVC   DLPSCRAC,VEN        NO, SET UP FOR VENDOR                        
         MVC   DLPSCRNM,VENN                                                    
         MVC   DLPSDBAC,SPACES     VENDOR CONTRA IS CLIENT                      
         MVC   DLPSDBNM,CLTN        AND CLIENT NAME                             
                                                                                
POSTB08  MVC   DLPSDBA+9(3),CLT                                                 
POSTB09  MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,OFFICE                                                  
         ZAP   DLPSAMNT,NETAMT                                                  
         AHI   R8,DLPSLNQ                                                       
                                                                                
         CLC   MCA+1(2),=C'SI'                                                  
         BNE   POSTB10                                                          
         MVI   DLPSEL,DLPSECRQ     CREDIT THE MEDIA CONTROL                     
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,MCA                                                     
         MVC   DLPSDBC,CUABIN                                                   
         MVC   DLPSDBU(2),=C'SJ'                                                
         MVC   DLPSDBA(3),CLT                                                   
         MVC   DLPSDBNM,CLTN                                                    
         ZAP   DUB,NETAMT                                                       
         MP    DUB,=P'-1'                                                       
         ZAP   DLPSAMNT,DUB                                                     
         B     POSTB12                                                          
                                                                                
POSTB10  MVI   DLPSEL,DLPSEDRQ     DEBIT THE MEDIA CONTROL                      
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,MCA                                                     
         MVC   DLPSCRC,CUABIN                                                   
         MVC   DLPSCRU(2),=C'SJ'                                                
         MVC   DLPSCRA(3),CLT                                                   
         MVC   DLPSCRNM,CLTN                                                    
         ZAP   DLPSAMNT,NETAMT                                                  
                                                                                
POSTB12  MVI   DLPSTYPE,0                                                       
         MVC   DLPSANAL,OFFICE                                                  
         AHI   R8,DLPSLNQ                                                       
         MVI   0(R8),0                                                          
         BAS   RE,POSTIT                                                        
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              ADD TO POSTING FILE                                    *         
***********************************************************************         
                                                                                
POSTIT   NTR1                                                                   
         AHI   R8,1                END ADDRESS                                  
         LA    R1,IOAREA           START ADDRESS                                
         SR    R8,R1                                                            
         STCM  R8,3,IOAREA         TOTAL LENGTH                                 
                                                                                
         XC    BOWORK1,BOWORK1                                                  
         MVC   BOWORK1(L'TRANREF),TRANREF                                       
                                                                                
         ZAP   BOPL61,NETAMT       AMOUNT                                       
                                                                                
         GOTO1 AADDITE,BOPARM,IOAREA,BOPL61,BOWORK1                             
         LA    R0,FVFOK            TEST ADD OK                                  
         CLM   R0,3,FVMSGNO                                                     
         BE    POSTITX                                                          
         DC    H'0'                                                             
                                                                                
POSTITX  B     EXIT                                                             
***********************************************************************         
*              VERIFY DATE TO BE WITHIN RANGE                         *         
***********************************************************************         
                                                                                
DATECHK  ST    RE,SAVERE                                                        
         L     RE,0(R1)                                                         
         CLC   BCTDATL,0(RE)                                                    
         BH    DATEC02                                                          
         CLC   BCTDATH,0(RE)                                                    
         BNL   DATECX                                                           
                                                                                
DATEC02  MVI   0(R1),X'FF'         ERROR                                        
         MVC   FVMSGNO,=AL2(AE$DOPSP)                                           
                                                                                
DATECX   L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*              ERROR ROUTINES                                         *         
***********************************************************************         
                                                                                
ESTER    MVC   FVMSGNO,=AL2(AE$AEEWC)    ESTIMATE/AMOUNT ERROR                  
         B     ERRXIT                                                           
                                                                                
TOOSHRT  MVC   FVMSGNO,=AL2(FVFSHRT)     TOO SHORT                              
         B     ERRXIT                                                           
                                                                                
TAXERR   MVC   FVMSGNO,=AL2(AE$INVIF)    TAX ERROR                              
         B     ERRXIT                                                           
                                                                                
AMTERR   MVC   FVMSGNO,=AL2(AE$INVAM)    AMOUNT INVALID                         
         B     ERRXIT                                                           
                                                                                
AMTOOLGE MVC   FVMSGNO,=AL2(AE$AMLGE)    AMOUNT TOO LARGE                       
         B     ERRXIT                                                           
                                                                                
DATERR   MVC   FVMSGNO,=AL2(AE$DOPSP)    DATE OUTSIDE PERMITTED SPAN            
         B     ERRXIT                                                           
                                                                                
LOCKER   MVC   FVMSGNO,=AL2(AE$ACTLK)    ACCOUNT IS LOCKED                      
         B     ERRXIT                                                           
                                                                                
CLOSER   MVC   FVMSGNO,=AL2(AE$JBCLO)    ACCOUNT IS CLOSED                      
         B     ERRXIT                                                           
                                                                                
POSTER   MVC   FVMSGNO,=AL2(AE$INACP)    NOT VALID ACCOUNT FOR POSTING          
         B     ERRXIT                                                           
                                                                                
MISERR   MVC   FVMSGNO,=AL2(AE$MISIF)    MISSING INPUT FIELD                    
         B     ERRXIT                                                           
                                                                                
INVERR   MVC   FVMSGNO,=AL2(AE$INVIF)    INVALID ENTRY                          
         B     ERRXIT                                                           
                                                                                
NUMERR   MVC   FVMSGNO,=AL2(FVFNOTN)     NOT NUMERIC                            
         B     ERRXIT                                                           
                                                                                
ERRXIT   MVC   FVXTRA,BCSPACES     COMMON PORTION OF ERROR ROUTINE              
                                                                                
EXIT     ST    R2,FVADDR                                                        
EXIT1    XIT1                                                                   
         EJECT                                                                  
         EJECT                                                                  
*ACBATDSECT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBATDSECT                                                     
         PRINT ON                                                               
       ++INCLUDE ACBATA1D                                                       
         EJECT                                                                  
PROGD    DSECT                                                                  
INVNOS   DS    CL11                INVOICE NUMBER                               
INVDAT   DS    CL8                 INVOICE DATE                                 
INVDATP  DS    CL3                 INVOICE DATE PACKED                          
MEDIA    DS    CL2                 MI CODE                                      
MEDCNTL  DS    CL14                MEDIA INTERFACE CONTROL ACCOUNT              
MEDCOMM  DS    CL14                MEDIA INTERFACE COMMISSION ACCOUNT           
MCA      DS    CL15                MEDIA CONTROL ACCOUNT CODE                   
MCAN     DS    CL36                MEDIA CONTROL ACCOUNT NAME                   
MEDCASH  DS    CL14                MEDIA INTERFACE CASH ACCOUNT                 
CSH      DS    CL15                CASH ACCOUNT CODE                            
CSHN     DS    CL36                CASH ACCOUNT NAME                            
VEN      DS    CL15                VENDOR CODE                                  
VENN     DS    CL36                VENDOR NAME                                  
PAY      DS    CL15                PAYING REP CODE                              
PAYN     DS    CL36                PAYING REP NAME                              
CLT      DS    CL3                 CLIENT CODE                                  
CLTN     DS    CL36                CLIENT NAME                                  
PRD      DS    CL3                 PRODUCT CODE OR ALL OR POL                   
PRDN     DS    CL36                PRODUCT NAME                                 
ESTP     DS    PL2                 PACKED ESTIMATED                             
ESTNUM   DS    CL3                 ESTIMATE                                     
OFFICE   DS    CL2                 OFFICE CODE                                  
NETAMT   DS    PL8                 NET AMOUNT                                   
GRSAMT   DS    PL8                 GROSS AMOUNT                                 
SUBMED   DS    CL1                 NET SUB-MEDIA                                
MNSRV    DS    XL2                 MOS                                          
NARRTVE  DS    CL121               NARRATIVE                                    
NARRTVEL DS    X                   NARRATIVE LENGTH                             
TRANREF  DS    CL(L'DLDSREF)       PRODUCT/INVOICE NUMBER                       
                                                                                
SAVERE   DS    F                   RE SAVEAREA                                  
BLOCK    DS    CL42                                                             
KEY      DS    CL49                                                             
IOAREA   DS    CL3000                                                           
PROGDX   DS    0C                                                               
         EJECT                                                                  
SCAND    DSECT                                                                  
SCLEN1   DS    CL1                                                              
SCLEN2   DS    CL1                                                              
SCVAL1   DS    CL1                                                              
SCVAL2   DS    CL1                                                              
SCBIN1   DS    CL4                                                              
SCBIN2   DS    CL4                                                              
SCDATA1  DS    CL10                                                             
SCDATA2  DS    CL20                                                             
SCLNQ    EQU   *-SCAND                                                          
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACBAT0D   01/17/13'                                      
         END                                                                    
