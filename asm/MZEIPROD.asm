*          DATA SET MZEIPROD   AT LEVEL 048 AS OF 11/06/98                      
*PHASE MZEIPRD                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21750  -- AUTOPAY CLEARANCE MAINTENANCE & LIST      *         
*                                                                     *         
*  COMMENTS:     MAINTAINS AUTOPAY CLEARANCE RECORDS                  *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SPSFMED    INT) & SPSFM7B (LIST)              *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21703 - AUTOPAY CLEARANCE MAINTENANCE AND LIST'                
T21703   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1703**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       XC    KEYPRD,KEYPRD       CLEAR KEY                                    
         LA    R2,PRDMEDKH         POINT R2 TO MEDIA                            
         CLI   PRDMEDKH+5,0                                                     
         BNE   CHKMED                                                           
         CLI   ACTEQU,ACTLIST      LIST ACTION?                                 
         BE    VK10                                                             
CHKMED   GOTO1 VALIMED             CHECK VALIDITY FOR MEDIA                     
         FOUT  PRDMEDNH,MEDNM,10   DISPLAY MEDIA NAME                           
*                                                                               
         LA    R2,PRDCLIKH         POINT R2 TO CLIENT                           
         GOTO1 VALICLT             CHECK VALIDITY FOR CLIENT                    
         FOUT  PRDCLINH,CLTNM,20   DISPLAY CLIENT NAME                          
         LA    RE,AIO                                                           
         USING CLTHDR,RE           CLIENT RECORD DSECT                          
         MVC   SVP1USER,CPU1       PRODUCT USER FLD DESC 1                      
         MVC   SVP2USER,CPU2       PRODUCT USER FLD DESC 1                      
         MVC   SVCLOP1,COPT1       CLIENT OPTION 1                              
         MVC   SVCLOP2,COPT2       CLIENT OPTION 2                              
         MVC   SVP1TYPE,CPU1TYPE   PRODUCT USER TYPE                            
         MVC   SVP2TYPE,CPU2TYPE                                                
         MVC   SVP1LEN,CPU1LEN     PRODUCT USER LENGTH                          
         MVC   SVP2LEN,CPU2LEN                                                  
         MVC   SVP1FLG1,CPU1FLG1                                                
         MVC   SVP1FLG2,CPU1FLG2                                                
         MVC   SVP2FLG1,CPU2FLG1                                                
         MVC   SVP2FLG2,CPU2FLG2                                                
*                                                                               
         LA    R2,PRDPROKH         POINT R2 TO PRODUCT                          
         GOTO1 VALIPRD             CHECK VALIDITY FOR PRODUCT                   
         FOUT  PRDPRONH,PRDNM,20   DISPLAY PRODUCT NAME                         
         B     VK20                                                             
*                                                                               
VK10     XC    KEYPRD,KEYPRD                                                    
         XC    PRDCLIK,PRDCLIK                                                  
         XC    PRDPROK,PRDPROK                                                  
         MVI   PRDMEDK,C'T'                                                     
*                                                                               
VK20     LA    R4,KEY                                                           
         USING PRDHDR,R4                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   PKEYTYPE,X'00'                                                   
         MVC   PKEYAM,PRDMEDK      MEDIA CODE                                   
         MVC   PKEYCLT,PRDCLIK     CLIENT CODE                                  
         MVC   PKEYPRD,PRDPROK     PRODUCT CODE                                 
         DROP  R4                                                               
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       L     R4,AIO                                                           
         USING PRDHDR,R4                                                        
*                                                                               
VR0      MVC   PNAME,PRDPRON       STORE PRODUCT NAME FROM SCREEN               
*                                                                               
         LA    R2,PRDACCTH         ACCOUNT NUMBER                               
         GOTO1 ANY                                                              
         LA    R1,AGYTAB           TABLE OF CODES                               
*                                                                               
VR0A     CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    VR0B                                                             
         CLC   0(2,R1),SVCTAGY     CHECK FOR MATCH                              
         BE    VR1A                                                             
         LA    R1,2(R1)            BUMP TO NEXT ELEMENT IN TABLE                
         B     VR0A                REPEAT TILL MATCH                            
*                                                                               
VR0B     CLI   5(R2),4             INPUT LENGTH MUST BE 4 CHARS                 
         BNE   ERRINV                                                           
         CLC   QCLT,=C'BM '        BRISTOL MYERS                                
         BNE   VR1                                                              
         CLC   =C'BO',SVCTAGY                                                   
         BNE   VR1                                                              
         TM    4(R2),X'08'         MUST BE 4 NUMERICS                           
         BZ    ERRINV                                                           
*                                                                               
VR1      MVC   PACCT(4),8(R2)      STORE FIELD IN RECORD                        
         B     VR1C                                                             
*                                                                               
VR1A     CLI   5(R2),5         FM,GY,DR,GN,CE MUST BE 5 NUMERICS                
         BNE   ERRINV                                                           
         TM    4(R2),X'08'     NUMERICS                                         
         BZ    ERRINV                                                           
         PACK  PACCT(4),8(5,R2)  STORE FIELD IN RECORD                          
         MVI   PACCT,X'FF'     X'FF' MEANS ITS PACKED                           
*                                                                               
VR1C     DS    0H                  PRODUCT CLASS                                
         LA    R2,PRDCLASH         SINGLE OR DOUBLE                             
         MVI   BYTE,0                                                           
         CLI   5(R2),0                                                          
         BE    VR1H                NO CLASS                                     
         CLC   8(2,R2),SPACES      CHK FOR SPACES ANYWAY                        
         BE    VR1H                TREAT AS NO INPUT                            
         MVC   HALF,8(R2)                                                       
         MVC   BYTE,HALF                                                        
         CLI   HALF+1,C' '         IS 2ND CLASS GIVEN                           
         BNH   VR1H                NO                                           
*                                                                               
         CLI   HALF,C'A'           BOTH MUST BE A-I                             
         BL    ERRINV                                                           
         CLI   HALF,C'I'                                                        
         BH    ERRINV                                                           
         CLI   HALF+1,C'A'                                                      
         BL    ERRINV                                                           
         CLI   HALF+1,C'I'                                                      
         BH    ERRINV                                                           
*                                                                               
         CLC   HALF(1),HALF+1      AND NOT THE SAME                             
         BE    ERRINV                                                           
*                                  HOLD AS 2 NIBBLES 1-9                        
         NI    HALF,X'0F'                                                       
         NI    HALF+1,X'0F'                                                     
         PACK  BYTE,HALF(1)        1ST CLASS                                    
         OC    BYTE,HALF+1         2ND CLASS                                    
*                                                                               
VR1H     DS    0H                                                               
         MVC   PCLASS,BYTE         STORE PRODUCT CLASS IN RECORD                
*                                                                               
VR2      LA    R2,PRDBNAMH         BILL-TO-NAME                                 
         GOTO1 ANY                                                              
         XC    PADDR1(120),PADDR1  CLEAR BILL ADDRES1                           
         MVC   PADDR1,PRDBNAM      STORE BILL TO NAME IN RECORD                 
         OC    PADDR1,SPACES                                                    
         LA    R2,PRDADD2H         ADDRESS2                                     
         CLI   5(R2),0             IS LENGTH OF INPUT = 0                       
         BE    *+16                                                             
         MVC   PADDR2,8(R2)        STORE ADDRESS2 IN RECORD                     
         OC    PADDR2,SPACES                                                    
         LA    R2,PRDADD3H         ADDRESS 3                                    
         CLI   5(R2),0             IS THERE AN ADDRESS 3?                       
         BE    *+16                                                             
         MVC   PADDR3,8(R2)        STORE ADDRESS3 IN RECORD                     
         OC    PADDR3,SPACES                                                    
         LA    R2,PRDADD4H         ADDRESS 4                                    
         CLI   5(R2),0             IS THERE AN ADDRESS 4?                       
         BE    VR3                                                              
         MVC   PADDR4,8(R2)        STORE ADDRESS 4 IN RECORD                    
         OC    PADDR4,SPACES                                                    
*                                                                               
VR3      LA    R2,PRDOAFH          OTHER AGENCY FEE                             
         CLI   5(R2),0             IS THERE AN OAF?                             
         BNE   VR3A                                                             
         XC    PAGYFEE,PAGYFEE     NO,CLEAR STORAGE IN RECORD                   
         B     VR5                                                              
*                                                                               
VR3A     ZIC   R0,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,PRDOAF),(R0)                                     
         CLI   DMCB,X'FF'        INVALID CASH FIELD?                            
         BE    ERRINV                                                           
         L     R0,DMCB+4         CENTS IN BINARY                                
         CVD   R0,DUB                                                           
         CP    DUB,=P'999'       CAN'T EXCEED 9.99                              
         BH    LFMERR                                                           
         ZAP   PAGYFEE,DUB       STORE IN PAGYFEE IN RECORD                     
*                                                                               
VR5      LA    R2,PRDBBASH       BILL BASIS                                     
         XC    PBILLBAS(5),PBILLBAS                                             
         CLI   5(R2),0           IS THERE A BILL BASIS                          
         BE    VR5B                                                             
*                                                                               
VR5A2    DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+16(4),=C'sB1X'   's' MUST BE LOWER CASE - 3 CHAR            
         MVC   WORK+20(2),SVCTAGY    PROFILE NAME                               
         MVC   WORK+22(1),QMED                                                  
         MVC   WORK+23(3),QCLT                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         CLI   WORK+11,C' '        DON'T ALLOW BILL FORMULA IF OPT 12           
         BNH   VR5A3               IS SET                                       
         CLI   WORK+11,C'N'                                                     
         BNE   ERRINV                                                           
*                                                                               
VR5A3    GOTO1 ANY                      R2 $S AT ESTBBASH                       
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         CLI   8(R2),C'C'          CHK FOR COMMISSION ONLY                      
         BNE   VR5A4                                                            
         OI    PBILLBAS,X'40'                                                   
         BCTR  R4,0                                                             
         CLI   5(R2),1                                                          
         BE    ERRINV              NO 'C' ALONE                                 
         LA    R5,1(R5)            BUMP PAST 'C'                                
*                                                                               
VR5A4    EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'GROSS'   ALLOW G-GROSS                                
         BE    VR5B                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'NET  '   OR N-NET                                     
         BNE   ERRINV                                                           
         OI    PBILLBAS,X'10'                                                   
*                                                                               
VR5B     LA    R2,PRDCPCTH         COMMISSION PERCENTAGE                        
         CLI   5(R2),0                                                          
         BE    VR5B1                                                            
         ZIC   R0,5(R2)            STORE COMM PCT IN R0                         
         BCTR  R0,0                                                             
         GOTO1 CASHVAL,DMCB,(4,PRDCPCT+1),(R0)                                  
         CLI   DMCB,X'FF'          INVALID CASH FIELD?                          
         BE    ERRINV                                                           
         L     R0,DMCB+4           STORE CENTS IN R0                            
         C     R0,=F'1000000'      100.0000 MAX                                 
         BH    ERRINV                                                           
         C     R0,=F'0'            MUST BE GREATER THAN ZERO                    
         BNH   ERRINV                                                           
         CLI   PRDCPCT,C'+'        IS COMM PCT POSITIVE?                        
         BE    VR5BX                                                            
         CLI   PRDCPCT,C'-'        IS COMM PCT NEGATIVE?                        
         BNE   ERRINV              ERROR BNE                                    
         LCR   R0,R0               MAKE NEGATIVE                                
*                                                                               
VR5BX    STCM  R0,15,PBILLCOM                                                   
         B     VR5C                                                             
*                                                                               
VR5B1    MVI   GTMERR,MISSING                                                   
         CLI   PRDCBASH+5,0             REQUIRED IF COM BASIS PRESENT           
         BNE   LFMERR                                                           
*                                                                               
VR5C     LA    R2,PRDCBASH              COMM BASIS                              
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    VR5C1                                                            
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                 POINT R5 AT PRDCBAS                     
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'GROSS'        ALLOW G-GROSS                           
         BE    VR5CX                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'NET  '        OR N-NET                                
         BNE   ERRINV                                                           
         OI    PBILLBAS,X'01'                                                   
*                                                                               
VR5CX    B     VR5X                                                             
*                                                                               
VR5C1    MVI   GTMERR,MISSING                                                   
         CLI   PRDCPCTH+5,0                                                     
         BNE   LFMERR                                                           
*                                                                               
VR5X     OC    PBILLBAS(5),PBILLBAS           WILL BE ZEROS IF                  
         BNZ   VR5XC                          GROSS ALONE                       
         CLI   PRDBBASH+5,0                                                     
         BE    VR6             NO FORMULA                                       
*                              MUST HAVE BEEN GROSS ALONE                       
*                              PUT X'80' SO BILLING WILL THINK IT'S             
*                              A FORMULA                                        
         B     VR5XE                                                            
VR5XC    OC    PBILLCOM,PBILLCOM                                                
         BNZ   VR6                                                              
         CLI   PBILLBAS,X'40'      WAS GROSS ALONE + COMMISSION ONLY            
         BNE   VR6                                                              
*                                                                               
VR5XE    OI    PBILLBAS,X'80'                                                   
*                                                                               
VR6      LA    R2,PRDEDATH            EFFECTIVE DATE OF BILL FORMULA            
         OC    PBILLBAS(5),PBILLBAS           SEE IF FORMULA INPUT              
         BNZ   VR6A                                                             
         XC    PBILLDT,PBILLDT                                                  
         CLI   5(R2),0                                                          
         BE    VR8                                                              
         B     ERRINV             NO FORMULA SO EFF DATE INVALID                
*                                                                               
VR6A     GOTO1 ANY          REQUIRED IF FORMULA INPUT                           
         GOTO1 DATVAL,DMCB,(2,PRDEDAT),WORK                                     
         MVI   GTMERR,INVDATE                                                   
         OC    DMCB(4),DMCB                                                     
         BZ    LFMERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         GOTO1 DATCON,DMCB,(5,0),(3,WORK+16)  GET CURRENT DATE                  
         CLI   WORK+16,99                      DID WE GET TO 2000 YET           
         BH    VR6B                            YES - ANY DATE IS VALID          
         CLI   WORK+10,99                      TEST USER INPUT > 2000           
         BNH   VR6B                                                             
         IC    R0,WORK+10                                                       
         SH    R0,=H'100'                                                       
         STC   R0,WORK+10                                                       
VR6B     MVC   PBILLDT,WORK+10           YM                                     
*                                                                               
VR8      LA    R2,PRDGSTH         GST CODE                                      
         MVI   PGSTCODE,0                                                       
         CLI   5(R2),0            ANY GST INPUT?                                
         BE    VR11A              NO                                            
         LA    RE,GSTTAB          GST TABLE OF VALID ENTRIES                    
*                                                                               
VR10     CLC   0(1,RE),PRDGST     COMPARE GST                                   
         BE    VR11               MATCH                                         
         CLI   0(RE),X'FF'        END OF TABLE?                                 
         BE    ERRINV                                                           
         LA    RE,1(RE)           BUMP TO NEXT ELEMENT IN TAB                   
         B     VR10               REPEAT LOOP TILL MATCH                        
*                                                                               
VR11     MVC   PGSTCODE,PRDGST    MOVE INTO RECORD                              
*                                                                               
VR11A    GOTO1 (VALPST),DMCB,(RC),RR=RELO                                       
         BNE   ERRINV                                                           
*                                                                               
VR12     XC    USERDATA,USERDATA                                                
         OC    SVP1USER,SVP1USER    ANY "PRODUCT 1" INFO?                       
         BZ    VR12A                NO, DO "PRODUCT 2".                         
         LA    R2,PRDUSR1H                                                      
         ST    R2,AUSR             AUSR=A(INPUT FIELD).                         
         MVC   UTYPE,SVP1TYPE      TYPE.                                        
         MVC   LEN,SVP1LEN         LENGTH.                                      
         MVC   FLAG1,SVP1FLG1      1ST FLAG                                     
         MVC   FLAG2,SVP1FLG2      2ND FLAG                                     
         GOTO1 =A(EDTUSR),DMCB,(RC),RR=RELO                                     
         BNE   LFMERR                                                           
*                                                                               
VR12A    MVC   PUSER1,USERDATA                                                  
         MVC   PRDUSR1,USERDATA    CLEAR OR RE-TRANSMIT FIELD.                  
         OI    PRDUSR1H+6,X'80'                                                 
*                                                                               
         XC    USERDATA,USERDATA                                                
         OC    SVP2USER,SVP2USER   ANY "PRODUCT 2" INFO?                        
         BZ    VR12B               NO, MOVE ON.                                 
         LA    R2,PRDUSR2H                                                      
         ST    R2,AUSR             A(INPUT FIELD).                              
         MVC   UTYPE,SVP2TYPE      TYPE.                                        
         MVC   LEN,SVP2LEN         LENGTH.                                      
         MVC   FLAG1,SVP2FLG1      1ST FLAG                                     
         MVC   FLAG2,SVP2FLG2      2ND FLAG                                     
         GOTO1 =A(EDTUSR),DMCB,(RC),RR=RELO                                     
         BNE   LFMERR                                                           
*                                                                               
VR12B    MVC   PUSER2,USERDATA                                                  
         MVC   PRDUSR2,USERDATA    CLEAR OR RE-TRANSMIT FIELD.                  
         OI    PRDUSR2H+6,X'80'                                                 
*                                                                               
         LA    R2,PRDOPTNH         OPTIONS                                      
         MVI   OPTNFLAG,0                                                       
         GOTO1 SCANNER,DMCB,(R2),LIOS                                           
         ZICM  R0,DMCB+4           NUMBER OF ENTRIES                            
         BZ    VR14                                                             
*                                                                               
         LA    R5,LIOS                                                          
VR13     MVI   GTMERR,INVALID     INVALID INPUT                                 
         ZICM  R1,0(R5)                                                         
         BZ    VR14                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'NTP'                                                 
         BNE   LFMERR              TAL IS THE ONLY VALID OPTION NOW             
*                                                                               
* VALIDATE NTP=0-2                                                              
         TM    OPTNFLAG,TALOPTN                                                 
         BO    LFMERR                                                           
         MVC   ERRNUM,=AL2(GMINTSET)  GMI NOT SETUP FOR THIS CLIENT             
         MVI   GTMERR,NEWERR                                                    
         TM    SVCLOP1,COP1GMI                                                  
         BNO   LFMERR                                                           
*                                                                               
         MVC   ERRNUM,=AL2(TALNTVAL)  TAL NOT VALID FOR THIS PRODUCT            
         MVI   GTMERR,NEWERR                                                    
         CLC   QPRD,=C'AAA'                                                     
         BE    LFMERR                                                           
         CLC   QPRD,=C'POL'                                                     
         BE    LFMERR                                                           
*                                                                               
         MVI   GTMERR,INVALID                                                   
         TM    3(R5),X'80'         SECOND HALF IS VALID NUMERIC                 
         BNO   LFMERR                                                           
         CLI   1(R5),1             LENGTH OF SECOND HALF HAS TO BE 1            
         BNE   LFMERR                                                           
         CLI   11(R5),2            CAN ONLY BE 0-2                              
         BH    LFMERR                                                           
         CLI   SVACT,C'A'                                                       
         BE    *+14                                                             
         CLC   PTAL,11(R5)                                                      
         BNE   CNTCHNGE                                                         
         MVC   PTAL,11(R5)                                                      
         OI    OPTNFLAG,TALOPTN                                                 
         LA    R5,32(R5)                                                        
         BCT   R0,VR13                                                          
*                                                                               
VR14     TM    OPTNFLAG,TALOPTN    CHECK IF TAL OPTION REQUIRED                 
         BO    VRX                                                              
         CLC   QPRD,=C'AAA'    NOT ALLOWED WITH PRD AAA & POL                   
         BE    VRX                                                              
         CLC   QPRD,=C'POL'                                                     
         BE    VRX                                                              
         TM    SVCLOP1,COP1GMI                                                  
         BO    TALREQ              TAL REQUIRED FOR THIS CLIENT                 
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       LA    R0,PRDOPTNH         LAST FIELD ON SCREEN                         
         LA    R2,PRDACCTH         FIRST FIELD HEADER                           
*                                                                               
DR10     ZIC   R1,0(R2)            LENGTH OF FIELD + HEADER                     
         SH    R1,=H'9'            MINUS HEADER +1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DR20     ZIC   R1,0(R2)            RESTORE LENGTH                               
         AR    R2,R1               NEXT SCREEN FIELD                            
         CR    R2,R0               END OF SCREEN?                               
         BE    *+16                                                             
         TM    1(R2),X'20'         NO--FIELD IS PROTECTED?                      
         BZ    DR10                NO--CLEAR IT                                 
         B     DR20                YES -- BUMP TO NEXT FIELD                    
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDR,R6                                                        
         CLI   PACCT,X'FF'         IS PACCT PACKED?                             
         BNE   DR30                IF NOT PACKED                                
         UNPK  PRDACCT(5),PACCT+1(3)                                            
         FOUT  PRDACCTH            DISPLAY ACCOUNT NAME ON SCREEN               
         B     DR40                                                             
*                                                                               
DR30     FOUT  PRDACCTH,PACCT,4    DISPLAY ACCOUNT NAME (NOT PACKED)            
         MVC   HALF,SPACES         STORE BLANKS IN HALF                         
         CLI   PCLASS,0            CHECK FOR NO PCLASS                          
         BE    DR35                                                             
         MVC   HALF(1),PCLASS      STORE PCLASS IN HIGH BYTE OF HALF            
         MVC   HALF+1,C' '                                                      
         CLI   PCLASS,X'99'        TEST 2 CLASSES                               
         BH    DR35                NO                                           
         PACK  HALF(1),PCLASS      FIRST CLASS                                  
         NI    HALF,X'0F'                                                       
         OI    HALF,X'C0'          MAKE A - I                                   
         MVC   HALF+1(1),PCLASS    2ND CLASS                                    
         NI    HALF+1,X'0F'                                                     
         OI    HALF+1,X'C0'        MAKE A - I                                   
*                                                                               
DR35     FOUT  PRDCLASH,HALF,2     DISPLAY PRODUCT CLASS                        
*                                                                               
DR40     FOUT  PRDBNAMH,PADDR1,30  DISPLAY BILL TO NAME                         
         FOUT  PRDADD2H,PADDR2,30  DISPLAY ADDRESSES                            
         FOUT  PRDADD3H,PADDR3,30                                               
         FOUT  PRDADD4H,PADDR4,30                                               
         OC    PAGYFEE,PAGYFEE     IS THERE OTHER AGENCY FEE?                   
         BNZ   DR50                YES                                          
         FOUT  PRDOAFH,SPACES,5    IF NO FILL IN BLANKS IN FIELD                
         B     DR60                                                             
*                                                                               
DR50     EDIT  (P2,PAGYFEE),(4,PRDOAF),2                                        
         FOUT  PRDOAFH             DISPLAY OTHER AGENCY FEE                     
*                                                                               
DR60     OC    PBILLBAS(5),PBILLBAS       BILLING FORMULA?                      
         BNZ   DR70                YES                                          
         FOUT  PRDBBASH,SPACES,5   NO,FILL IN BLANKS                            
         FOUT  PRDCPCTH,SPACES,8                                                
         FOUT  PRDCBASH,SPACES,5                                                
         B     DR80                                                             
*                                                                               
DR70     MVC   PRDBBAS,=CL5'CNET'                                               
         TM    PBILLBAS,X'50'                                                   
         BO    DR75                                                             
         MVC   PRDBBAS,=CL5'NET'                                                
         TM    PBILLBAS,X'10'                                                   
         BO    DR75                                                             
         MVC   PRDBBAS,=C'CGROS'                                                
         TM    PBILLBAS,X'40'                                                   
         BO    DR75                                                             
         MVC   PRDBBAS,=C'GROSS'                                                
*                                                                               
DR75     FOUT  PRDBBASH             DISPLAY BILL BASIS TO SCREEN                
         ICM   R5,15,PBILLCOM       COMMISSION                                  
         LTR   R5,R5                SET CONDITION CODE                          
         BNZ   DR75B                                                            
         FOUT  PRDCPCTH,SPACES,8    DISPLAY BLANKS                              
         FOUT  PRDCBASH,SPACES,5    DISPLAY BLANKS                              
         B     DR80                                                             
*                                                                               
DR75B    LPR   RF,R5                                                            
         C     RF,=F'1000000'       +/-100.0000 WONT FIT                        
         BNE   DR75C                                                            
         MVC   PRDCPCT+1(3),=C'100'                                             
         B     DR75D                                                            
*                                                                               
DR75C    EDIT  (R5),(8,PRDCPCT),4,FLOAT=+,ALIGN=LEFT                            
*                                                                               
DR75D    LTR   R5,R5                                                            
         BNM   *+8                                                              
         MVI   PRDCPCT,C'-'                                                     
         FOUT  PRDCPCTH             DISPLAY                                     
         MVC   PRDCBAS,=C'GROSS'                                                
         TM    PBILLBAS,X'01'                                                   
         BZ    *+10                                                             
         MVC   PRDCBAS,=CL5'NET'                                                
         FOUT  PRDCBASH             DISPLAY                                     
*                                                                               
DR80     OC    PBILLDT,PBILLDT      EFFECTIVE Y/M OF SERVICE                    
         BNZ   DR85                                                             
         FOUT  PRDEDATH,SPACES,8   DISPLAY SPACES                               
         B     DR90                                                             
*                                                                               
DR85     GOTO1 DATCON,DMCB,(3,PBILLDT),(6,PRDEDAT)                              
         FOUT  PRDEDATH             DISPLAY Y/M OF SERVICE                      
*                                                                               
DR90     FOUT  PRDGSTH,PGSTCODE,1   GOODS AND SERVICE TAX                       
         OI    PRDGSTH+6,OI1T                                                   
*                                                                               
         GOTO1 =A(DISPPST),DMCB,(RC),RR=RELO                                    
*        BAS   RE,DISPPST                                                       
*                                                                               
         XC    PRDUSR1,PRDUSR1                                                  
         OC    SVP1USER,SVP1USER                                                
         BZ    *+10                                                             
         MVC   PRDUSR1,PUSER1        DISPLAY ON SCREEN                          
*                                                                               
         XC    PRDUSR2,PRDUSR2                                                  
         OC    SVP2USER,SVP2USER                                                
         BZ    *+10                                                             
         MVC   PRDUSR2,PUSER2        DISPLAY ON SCREEN                          
*                                                                               
         OI    PRDUSR1H+6,X'80'      TRANSMIT                                   
         OI    PRDUSR2H+6,X'80'                                                 
*                                                                               
         MVC   PRDOPTN,SPACES                                                   
         TM    SVCLOP1,COP1GMI        CLIENT OPTION 1                           
         BNO   DR95                                                             
         CLC   QPRD,=C'AAA'                                                     
         BE    DR95                                                             
         CLC   QPRD,=C'POL'                                                     
         BE    DR95                                                             
         MVC   PRDOPTN(4),=C'NTP='                                              
         ZIC   R1,PTAL                                                          
         CVD   R1,DUB                                                           
         UNPK  PRDOPTN+4(1),DUB                                                 
         OI    PRDOPTN+4,X'F0'                                                  
DR95     OI    PRDOPTNH+6,X'80'                                                 
         DROP  R6                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
*                                                                               
DKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0X                                                               
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
AGYTAB   DC    C'GY'                                                            
         DC    C'DR'                                                            
         DC    C'GN'                                                            
         DC    C'CE'                                                            
         DC    C'FM'                                                            
         DC    C'RE'                                                            
         DC    X'FF'                                                            
*                                                                               
GSTTAB   DC    C'S'                 STANDARD                                    
         DC    C'U'                                                             
         DC    C'X'                                                             
         DC    C'Z'                 ZERO                                        
         DC    X'FF'                END OF TABLE                                
*                                                                               
TALTAB   DC    X'0',X'01',X'3F'     TAL VALUE/MIN PRDCODE/MAX PRDCODE           
         DC    X'1',X'40',X'5F'                                                 
         DC    X'2',X'60',X'7F'                                                 
         DC    X'3',X'80',X'9F'                                                 
         DC    X'FF'                                                            
TALERR   MVC   ERRNUM,=AL2(ERPRCTAL) ERROR PROCESSINT TAL VALUE                 
         B     NEWERRS                                                          
*                                                                               
TALREQ   MVC   ERRNUM,=SL2(TALREQRD) TAL OPTION REQUIRED FOR CLIENT             
NEWERRS  MVI   GTMERR,NEWERR                                                    
         B     LFMERR                                                           
*                                                                               
CNTCHNGE MVI   GTMERR,NOCHGERR                                                  
         B     LFMERR                                                           
*                                                                               
LFMERR   GOTO1 ERROR                                                            
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
SETUPX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
DISPPST  NMOD1 0,12/DIPST                                                       
         L     RC,0(R1)                                                         
         MVC   PRDPST,SPACES        OUTPUT                                      
         OI    PRDPSTH+6,X'80'                                                  
         OC    PPST,PPST            IS THERE ANYTHING TO DISPLAY                
         BZ    DPX                                                              
*                                                                               
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM            CLEAR INTERFACE BLOCK                       
         MVI   PSTACT,PSTFMTQ       ACTION= FORMAT                              
         LA    R1,PPST                                                          
         ST    R1,PSTADIN           INPUT ADDRESS                               
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    RE,PSTADOUT          OUTPUT ADDRESS                              
         MVC   PSTACOM,COMFACS     A(COMFACS)                                   
         GOTO1 PSTVAL,DMCB,(R4)                                                 
         MVC   PRDPST,PSTOUT        OUTPUT                                      
*                                                                               
DPX      XIT1                                                                   
*                                                                               
EDTUSR   NMOD1 0,12/EDUSR                                                       
         L     RC,0(R1)                                                         
         L     R3,AUSR                                                          
         CLI   5(R3),0                INPUT?                                    
         BNE   EDTUSR10               YES, PROCESS IT                           
*                                                                               
         MVI   GTMERR,MISSING         NO,MISSING                                
         TM    FLAG1,CFLGREQQ         WAS INPUT REQUIRED?                       
         BZ    YES2                   NO, OKAY                                  
         B     NO2                    YES, ERROR                                
*                                                                               
EDTUSR10 MVI   GTMERR,TOOLONG         INPUT TOO LONG                            
         CLC   LEN,5(R3)              CHECK IF LENGTH VALID                     
         BL    NO2                    NOT VALID                                 
         CLI   UTYPE,C' '             IS TYPE SUPPOSED TO BE WILD?              
         BNH   EDTUSR80                                                         
         CLI   UTYPE,C'C'             IS TYPE SUPPOSED TO BE CHAR?              
         BNE   EDTUSR60                                                         
         LA    R4,8(R3)               R4 ---> INPUT                             
         ZIC   R1,5(R3)               R1 = L(INPUT)                             
*                                                                               
EDTUSR40 CLI   0(R4),C'0'             ALLOW ALL INPUT BUT NUMBERS               
         BL    EDTUSR50                                                         
         MVI   GTMERR,INVERR                                                    
         CLI   0(R4),C'9'                                                       
         BNH   NO2                                                              
*                                                                               
EDTUSR50 LA    R4,1(R4)                CHECK NEXT CHAR INPUT                    
         BCT   R1,EDTUSR40                                                      
         B     EDTUSR80                                                         
*                                                                               
EDTUSR60 CLI   UTYPE,C'N'              IS TYPE SUPPOSED TO BE NUMERIC?          
         BNE   EDTUSR70                NO, THEN ITS S/B DATE                    
         GOTO1 =A(CHKNTYP),DMCB,(RC),RR=RELO  YES SO IS THE INPUT NUM           
         BE    EDTUSR80                YES, INPUT IS VALID                      
         MVI   GTMERR,INVERR                                                    
         B     NO2                                                              
*                                                                               
EDTUSR70 MVI   GTMERR,INVDATE                                                   
         CLI   UTYPE,C'D'               IS UTYPE SUPPOSED TO BE DATE?           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATVAL,DMCB,(0,8(R3)),TDATE                                      
         OC    DMCB(4),DMCB              ANY ERRORS?                            
         BZ    NO2                       YES                                    
         L     R1,0(R1)                   L'INPUT FIELD                         
         ZIC   R4,5(R3)                                                         
         SR    R1,R4                                                            
         BNZ   NO2                                                              
*                                                                               
EDTUSR80 ZIC   R1,5(R3)                  R1= L'INPUT                            
         BCTR  R1,0                                                             
         EXMVC R1,USERDATA,8(R3)          MOVE INPUT INTO USERDATA              
*                                                                               
YES2     SR    RC,RC                                                            
NO2      LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM70D         MAINTENACE SCREEN                             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM7BD          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAPY          AUTOPAY RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE SPGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
       ++INCLUDE SPGENCLT          CLIENT RECORD                                
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE DDFLDIND          EQUATES                                      
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                   RELOCATION FACTOR                            
*                                                                               
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
KEYPRD   DS    CL7                                                              
SVP1USER DS    CL20                                                             
SVP2USER DS    CL20                                                             
SVCLOP1  DS    CL1                                                              
SVCLOP2  DS    CL1                                                              
SVP1TYPE DS    CL1                                                              
SVP2TYPE DS    CL1                                                              
SVP1LEN  DS    XL1                                                              
SVP2LEN  DS    XL1                                                              
SVP1FLG1 DS    XL1                                                              
SVP1FLG2 DS    XL1                                                              
SVP2FLG1 DS    XL1                                                              
SVP2FLG2 DS    XL2                                                              
AUSR     DS    A                                                                
OPTNFLAG DS    XL1                                                              
TALOPTN  EQU   X'80'                                                            
FLAG1    DS    XL1                                                              
FLAG2    DS    XL1                                                              
LEN      DS    XL1                                                              
UTYPE    DS    CL1                                                              
TDATE    DS    CL6                                                              
USERDATA DS    CL32                                                             
PSTOUT   DS    CL64                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSMED    DS    CL1                                                              
         DS    CL3                                                              
LSCLT    DS    CL3                                                              
         DS    CL1                                                              
LSPRD    DS    CL3                                                              
         DS    CL1                                                              
LSPRD2   DS    CL3                                                              
         DS    CL1                                                              
LSEST    DS    CL3                                                              
         DS    CL1                                                              
LSSTA    DS    CL8                                                              
         DS    CL1                                                              
LSMOY    DS    CL6                                                              
         DS    CL1                                                              
LSSREP   DS    CL4                                                              
         DS    CL2                                                              
LSPAYDT  DS    CL8                                                              
         DS    CL2                                                              
LSINV    DS    CL11                                                             
         DS    CL1                                                              
LSDATE   DS    CL8                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048MZEIPROD  11/06/98'                                      
         END                                                                    
