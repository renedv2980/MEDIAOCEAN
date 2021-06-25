*          DATA SET SPLFM12    AT LEVEL 063 AS OF 01/14/99                      
*PHASE T21912A                                                                  
*INCLUDE BINSRCH                                                                
         TITLE 'SPLFM12 - PRDHDR  T21912'                                       
***********************************************************************         
*                                                                     *         
*               M O D I F I C A T I O N S   L O G                     *         
*                                                                     *         
*-DATE---------BY------------------CHANGE-----------------------------*         
* 11/20/98    MHER   DON'T ADD PRODUCTS AAT AND POT !                 *         
*                                                                     *         
* 04/22/98     NRK   FOR TRADE CLIENT: 1) FORCE PRODUCT CODE TO END   *         
*                    IN C AND ADD SECOND PRODUCT THAT ENDS IN #       *         
*                    TRADE PRODUCT GETS SAME NUMBER WITH X'80' ON     *         
*                                                                     *         
***********************************************************************         
T21912   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21912,RR=R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T219FFD,RA                                                       
         LA    R8,REC                                                           
         ST    R8,AREC                                                          
         USING PRDHDRD,R8                                                       
         LR    R7,RC                    USE DEMAREA+2000 IN GENOLD FOR          
         AH    R7,=Y(DEMAREA+2000-GENOLD)   WORKING STORAGE.                    
         USING DEMAREA+2000,R7                                                  
         ST    R9,RELO                                                          
         CLI   SVFMTSW,0                TEST FORMAT OR EDIT                     
         BNE   EDT                                                              
*                                                                               
         EJECT                                                                  
FMT      DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC                                                           
         FOUT  PRDNAMEH,PNAME,20                                                
         CLI   PACCT,X'FF'                                                      
         BNE   FMT1                                                             
         UNPK  PRDACCT(5),PACCT+1(3)                                            
         FOUT  PRDACCTH                                                         
         B     FMT1D                                                            
*                                                                               
FMT1     FOUT  PRDACCTH,PACCT,4                                                 
         MVC   FULL(2),SPACES                                                   
         CLI   PCLASS,0            FIRST CHK FOR NO PCLASS                      
         BE    FMT1C                                                            
         MVC   FULL(1),PCLASS                                                   
         MVI   FULL+1,C' '                                                      
         CLI   PCLASS,X'99'        TEST 2 CLASSES                               
         BH    FMT1C               NO                                           
         PACK  FULL(1),PCLASS      FIRST CLASS                                  
         NI    FULL,X'0F'                                                       
         OI    FULL,X'C0'          MAKE A - I                                   
         MVC   FULL+1(1),PCLASS    2ND CLASS                                    
         NI    FULL+1,X'0F'                                                     
         OI    FULL+1,X'C0'        MAKE A - I                                   
*                                                                               
FMT1C    DS    0H                                                               
         FOUT  PRDCLASH,FULL,2                                                  
*                                                                               
FMT1D    FOUT  PRDBNAMH,PADDR1,30                                               
         FOUT  PRDADD2H,PADDR2,30                                               
         FOUT  PRDADD3H,PADDR3,30                                               
         FOUT  PRDADD4H,PADDR4,30                                               
         OC    PAGYFEE,PAGYFEE                                                  
         BNZ   FMT1F                                                            
         FOUT  PRDOAFH,SPACES,5                                                 
         B     FMT1X                                                            
*                                                                               
FMT1F    EDIT  (P2,PAGYFEE),(4,PRDOAF),2                                        
         FOUT  PRDOAFH                                                          
*                                                                               
FMT1X    OC    PBILLBAS(5),PBILLBAS                                             
         BNZ   FMT2                     NO BILLING FORMULA                      
         FOUT  PRDBBASH,SPACES,5                                                
         FOUT  PRDCPCTH,SPACES,8                                                
         FOUT  PRDCBASH,SPACES,5                                                
         B     FMT3                                                             
*                                                                               
FMT2     MVC   PRDBBAS,=CL5'CNET'                                               
         TM    PBILLBAS,X'50'                                                   
         BO    FMT2A                                                            
         MVC   PRDBBAS,=CL5'NET'                                                
         TM    PBILLBAS,X'10'                                                   
         BO    FMT2A                                                            
         MVC   PRDBBAS,=C'CGROS'                                                
         TM    PBILLBAS,X'40'                                                   
         BO    FMT2A                                                            
         MVC   PRDBBAS,=C'GROSS'                                                
*                                                                               
FMT2A    FOUT  PRDBBASH                                                         
         L     R5,PBILLCOM                                                      
         LTR   R5,R5                                                            
         BNZ   FMT2B                                                            
         FOUT  PRDCPCTH,SPACES,8                                                
         FOUT  PRDCBASH,SPACES,5                                                
         B     FMT3                                                             
*                                                                               
FMT2B    DS    0H                                                               
         LPR   RF,R5                                                            
         C     RF,=F'1000000'      +/-100.0000 WONT FIT                         
         BNE   FMT2C                                                            
         MVC   PRDCPCT+1(3),=C'100'                                             
         B     FMT2C2                                                           
*                                                                               
FMT2C    EDIT  (R5),(8,PRDCPCT),4,FLOAT=+,ALIGN=LEFT                            
*                                                                               
FMT2C2   LTR   R5,R5                                                            
         BNM   *+8                                                              
         MVI   PRDCPCT,C'-'        SET TO MINUS                                 
         FOUT  PRDCPCTH                                                         
         MVC   PRDCBAS,=C'GROSS'                                                
         TM    PBILLBAS,X'01'                                                   
         BZ    *+10                                                             
         MVC   PRDCBAS,=CL5'NET'                                                
         FOUT  PRDCBASH                                                         
*                                                                               
FMT3     OC    PBILLDT,PBILLDT                                                  
         BNZ   FMT3A                                                            
         FOUT  PRDEDATH,SPACES,8                                                
         B     FMT3G                                                            
*                                                                               
FMT3A    GOTO1 VDATCON,DMCB,(3,PBILLDT),(6,PRDEDAT)                             
         FOUT  PRDEDATH                                                         
*                                                                               
FMT3G    FOUT PRDGSTH,PGSTCODE,1  GOODS AND SERVICE TAX                         
         OI   PRDGSTH+6,OI1T                                                    
*                                                                               
         GOTO1 =A(DISPPST),DMCB,(RC),RR=RELO                                    
*                                                                               
         XC    PRDUSR1,PRDUSR1                                                  
         OC    SVP1USER,SVP1USER                                                
         BZ    *+10                                                             
         MVC   PRDUSR1,PUSER1                                                   
*                                                                               
         XC    PRDUSR2,PRDUSR2                                                  
         OC    SVP2USER,SVP2USER                                                
         BZ    *+10                                                             
         MVC   PRDUSR2,PUSER2                                                   
*                                                                               
         OI    PRDUSR1H+6,X'80'                                                 
         OI    PRDUSR2H+6,X'80'                                                 
**********************************************************************          
*                                                                               
         LA    R5,PRDOPTN            OPTIONS FIELD                              
         MVC   PRDOPTN,SPACES                                                   
*                                                                               
         TM    SVCLOP1,COP1GMI       NTP NEVER PRESENT FOR CLIENTS W/O          
         BNO   FMT4                  BMI SETUP OR ANY AAA OR POL PRDS           
         CLC   SVEBCPRD,=C'AAA'                                                 
         BE    FMT4                                                             
         CLC   SVEBCPRD,=C'POL'                                                 
         BE    FMT4                                                             
*                                                                               
         MVC   PRDOPTN(4),=C'NTP='   COPY NTP INTO OPTIONS FIELD                
         ZIC   R1,PTAL                                                          
         CVD   R1,DUB                                                           
         UNPK  PRDOPTN+4(1),DUB                                                 
         OI    PRDOPTN+4,X'F0'                                                  
         MVC   PRDOPTN+5(1),=C','    BUMP UP OPTIONS FIELD FOR                  
         LA    R5,6(R5)              POSSIBLE PRATE                             
*                                                                               
FMT4     OC    PRATE,PRATE                                                      
         BZ    FMT4A                                                            
         MVC   0(5,R5),=C'RATE='     COPY RATE INTO OPTIONS FIELD               
         LA    R5,5(R5)                                                         
         MVC   0(1,R5),PRATE                                                    
         B     FMT5                                                             
*                                                                               
FMT4A    MVC   PRDOPTN+5(1),=C' '    NO PRATE SPACE OUT COMMA                   
FMT5     OI    PRDOPTNH+6,X'80'      TRANSMIT OPTIONS FIELD                     
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
EDT      CLI   SVACT,C'A'                                                       
         BE    EDT0                                                             
         MVC   KEY,SVKEY                                                        
         GOTO1 GETREC        REREAD REC ON CHANGE                               
*                                                                               
EDT0     LA    R2,PRDNAMEH                                                      
         GOTO1 ANY                                                              
         CLC   8(6,R2),=C'DELETE'                                               
         BE    ERRINV                                                           
         CLC   =C'XPGR',8(R2)                                                   
         BNE   EDT0A                                                            
         XC    PGRP1(9),PGRP1   PGR1/2/3                                        
         XC    PGRP4(6),PGRP4                                                   
         B     WRTPRD                                                           
*                                                                               
EDT0A    MVC   PNAME,PRDNAME                                                    
*                                                                               
         LA    R2,PRDACCTH                                                      
         GOTO1 ANY                                                              
         LA    R1,AGYTAB                                                        
*                                                                               
EDT0B    CLI   0(R1),X'FF'                                                      
         BE    EDT0C                                                            
         CLC   0(2,R1),AGYALPHA                                                 
         BE    EDT1A                                                            
         LA    R1,2(R1)                                                         
         B     EDT0B                                                            
*                                                                               
EDT0C    CLI   5(R2),4                                                          
         BNE   ERRINV                                                           
         CLC   SVEBCCLT,=C'BM '     BRISTOL MYERS                               
         BNE   EDT1                                                             
         CLC   =C'BO',AGYALPHA                                                  
         BNE   EDT1                                                             
         TM    4(R2),X'08'         MUST BE 4 NUMERICS                           
         BZ    ERRINV                                                           
*                                                                               
EDT1     MVC   PACCT(4),8(R2)                                                   
         B     EDT1C                                                            
*                                                                               
EDT1A    CLI   5(R2),5         FM,GY,DR,GN,CE MUST BE 5 NUMERICS                
         BNE   ERRINV                                                           
         TM    4(R2),X'08'                                                      
         BZ    ERRINV                                                           
         PACK  PACCT(4),8(5,R2)                                                 
         MVI   PACCT,X'FF'                                                      
*                                                                               
EDT1C    DS    0H                  PRODUCT CLASS                                
         LA    R2,PRDCLASH         SINGLE OR DOUBLE                             
         MVI   BYTE,0                                                           
         CLI   5(R2),0                                                          
         BE    EDT1H               NO CLASS                                     
         CLC   8(2,R2),SPACES      CHK FOR SPACES ANYWAY                        
         BE    EDT1H               TREAT AS NO INPUT                            
         MVC   FULL(2),8(R2)                                                    
         MVC   BYTE,FULL                                                        
         CLI   FULL+1,C' '         IS 2ND CLASS GIVEN                           
         BNH   EDT1H               NO                                           
*                                                                               
         CLI   FULL,C'A'           BOTH MUST BE A-I                             
         BL    ERRINV                                                           
         CLI   FULL,C'I'                                                        
         BH    ERRINV                                                           
         CLI   FULL+1,C'A'                                                      
         BL    ERRINV                                                           
         CLI   FULL+1,C'I'                                                      
         BH    ERRINV                                                           
*                                                                               
         CLC   FULL(1),FULL+1      AND NOT THE SAME                             
         BE    ERRINV                                                           
*                                  HOLD AS 2 NIBBLES 1-9                        
         NI    FULL,X'0F'                                                       
         NI    FULL+1,X'0F'                                                     
         PACK  BYTE,FULL(1)        1ST CLASS                                    
         OC    BYTE,FULL+1         2ND CLASS                                    
*                                                                               
EDT1H    DS    0H                                                               
         MVC   PCLASS,BYTE                                                      
*                                                                               
EDT2     LA    R2,PRDBNAMH                                                      
         GOTO1 ANY                                                              
         XC    PADDR1(120),PADDR1                                               
         MVC   PADDR1,PRDBNAM                                                   
         OC    PADDR1,SPACES                                                    
         LA    R2,PRDADD2H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   PADDR2,8(R2)                                                     
         OC    PADDR2,SPACES                                                    
         LA    R2,PRDADD3H                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         MVC   PADDR3,8(R2)                                                     
         OC    PADDR3,SPACES                                                    
         LA    R2,PRDADD4H                                                      
         CLI   5(R2),0                                                          
         BE    EDT3                                                             
         MVC   PADDR4,8(R2)                                                     
         OC    PADDR4,SPACES                                                    
*                                                                               
EDT3     LA    R2,PRDOAFH                                                       
         CLI   5(R2),0                                                          
         BNE   EDT3A                                                            
         XC    PAGYFEE,PAGYFEE                                                  
         B     EDT4                                                             
*                                                                               
EDT3A    SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,PRDOAF),(R0)                                    
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'999'        CAN'T EXCEED 9.99                             
         BH    LFMERR                                                           
         ZAP   PAGYFEE,DUB                                                      
EDT4     DS    0H                                                               
*                                                                               
EDT5     LA    R2,PRDBBASH                                                      
         XC    PBILLBAS(5),PBILLBAS                                             
         CLI   5(R2),0                                                          
         BE    EDT5B                                                            
*                                                                               
EDT5A2   DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+16(4),=C'sB1X'   's' MUST BE LOWER CASE - 3 CHAR            
         MVC   WORK+20(2),AGYALPHA   PROFILE NAME                               
         MVC   WORK+22(1),SVEBCMED                                              
         MVC   WORK+23(3),SVEBCCLT                                              
         GOTO1 VGETPROF,DMCB,WORK+16,WORK,VDATAMGR                              
         CLI   WORK+11,C' '        DON'T ALLOW BILL FORMULA IF OPT 12           
         BNH   EDT5A3              IS SET                                       
         CLI   WORK+11,C'N'                                                     
         BNE   ERRINV                                                           
*                                                                               
EDT5A3   GOTO1 ANY                      R2 $S AT ESTBBASH                       
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         CLI   8(R2),C'C'          CHK FOR COMMISSION ONLY                      
         BNE   EDT5A4                                                           
         OI    PBILLBAS,X'40'                                                   
         BCTR  R4,0                                                             
         CLI   5(R2),1                                                          
         BE    ERRINV              NO 'C' ALONE                                 
         LA    R5,1(R5)            BUMP PAST 'C'                                
*                                                                               
EDT5A4   EX    R4,GROSCOM                                                       
         BE    EDT5B                                                            
         EX    R4,NETCOM                                                        
         BNE   ERRINV                                                           
         OI    PBILLBAS,X'10'                                                   
*                                                                               
EDT5B    LA    R2,PRDCPCTH                                                      
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    EDT5B1                                                           
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         BCTR  R0,R0                                                            
         GOTO1 VCASHVAL,DMCB,(4,PRDCPCT+1),(R0)                                 
         CLI   DMCB,X'FF'                                                       
         BE    ERRINV                                                           
         L     R0,DMCB+4                                                        
         C     R0,=F'1000000'      100.0000 MAX                                 
         BH    ERRINV                                                           
         C     R0,=F'0'                                                         
         BNH   ERRINV                                                           
         CLI   PRDCPCT,C'+'                                                     
         BE    EDT5BX                                                           
         CLI   PRDCPCT,C'-'                                                     
         BNE   ERRINV              ERROR BNE                                    
         LCR   R0,R0                    MAKE NEGATIVE                           
*                                                                               
EDT5BX   ST    R0,FULL                                                          
         MVC   PBILLCOM,FULL                                                    
         B     EDT5C                                                            
*                                                                               
EDT5B1   MVI   ERRCD,MSSNGERR                                                   
         CLI   PRDCBASH+5,0             REQUIRED IF COM BASIS PRESENT           
         BNE   LFMERR                                                           
*                                                                               
EDT5C    LA    R2,PRDCBASH                                                      
         CLI   5(R2),0                  NOT REQUIRED                            
         BE    EDT5C1                                                           
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         LA    R5,8(R2)                                                         
         EX    R4,GROSCOM                                                       
         BE    EDT5CX                                                           
         EX    R4,NETCOM                                                        
         BNE   ERRINV                                                           
         OI    PBILLBAS,X'01'                                                   
*                                                                               
EDT5CX   B     EDT5X                                                            
*                                                                               
EDT5C1   MVI   ERRCD,MSSNGERR                                                   
         CLI   PRDCPCTH+5,0                                                     
         BNE   LFMERR                                                           
*                                                                               
EDT5X    OC    PBILLBAS(5),PBILLBAS           WILL BE ZEROS IF                  
         BNZ   EDT5XC                         GROSS ALONE                       
         CLI   PRDBBASH+5,0                                                     
         BE    EDT6            NO FORMULA                                       
*                              MUST HAVE BEEN GROSS ALONE                       
*                              PUT X'80' SO BILLING WILL THINK IT'S             
*                              A FORMULA                                        
         B     EDT5XE                                                           
EDT5XC   OC    PBILLCOM,PBILLCOM                                                
         BNZ   EDT6                                                             
         CLI   PBILLBAS,X'40'      WAS GROSS ALONE + COMMISSION ONLY            
         BNE   EDT6                                                             
*                                                                               
EDT5XE   OI    PBILLBAS,X'80'                                                   
*                                                                               
EDT6     LA    R2,PRDEDATH            EFFECTIVE DATE OF BILL FORMULA            
         OC    PBILLBAS(5),PBILLBAS           SEE IF FORMULA INPUT              
         BNZ   EDT6A                                                            
         XC    PBILLDT,PBILLDT                                                  
         CLI   5(R2),0                                                          
         BE    EDT8                                                             
         B     ERRINV             NO FORMULA SO EFF DATE INVALID                
*                                                                               
EDT6A    GOTO1 ANY          REQUIRED IF FORMULA INPUT                           
         GOTO1 VDATVAL,DMCB,(2,PRDEDAT),WORK                                    
         MVI   ERRCD,DATERR                                                     
         OC    DMCB(4),DMCB                                                     
         BZ    LFMERR                                                           
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+10)                                
         GOTO1 VDATCON,DMCB,(5,0),(3,WORK+16)  GET CURRENT DATE                 
         CLI   WORK+16,99                      DID WE GET TO 2000 YET           
         BH    EDT6B                           YES - ANY DATE IS VALID          
         CLI   WORK+10,99                      TEST USER INPUT > 2000           
         BNH   EDT6B                                                            
         IC    R0,WORK+10                                                       
         SH    R0,=H'100'                                                       
         STC   R0,WORK+10                                                       
EDT6B    MVC   PBILLDT,WORK+10           YM                                     
*                                                                               
EDT8     DS    0H                                                               
         LA    R2,PRDGSTH                                                       
         MVI   PGSTCODE,0                                                       
         CLI   5(R2),0            ANY GST INPUT?                                
         BE    EDT11A             NO                                            
         LA    RE,GSTTAB          GST TABLE OF VALID ENTRIES                    
*                                                                               
EDT10    CLC   0(1,RE),PRDGST                                                   
         BE    EDT11                                                            
         CLI   0(RE),X'FF'        END OF TABLE?                                 
         BE    ERRINV                                                           
         LA    RE,1(RE)                                                         
         B     EDT10                                                            
*                                                                               
EDT11    MVC   PGSTCODE,PRDGST    MOVE INTO RECORD                              
*                                                                               
EDT11A   GOTO1 =A(VALPST),DMCB,(RC),RR=RELO                                     
         BNE   ERRINV                                                           
*                                                                               
EDT12    XC    USERDATA,USERDATA                                                
         OC    SVP1USER,SVP1USER   ANY "PRODUCT 1" INFO?                        
         BZ    EDT12A               NO, DO "PRODUCT 2".                         
         LA    R2,PRDUSR1H                                                      
         ST    R2,AUSR             AUSR=A(INPUT FIELD).                         
         MVC   UTYPE,SVP1TYPE      TYPE.                                        
         MVC   LEN,SVP1LEN         LENGTH.                                      
         MVC   FLAG1,SVP1FLG1      1ST FLAG                                     
         MVC   FLAG2,SVP1FLG2      2ND FLAG                                     
         GOTO1 =A(EDTUSR),DMCB,(RC),RR=RELO                                     
         BNE   LFMERR                                                           
*                                                                               
EDT12A   MVC   PUSER1,USERDATA                                                  
         MVC   PRDUSR1,USERDATA    CLEAR OR RE-TRANSMIT FIELD.                  
         OI    PRDUSR1H+6,X'80'                                                 
*                                                                               
         XC    USERDATA,USERDATA                                                
         OC    SVP2USER,SVP2USER   ANY "PRODUCT 2" INFO?                        
         BZ    EDT12B               NO, MOVE ON.                                
         LA    R2,PRDUSR2H                                                      
         ST    R2,AUSR             A(INPUT FIELD).                              
         MVC   UTYPE,SVP2TYPE      TYPE.                                        
         MVC   LEN,SVP2LEN         LENGTH.                                      
         MVC   FLAG1,SVP2FLG1      1ST FLAG                                     
         MVC   FLAG2,SVP2FLG2      2ND FLAG                                     
         GOTO1 =A(EDTUSR),DMCB,(RC),RR=RELO                                     
         BNE   LFMERR                                                           
*                                                                               
EDT12B   MVC   PUSER2,USERDATA                                                  
         MVC   PRDUSR2,USERDATA    CLEAR OR RE-TRANSMIT FIELD.                  
         OI    PRDUSR2H+6,X'80'                                                 
*                                                                               
         GOTO1 =A(VALOPTS),RR=RELO                                              
*                                                                               
OUTPUT   MVC   KEY,SVKEY                                                        
         ST    R8,AREC                                                          
         MVC   PPROF(30),=30C'0'          SET DEFAULT PROFILE                   
         CLI   SVACT,C'A'                                                       
         BNE   WRTPRD                                                           
*             NOW ADD PRODUCT TO CLTHDR (IN REC2)                               
         LA    R9,REC2                                                          
         ST    R9,AREC            ADDRESS OF REC2                               
         OC    SVADVDA,SVADVDA     SEE IF DOING ADV CLT                         
         BZ    OUT0                NO                                           
*                                  YES - USE ADV CLTHDR INSTEAD                 
         NI    KEY+1,X'0F'                                                      
         OC    KEY+1(1),SVADVAGY                                                
         CLC   KEY(2),SVKEY        SEE IF DOING ADV PRD                         
         BNE   OUTP1               NO                                           
         MVC   KEY,SVKEY           RESET KEY                                    
         B     OUT0                                                             
*                                                                               
OUTP1    XC    KEY+4(3),KEY+4      CLEAR PRD CODE                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    OUT0A                                                            
         DC    H'0'                CLTHDR MUST EXIST                            
*                                                                               
OUT0     MVC   KEY+14(4),SVCLTDA                                                
OUT0A    GOTO1 GETREC                                                           
         USING CLTHDRD,R9                                                       
         LA    R1,CLIST                                                         
         CLC   SVEBCPRD,=C'POL'                                                 
         BNE   OUT2                                                             
         MVI   PCODE+1,X'FF'                                                    
OUT1     CLI   0(R1),0       ENDOF LIST                                         
         BE    OUT1A                                                            
         CLI   3(R1),X'FF'                                                      
         BE    INSERTX        POL ALREADY THERE                                 
         LA    R1,4(R1)                                                         
         B     OUT1                                                             
******         WILL CHANGE WITH NEW CLT RECS                                    
OUT1A    LA    R3,CLIST+876                                                     
         CR    R1,R3                                                            
         BNL   INS2          CLIST FULL                                         
         MVC   0(3,R1),=C'POL'                                                  
         MVI   3(R1),X'FF'                                                      
         B     INSERTX                                                          
*                                                                               
******       CHANGE 110 TO 220 FOR NEW CLTHDRS                                  
OUT2     XC    ELEM(220),ELEM                                                   
         LA    R0,220                                                           
         SR    R6,R6         USEDUSED TO COUNT PRDS                             
OUT2A    CLI   0(R1),0                                                          
         BE    OUT3                                                             
         CLI   3(R1),X'FF'                                                      
         BNE   OUT2B                                                            
         LA    R6,1(R6)                                                         
         MVC   0(3,R1),=3X'FF'             FOR PROPER LAST ENTRY                
*                                   OF BINSRCH TABLE                            
         B     OUT2C                                                            
*                                                                               
OUT2B    SR    R5,R5                                                            
         IC    R5,3(R1)                                                         
         LA    R5,ELEM-1(R5)                                                    
         MVI   0(R5),X'FF'   SET USED FLAG                                      
         LA    R6,1(R6)         BUMP PRD COUNTER                                
OUT2C    LA    R1,4(R1)                                                         
         B     OUT2A                                                            
*                                                                               
OUT3     LA    R1,ELEM      FIND LOWEST UNUSED CODE                             
         LA    RE,TALTAB                                                        
OUT3A    CLI   0(RE),X'FF'         CHECK TABLE FOR MINIMUM PRD VALUE            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PTAL,0(RE)                                                       
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     OUT3A                                                            
         ZIC   RF,1(RE)                                                         
         BCTR  RF,0                                                             
         AR    R1,RF                                                            
*                                                                               
OUT3B    CLI   0(R1),0                                                          
         BE    OUT4                                                             
         LA    R1,1(R1)                                                         
         B     OUT3B                                                            
*                                                                               
OUT4     LA    R0,ELEM-1                                                        
         SR    R1,R0                                                            
         STH   R1,PCODE                                                         
         TM    SVCLOP1,COP1GMI                                                  
         BNO   INSERT                                                           
*                                                                               
         ZIC   RF,2(RE)            CHECK MAX PRODUCT CODES                      
         ZIC   RE,PCODE                                                         
         CR    RE,RF                                                            
         BH    TALERR                                                           
*                                                                               
         EJECT                                                                  
INSERT   MVC   WORK(3),SVEBCPRD                                                 
         MVC   WORK+3(1),PCODE+1      R6 HAS NUMBER OF PRDS                     
         OC    CMCLTCOD,CMCLTCOD      ANY TRAFFIC MASTER CLT?                   
         BZ    INS1X                                                            
         CLI   CMCLTPRD,0             YES - BUT IF TRF PRD                      
         BNE   INS1X                        NO NEED TO CHECK                    
         MVC   WORK2(13),0(R9)        SAVE CURRENT CLIENT RECORD                
         XC    KEY,KEY                & READ TRAFFIC MASTER CLT RECORD          
         MVC   KEY+1(1),SVKEY+1                                                 
         MVC   KEY+2(2),CMCLTCOD                                                
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOFNDERR                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LFMERR                                                           
         LH    R2,=Y(DEMAREA-GENOLD)                                            
         LA    R2,GENOLD(R2)                                                    
         ST    R2,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R2,CLIST-CLTHDRD(R2)                                             
INS1     OC    0(4,R2),0(R2)                                                    
         BZ    INS1T              PRD MUST BE IN CLT RECORD                     
         CLC   SVEBCPRD,0(R2)                                                   
         BE    *+12                                                             
         LA    R2,4(R2)                                                         
         B     INS1                                                             
         CLC   PCODE+1(1),3(R2)                                                 
         BE    INS1V                                                            
*                                 SEQUENCE NUMBERS MUST MATCH                   
INS1T    LA    R2,LFMKEYH                                                       
         MVI   ERRAREA,X'FF'                                                    
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(37),=CL37'PRD ERROR-CHECK TRAFFIC MASTER PRDS'            
         FOUT  LFMMSGH                                                          
         XIT1  REGS=(R2)                                                        
*                                                                               
INS1V    MVC   KEY(13),WORK2        RESET TO CURRENT CLT KEY                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),WORK2                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,REC2            RESET AREC                                    
         ST    R0,AREC                                                          
*                                                                               
INS1X    EQU   *                                                    NRK         
*                                                                   NRK         
         LA    R2,LFMKEYH          SET FOR CURSOR POSITION J.I.C.   NRK         
*                                                                   NRK         
         GOTO1 =A(TESTTRAD),RR=RELO   TEST FOR TRADE                NRK         
         BNZ   LFMERR              ERROR FOUND                      NRK         
*                                                                   NRK         
         L     R6,FULL             SET NEW COUNT OF PRODUCTS        NRK         
*                                                                               
         GOTO1 =V(BINSRCH),DMCB,(X'01',WORK),CLIST,(R6),4,(0,3),218,   X        
               RR=RELO                                                          
         OC    DMCB(4),DMCB                                                     
         BNZ   INS3                                                             
INS2     MVI   ERRCD,CLTFULL           TOO MANY PRODUCTS                        
         B     LFMERR                                                           
*                                                                               
INS3     CLI   DMCB,1                 PRD NOT IN LIST                           
         BE    INS4                                                             
         L     R1,DMCB          HAS ADDR OF PRD                                 
         MVC   PCODE+1(1),3(R1)                                                 
*                                                                               
INS4     LA    R1,CLIST                                                         
INS4A    CLI   0(R1),0                                                          
         BE    INSERTX                                                          
         CLC   0(3,R1),=3X'FF'                                                  
         BE    INS4B                                                            
         LA    R1,4(R1)                                                         
         B     INS4A                                                            
*                                                                               
INS4B    MVC   0(3,R1),=C'POL'          RESET TO POL                            
         B     INSERTX                                                          
*                                                                               
INSERTX  LA    R3,4                                                             
         LA    R4,SVCLTLST          SAVE CLIST IN SVCLTLST                      
         LA    R5,CLIST                                                         
         MVC   0(220,R4),0(R5)                                                  
         LA    R4,220(R4)                                                       
         LA    R5,220(R5)                                                       
         BCT   R3,*-14                                                          
*                       WRITE BACK CLTHDR(S)                                    
         CLC   KEY(2),SVKEY    WILL NOT BE EQUAL IF DOING ADVCLTHDR             
         BNE   PUTCLT          KEY+14 WILL STILL HAVE DISK ADDR                 
*                                                                               
         MVC   KEY+14(4),SVCLTDA                                                
PUTCLT   GOTO1 PUTREC                                                           
*                                                                               
*        IF CANADIAN TV - UPDATE NWK AND COMBINED CLTHDRS                       
*                                                                               
         CLI   SVAPROF+7,C'C'                                                   
         BNE   PUTCLT8                                                          
         CLI   SVEBCMED,C'T'                                                    
         BNE   PUTCLT8                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY                                                     
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PUTCLT2                                                          
         MVC   REC2(13),KEYSAVE                                                 
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 ADDREC                                                           
         B     PUTCLT4                                                          
*                                                                               
PUTCLT2  LA    R0,REC+500                                                       
         ST    R0,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         MVC   REC2(13),REC+500      SWITCH KEYS                                
         GOTO1 PUTREC                                                           
*                                                                               
PUTCLT4  XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY                                                     
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PUTCLT6                                                          
         MVC   REC2(13),KEYSAVE                                                 
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 ADDREC                                                           
         B     PUTCLT7                                                          
*                                                                               
PUTCLT6  LA    R0,REC+500                                                       
         ST    R0,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R0,REC2                                                          
         ST    R0,AREC                                                          
         MVC   REC2(13),REC+500      SWITCH KEYS                                
         GOTO1 PUTREC                                                           
*                                                                               
PUTCLT7  MVC   KEY,SVKEY                                                        
*                                                                               
PUTCLT8  DS    0H                                                               
         OC    SVADVDA,SVADVDA                                                  
         BZ    ADDPRD          NOT AN ADV CLT                                   
         MVC   KEY+14(4),SVADVDA                                                
         GOTO1 GETREC        GET ADVHDR INTO REC2                               
         LA    R5,REC2                                                          
         USING ADVHDRD,R5                                                       
         MVC   SVADVLST,ADVLIST          SAVE LIST OF AGENCIES                  
         DROP  R5                                                               
*                                                                               
         LA    R6,SVADVLST-1                                                    
WRTCLT   LA    R6,1(R6)                                                         
         CLI   0(R6),0                                                          
         BE    ADDPRD        END OF LIST                                        
         XC    KEY,KEY                                                          
         MVC   KEY(4),SVKEY                                                     
         NI    KEY+1,X'0F'                                                      
         IC    R0,0(R6)                                                         
         SLL   R0,4                                                             
         STC   R0,DUB                                                           
         OC    KEY+1(1),DUB                                                     
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
******        CHANGE NEXT INS TO LA R0,4 FOR NEW CLT RECS                       
         LA    R0,4                                                             
         LA    R4,CLIST                                                         
         LA    R5,SVCLTLST                                                      
WRTC1    MVC   0(220,R4),0(R5)          MOVE NEW PDR LIST                       
         LA    R4,220(R4)                                                       
         LA    R5,220(R5)                                                       
         BCT   R0,WRTC1                                                         
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 FIL                                                              
         B     WRTCLT                                                           
*                                                                               
         EJECT                                                                  
ADDPRD   ST    R8,AREC           RESET AREC TO PRDHDR                           
         MVC   KEY,SVKEY                                                        
         MVC   PKEY,SVKEY                                                       
         MVC   PLEN,=H'336'          THIS S/B NEW LENGTH                        
*                                                                               
         GOTO1 =A(ADDPRD2),RR=RELO GO ADD THE CASH PRODUCT          NRK         
*                                                                   NRK         
         TM    SVAGYFL1,X'02'      TRADE AGENCY?                    NRK         
         BO    ADPR0100            YES - SO CONTINUE                NRK         
*                                                                   NRK         
         TM    SVCLOP2,COP2TRAD    ELSE - TRADE CLIENT?             NRK         
         BZ    EXXMOD              NO - SO CONTINUE                 NRK         
*                                                                   NRK         
ADPR0100 EQU   *                                                    NRK         
         CLC   SVEBCPRD,=C'POL'                                                 
         BE    EXXMOD                                                           
         CLC   SVEBCPRD,=C'AAA'                                                 
         BE    EXXMOD                                                           
*                                                                   NRK         
         MVI   KEY+6,C'#'          ELSE - SET KEY TO TRADE PRODUCT  NRK         
         MVI   SVEBCPRD+2,C'#'     AND IN SAVE FIELD (FOR GENREQ)   NRK         
         MVI   PKEYPRD+2,C'#'      AND IN THE REOCRD                NRK         
         OI    PCODE+1,X'80'       SET THE TRADE BIT IN PRODUCT #   NRK         
         GOTO1 =A(ADDPRD2),RR=RELO GO ADD THE TRADE PRODUCT         NRK         
         MVI   SVEBCPRD+2,C'C'     RESTORE THE SAVE FIELD           NRK         
         B     EXXMOD                                                           
*                                                                               
WRTPRD   MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 FIL                                                              
         GOTO1 CNCHASPT                                                         
****     B     REQREC                                                           
*                        GENERATE REQUEST RECORD                                
REQREC   GOTO1 =A(GENREQ),RR=RELO                                               
         B     EXXMOD                                                           
         EJECT                                                                  
GROSCOM  CLC   0(0,R5),=C'GROSS'        ALLOW G-GROSS                           
NETCOM   CLC   0(0,R5),=C'NET  '        OR N-NET                                
*                                                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
TALERR   MVC   NERRCD,=AL2(ERPRCTAL)  ERROR PROCESSING TAL VALUE                
         B     NEWERRS                                                          
*                                                                               
TALREQ   MVC   NERRCD,=AL2(TALREQRD)  TAL OPTION IS REQUIRED FOR CLIENT         
NEWERRS  MVI   ERRCD,NEWERR                                                     
         B     LFMERR                                                           
*                                                                               
ERRINV   MVI   ERRCD,INVERR                                                     
LFMERR   GOTO1 ERROR                                                            
*                                                                               
*======================= MISC DEFINE CONSTANTS =======================*         
AGYTAB   DC    C'GY'                                                            
         DC    C'DR'                                                            
         DC    C'GN'                                                            
         DC    C'CE'                                                            
         DC    C'FM'                                                            
         DC    C'RE'                                                            
         DC    X'FF'                                                            
*                                                                               
GSTTAB   DC    C'S'               STANDARD                                      
         DC    C'U'                                                             
         DC    C'X'                                                             
         DC    C'Z'               ZERO                                          
         DC    X'FF'              END OF TABLE                                  
*                                                                               
TALTAB   DC    X'0',X'01',X'3F'   TAL VALUE/ MIN PRD CODE/ MAX PRD CODE         
         DC    X'1',X'40',X'5F'                                                 
         DC    X'2',X'60',X'7F'                                                 
         DC    X'3',X'80',X'9F'                                                 
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        VALIDATE PST CODES                                                     
VALPST   NMOD1 0,12/VALPS                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R2,PRDPSTH                                                       
         CLI   5(R2),0                                                          
         BE    YES                                                              
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,PRDPSTH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         GOTO1 VPSTVAL,DMCB,(R4)                                                
         CLI   PSTERR,0                                                         
         BNE   NO                                                               
         MVC   PPST,PSTOUT         OUTPUT                                       
         GOTO1 =A(DISPPST),DMCB,(RC),RR=RELO                                    
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        DISPLAY PST CODES                                                      
*                                                                               
DISPPST  NMOD1 0,12/DIPST                                                       
         L     RC,0(R1)                                                         
         MVC   PRDPST,SPACES       OUTPUT                                       
         OI    PRDPSTH+6,X'80'                                                  
         OC    PPST,PPST           IS THERE ANYTHING TO DISPLAY                 
         BZ    DPX                                                              
*                                                                               
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,PPST                                                          
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,VCOMFACS    A(COMFACS)                                   
         GOTO1 VPSTVAL,DMCB,(R4)                                                
         MVC   PRDPST,PSTOUT       OUTPUT                                       
*                                                                               
DPX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*         ON ENTRY:                                                             
*             AUSR = A(INPUT FIELD),                                            
*             LEN  = ALLOWABLE LENGTH OF INPUT,                                 
*             UTYPE = TYPE ALLOWABLE FOR INPUT,                                 
*             FLAG1 = FIRST FLAG,                                               
*             FLAG2 = SECOND FLAG.                                              
*         ON EXIT:                                                              
*             USERDATA = INPUTTED DATA OR NULLS.                                
*                                                                               
*=============================== EDTUSR ==============================*         
EDTUSR   NMOD1 0,12/EDUSR                                                       
         L     RC,0(R1)                                                         
         L     R3,AUSR                                                          
         CLI   5(R3),0             CHECK FOR ANY INPUT                          
         BNE   EDTUSR10            THERE IS INPUT, PROCESS IT                   
*                                                                               
         MVI   ERRCD,MSSNGERR      NO INPUT, ASSUME IT'S MISSING                
         TM    FLAG1,CFLGREQQ      WAS INPUT REQUIRED?                          
         BZ    YES2                NO, SO IT'S OK                               
         B     NO2                 YES IT WAS, SHOW ERROR                       
*                                                                               
EDTUSR10 MVI   ERRCD,TOOLONG       ASSUME INPUT IS TOO LONG                     
         CLC   LEN,5(R3)           CHECK IF L(INPUT) IS VALID                   
         BL    NO2                 NOT VALID                                    
*                                                                               
         CLI   UTYPE,C' '          IS TYPE SUPPOSE TO BE "WILD"?                
         BNH   EDTUSR80                                                         
         CLI   UTYPE,C'C'          IS TYPE SUPPOSE TO BE CHARACTER?             
         BNE   EDTUSR60                                                         
         LA    R4,8(R3)            R4-->INPUT                                   
         ZIC   R1,5(R3)            R1=L(INPUT)                                  
*                                                                               
EDTUSR40 CLI   0(R4),C'0'          ALLOW ALL INPUT EXCEPT NUMBERS               
         BL    EDTUSR50                                                         
         MVI   ERRCD,INVERR                                                     
         CLI   0(R4),C'9'                                                       
         BNH   NO2                                                              
*                                                                               
EDTUSR50 LA    R4,1(R4)            CHECK NEXT CHAR IN INPUT                     
         BCT   R1,EDTUSR40                                                      
         B     EDTUSR80                                                         
*                                                                               
EDTUSR60 CLI   UTYPE,C'N'          IS TYPE SUPPOSE TO BE NUMERIC?               
         BNE   EDTUSR70             NOPE, THEN IT'S S/B DATE                    
         GOTO1 =A(CHKNTYP),DMCB,(RC),RR=RELO  YES, SO IS THE INPUT NUM          
         BE    EDTUSR80              YEP, INPUT IS VALID.                       
         MVI   ERRCD,INVERR                                                     
         B     NO2                                                              
*                                                                               
EDTUSR70 MVI   ERRCD,DATERR        ASSUME INVALID DATE FORMAT                   
         CLI   UTYPE,C'D'          IS UTYPE SUPPOSE TO BE DATE?                 
         BE    *+6                  YES, VALIDATE INPUT FOR DATE                
         DC    H'0'                 NO, SOMETHING IS AMISS.                     
         GOTO1 VDATVAL,DMCB,(0,8(R3)),TDATE                                     
         OC    DMCB(4),DMCB        ANY ERRORS?                                  
         BZ    NO2                  YEP                                         
         L     R1,0(R1)            L'INPUT FIELD                                
         ZIC   R4,5(R3)                                                         
         SR    R1,R4                                                            
         BNZ   NO2                                                              
*                                                                               
EDTUSR80 ZIC   R1,5(R3)            R1=L(INPUT).                                 
         BCTR  R1,0                                                             
         EXMVC R1,USERDATA,8(R3)   MOVE INPUT INTO USERDATA.                    
*                                                                               
YES2     SR    RC,RC                                                            
NO2      LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*-------------------- VALIDATE INPUT FOR NUMERICS --------------------*         
CHKNTYP  NMOD1 0,12/CHNTY                                                       
         L     RC,0(R1)                                                         
         LA    R4,8(R3)            R4-->INPUT                                   
         ZIC   R1,5(R3)            R1=L(INPUT)                                  
CHKN10   LA    R5,VALDNTBL         R5-->TABLE OF VALID DIGITS                   
CHKN20   CLC   0(1,R4),0(R5)       MATCH DIGIT BY DIGIT                         
         BE    CHKN30               MATCHED,                                    
         LA    R5,1(R5)             ELSE, TRY NEXT CHAR IN TABLE                
         CLC   0(1,R5),EOTBLQ      ARE WE AT END-OF-TABLE YET?                  
         BE    XCHKN                YEP, EXIT WITH CC<>0 (R1  STILL HAS         
*                                   SOME VALUE IN IT)                           
         B     CHKN20              NO, WE STILL HAVE HOPE                       
*                                                                               
CHKN30   LA    R4,1(R4)            MATCHED, CHECK NEXT CHAR IN INPUT            
         BCT   R1,CHKN10                                                        
*                                                                               
XCHKN    LTR   R1,R1                                                            
         XIT1                                                                   
         LTORG                                                                  
VALDNTBL DC    C' 0123456789-/'                                                 
EOTBLQ   DC    C'A'                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                                       
***********************************************************************         
VALOPTS  NTR1  BASE=*,LABEL=*                                                   
         MVI   PRATE,0                                                          
         LA    R2,PRDOPTNH           CALL SCANNER TO BREAK UP OPTIONS           
         MVI   OPTNFLAG,0                                                       
         GOTO1 VSCANNER,DMCB,(R2),REC2                                          
         ZICM  R0,DMCB+4             NUMBER OF OPTIONS SCANNED                  
         BZ    EDT14                                                            
*                                                                               
         LA    R5,REC2                                                          
EDT13    ZICM  R1,0(R5)              ANY OPTIONS LEFT?                          
         BZ    EDT14                                                            
*                                                                               
         MVI   ERRCD,INVERR          NTP OR RATE ONLY VALID OPTIONS             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'NTP'                                                 
         BE    EDT13A                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R5),=C'RATE'                                                
         BE    EDT13B                                                           
         B     VOERROR                                                          
*                                                                               
EDT13A   TM    OPTNFLAG,TALOPTN      NTP CAN ONLY BE SET ONCE                   
         BO    VOERROR                                                          
*                                                                               
         MVC   NERRCD,=AL2(GMINTSET) GMI NOT SETUP FOR THIS CLIENT              
         MVI   ERRCD,NEWERR                                                     
         TM    SVCLOP1,COP1GMI                                                  
         BNO   VOERROR                                                          
*                                                                               
         MVC   NERRCD,=AL2(TALNTVAL) NTP NOT VALID FOR AAA OR POL               
         MVI   ERRCD,NEWERR          PRODUCTS                                   
         CLC   SVEBCPRD,=C'AAA'                                                 
         BE    VOERROR                                                          
         CLC   SVEBCPRD,=C'POL'                                                 
         BE    VOERROR                                                          
*                                                                               
         MVI   ERRCD,INVERR                                                     
         TM    3(R5),X'80'           NTP MUST BE VALID NUMERIC...               
         BNO   VOERROR               BETWEEN 0 AND 2                            
         CLI   1(R5),1                                                          
         BNE   VOERROR                                                          
         CLI   11(R5),2                                                         
         BH    VOERROR                                                          
*                                                                               
         CLI   SVACT,C'A'             IF PRODUCT ALREADY HAD AN                 
         BE    *+14                   NTP, CANNOT CHANGE                        
         CLC   PTAL,11(R5)                                                      
         BNE   CNTCHNGE                                                         
*                                                                               
         MVC   PTAL,11(R5)                                                      
         OI    OPTNFLAG,TALOPTN                                                 
         LA    R5,32(R5)                                                        
         BCT   R0,EDT13                                                         
         B     EDT14                                                            
*                                                                               
EDT13B   MVI   ERRCD,INVERR        RATE CAN ONLY BE SET ONCE                    
         CLI   PRATE,0                                                          
         BNE   VOERROR                                                          
*                                                                               
         CLI   SVCLPROF+14,C'*'    CHECK IF CLIENT ALLOWS SPEC RATES            
         BE    VOERROR                                                          
*                                                                               
         CLI   1(R5),1             VALID RATES ARE * OR 0-9                     
         BNE   VOERROR                                                          
         CLI   22(R5),C'*'                                                      
         BE    EDT13C                                                           
         CLI   22(R5),C'0'                                                      
         BL    VOERROR                                                          
         CLI   22(R5),C'9'                                                      
         BH    VOERROR                                                          
*                                                                               
EDT13C   MVC   PRATE,22(R5)          RATE HAS BEEN VALIDATED...                 
         LA    R5,32(R5)             BUMP TO NEXT OPTION                        
         BCT   R0,EDT13                                                         
*                                                                               
EDT14    TM    OPTNFLAG,TALOPTN      CHECK IF NTP OPTION REQUIRED               
         BO    EDT20                                                            
         CLC   SVEBCPRD,=C'AAA'      NOT ALLOWED WITH PRD AAA & POL             
         BE    EDT20                                                            
         CLC   SVEBCPRD,=C'POL'                                                 
         BE    EDT20                                                            
         TM    SVCLOP1,COP1GMI                                                  
         BNO   EDT20                                                            
         MVC   NERRCD,=AL2(TALREQRD) NTP REQUIRED FOR THIS CLIENT               
         MVI   ERRCD,NEWERR                                                     
         B     VOERROR                                                          
*                                                                               
CNTCHNGE MVI   ERRCD,NOCHGERR                                                   
VOERROR  GOTO1 ERROR                                                            
*                                                                               
EDT20    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
         EJECT                                                                  
GENREQ   NTR1  BASE=*,LABEL=*                                                   
         XC    REC+1000(150),REC+1000                                           
         LA    R1,REC+1000                                                      
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,REC+1026                                                      
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'41'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),SVEBCMED                                                 
         MVC   5(3,R1),SVEBCCLT                                                 
         MVC   11(3,R1),SVEBCPRD                                                
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   61(R1),C'P'                                                      
         MVI   63(R1),C'A'                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+8                                                              
         MVI   63(R1),C'C'                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC+1000,REC+1000            
         XIT1                                                                   
         EJECT                                                                  
*                                                                   NRK         
* THIS ROUTINE CHECKS IF CLIENT IS TRADE AND IF SO ADDS THE 'TRADE' NRK         
* TYPE PRODUCT CODE AND NUMBER TO THE CLIENT RECORD TABLE.          NRK         
*                                                                   NRK         
TESTTRAD NTR1  BASE=*,LABEL=*                                       NRK         
         TM    SVAGYFL1,X'02'      TRADE AGENCY?                                
         BO    TTRD0100            YES - SO CONTINUE                            
*                                                                               
         TM    SVCLOP2,COP2TRAD    ELSE - TRADE CLIENT?                         
         BZ    TTRD0400            NO - SO CONTINUE                             
*                                                                               
TTRD0100 EQU   *                                                                
         CLC   SVEBCPRD,=C'POL'                                                 
         BE    TTRDEXIT                                                         
         CLC   SVEBCPRD,=C'AAA'                                                 
         BE    TTRDEXIT                                                         
*                                                                               
         CLI   SVEBCPRD+2,C'C'     IS THIRD CHAR 'C' (FOR CASH)?                
         BE    TTRD0200            YES - SO CONTINUE                            
*                                                                               
         MVI   ERRAREA,X'FF'       ELSE - SET ERROR FLAG                        
         XC    LFMMSG,LFMMSG       CLEAR THE MESSAGE FIELD                      
         FOUT  LFMMSGH,=C'ERROR-3RD CHAR IN PRD CODE MUST BE "C".',38           
         B     TTRDERR             AND EXIT                                     
*                                                                               
TTRD0200 EQU   *                                                                
*                                                                               
         MVI   ERRCD,CLTFULL       TOO MANY PRODUCTS ERROR CODE                 
         CLI   PCODE+1,104         MORE THAN 104 PRODUCTS?                      
         BH    TTRDERR             YES - SO ERROR                               
*                                                                               
         L     R4,WORK             ELSE - STORE OLD PRD CODE AND #              
         MVI   WORK+2,C'#'         SET THE TRADE PROD CODE                      
         OI    WORK+3,X'80'        TURN ON THE TRADE BIT IN PRD #               
         GOTO1 =V(BINSRCH),DMCB,(X'01',WORK),CLIST,(R6),4,(0,3),218,   X        
               RR=RELO                                                          
*                                                                               
         L     R6,DMCB+8           NEW COUNT OF PRODUCTS IN TABLE               
         CLI   DMCB,X'01'          WAS PRODUCT CODE ALREADY THERE?              
         BE    TTRD0300            NO - SO CONTINUE                             
*                                                                               
         MVI   ERRAREA,X'FF'       ELSE - SET ERROR FLAG                        
         XC    LFMMSG,LFMMSG       CLEAR THE MESSAGE FIELD                      
         FOUT  LFMMSGH,=C'ERROR-TRADE PRODUCT CODE ALREADY IN CLIENT LIX        
               ST',47                                                           
         B     TTRDERR             AND EXIT                                     
*                                                                               
TTRD0300 EQU   *                                                                
*                                                                               
         ST    R4,WORK             RESTORE CASH PRODUCT CODE/#                  
*                                                                               
TTRD0400 EQU   *                                                                
*                                                                               
         ST    R6,FULL             PASS BACK # PRODUCTS IN TABLE    NRK         
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
*                                                                               
TTRDEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
TTRDERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     TTRDEXIT            AND EXIT                                     
         LTORG                                                                  
         EJECT                                                      NRK         
*                                                                   NRK         
* THIS ROUTINE ADDS THE PRODUCT RECORD AND GENERATES THE REQUEST    NRK         
* RECORD.                                                           NRK         
*                                                                   NRK         
ADDPRD2  NTR1  BASE=*,LABEL=*                                       NRK         
*                            PAGENCY WAS SET TO C'00' HERE                      
         GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         GOTO1 CNADDSPT                                                         
         OC    SVADVDA,SVADVDA                                                  
         BZ    ADP2EXIT                                                         
*                                                                               
         NI    PKEY+1,X'0F'         SEE IF I MUST ADD PRD TO ADV                
         OC    PKEY+1(1),SVADVAGY                                               
         CLC   PKEY(13),SVKEY        I AM ADDING TO ADV - SO I'M DONE           
         BE    ADP2EXIT                                                         
*                                                                               
         MVC   KEY(13),PKEY                                                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    ADP2EXIT      ADV PRD ALREADY ON FILE                            
*                                                                               
         MVC   KEY(13),PKEY                                                     
         GOTO1 ADDREC                                                           
*                                                                               
ADP2EXIT EQU   *                                                    NRK         
*                                                                   NRK         
         GOTO1 =A(GENREQ),RR=RELO                                               
         XIT1                                                       NRK         
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPLFMWRK                                                       
         ORG   LFMTABH                                                          
*SPLFMF2                                                                        
       ++INCLUDE SPLFMF2D                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
*                                                                               
ADVHDRD  DSECT                                                                  
       ++INCLUDE SPGENADV                                                       
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
         EJECT                                                                  
***********************************************************************         
*======================= MISC WORKING STORAGE ========================*         
GENOLD   DSECT                                                                  
         ORG   DEMAREA+2000                                                     
MISCWRK  DS    0H                                                               
SAVERE   DS    F                   SAVE STORAGE FOR RE                          
ADSC     DS    A                                                                
AUSR     DS    A                                                                
RELO     DS    F                                                                
*                                                                               
*                                                                               
DESC     DS    CL20                                                             
OPTNFLAG DS    XL1                                                              
TALOPTN  EQU   X'80'                                                            
FLAG1    DS    XL1                                                              
FLAG2    DS    XL1                                                              
LEN      DS    XL1                                                              
UTYPE    DS    CL1                                                              
TDATE    DS    CL6                 OUTPUT FOR DATVAL                            
USERDATA DS    CL32                                                             
PSTOUT   DS    CL64                                                             
SVADVLST DS    CL30           SAVED LIST OF BUYING AGENCIES                     
SVCLTLST DS    CL(4*220)      SAVED CLIST                                       
MISCWRKQ EQU   *-MISCWRK                                                        
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063SPLFM12   01/14/99'                                      
         END                                                                    
