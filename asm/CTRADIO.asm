*          DATA SET CTRADIO    AT LEVEL 140 AS OF 09/22/17                      
**********************************************************************          
*  CTRADIO:  GENERATE RECORDS FROM MSTREET DATA WHICH ARE LOADED     *          
*         INTO THE CONTROL FILE.                                     *          
**********************************************************************          
*  HISTORY OF CHANGES:                                               *          
*  MAR22/10 (AKAT)--- NEW MEDIA FRAMEWORKS RECORDS SUPPORT           *          
*                                                                    *          
*  FEB01/07 (MCHO)--- NEW TAPE AND MAILING ADDRESSES, MINORITY OWN   *          
*                                                                    *          
*  JUN19/03 (BU ) --- NEW DDS ID CODE, MULTI-MEDIA DATA              *          
*                                                                    *          
*  OCT27/03 (BU ) --- NEW PASSIVE FOR UID                            *          
*                                                                    *          
*  FEB13/04 (BU ) --- REVERT TO UID                                  *          
*                     KILL NEW PASSIVE ID, RESTORE ORIGINAL PASSIVE  *          
*                     OUTPUT 99 RECORD WITH STATION CALLS            *          
*                                                                    *          
*  MAR19/04 (BU ) --- SUPPRESS DISPLAYS FOR DEBUGGING                *          
*                                                                    *          
*  APR06/04 (BU ) --- DON'T OUTPUT KEYS WITH SPACE FOR STATION       *          
*                                                                    *          
*  APR16/04 (BU ) --- OWNERSHIP 99 RECORDS TO SORT                   *          
*                                                                    *          
*                                                                    *          
*                                                                    *          
**********************************************************************          
*PHASE CTRADIOA                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
         PRINT NOGEN                                                            
CTRADIO  START                                                                  
         NBASE WORKL,*CTRADIO,=V(WORKAREA),RA,R5                                
         USING WORKD,RC                                                         
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         L     R8,VBOXAREA                                                      
         USING BOXD,R8                                                          
*                                                                               
         MVI   SOURCE,CT99KOMS     C'S'-PROCESS MSTREET RECORDS FIRST           
CTRAD00  BRAS  RE,INIT             INITIALISE JOB                               
*                                                                               
         BRAS  RE,BLDRAD           BUILD RADIO OUTPUT                           
         BRAS  RE,BLDMET                                                        
         BRAS  RE,BLDOWN                                                        
         BRAS  RE,BLDREP                                                        
         BRAS  RE,BLDFRM                                                        
         BRAS  RE,BLDMMD                                                        
         GOTO1 =A(DISPTOTS),DMCB,(RC)                                           
*                                                                               
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSED MSTREET?                      
         BNE   CTRAD10             NO, COULD BE MFW OR SRDS                     
         MVC   P(23),=C'** DONE WITH MSTREET **'                                
         GOTO1 VPRINTER                                                         
         CLOSE TAPEOUT             CLOSE MSTREET TAPE IN CASE MF ABENDS         
         XC    RADCTR(CLEARWRK),RADCTR                                          
         MVI   SOURCE,CT99KOMF     C'F'-NOW PROCESS MEDIA FRAMEWORKS            
         B     CTRAD00                                                          
*                                                                               
CTRAD10  CLI   SOURCE,CT99KOMF     C'F'- PROCESSED MEDIA FRAMEWORKS?            
         BNE   CTRAD20             YES - DONE!                                  
         MVC   P(23),=C'** DONE WITH MFW     **'                                
         GOTO1 VPRINTER                                                         
         CLOSE TAPEOUT2            CLOSE MFW TAPE IN CASE MF ABENDS             
         XC    RADCTR(CLEARWRK),RADCTR                                          
         MVI   SOURCE,CT99KOSD     C'R'-NOW PROCESS SRDS                        
         B     CTRAD00                                                          
*                                                                               
CTRAD20  CLI   SOURCE,CT99KOSD     C'R'- PROCESSED SRDS?                        
         BNE   CTRADX              YES - DONE!                                  
         MVC   P(23),=C'** DONE WITH SRDS    **'                                
         GOTO1 VPRINTER                                                         
         CLOSE TAPEOUT3            CLOSE SRDS TAPE IN CASE MF ABENDS            
         XC    RADCTR(CLEARWRK),RADCTR                                          
         MVI   SOURCE,CT99KODS     C'D'-NOW PROCESS DDS                         
         B     CTRAD00                                                          
*                                                                               
CTRADX   MVC   P(23),=C'** DONE WITH DDS     **'                                
         GOTO1 VPRINTER                                                         
         CLOSE TAPEOUT4            CLOSE SRDS TAPE IN CASE MF ABENDS            
         B     XBASE                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD OUTPUT RECORDS FOR RADIO                                      *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDRAD   NTR1  ,                                                                
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BNE   BRAD0000            YES                                          
         LA    R6,RADIO                                                         
         B     BRAD0009                                                         
*                                                                               
BRAD0000 CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MFW?                         
         BNE   BRAD0005            YES                                          
         LA    R6,RADIOF                                                        
         B     BRAD0009                                                         
*                                                                               
BRAD0005 CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BNE   BRAD0007            YES                                          
         LA    R6,RADIOR                                                        
         B     BRAD0009                                                         
*                                                                               
BRAD0007 LA    R6,RADIOD           DDS                                          
*                                                                               
BRAD0009 OPEN  ((R6),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(RADBOX),DMCB,(RC)                                             
*                                                                               
         MVI   EOF,C'N'                                                         
         LA    R0,RECSVL           CLEAR SAVE RECORD AREA                       
         LHI   R1,RECSVLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING CRD,RECSV                                                        
BRAD0010 LA    R0,RECINL           CLEAR INPUT RECORD AREA                      
         LHI   R1,RECINLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GET   (R6),RECINL         GET INPUT AND TRANSLATE IT                   
*                                                                               
BRAD10   TR    RECIN(CRSPARE-CRD),TRTAB                                         
         TR    RECIN+CRSPARE-CRD(CRDL1Q-(CRSPARE-CRD)),TRTAB                    
         TR    RECIN+TPEADDR1-CRD(CRDL2Q-(TPEADDR1-CRD)),TRTAB                  
         TR    RECIN+MLGADDR1-CRD(CRDL3Q-(MLGADDR1-CRD)),TRTAB                  
         TR    RECIN+MINOWNF-CRD(CRDL4Q-(MINOWNF-CRD)),TRTAB                    
*                                                                               
         CLI   CRCALL+4-CRD+RECIN,C' '  TOO MANY CALL LETTERS FOR DDS           
         BE    BRAD0020                                                         
         GOTO1 VPRINTER                                                         
         MVC   P+01(22),=C'MSTREET RECORD SKIPPED'                              
*                                                                               
BRADSKIP MVC   P+24(6),CRCALL-CRD+RECIN                                         
         MVC   P+40(L'CRUID),CRUID-CRD+RECIN                                    
         MVC   P+48(L'CRSERVCE),CRSERVCE-CRD+RECIN                              
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*&&DO                                                                           
         LA    R4,RECIN            A(I/P  RECORD)                               
         LA    RF,16                                                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
*&&                                                                             
         B     BRAD0010                                                         
BRAD0020 EQU   *                                                                
         LA    RE,SRVCVALS                                                      
BRAD0022 CLI   0(RE),0                                                          
         BNE   BRAD0024                                                         
         GOTO1 VPRINTER                                                         
         L     RF,SRVCSKPD         COUNT SERVICE RECORD SKIPS                   
         LA    RF,1(RF)                                                         
         ST    RF,SRVCSKPD                                                      
         MVC   P+01(22),=C'SERVICE RECORD SKIPPED'                              
         B     BRADSKIP                                                         
*                                                                               
BRAD0024 CLC   0(2,RE),CRSERVCE-CRD+RECIN                                       
         BE    BRAD0025                                                         
         LA    RE,L'SRVCVALS(RE)                                                
         B     BRAD0022                                                         
*                                                                               
**  WE HAVE COMMENTED OUT SERVICE VALUES AS DDS CANNOT SETUP STATIONS           
**   FOR THESE ANYWAY. JUST WANT TO KEEP THEM SO WE KNOW WHAT THEY ARE          
*                                                                               
SRVCVALS DS    0CL2                POSSIBLE SERVICE VALUES                      
         DC    C'AM'                                                            
         DC    C'FM'                                                            
         DC    C'TV'                                                            
         DC    C'CA'               LOW POWER TV                                 
         DC    C'TX'               LOW POWER TV                                 
***      DC    C'LP'               LOW POWER RADIO                              
         DC    C'DT'               DIGITAL                                      
***      DC    C'LD'               LOW POWER DIGITAL                            
         DC    C'HD'               HIGH-DEF                                     
         DC    C'SM'               STREAMING MEDIA                              
         DC    C'DV'               DIGITAL VIDEO                                
         DC    C'CM'               COMBINED MEDIA                               
         DC    X'00'               E-O-T                                        
*                                                                               
BRAD0025 CLC   RECIN(64),SPACES    ANY DATA IN RECORD?                          
         BH    BRAD0030            YES                                          
*                                                                               
*&&DO                                                                           
         MVC   P+1(20),=C'MSTREET RECORD BLANK'                                 
         GOTO1 VPRINTER                                                         
         LA    R4,RECIN            A(I/P  RECORD)                               
         LA    RF,512                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
*&&                                                                             
*                                                                               
         L     RF,BLANKCTR         COUNT RECORD TYPE                            
         LA    RF,1(RF)                                                         
         ST    RF,BLANKCTR                                                      
         B     BRAD0010            GO BACK FOR NEXT RECORD                      
BRAD0030 EQU   *                                                                
*                                                                               
         L     RF,RADCTR           COUNT RECORD TYPE                            
         LA    RF,1(RF)                                                         
         ST    RF,RADCTR                                                        
*                                                                               
*   TEST FORCE END                                                              
****     CLC   RADCTR,=F'50'       END AFTER 50                                 
****     BH    BRAD0480            FORCE END                                    
*   TEST FORCE END                                                              
*                                                                               
         LA    R0,RECIN                                                         
         LHI   R1,RECINLQ                                                       
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         CLCL  R0,RE               SEE IF THIS RECORD IS THE SAME               
         BE    BRAD0010                                                         
*                                                                               
         OC    CRUID,CRUID                                                      
         BZ    BRAD0380            FIRST TIME - JUST SAVE RECORD                
*                                                                               
BRAD0040 LA    R0,RECOUTH          CLEAR OUTPUT RECORD AREA                     
         LHI   R1,RECOUTLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,RADPRT                                                        
*                                                                               
         LA    R2,RECOUT                                                        
         USING CT99RECD,R2                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSRA                                                
***      MVC   CT99KUID,CRDDSID    INSERT DDS ID INTO KEY                       
*                                                                               
         MVI   CT99KSRC,CT99KOMS   C'S'-MSTREET                                 
         CLI   SOURCE,CT99KOMS     PROCESSING MSTREET?                          
         BE    BRAD0050            YES                                          
         MVI   CT99KSRC,CT99KOMF   C'F'-MFW                                     
         CLI   SOURCE,CT99KOMF     PROCESSING MFW?                              
         BE    BRAD0050            YES                                          
         MVI   CT99KSRC,CT99KOSD   C'R'-SRDS                                    
         CLI   SOURCE,CT99KOSD     PROCESSING SRDS?                             
         BE    BRAD0050            YES                                          
         MVI   CT99KSRC,CT99KODS   C'D'-DDS                                     
*                                                                               
BRAD0050 MVC   CT99KUID,CRUID      INSERT UID    INTO KEY                       
         MVC   SAVCRUID,CRUID      SAVE THIS RECORD'S UID                       
*                                                                               
         LA    R3,CT99DATA                                                      
         USING CRCLD,R3                                                         
         MVI   CRCLEL,CRCLELQ                                                   
         MVI   CRCLLN,CRCLLNQ                                                   
         MVC   CRCLCLL,CRCALL      CALL LETTER                                  
*                                                                               
         CLC   CRSERVCE,=C'TX'     SERVICE TX IS LOW POWER TV                   
         BE    *+14                                                             
         CLC   CRSERVCE,=C'CA'       AND   CA IS LOW POWER TV                   
         BNE   *+8                         LP IS LOW POWER RADIO                
         MVI   CRBAND,C'L'                                                      
*                                                                               
         MVC   CRCLBND,CRBAND      BAND                                         
*                                                                               
         MVC   SAVCALL,CRCALL      SAVE THIS RECORD'S CALL LETTERS              
         MVC   SAVBAND,CRBAND      SAVE THIS RECORD'S BAND                      
*                                                                               
         MVC   CRCLFRQ,CRFREQ      FREQUENCY                                    
         MVC   CRCLCTY,CRLCITY     CITY                                         
         MVC   CRCLSTE,CRLSTATE    STATE                                        
***      MVC   CRCLUID,CRUID       ORIGINAL MSTREET UID                         
         MVC   CRCLDDS,CRDDSID     SAVE DDS ID                                  
         MVC   CRCLPRNT,CRPARENT   MULTIMEDIA PARENT                            
*                                                                               
         XC    CARD,CARD           OUTPUT LIVE 9A PASSIVE RECORD                
         MVC   CARD(2),=AL2(CT9ALENQ+4)                                         
         USING CT9ARECD,CARD+4                                                  
         MVI   CT9AKTYP,CT9AKTYQ                                                
         MVC   CT9AKSRC,SOURCE     SET THE SOURCE                               
         MVI   CT9AKMED,CT9AKMRD   SET MEDIA TO RADIO                           
         CLI   CRBAND,C'A'         AM STATION?                                  
         BE    BRAD0060                                                         
         CLI   CRBAND,C'F'         FM STATION?                                  
         BE    BRAD0060                                                         
         MVI   CT9AKMED,CT9XKMTV   SET MEDIA TO TV                              
BRAD0060 EQU   *                                                                
         MVC   CT9AKCLL+0(4),CRCALL                                             
         MVC   CT9AKCLL+4(1),CRBAND                                             
         XC    CT9AKDTE,CT9AKDTE                                                
         MVC   CT9ALEN,=AL2(CT9ALENQ)                                           
         MVI   CT9AEL,CT9AELQ                                                   
         MVI   CT9ALN,CT9ALNQ                                                   
****     MVC   CT9AUID,CRDDSID     USE DDS ID FOR PASSIVE KEY                   
         MVC   CT9AUID,CRUID       USE UID    FOR PASSIVE KEY                   
         BRAS  RE,PUTCARD                                                       
*                                                                               
         AHI   R3,CRCLLNQ                                                       
         USING CRFMD,R3                                                         
         MVI   CRFMEL,CRFMELQ                                                   
****     MVI   CRFMLN,CRFMLNQ                                                   
         MVI   CRFMLN,CRFMLNQ2     USE NEW LENGTH                               
         MVC   CRFMFMT,CRFMT                                                    
         MVC   CRFMOWN,CROWNER                                                  
         MVC   CRFMREP1,CRREP1                                                  
         MVC   CRFMREP2,CRREP2                                                  
         MVC   CRFMAMC,CRAMCODE                                                 
         MVC   CRFMMIN,RECSV+(MINOWN-CRD)                                       
         MVC   CRFMMINF,RECSV+(MINOWNF-CRD)                                     
         MVC   CRFMQFCC,RECSV+(QLFCCMIN-CRD)                                    
         MVC   CRFMQFCF,RECSV+(QLFCMINF-CRD)                                    
*                                                                               
         CLI   CRFMMINF,C'0'      MODIFY THE 0/1 TO N/Y LEAVING                 
         JNE   *+12                 ANYTHING ELSE ALONE IN CASE OF NULL         
         MVI   CRFMMINF,C'N'                                                    
         J     BRAD0070                                                         
*                                                                               
         CLI   CRFMMINF,C'1'                                                    
         JNE   *+8                                                              
         MVI   CRFMMINF,C'Y'                                                    
*                                                                               
BRAD0070 CLI   CRFMQFCF,C'0'      MODIFY THE 0/1 TO N/Y LEAVING                 
         JNE   *+12                 ANYTHING ELSE ALONE IN CASE OF NULL         
         MVI   CRFMQFCF,C'N'                                                    
         J     BRAD0095                                                         
*                                                                               
         CLI   CRFMQFCF,C'1'                                                    
         JNE   *+8                                                              
         MVI   CRFMQFCF,C'Y'                                                    
*                                                                               
BRAD0095 AHI   R3,CRFMLNQ2         USE NEW LENGTH                               
         USING CRCHD,R3                                                         
         MVI   CRCHEL,CRCHELQ                                                   
         MVI   CRCHLN,CRCHLNQ                                                   
         MVC   CRCHHST1,CRCHST1                                                 
         MVC   CRCHHST2,CRCHST2                                                 
BRAD0100 MVC   DUB+0(4),CRCHSTD1+4 TURN MMDDYYYY INTO YYYYMMDD                  
         MVC   DUB+4(4),CRCHSTD1+0                                              
         CLC   DUB,SPACES                                                       
         BNH   BRAD0120                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(3,CRCHHDT1)                                 
*                                                                               
BRAD0120 MVC   DUB+0(4),CRCHSTD2+4                                              
         MVC   DUB+4(4),CRCHSTD2+0                                              
         CLC   DUB,SPACES                                                       
         BNH   BRAD0140                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(3,CRCHHDT2)                                 
*                                                                               
BRAD0140 CLC   CRCHHST1,SPACES     -1 HISTORY?                                  
         BNH   BRAD0160            NO                                           
         CLC   =CL7'NEW',CRCHHST1                                               
         BE    BRAD0160                                                         
*                                                                               
         XC    CARD,CARD           OUTPUT -1 9A PASSIVE RECORD                  
         MVC   CARD(2),=AL2(CT9ALENQ+4)                                         
         MVI   CT9AKTYP,CT9AKTYQ                                                
         MVC   CT9AKSRC,SOURCE     SET THE SOURCE                               
         MVI   CT9AKMED,CT9AKMRD                                                
         MVC   CT9AKCLL,CRCHST1                                                 
         GOTO1 DATCON,DMCB,(3,CRCHHDT1),(2,CT9AKDTE)                            
         XC    CT9AKDTE,=XL2'FFFF'                                              
         MVC   CT9ALEN,=AL2(CT9ALENQ)                                           
         MVI   CT9AEL,CT9AELQ                                                   
         MVI   CT9ALN,CT9ALNQ                                                   
***      MVC   CT9AUID,CRDDSID     USE DDS ID FOR PASSIVE KEY                   
         MVC   CT9AUID,CRUID       USE UID    FOR PASSIVE KEY                   
         BRAS  RE,PUTCARD                                                       
*                                                                               
BRAD0160 CLC   CRCHHST2,SPACES     -2 HISTORY                                   
         BNH   BRAD0180                                                         
         CLC   CRCHHST2(CRCHHST2-CRCHHST1),CRCHHST1  SAME AS PREV?              
         BE    BRAD0180              WHY DOES THIS HAPPEN WITH MFW?             
         CLC   =CL7'NEW',CRCHHST2                                               
         BE    BRAD0180                                                         
*                                                                               
         XC    CARD,CARD           OUTPUT -2 9A PASSIVE RECORD                  
         MVC   CARD(2),=AL2(CT9ALENQ+4)                                         
         MVI   CT9AKTYP,CT9AKTYQ                                                
         MVC   CT9AKSRC,SOURCE     SET THE SOURCE                               
         MVI   CT9AKMED,CT9AKMRD                                                
         MVC   CT9AKCLL,CRCHST2                                                 
         GOTO1 DATCON,DMCB,(3,CRCHHDT2),(2,CT9AKDTE)                            
         XC    CT9AKDTE,=XL2'FFFF'                                              
         MVC   CT9ALEN,=AL2(CT9ALENQ)                                           
         MVI   CT9AEL,CT9AELQ                                                   
         MVI   CT9ALN,CT9ALNQ                                                   
****     MVC   CT9AUID,CRDDSID     USE DDS ID FOR PASSIVE KEY                   
         MVC   CT9AUID,CRUID       USE UID    FOR PASSIVE KEY                   
         BRAS  RE,PUTCARD                                                       
*                                                                               
BRAD0180 AHI   R3,CRCHLNQ                                                       
         USING CRFHD,R3                                                         
         MVI   CRFHEL,CRFHELQ                                                   
         MVI   CRFHLN,CRFHLNQ                                                   
         MVC   CRFHHST1,CRFHST1                                                 
         MVC   CRFHHST2,CRFHST2                                                 
*                                                                               
         MVC   DUB+0(4),CRFHSTD1+4 TURN MMDDYYYY INTO YYYYMMDD                  
         MVC   DUB+4(4),CRFHSTD1+0                                              
         CLC   DUB,SPACES                                                       
         BNH   BRAD0200                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(3,CRFHHDT1)                                 
*                                                                               
BRAD0200 MVC   DUB+0(4),CRFHSTD2+4                                              
         MVC   DUB+4(4),CRFHSTD2+0                                              
         CLC   DUB,SPACES                                                       
         BNH   BRAD0220                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(3,CRFHHDT2)                                 
*                                                                               
BRAD0220 AHI   R3,CRFHLNQ                                                       
         USING CRMHD,R3                                                         
         MVI   CRMHEL,CRMHELQ                                                   
         MVI   CRMHLN,CRMHLNQ                                                   
         MVC   CRMHHST1,CRMHST1                                                 
         MVC   CRMHHST2,CRMHST2                                                 
*                                                                               
         MVC   DUB+0(4),CRMHSTD1+4 TURN MMDDYYYY INTO YYYYMMDD                  
         MVC   DUB+4(4),CRMHSTD1+0                                              
         CLC   DUB,SPACES                                                       
         BNH   BRAD0240                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(3,CRMHHDT1)                                 
*                                                                               
BRAD0240 MVC   DUB+0(4),CRMHSTD2+4                                              
         MVC   DUB+4(4),CRMHSTD2+0                                              
         CLC   DUB,SPACES                                                       
         BNH   BRAD0260                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(3,CRMHHDT2)                                 
*                                                                               
BRAD0260 AHI   R3,CRMHLNQ          TEST OLD-STYLE INPUT STREAM                  
**       LH    RF,RECINL                                                        
**       CHI   RF,CRDLQ+4                                                       
**       BNH   BRAD0360            YES - IGNORE NEXT ELEMS AND FINISH           
*                                                                               
         USING CRZHD,R3                                                         
BRAD0270 MVI   CRZHEL,CRZHELQ                                                   
         MVI   CRZHLN,CRZHLNQ                                                   
         MVC   CRZHHST1,CRZHST1                                                 
         MVC   CRZHHST2,CRZHST2                                                 
*                                                                               
         MVC   DUB+0(4),CRZHSTD1+4 TURN MMDDYYYY INTO YYYYMMDD                  
         MVC   DUB+4(4),CRZHSTD1+0                                              
         CLC   DUB,SPACES                                                       
         BNH   BRAD0280                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(3,CRZHHDT1)                                 
*                                                                               
BRAD0280 MVC   DUB+0(4),CRZHSTD2+4                                              
         MVC   DUB+4(4),CRZHSTD2+0                                              
         CLC   DUB,SPACES                                                       
         BNH   BRAD0300                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(3,CRZHHDT2)                                 
*                                                                               
BRAD0300 AHI   R3,CRZHLNQ                                                       
         USING CRSHD,R3                                                         
         MVI   CRSHEL,CRSHELQ                                                   
         MVI   CRSHLN,CRSHLNQ                                                   
         MVC   CRSHHST1,CRSHST1                                                 
         MVC   CRSHHST2,CRSHST2                                                 
*                                                                               
         MVC   DUB+0(4),CRSHSTD1+4 TURN MMDDYYYY INTO YYYYMMDD                  
         MVC   DUB+4(4),CRSHSTD1+0                                              
         CLC   DUB,SPACES                                                       
         BNH   BRAD0320                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(3,CRSHHDT1)                                 
*                                                                               
BRAD0320 MVC   DUB+0(4),CRSHSTD2+4                                              
         MVC   DUB+4(4),CRSHSTD2+0                                              
         CLC   DUB,SPACES                                                       
         BNH   BRAD0340                                                         
         GOTO1 DATCON,DMCB,(9,DUB),(3,CRSHHDT2)                                 
*                                                                               
BRAD0340 AHI   R3,CRSHLNQ                                                       
*                                                                               
BRAD0341 LA    R4,TPEADDR1                                                      
         LAY   R0,MLGADDR1                                                      
BRAD0342 CR    R4,R0                                                            
         BNL   BRAD0345                                                         
         CLI   0(R4),C' '                                                       
         BH    BRAD0343                                                         
         LA    R4,1(R4)                                                         
         B     BRAD0342                                                         
*                                                                               
BRAD0343 DS    0H                                                               
         MVI   BYTE,C'T'                                                        
         LA    R4,TPEADDR1                                                      
         BRAS  RE,BRADCMAD         ADDED THE ELEMENT                            
*                                                                               
BRAD0345 LAY   R4,MLGADDR1                                                      
         LAY   R0,MINOWN           MINOWN USED AS A BOUNDARY CHECK              
BRAD0347 CR    R4,R0                                                            
         BNL   BRAD0350                                                         
         CLI   0(R4),C' '                                                       
         BH    BRAD0348                                                         
         LA    R4,1(R4)                                                         
         B     BRAD0347                                                         
*                                                                               
BRAD0348 DS    0H                                                               
         MVI   BYTE,C'M'                                                        
         LAY   R4,MLGADDR1                                                      
         BRAS  RE,BRADCMAD         ADDED THE ELEMENT                            
*                                                                               
         B     BRAD0350            WE'RE DONE                                   
*                                                                               
******                                                                          
         USING CMADREL,R3                                                       
BRADCMAD MVI   CMADREL,CMADRELQ    X'08' ELEMENT                                
         MVI   CMADRLN,CMADRLNQ                                                 
         MVC   CMADRTY,BYTE        TAPE                                         
         MVC   CMADAD1(CMADRLNQ-3),0(R4)   R4 HAS INPUT                         
         AHI   R3,CMADRLNQ                                                      
         BR    RE                                                               
******                                                                          
*                                                                               
BRAD0350 MVI   0(R3),0             TERMINATE RECORD                             
         AHI   R3,1                                                             
*                                                                               
         SR    R3,R2                                                            
         STCM  R3,3,CT99LEN        SET LENGTH                                   
         AHI   R3,4                                                             
         STH   R3,RECOUTH          SET RECORD LENGTH FOR PUT (=LEN+4)           
*                                                                               
BRAD0360 GOTO1 CHECKKEY,DMCB,0                                                  
         BNZ   BRAD0380            BAD KEY:  SKIP                               
*                                                                               
*   OUTPUT SORT RECORD OF 'TAPEOUT' FOR REVIEW:  UID SEQUENCE                   
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECOUT                                   
         L     RF,SORTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR                                                       
*                                                                               
         MVC   CT99KUID,SPACES     CLEAR UID FROM KEY                           
         MVC   CT99KUID(4),SAVCALL INSERT STATION CALL LETTERS                  
         MVC   CT99KUID+4(1),SAVBAND                                            
         MVI   CT99KUID+5,0        SET 6TH POSITION TO SEQ # 0                  
*                                  INSERT MEDIA                                 
         LA    R3,CT99DATA                                                      
         USING CRCLD,R3                                                         
*                                                                               
         MVC   CRCLUIDX,SAVCRUID   INSERT ORIGINAL UID                          
*                                                                               
*   OUTPUT SORT RECORD OF 'TAPEOUT' FOR REVIEW:  STN SEQUENCE                   
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',RECOUT                                   
         L     RF,SORTCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR                                                       
*                                                                               
         CLI   EOF,C'Y'                                                         
         BE    BRAD0400                                                         
*                                                                               
BRAD0380 LA    R0,RECIN                                                         
         LHI   R1,RECINLQ                                                       
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         MVCL  RE,R0               SAVE THIS RECORD                             
         B     BRAD0010                                                         
*                                                                               
BRAD0400 EQU   *                                                                
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     RE,DMCB+4                                                        
         LTR   RE,RE               TEST RETURN ZERO=EOF                         
         BZ    BRAD0460            YES -                                        
         LA    R0,SORTREC                                                       
         LHI   R1,L'SORTREC        537 BYTES                                    
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE                                         
*                                                                               
         L     RF,SORTCTR2                                                      
         LA    RF,1(RF)                                                         
         ST    RF,SORTCTR2                                                      
*                                                                               
         CLC   SORTREC(25),SVTAPOUT    SAME KEY RETURNED?                       
         BNE   BRAD0420            NO                                           
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+01(13),=C'1ST   REC    '                                       
         MVC   P+14(64),SVTAPOUT                                                
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+01(13),=C'2ND   REC    '                                       
         MVC   P+14(64),SORTREC                                                 
*                                                                               
         GOTO1 VPRINTER                                                         
         MVC   P+1(05),=C'EQUAL'                                                
         CLC   SORTREC(222),SVTAPOUT   THIS IS FINE, 222 COMPARE IS OK          
         BE    BRAD0410                                                         
         MVC   P+1(07),=C'UNEQUAL'                                              
BRAD0410 EQU   *                                                                
         GOTO1 VPRINTER                                                         
         MVC   P+1(24),=C'DUPLICATE RECORD SKIPPED'                             
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         L     RF,RADSKPD                                                       
         LA    RF,1(RF)                                                         
         ST    RF,RADSKPD                                                       
*                                                                               
         B     BRAD0400                                                         
*                                                                               
BRAD0420 EQU   *                                                                
         LA    R0,SVTAPOUT                                                      
         LHI   R1,L'SVTAPOUT       537 BYTES                                    
         LR    RF,R1                                                            
         LA    RE,SORTREC                                                       
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,RECOUT                                                        
         LHI   R1,L'SORTREC        537 BYTES                                    
         LR    RF,R1                                                            
         LA    RE,SORTREC                                                       
         MVCL  R0,RE                                                            
*                                                                               
         LA    RF,541              LOAD RECORD FOR O/P TO TAPE                  
         STCM  RF,3,RECOUTH                                                     
*                                  SET RECORD LENGTH                            
         BRAS  RE,PUTRECO                                                       
*                                                                               
         CLI   SORTREC+24,0        STATION?                                     
         BH    BRAD0440            NO  - UID                                    
         L     RF,RADOUT           YES - ADD TO STATION COUNTER                 
         LA    RF,1(RF)                                                         
         ST    RF,RADOUT                                                        
         B     BRAD0400            GO BACK FOR NEXT                             
*                                                                               
BRAD0440 L     RF,RADOUT2          ADD TO UID COUNTER                           
         LA    RF,1(RF)                                                         
         ST    RF,RADOUT2                                                       
         B     BRAD0400            GO BACK FOR NEXT                             
*                                                                               
BRAD0460 CLOSE (R6)                                                             
*                                                                               
BRAD0470 MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
BRAD0480 MVI   EOF,C'Y'                                                         
         B     BRAD0040                                                         
         POP   USING                                                            
         EJECT                                                                  
*                                                                               
PUTCARD  NTR1                                                                   
         LA    R2,TAPEOUT                                                       
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BE    PUTCARD5                                                         
         LA    R2,TAPEOUT2                                                      
         CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MFW?                         
         BE    PUTCARD5                                                         
         LA    R2,TAPEOUT3                                                      
         CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BE    PUTCARD5                                                         
         LA    R2,TAPEOUT4         C'D'-PROCESSING DDS                          
PUTCARD5 PUT   0(R2),CARD                                                       
         B     EXIT                                                             
***********************************************************************         
* CHECK FOR A KEY WITH NO STATION OR UID IN IT                        *         
***********************************************************************         
CHECKKEY NTR1                                                                   
         LA    R2,RECOUT                                                        
         USING CT99RECD,R2                                                      
         CLC   CT99KUID,SPACES                                                  
*                                                                               
         DROP  R2                                                               
*                                                                               
         BH    CKEY0800            KEY ACCEPTED: EXIT CC ZERO                   
         MVC   P+1(13),=C'BAD KEY / UID'                                        
         OC    DMCB(4),DMCB        CHECK FLAG                                   
         BZ    CKEY0020            ZERO = UID KEY                               
         MVC   P+1(13),=C'BAD KEY / STA'                                        
CKEY0020 EQU   *                                                                
         GOTO1 VPRINTER                                                         
         L     RF,BADKEY                                                        
         LA    RF,1(RF)                                                         
         ST    RF,BADKEY                                                        
*                                                                               
         LA    R4,RECOUTH          A(O/P  RECORD)                               
         SR    RF,RF                                                            
         ICM   RF,3,RECOUTH                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
         MVC   P+1(13),=C'MSTREET INPUT'                                        
         GOTO1 VPRINTER                                                         
         LA    R4,RECIN            A(I/P  RECORD)                               
         SR    RF,RF                                                            
***      LA    RF,512                                                           
         LA    RF,616                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
         LTR   RB,RB                                                            
         B     CKEY0900            EXIT CC NOT ZERO:  ERROR                     
CKEY0800 EQU   *                                                                
         SR    R0,R0                                                            
CKEY0900 EQU   *                                                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PRINT LINE FROM RADIO DATA                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
RADPRT   NTR1  ,                                                                
         USING RADLINED,P                                                       
         USING CRD,RECSV                                                        
         MVI   BYTE1,NO                                                         
         MVC   RADUID,CRDDSID      USE DDS ID FOR 1ST LINE DISPLAY              
*                                                                               
         CLC   CRDDSID,CRUID       DDS ID = ORIG UID?                           
         BE    RPRT0020            YES                                          
         MVI   RADUID+6,C'#'                                                    
RPRT0020 EQU   *                                                                
         MVC   RADCALL,CRCALL                                                   
         MVC   RADBND,CRBAND                                                    
         MVC   RADFREQ,CRFREQ                                                   
         MVC   RADLCITY,CRLCITY                                                 
         MVC   RADSTATE,CRLSTATE                                                
         MVC   RADFMT,CRFMT                                                     
         MVC   RADOWNER,CROWNER                                                 
         MVC   RADREP1,CRREP1                                                   
         MVC   RADREP2,CRREP2                                                   
         MVC   RADAMC,CRAMCODE                                                  
*                                                                               
         CLC   CRCHST1,SPACES                                                   
         BNH   RPRT0040                                                         
         MVC   RADTYPE,=CL05'Call1'                                             
         MVC   RADTDATA(L'CRCHST1),CRCHST1                                      
         MVC   RADDATE,CRCHSTD1                                                 
         GOTO1 VPRINTER                                                         
         MVI   BYTE1,YES                                                        
*                                                                               
RPRT0040 CLC   CRCHST2,SPACES                                                   
         BNH   RPRT0060                                                         
         MVC   RADTYPE,=CL05'Call2'                                             
         MVC   RADTDATA(L'CRCHST2),CRCHST2                                      
         MVC   RADDATE,CRCHSTD2                                                 
         GOTO1 VPRINTER                                                         
         MVI   BYTE1,YES                                                        
*                                                                               
RPRT0060 CLC   CRFHST1,SPACES                                                   
         BNH   RPRT0080                                                         
         MVC   RADTYPE,=CL05'Freq1'                                             
         MVC   RADTDATA(L'CRFHST1),CRFHST1                                      
         MVC   RADDATE,CRFHSTD1                                                 
         GOTO1 VPRINTER                                                         
         MVI   BYTE1,YES                                                        
*                                                                               
RPRT0080 CLC   CRFHST2,SPACES                                                   
         BNH   RPRT0100                                                         
         MVC   RADTYPE,=CL05'Freq2'                                             
         MVC   RADTDATA(L'CRFHST2),CRFHST2                                      
         MVC   RADDATE,CRFHSTD2                                                 
         GOTO1 VPRINTER                                                         
         MVI   BYTE1,YES                                                        
*                                                                               
RPRT0100 CLC   CRMHST1,SPACES                                                   
         BNH   RPRT0120                                                         
         MVC   RADTYPE,=CL05'Fmt1 '                                             
         MVC   RADTDATA(L'CRMHST1),CRMHST1                                      
         MVC   RADDATE,CRMHSTD1                                                 
         GOTO1 VPRINTER                                                         
         MVI   BYTE1,YES                                                        
*                                                                               
RPRT0120 CLC   CRMHST2,SPACES                                                   
         BNH   RPRT0140                                                         
         MVC   RADTYPE,=CL05'Fmt2 '                                             
         MVC   RADTDATA(L'CRMHST2),CRMHST2                                      
         MVC   RADDATE,CRMHSTD2                                                 
         GOTO1 VPRINTER                                                         
         MVI   BYTE1,YES                                                        
*                                                                               
RPRT0140 CLI   BYTE1,YES           MAKE SURE WE DID AT LEAST 1 PRINT            
         BE    RPRT0160                                                         
         GOTO1 VPRINTER                                                         
RPRT0160 EQU   *                                                                
         MVC   RADUID,CRUID        DISPLAY ORIGINAL MSTREET UID                 
         MVC   RADCALL(5),CRPARENT DISPLAY PARENT                               
***   TESTING                                                                   
         MVC   RADLCITY(10),TPEADDR1                                            
         MVC   RADSTATE(10),RECSV+(MLGADDR1-CRD)                                
         MVC   RADREP1(4),RECSV+(MINOWN-CRD)                                    
***   TESTING                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***>>>                                                                          
***********************************************************************         
* BUILD OUTPUT RECORDS FOR MULTI-MEDIA                                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDMMD   NTR1  ,                                                                
         MVI   EOF,C'N'                                                         
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BNE   MMD00F              YES                                          
         LA    R6,MULTIMED                                                      
         B     MMD00X                                                           
*                                                                               
MMD00F   CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MFW?                         
         BNE   MMD00R              YES                                          
         LA    R6,MULTIMDF                                                      
         B     MMD00X                                                           
*                                                                               
MMD00R   CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BNE   MMD00D              YES                                          
         LA    R6,MULTIMDR                                                      
         B     MMD00X                                                           
*                                                                               
MMD00D   LA    R6,MULTIMDD         DDS                                          
*                                                                               
MMD00X   OPEN  ((R6),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,MMDBOX                                                        
*                                                                               
         LA    R0,RECSVL           CLEAR SAVE RECORD AREA                       
         LHI   R1,RECSVLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
MMD02    LA    R0,RECINL           CLEAR INPUT RECORD AREA                      
         LHI   R1,RECINLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GET   (R6),RECINL         GET INPUT AND TRANSLATE IT                   
*                                                                               
         TR    RECIN(CMDLQ),TRTAB                                               
*                                                                               
         L     RF,MULCTR           COUNT RECORD TYPE                            
         LA    RF,1(RF)                                                         
         ST    RF,MULCTR                                                        
*                                                                               
         LA    R0,RECIN                                                         
         LHI   R1,CMDLQ                                                         
         LA    RE,RECSV                                                         
         LHI   RF,CMDLQ                                                         
         CLCL  R0,RE               SEE IF THIS RECORD IS THE SAME               
         BE    MMD02                                                            
*                                                                               
         USING CMMD,RECSV                                                       
         OC    CMMCODE,CMMCODE                                                  
         BZ    MMD06               FIRST TIME - JUST SAVE RECORD                
*                                                                               
MMD04    LA    R0,RECOUTH          CLEAR OUTPUT RECORD AREA                     
         LHI   R1,RECOUTLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,MMDPRT                                                        
*                                                                               
         LA    R2,RECOUT                                                        
         USING CT99RECD,R2                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSMM   INSERT MULTIMEDIA TYPE                       
         MVI   CT99KSRC,CT99KOMS   MSTREET                                      
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BE    MMD05               YES                                          
         MVI   CT99KSRC,CT99KOMF   USE MEDIA FRAMEWORKS                         
         CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MEDIA FRAMEWORKS?            
         BE    MMD05               YES                                          
         MVI   CT99KSRC,CT99KOSD   USE SRDS                                     
         CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BE    MMD05               YES                                          
         MVI   CT99KSRC,CT99KODS   USE DDS                                      
MMD05    MVC   CT99KMMD,CMMCODE                                                 
*                                                                               
         LA    R3,CT99DATA                                                      
         USING CMMNAMD,R3                                                       
         MVI   CMMNMEL,CMMNMELQ                                                 
         MVI   CMMNMLN,CMMNMLNQ                                                 
         MVC   CMMNAME,CMMINAME    NAME                                         
*                                                                               
         AHI   R3,CMMNMLNQ                                                      
         MVI   0(R3),0             TERMINATE RECORD                             
         AHI   R3,1                                                             
*                                                                               
         SR    R3,R2                                                            
         STCM  R3,3,CT99LEN        SET LENGTH                                   
         AHI   R3,4                                                             
         STH   R3,RECOUTH          SET RECORD LENGTH FOR PUT (=LEN+4)           
         BRAS  RE,PUTRECO                                                       
*                                                                               
         L     RF,MULOUT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,MULOUT                                                        
                                                                                
*                                                                               
***      LA    R4,RECOUTH          A(O/P  RECORD)                               
***      SR    RF,RF                                                            
***      ICM   RF,3,RECOUTH                                                     
***      GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*                                                                               
         CLI   EOF,C'Y'                                                         
         BE    MMD08                                                            
*                                                                               
MMD06    LA    R0,RECIN                                                         
         LHI   R1,CMDLQ                                                         
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         MVCL  RE,R0               SAVE THIS RECORD                             
         B     MMD02                                                            
*                                                                               
MMD08    CLOSE (R6)                                                             
*                                                                               
MMD09A   MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
EODMMD   MVI   EOF,C'Y'                                                         
         B     MMD04                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PRINT LINE FROM MULTIMED DATA                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
MMDPRT   NTR1  ,                                                                
         USING MMDLINED,P                                                       
         USING CMMD,RECSV                                                       
         MVC   MMDCODE(L'CMMCODE),CMMCODE                                       
         MVC   MMDINAME,CMMINAME                                                
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***>>>                                                                          
***********************************************************************         
* BUILD OUTPUT RECORDS FOR FORMATS                                    *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDFRM   NTR1  ,                                                                
         MVI   EOF,C'N'                                                         
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BNE   FRM00F              YES                                          
         LA    R6,FORMAT                                                        
         B     FRM00X                                                           
*                                                                               
FRM00F   CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MFW?                         
         BNE   FRM00R              YES                                          
         LA    R6,FORMATF                                                       
         B     FRM00X                                                           
*                                                                               
FRM00R   CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BNE   FRM00D              YES                                          
         LA    R6,FORMATR                                                       
         B     FRM00X                                                           
*                                                                               
FRM00D   LA    R6,FORMATD          DDS                                          
*                                                                               
FRM00X   OPEN  ((R6),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,FRMBOX                                                        
*                                                                               
         LA    R0,RECSVL           CLEAR SAVE RECORD AREA                       
         LHI   R1,RECSVLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING CFD,RECSV                                                        
FRM02    LA    R0,RECINL           CLEAR INPUT RECORD AREA                      
         LHI   R1,RECINLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GET   (R6),RECINL         GET INPUT AND TRANSLATE IT                   
*                                                                               
         TR    RECIN(CFDLQ),TRTAB                                               
*                                                                               
         L     RF,FORCTR           COUNT RECORD TYPE                            
         LA    RF,1(RF)                                                         
         ST    RF,FORCTR                                                        
*                                                                               
         LA    R0,RECIN                                                         
         LHI   R1,RECINLQ                                                       
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         CLCL  R0,RE               SEE IF THIS RECORD IS THE SAME               
         BE    FRM02                                                            
*                                                                               
         OC    CFCODE,CFCODE                                                    
         BZ    FRM06               FIRST TIME - JUST SAVE RECORD                
*                                                                               
FRM04    LA    R0,RECOUTH          CLEAR OUTPUT RECORD AREA                     
         LHI   R1,RECOUTLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,FRMPRT                                                        
*                                                                               
         LA    R2,RECOUT                                                        
         USING CT99RECD,R2                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSFM                                                
*                                                                               
         MVI   CT99KSRC,CT99KOMS   MSTREET                                      
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BE    FRM05               YES                                          
         MVI   CT99KSRC,CT99KOMF   MEDIA FRAMEWORKS                             
         CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MEDIA FRAMEWORKS?            
         BE    FRM05               YES                                          
         MVI   CT99KSRC,CT99KOSD   SRDS                                         
         CLI   SOURCE,CT99KOSD     C'R'-SRDS?                                   
         BE    FRM05               YES                                          
         MVI   CT99KSRC,CT99KODS   DDS                                          
*                                                                               
FRM05    MVC   CT99KFME,CFMEDIA    INSERT MEDIA CODE                            
         MVC   CT99KMET,CFCODE                                                  
*                                                                               
         LA    R3,CT99DATA                                                      
         USING CFNAMD,R3                                                        
         MVI   CFNMEL,CFNMELQ                                                   
         MVI   CFNMLN,CFNMLNQ                                                   
         MVC   CFNAME,CFINAME      NAME                                         
*                                                                               
         AHI   R3,CFNMLNQ                                                       
         MVI   0(R3),0             TERMINATE RECORD                             
         AHI   R3,1                                                             
*                                                                               
         SR    R3,R2                                                            
         STCM  R3,3,CT99LEN        SET LENGTH                                   
         AHI   R3,4                                                             
         STH   R3,RECOUTH          SET RECORD LENGTH FOR PUT (=LEN+4)           
         BRAS  RE,PUTRECO                                                       
*                                                                               
         L     RF,FOROUT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,FOROUT                                                        
*                                                                               
         CLI   EOF,C'Y'                                                         
         BE    FRM08                                                            
*                                                                               
FRM06    LA    R0,RECIN                                                         
         LHI   R1,RECINLQ                                                       
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         MVCL  RE,R0               SAVE THIS RECORD                             
         B     FRM02                                                            
*                                                                               
FRM08    CLOSE (R6)                                                             
*                                                                               
FRM09A   MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
FRM10    EQU   *                                                                
         GOTO1 VPRINTER                                                         
         MVC   P+1(16),=C'FORMT/TV SKIPPED'                                     
         GOTO1 VPRINTER                                                         
         LA    R4,RECIN            A(I/P  RECORD)                               
         LA    RF,512                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
         B     FRM02               GO BACK FOR NEXT                             
*                                                                               
EODFRM   MVI   EOF,C'Y'                                                         
         B     FRM04                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PUT COLUMNS FOR BOX FOR FORMAT RECORDS                              *         
***********************************************************************         
         SPACE 1                                                                
FRMBOX   NTR1  ,                                                                
         ZAP   LINE,P99                                                         
         ZAP   PAGE,PONE                                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'FTITLE),FTITLE                                           
*                                                                               
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         XC    BOXINIT,BOXINIT                                                  
*                                                                               
B1       USING FRMLINED,BOXCOLS                                                 
         MVC   BOXCOLS,SPACES                                                   
         MVI   B1.FRMC1,C'L'                                                    
         MVI   B1.FRMC2,C'C'                                                    
         MVI   B1.FRMC3,C'C'                                                    
         MVI   B1.FRMC4,C'R'                                                    
         DROP  B1                                                               
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+2,C'T'                                                   
         MVI   BOXROWS+4,C'M'                                                   
         XR    RF,RF                                                            
         ICM   RF,3,MAXLINE                                                     
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'B'                                                       
*                                                                               
M1       USING FRMLINED,MID1                                                    
         MVC   MID1,SPACES                                                      
         MVC   M1.FRMMEDIA,=CL03'T/R'                                           
         MVC   M1.FRMCODE,=CL04'Code'                                           
         MVC   M1.FRMINAME,=CL36'Format Name'                                   
         B     EXITOK                                                           
         DROP  M1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PRINT LINE FROM FORMAT DATA                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
FRMPRT   NTR1  ,                                                                
         USING FRMLINED,P                                                       
         USING CFD,RECSV                                                        
         MVC   FRMMEDIA+1(1),CFMEDIA                                            
         MVC   FRMCODE(L'CFCODE),CFCODE                                         
         MVC   FRMINAME,CFINAME                                                 
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD OUTPUT RECORDS FOR REPS                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDREP   NTR1  ,                                                                
         MVI   EOF,C'N'                                                         
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BNE   REP00F              YES                                          
         LA    R6,REP                                                           
         B     REP00X                                                           
*                                                                               
REP00F   CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MFW?                         
         BNE   REP00R              YES                                          
         LA    R6,REPF                                                          
         B     REP00X                                                           
*                                                                               
REP00R   CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BNE   REP00D              YES                                          
         LA    R6,REPR                                                          
         B     REP00X                                                           
*                                                                               
REP00D   LA    R6,REPD             DDS                                          
*                                                                               
REP00X   OPEN  ((R6),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,REPBOX                                                        
*                                                                               
         LA    R0,RECSVL           CLEAR SAVE RECORD AREA                       
         LHI   R1,RECSVLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING CRED,RECSV                                                       
REP02    LA    R0,RECINL           CLEAR INPUT RECORD AREA                      
         LHI   R1,RECINLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GET   (R6),RECINL         GET INPUT AND TRANSLATE IT                   
*                                                                               
REP03A   TR    RECIN(CREDLQ),TRTAB                                              
*                                                                               
*&&DO                                                                           
         CLI   CRMEDIA-CRECODE+RECIN,C'T'  TELEVISION REP  ?                    
         BE    REP10               YES - SKIP IT                                
*                                                                               
         MVC   P+1(19),=C'REP     RECORD READ'                                  
         GOTO1 VPRINTER                                                         
         LA    R4,RECSV            A(I/P  RECORD)                               
         LA    RF,512                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
*&&                                                                             
*                                                                               
         L     RF,REPCTR           COUNT RECORD TYPE                            
         LA    RF,1(RF)                                                         
         ST    RF,REPCTR                                                        
*                                                                               
         LA    R0,RECIN                                                         
         LHI   R1,RECINLQ                                                       
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         CLCL  R0,RE               SEE IF THIS RECORD IS THE SAME               
         BE    REP02                                                            
*                                                                               
         OC    CRECODE,CRECODE                                                  
         BZ    REP06               FIRST TIME - JUST SAVE RECORD                
*                                                                               
REP04    LA    R0,RECOUTH          CLEAR OUTPUT RECORD AREA                     
         LHI   R1,RECOUTLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,REPPRT                                                        
*                                                                               
         LA    R2,RECOUT                                                        
         USING CT99RECD,R2                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSRE                                                
*                                                                               
         MVI   CT99KSRC,CT99KOMS   MSTREET                                      
         CLI   SOURCE,CT99KOMS     PROCESSING MSTREET?                          
         BE    REP05               YES                                          
         MVI   CT99KSRC,CT99KOMF   MEDIA FRAMEWORKS                             
         CLI   SOURCE,CT99KOMF     PROCESSING MEDIA FRAMEWORKS?                 
         BE    REP05               YES                                          
         MVI   CT99KSRC,CT99KOSD   SRDS                                         
         CLI   SOURCE,CT99KOSD     PROCESSING SRDS?                             
         BE    REP05               YES                                          
         MVI   CT99KSRC,CT99KODS   DDS                                          
*                                                                               
REP05    MVC   CT99KRME,CRMEDIA    INSERT MEDIA CODE                            
         MVC   CT99KREP,CRECODE                                                 
*                                                                               
         LA    R3,CT99DATA                                                      
         USING CRENAMD,R3                                                       
         MVI   CREMEL,CREMELQ                                                   
         MVI   CREMLN,CREMLNQ                                                   
         MVC   CREAME,CREINAME     NAME                                         
*                                                                               
         AHI   R3,CREMLNQ                                                       
         MVI   0(R3),0             TERMINATE RECORD                             
         AHI   R3,1                                                             
*                                                                               
         SR    R3,R2                                                            
         STCM  R3,3,CT99LEN        SET LENGTH                                   
         AHI   R3,4                                                             
         STH   R3,RECOUTH          SET RECORD LENGTH FOR PUT (=LEN+4)           
         BRAS  RE,PUTRECO                                                       
*                                                                               
         L     RF,REPOUT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,REPOUT                                                        
                                                                                
*                                                                               
         CLI   EOF,C'Y'                                                         
         BE    REP08                                                            
*                                                                               
REP06    LA    R0,RECIN                                                         
         LHI   R1,RECINLQ                                                       
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         MVCL  RE,R0               SAVE THIS RECORD                             
         B     REP02                                                            
*                                                                               
REP08    CLOSE (R6)                                                             
*                                                                               
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
REP10    EQU   *                                                                
         GOTO1 VPRINTER                                                         
         MVC   P+1(16),=C'REP  /TV SKIPPED'                                     
         GOTO1 VPRINTER                                                         
         LA    R4,RECIN            A(I/P  RECORD)                               
         LA    RF,512                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
         B     REP02               GO BACK FOR NEXT                             
*                                                                               
EODREP   MVI   EOF,C'Y'                                                         
         B     REP04                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PUT COLUMNS FOR BOX FOR REP RECORDS                                 *         
***********************************************************************         
         SPACE 1                                                                
REPBOX   NTR1  ,                                                                
         ZAP   LINE,P99                                                         
         ZAP   PAGE,PONE                                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'PTITLE),PTITLE                                           
*                                                                               
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         XC    BOXINIT,BOXINIT                                                  
*                                                                               
B1       USING REPLINED,BOXCOLS                                                 
         MVC   BOXCOLS,SPACES                                                   
         MVI   B1.REPC1,C'L'                                                    
         MVI   B1.REPC2,C'C'                                                    
         MVI   B1.REPC3,C'C'                                                    
         MVI   B1.REPC4,C'R'                                                    
         DROP  B1                                                               
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+2,C'T'                                                   
         MVI   BOXROWS+4,C'M'                                                   
         XR    RF,RF                                                            
         ICM   RF,3,MAXLINE                                                     
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'B'                                                       
*                                                                               
M1       USING REPLINED,MID1                                                    
         MVC   MID1,SPACES                                                      
         MVC   M1.REPMEDIA,=CL03'T/R'                                           
         MVC   M1.REPCODE,=CL05'Code'                                           
         MVC   M1.REPINAME,=CL36'Rep Name'                                      
         B     EXITOK                                                           
         DROP  M1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PRINT LINE FROM REP DATA                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
REPPRT   NTR1  ,                                                                
         USING REPLINED,P                                                       
         USING CRED,RECSV                                                       
         MVC   REPMEDIA+1(1),CRMEDIA                                            
         MVC   REPCODE(L'CRECODE),CRECODE                                       
         MVC   REPINAME,CREINAME                                                
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         MVI   SORTREC,X'FF'                                                    
         MVC   SORTREC+1(31),SORTREC                                            
*                                  SET INITIAL KEY TO X'FF' FOR 32              
*                                                                               
         CLI   SOURCE,CT99KOMS     MSTREET?                                     
         BNE   INIT10              YES                                          
         OPEN  (TAPEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
INIT10   CLI   SOURCE,CT99KOMF     MFW?                                         
         BNE   INIT20              YES                                          
         OPEN  (TAPEOUT2,OUTPUT)                                                
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
INIT20   CLI   SOURCE,CT99KOSD     SRDS?                                        
         BNE   INIT30              YES                                          
         OPEN  (TAPEOUT3,OUTPUT)                                                
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
INIT30   CLI   SOURCE,CT99KODS     DDS?                                         
         JNE   *+2                 YES                                          
         OPEN  (TAPEOUT4,OUTPUT)                                                
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
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
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    CLOSE SYSPRINT                                                         
         XBASE ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
DATCON   DC    V(DATCON)                                                        
HEXOUT   DC    V(HEXOUT)                                                        
CASHVAL  DC    V(CASHVAL)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VBOXAREA DC    V(BOXAREA)                                                       
*                                                                               
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
P99      DC    P'99'                                                            
*                                                                               
RTITLE   DC    C'Radio records (9900 and 9A)'                                   
MTITLE   DC    C'Metro records (9901)'                                          
OTITLE   DC    C'Owner records (9902)'                                          
PTITLE   DC    C'Rep records (9903)'                                            
FTITLE   DC    C'Format records (9904)'                                         
MMTITLE  DC    C'Multimed records (9905)'                                       
*                                                                               
WORK     DS    CL64                                                             
*                                                                               
RADCTR   DS    F                                                                
BADKEY   DS    F                                                                
BLANKCTR DS    F                                                                
METCTR   DS    F                                                                
OWNCTR   DS    F                                                                
REPCTR   DS    F                                                                
FORCTR   DS    F                                                                
MULCTR   DS    F                                                                
RADOUT   DS    F                                                                
RADOUT2  DS    F                                                                
RADSKPD  DS    F                                                                
SRVCSKPD DS    F                                                                
METOUT   DS    F                                                                
OWNOUT   DS    F                                                                
REPOUT   DS    F                                                                
FOROUT   DS    F                                                                
MULOUT   DS    F                                                                
SORTCTR  DS    F                                                                
SORTCTR2 DS    F                                                                
*                                                                               
SAVCRUID DS    CL6                                                              
SAVCALL  DS    CL4                                                              
SAVBAND  DS    CL1                                                              
CLEARWRK EQU   *-RADCTR                                                         
SOURCE   DS    CL1                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TRANSLATE TABLE FOR INPUT DATA                                      *         
***********************************************************************         
         SPACE 1                                                                
TRTAB    DC    XL16'40404040404040404040404040404040' 00-0F                     
         DC    XL16'40404040404040404040404040404040' 10-1F                     
         DC    XL16'40404040404040404040404040404040' 20-2F                     
         DC    XL16'40404040404040404040404040404040' 30-3F                     
         DC    XL16'404040404040404040404A4B4C4D4E4F' 40-4F                     
         DC    XL16'504040404040404040405A5B5C5D5E5F' 50-5F                     
         DC    XL16'606140404040404040406A6B6C6D6E6F' 60-6F                     
         DC    XL16'404040404040404040407A7B7C7D7E7F' 70-7F                     
         DC    XL16'40818283848586878889404040404040' 80-8F                     
         DC    XL16'40919293949596979899404040404040' 90-9F                     
         DC    XL16'40A1A2A3A4A5A6A7A8A9404040404040' A0-AF                     
         DC    XL16'40404040404040404040404040404040' B0-BF                     
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404040404040' C0-CF                     
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040' D0-DF                     
         DC    XL16'E0E1E2E3E4E5E6E7E8E9404040404040' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF' F0-FF                     
         EJECT                                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,25,A),FORMAT=BI,WORK=1'                      
***  LENGTH USED TO BE 222  ***                                                 
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=537'                                   
         DS    0D                                                               
         DC    CL8'*RECSAVE'                                                    
RECSVL   DS    F                                                                
**RECSV    DS    512C                                                           
RECSV    DS    621C                                                             
RECSVLQ  EQU   *-RECSV                                                          
         DS    0D                                                               
         DC    CL8'*RECOUT*'                                                    
RECOUTH  DS    F                                                                
**RECOUT   DS    512C                                                           
RECOUT   DS    537C                                                             
RECOUTLQ EQU   *-RECOUTH                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'**RECIN*'                                                    
RECINL   DS    F                                                                
**RECIN    DS    512C                                                           
RECIN    DS    621C                                                             
RECINLQ  EQU   *-RECIN                                                          
*                                                                               
**SORTREC  DS    CL222                                                          
**SVTAPOUT DS    CL222                                                          
SORTREC  DS    CL537                                                            
SVTAPOUT DS    CL537                                                            
*                                                                               
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
         SPACE 1                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
**DIO    DCB   DSORG=PS,MACRF=GM,DDNAME=RADIO,RECFM=VB,LRECL=320,               
**             BLKSIZE=6233,EODAD=BRAD0480                                      
RADIO    DCB   DSORG=PS,MACRF=GM,DDNAME=RADIO,RECFM=VB,LRECL=625,      +        
               BLKSIZE=6233,EODAD=BRAD0480                                      
*                                                                               
METRO    DCB   DSORG=PS,MACRF=GM,DDNAME=METRO,RECFM=VB,LRECL=255,      +        
               BLKSIZE=6233,EODAD=EODMET                                        
*                                                                               
OWNER    DCB   DSORG=PS,MACRF=GM,DDNAME=OWNER,RECFM=VB,LRECL=255,      +        
               BLKSIZE=6233,EODAD=EODOWN                                        
*                                                                               
REP      DCB   DSORG=PS,MACRF=GM,DDNAME=REP,RECFM=VB,LRECL=255,        +        
               BLKSIZE=6233,EODAD=EODREP                                        
*                                                                               
FORMAT   DCB   DSORG=PS,MACRF=GM,DDNAME=FORMAT,RECFM=VB,LRECL=255,     +        
               BLKSIZE=6233,EODAD=EODFRM                                        
*                                                                               
MULTIMED DCB   DSORG=PS,MACRF=GM,DDNAME=MULTIMED,RECFM=VB,LRECL=255,   +        
               BLKSIZE=6233,EODAD=EODMMD                                        
*                                                                               
TAPEOUT  DCB   DSORG=PS,MACRF=PM,DDNAME=TAPEOUT,RECFM=VB,LRECL=2048,   +        
               BLKSIZE=25000                                                    
*                                                                               
RADIOF   DCB   DSORG=PS,MACRF=GM,DDNAME=RADIOF,RECFM=VB,LRECL=625,     +        
               BLKSIZE=6233,EODAD=BRAD0480                                      
*                                                                               
METROF   DCB   DSORG=PS,MACRF=GM,DDNAME=METROF,RECFM=VB,LRECL=255,     +        
               BLKSIZE=6233,EODAD=EODMET                                        
*                                                                               
OWNERF   DCB   DSORG=PS,MACRF=GM,DDNAME=OWNERF,RECFM=VB,LRECL=255,     +        
               BLKSIZE=6233,EODAD=EODOWN                                        
*                                                                               
REPF     DCB   DSORG=PS,MACRF=GM,DDNAME=REPF,RECFM=VB,LRECL=255,       +        
               BLKSIZE=6233,EODAD=EODREP                                        
*                                                                               
FORMATF  DCB   DSORG=PS,MACRF=GM,DDNAME=FORMATF,RECFM=VB,LRECL=255,    +        
               BLKSIZE=6233,EODAD=EODFRM                                        
*                                                                               
MULTIMDF DCB   DSORG=PS,MACRF=GM,DDNAME=MULTIMDF,RECFM=VB,LRECL=255,   +        
               BLKSIZE=6233,EODAD=EODMMD                                        
*                                                                               
TAPEOUT2 DCB   DSORG=PS,MACRF=PM,DDNAME=TAPEOUT2,RECFM=VB,LRECL=2048,  +        
               BLKSIZE=25000                                                    
*                                                                               
RADIOR   DCB   DSORG=PS,MACRF=GM,DDNAME=RADIOR,RECFM=VB,LRECL=625,     +        
               BLKSIZE=6233,EODAD=BRAD0480                                      
*                                                                               
METROR   DCB   DSORG=PS,MACRF=GM,DDNAME=METROR,RECFM=VB,LRECL=255,     +        
               BLKSIZE=6233,EODAD=EODMET                                        
*                                                                               
OWNERR   DCB   DSORG=PS,MACRF=GM,DDNAME=OWNERR,RECFM=VB,LRECL=255,     +        
               BLKSIZE=6233,EODAD=EODOWN                                        
*                                                                               
REPR     DCB   DSORG=PS,MACRF=GM,DDNAME=REPR,RECFM=VB,LRECL=255,       +        
               BLKSIZE=6233,EODAD=EODREP                                        
*                                                                               
FORMATR  DCB   DSORG=PS,MACRF=GM,DDNAME=FORMATR,RECFM=VB,LRECL=255,    +        
               BLKSIZE=6233,EODAD=EODFRM                                        
*                                                                               
MULTIMDR DCB   DSORG=PS,MACRF=GM,DDNAME=MULTIMDR,RECFM=VB,LRECL=255,   +        
               BLKSIZE=6233,EODAD=EODMMD                                        
*                                                                               
TAPEOUT3 DCB   DSORG=PS,MACRF=PM,DDNAME=TAPEOUT3,RECFM=VB,LRECL=2048,  +        
               BLKSIZE=25000                                                    
*                                                                               
RADIOD   DCB   DSORG=PS,MACRF=GM,DDNAME=RADIOD,RECFM=VB,LRECL=625,     +        
               BLKSIZE=6233,EODAD=BRAD0480                                      
*                                                                               
METROD   DCB   DSORG=PS,MACRF=GM,DDNAME=METROD,RECFM=VB,LRECL=255,     +        
               BLKSIZE=6233,EODAD=EODMET                                        
*                                                                               
OWNERD   DCB   DSORG=PS,MACRF=GM,DDNAME=OWNERD,RECFM=VB,LRECL=255,     +        
               BLKSIZE=6233,EODAD=EODOWN                                        
*                                                                               
REPD     DCB   DSORG=PS,MACRF=GM,DDNAME=REPD,RECFM=VB,LRECL=255,       +        
               BLKSIZE=6233,EODAD=EODREP                                        
*                                                                               
FORMATD  DCB   DSORG=PS,MACRF=GM,DDNAME=FORMATD,RECFM=VB,LRECL=255,    +        
               BLKSIZE=6233,EODAD=EODFRM                                        
*                                                                               
MULTIMDD DCB   DSORG=PS,MACRF=GM,DDNAME=MULTIMDD,RECFM=VB,LRECL=255,   +        
               BLKSIZE=6233,EODAD=EODMMD                                        
*                                                                               
TAPEOUT4 DCB   DSORG=PS,MACRF=PM,DDNAME=TAPEOUT4,RECFM=VB,LRECL=2048,  +        
               BLKSIZE=25000                                                    
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PUT COLUMNS FOR BOX FOR MULTIMED RECORDS                            *         
***********************************************************************         
         SPACE 1                                                                
MMDBOX   NTR1  BASE=*,LABEL=*                                                   
         ZAP   LINE,P99                                                         
         ZAP   PAGE,PONE                                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'MMTITLE),MMTITLE                                         
*                                                                               
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         XC    BOXINIT,BOXINIT                                                  
*                                                                               
B1       USING MMDLINED,BOXCOLS                                                 
         MVC   BOXCOLS,SPACES                                                   
         MVI   B1.MMDC1,C'L'                                                    
         MVI   B1.MMDC2,C'C'                                                    
         MVI   B1.MMDC3,C'R'                                                    
         DROP  B1                                                               
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+2,C'T'                                                   
         MVI   BOXROWS+4,C'M'                                                   
         XR    RF,RF                                                            
         ICM   RF,3,MAXLINE                                                     
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'B'                                                       
*                                                                               
M1       USING MMDLINED,MID1                                                    
         MVC   MID1,SPACES                                                      
         MVC   M1.MMDCODE,=CL05'CODE '                                          
         MVC   M1.MMDINAME,=CL36'MULTIMED Name'                                 
         J     EXITOK                                                           
         DROP  M1                                                               
         EJECT                                                                  
***********************************************************************         
* PUT RECORD OUT TO THE TAPE                                          *         
***********************************************************************         
PUTRECO  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TAPEOUT                                                       
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BE    PUTRECO5                                                         
         LA    R2,TAPEOUT2                                                      
         CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MFW?                         
         BE    PUTRECO5                                                         
         LA    R2,TAPEOUT3                                                      
         CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BE    PUTRECO5                                                         
         LA    R2,TAPEOUT4         C'D'-PROCESSING DDS                          
PUTRECO5 PUT   0(R2),RECOUTH                                                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD OUTPUT RECORDS FOR OWNERS                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDOWN   NTR1  BASE=*,LABEL=*                                                   
         MVI   EOF,C'N'                                                         
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BNE   OWN00F              YES                                          
         LA    R6,OWNER                                                         
         B     OWN00X                                                           
*                                                                               
OWN00F   CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MFW?                         
         BNE   OWN00R              YES                                          
         LA    R6,OWNERF                                                        
         B     OWN00X                                                           
*                                                                               
OWN00R   CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BNE   OWN00D              YES                                          
         LA    R6,OWNERR                                                        
         B     OWN00X                                                           
*                                                                               
OWN00D   LA    R6,OWNERD           DDS                                          
*                                                                               
OWN00X   OPEN  ((R6),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,OWNBOX                                                        
*                                                                               
         LA    R0,RECSVL           CLEAR SAVE RECORD AREA                       
         LHI   R1,RECSVLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING COD,RECSV                                                        
OWN02    LA    R0,RECINL           CLEAR INPUT RECORD AREA                      
         LHI   R1,RECINLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GET   (R6),RECINL         GET INPUT AND TRANSLATE IT                   
*                                                                               
         TR    RECIN(CODLQ),TRTAB                                               
*                                                                               
         L     RF,OWNCTR           COUNT RECORD TYPE                            
         LA    RF,1(RF)                                                         
         ST    RF,OWNCTR                                                        
*                                                                               
         LA    R0,RECIN                                                         
         LHI   R1,RECINLQ                                                       
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         CLCL  R0,RE               SEE IF THIS RECORD IS THE SAME               
         BE    OWN02                                                            
*                                                                               
         OC    COCODE,COCODE                                                    
         BZ    OWN06               FIRST TIME - JUST SAVE RECORD                
*                                                                               
OWN04    LA    R0,RECOUTH          CLEAR OUTPUT RECORD AREA                     
         LHI   R1,RECOUTLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,OWNPRT                                                        
*                                                                               
         LA    R2,RECOUT                                                        
         USING CT99RECD,R2                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSOW                                                
*                                                                               
         MVI   CT99KSRC,CT99KOMS   MSTREET                                      
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BE    OWN05               NO                                           
         MVI   CT99KSRC,CT99KOMF   YES - USE MEDIA FRAMEWORKS                   
         CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MEDIA FRAMEWORKS?            
         BE    OWN05               NO                                           
         MVI   CT99KSRC,CT99KOSD   SRDS                                         
         CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BE    OWN05               NO                                           
         MVI   CT99KSRC,CT99KODS   DDS                                          
*                                                                               
OWN05    MVC   CT99KOME,COMEDIA    INSERT MEDIA CODE                            
         MVC   CT99KOWN,COCODE                                                  
*                                                                               
         LA    R3,CT99DATA                                                      
         USING CONAMD,R3                                                        
         MVI   CONMEL,CONMELQ                                                   
         MVI   CONMLN,CONMLNQ                                                   
         MVC   CONAME,COINAME      NAME                                         
*                                                                               
         AHI   R3,CONMLNQ                                                       
         MVI   0(R3),0             TERMINATE RECORD                             
         AHI   R3,1                                                             
*                                                                               
         SR    R3,R2                                                            
         STCM  R3,3,CT99LEN        SET LENGTH                                   
         AHI   R3,4                                                             
         STH   R3,RECOUTH          SET RECORD LENGTH FOR PUT (=LEN+4)           
         BRAS  RE,PUTRECO                                                       
*                                                                               
         L     RF,OWNOUT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,OWNOUT                                                        
*                                                                               
         CLI   EOF,C'Y'                                                         
         BE    OWN08                                                            
*                                                                               
OWN06    LA    R0,RECIN                                                         
         LHI   R1,RECINLQ                                                       
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         MVCL  RE,R0               SAVE THIS RECORD                             
         B     OWN02                                                            
*                                                                               
OWN08    CLOSE (R6)                                                             
*                                                                               
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
OWN10    EQU   *                                                                
         GOTO1 VPRINTER                                                         
         MVC   P+1(16),=C'OWNER/TV SKIPPED'                                     
         GOTO1 VPRINTER                                                         
         LA    R4,RECIN            A(I/P  RECORD)                               
         LA    RF,512                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
         B     OWN02               GO BACK FOR NEXT                             
*                                                                               
EODOWN   MVI   EOF,C'Y'                                                         
         B     OWN04                                                            
         EJECT                                                                  
***********************************************************************         
* PUT COLUMNS FOR BOX FOR OWNER RECORDS                               *         
***********************************************************************         
         SPACE 1                                                                
OWNBOX   NTR1  ,                                                                
         ZAP   LINE,P99                                                         
         ZAP   PAGE,PONE                                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'OTITLE),OTITLE                                           
*                                                                               
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         XC    BOXINIT,BOXINIT                                                  
*                                                                               
B1       USING OWNLINED,BOXCOLS                                                 
         MVC   BOXCOLS,SPACES                                                   
         MVI   B1.OWNC1,C'L'                                                    
         MVI   B1.OWNC2,C'C'                                                    
         MVI   B1.OWNC3,C'C'                                                    
         MVI   B1.OWNC4,C'R'                                                    
         DROP  B1                                                               
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+2,C'T'                                                   
         MVI   BOXROWS+4,C'M'                                                   
         XR    RF,RF                                                            
         ICM   RF,3,MAXLINE                                                     
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'B'                                                       
*                                                                               
M1       USING OWNLINED,MID1                                                    
         MVC   MID1,SPACES                                                      
         MVC   M1.OWNMEDIA,=CL03'T/R'                                           
         MVC   M1.OWNCODE,=CL05'Code'                                           
         MVC   M1.OWNINAME,=CL36'Owner Name'                                    
         B     EXITOK                                                           
         DROP  M1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PRINT LINE FROM OWNER DATA                        *         
***********************************************************************         
         SPACE 1                                                                
OWNPRT   NTR1  ,                                                                
         USING OWNLINED,P                                                       
         USING COD,RECSV                                                        
         MVC   OWNMEDIA+1(1),COMEDIA                                            
         MVC   OWNCODE(L'COCODE),COCODE                                         
         MVC   OWNINAME,COINAME                                                 
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         POP   USING                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUILD OUTPUT RECORDS FOR METROS                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDMET   NTR1  BASE=*,LABEL=*                                                   
         MVI   EOF,C'N'                                                         
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BNE   MET00F                                                           
         LA    R6,METRO            YES                                          
         B     MET00X                                                           
*                                                                               
MET00F   CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MFW?                         
         BNE   MET00R                                                           
         LA    R6,METROF           YES                                          
         B     MET00X                                                           
*                                                                               
MET00R   CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BNE   MET00D                                                           
         LA    R6,METROR           YES                                          
         B     MET00X                                                           
*                                                                               
MET00D   LA    R6,METROD           DDS                                          
*                                                                               
MET00X   OPEN  ((R6),INPUT)                                                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,METBOX                                                        
*                                                                               
         LA    R0,RECSVL           CLEAR SAVE RECORD AREA                       
         LHI   R1,RECSVLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
MET02    LA    R0,RECINL           CLEAR INPUT RECORD AREA                      
         LHI   R1,RECINLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GET   (R6),RECINL         GET INPUT AND TRANSLATE IT                   
*                                                                               
         TR    RECIN(CMDLQ),TRTAB                                               
*                                                                               
         L     RF,METCTR           COUNT RECORD TYPE                            
         LA    RF,1(RF)                                                         
         ST    RF,METCTR                                                        
*                                                                               
         LA    R0,RECIN                                                         
         LHI   R1,CMDLQ                                                         
         LA    RE,RECSV                                                         
         LHI   RF,CMDLQ                                                         
         CLCL  R0,RE               SEE IF THIS RECORD IS THE SAME               
         BE    MET02                                                            
*                                                                               
         USING CMD,RECSV                                                        
         OC    CMCODE,CMCODE                                                    
         BZ    MET06               FIRST TIME - JUST SAVE RECORD                
*                                                                               
MET04    LA    R0,RECOUTH          CLEAR OUTPUT RECORD AREA                     
         LHI   R1,RECOUTLQ                                                      
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,METPRT                                                        
*                                                                               
         LA    R2,RECOUT                                                        
         USING CT99RECD,R2                                                      
         MVI   CT99KTYP,CT99KTYQ                                                
         MVI   CT99KSUB,CT99KSME                                                
*                                                                               
         MVI   CT99KSRC,CT99KOMS   MSTREET                                      
         CLI   SOURCE,CT99KOMS     C'S'-PROCESSING MSTREET?                     
         BE    MET05               YES                                          
         MVI   CT99KSRC,CT99KOMF   MFW                                          
         CLI   SOURCE,CT99KOMF     C'F'-PROCESSING MFW?                         
         BE    MET05               YES                                          
         MVI   CT99KSRC,CT99KOSD   SRDS                                         
         CLI   SOURCE,CT99KOSD     C'R'-PROCESSING SRDS?                        
         BE    MET05               YES                                          
         MVI   CT99KSRC,CT99KODS   DDS                                          
*                                                                               
MET05    MVC   CT99KMME,CMMEDIA    INSERT MEDIA CODE                            
         MVC   CT99KMET,CMCODE                                                  
*                                                                               
         LA    R3,CT99DATA                                                      
         USING CMNAMD,R3                                                        
         MVI   CMNMEL,CMNMELQ                                                   
         MVI   CMNMLN,CMNMLNQ                                                   
         MVC   CMNAME,CMINAME      NAME                                         
*                                                                               
         AHI   R3,CMNMLNQ                                                       
         MVI   0(R3),0             TERMINATE RECORD                             
         AHI   R3,1                                                             
*                                                                               
         SR    R3,R2                                                            
         STCM  R3,3,CT99LEN        SET LENGTH                                   
         AHI   R3,4                                                             
         STH   R3,RECOUTH          SET RECORD LENGTH FOR PUT (=LEN+4)           
         BRAS  RE,PUTRECO                                                       
*                                                                               
         L     RF,METOUT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,METOUT                                                        
*                                                                               
         CLI   EOF,C'Y'                                                         
         BE    MET08                                                            
*                                                                               
MET06    LA    R0,RECIN                                                         
         LHI   R1,CMDLQ                                                         
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         MVCL  RE,R0               SAVE THIS RECORD                             
         B     MET02                                                            
*                                                                               
MET08    CLOSE (R6)                                                             
*                                                                               
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
MET10    EQU   *                                                                
         GOTO1 VPRINTER                                                         
         MVC   P+1(16),=C'MET  /TV SKIPPED'                                     
         GOTO1 VPRINTER                                                         
         LA    R4,RECIN            A(I/P  RECORD)                               
         LA    RF,512                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
         B     MET02               GO BACK FOR NEXT                             
*                                                                               
EODMET   MVI   EOF,C'Y'                                                         
         B     MET04                                                            
         EJECT                                                                  
***********************************************************************         
* PUT COLUMNS FOR BOX FOR METRO RECORDS                               *         
***********************************************************************         
         SPACE 1                                                                
METBOX   NTR1  ,                                                                
         ZAP   LINE,P99                                                         
         ZAP   PAGE,PONE                                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'MTITLE),MTITLE                                           
*                                                                               
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         XC    BOXINIT,BOXINIT                                                  
*                                                                               
B1       USING METLINED,BOXCOLS                                                 
         MVC   BOXCOLS,SPACES                                                   
         MVI   B1.METC1,C'L'                                                    
         MVI   B1.METC2,C'C'                                                    
         MVI   B1.METC3,C'C'                                                    
         MVI   B1.METC4,C'R'                                                    
         DROP  B1                                                               
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+2,C'T'                                                   
         MVI   BOXROWS+4,C'M'                                                   
         XR    RF,RF                                                            
         ICM   RF,3,MAXLINE                                                     
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'B'                                                       
*                                                                               
M1       USING METLINED,MID1                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVC   M1.METMEDIA,=CL03'T/R'                                           
         MVC   M1.METCODE,=CL04'Code'                                           
         MVC   M1.METINAME,=CL36'Metro Name'                                    
         B     EXITOK                                                           
         DROP  M1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PRINT LINE FROM METRO DATA                        *         
***********************************************************************         
         SPACE 1                                                                
METPRT   NTR1  ,                                                                
         USING METLINED,P                                                       
         USING CMD,RECSV                                                        
         MVC   METMEDIA+1(1),CMMEDIA                                            
         MVC   METCODE(L'CMCODE),CMCODE                                         
         MVC   METINAME,CMINAME                                                 
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         POP   USING                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
*                                                                               
DMCB     DS    6F                                                               
*                                                                               
CARD     DS    CL80                                                             
EOF      DS    X                                                                
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* INCOMING RADIO FILE DSECT                                           *         
***********************************************************************         
       ++INCLUDE CTGENMST                                                       
***********************************************************************         
* PRINT LINE FOR RADIO INPUT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
RADLINED DSECT                                                                  
RADC1    DS    C                                                                
         DS    C                                                                
RADUID   DS    CL6                                                              
         DS    C                                                                
RADC2    DS    C                                                                
         DS    C                                                                
RADCALL  DS    CL5                                                              
         DS    C                                                                
RADC3    DS    C                                                                
         DS    C                                                                
RADBND   DS    C                                                                
         DS    C                                                                
RADC4    DS    C                                                                
         DS    C                                                                
RADFREQ  DS    CL5                                                              
         DS    C                                                                
RADC5    DS    C                                                                
         DS    C                                                                
RADLCITY DS    CL24                                                             
         DS    C                                                                
RADC6    DS    C                                                                
         DS    C                                                                
RADSTATE DS    CL2                                                              
         DS    C                                                                
RADC7    DS    C                                                                
         DS    C                                                                
RADFMT   DS    CL3                                                              
         DS    C                                                                
RADC8    DS    C                                                                
         DS    C                                                                
RADOWNER DS    CL5                                                              
         DS    C                                                                
RADC9    DS    C                                                                
         DS    C                                                                
RADREP1  DS    CL4                                                              
         DS    C                                                                
RADC10   DS    C                                                                
         DS    C                                                                
RADREP2  DS    CL4                                                              
         DS    C                                                                
RADC11   DS    C                                                                
         DS    C                                                                
RADAMC   DS    CL3                                                              
         DS    C                                                                
RADC12   DS    C                                                                
         DS    C                                                                
RADTYPE  DS    CL5                                                              
         DS    C                                                                
RADC13   DS    C                                                                
         DS    C                                                                
RADTDATA DS    CL7                                                              
         DS    C                                                                
RADC14   DS    C                                                                
         DS    C                                                                
RADDATE  DS    CL8                                                              
         DS    C                                                                
RADC15   DS    C                                                                
         DS    C                                                                
RADORUID DS    CL6                                                              
         DS    C                                                                
RADPARNT DS    CL5                                                              
LRADLINE EQU   *-RADLINED                                                       
         EJECT                                                                  
***********************************************************************         
* PRINT LINE FOR MULTIMEDIA INPUT RECORD                              *         
***********************************************************************         
MMDLINED DSECT                                                                  
MMDC1    DS    C                                                                
         DS    C                                                                
MMDCODE  DS    CL5                                                              
         DS    C                                                                
MMDC2    DS    C                                                                
         DS    C                                                                
MMDINAME DS    CL36                                                             
         DS    C                                                                
MMDC3    DS    C                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT LINE FOR METRO INPUT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
METLINED DSECT                                                                  
METC1    DS    C                                                                
         DS    C                                                                
METMEDIA DS    CL3                                                              
         DS    C                                                                
METC2    DS    C                                                                
         DS    C                                                                
METCODE  DS    CL4                                                              
         DS    C                                                                
METC3    DS    C                                                                
         DS    C                                                                
METINAME DS    CL36                                                             
         DS    C                                                                
METC4    DS    C                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT LINE FOR FORMAT INPUT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
FRMLINED DSECT                                                                  
FRMC1    DS    C                                                                
         DS    C                                                                
FRMMEDIA DS    CL3                                                              
         DS    C                                                                
FRMC2    DS    C                                                                
         DS    C                                                                
FRMCODE  DS    CL4                                                              
         DS    C                                                                
FRMC3    DS    C                                                                
         DS    C                                                                
FRMINAME DS    CL36                                                             
         DS    C                                                                
FRMC4    DS    C                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT LINE FOR OWNER INPUT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
OWNLINED DSECT                                                                  
OWNC1    DS    C                                                                
         DS    C                                                                
OWNMEDIA DS    CL3                                                              
         DS    C                                                                
OWNC2    DS    C                                                                
         DS    C                                                                
OWNCODE  DS    CL5                                                              
         DS    C                                                                
OWNC3    DS    C                                                                
         DS    C                                                                
OWNINAME DS    CL36                                                             
         DS    C                                                                
OWNC4    DS    C                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT LINE FOR REP INPUT RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
REPLINED DSECT                                                                  
REPC1    DS    C                                                                
         DS    C                                                                
REPMEDIA DS    CL3                                                              
         DS    C                                                                
REPC2    DS    C                                                                
         DS    C                                                                
REPCODE  DS    CL4                                                              
         DS    C                                                                
REPC3    DS    C                                                                
         DS    C                                                                
REPINAME DS    CL36                                                             
         DS    C                                                                
REPC4    DS    C                                                                
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* CTGENRAD                                                                      
       ++INCLUDE CTGENRAD                                                       
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE CHAIN                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKAREA CSECT                                                                  
         DC    60000X'00'                                                       
***********************************************************************         
* DISPLAY TOTAL RECORDS PROCESSED                                     *         
***********************************************************************         
         SPACE 1                                                                
DISPTOTS NTR1  LABEL=*,BASE=*                                                   
         ZAP   LINE,P99                                                         
         ZAP   PAGE,PONE                                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(13),=C'RECORD TOTALS'                                      
*                                                                               
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
         XC    BOXINIT,BOXINIT                                                  
*                                                                               
B1       USING MMDLINED,BOXCOLS                                                 
         MVC   BOXCOLS,SPACES                                                   
         MVI   B1.MMDC1,C'L'                                                    
         MVI   B1.MMDC2,C'C'                                                    
         MVI   B1.MMDC3,C'R'                                                    
         DROP  B1                                                               
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+2,C'T'                                                   
         MVI   BOXROWS+4,C'M'                                                   
         XR    RF,RF                                                            
         ICM   RF,3,MAXLINE                                                     
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'B'                                                       
*                                                                               
M1       USING MMDLINED,MID1                                                    
         MVC   MID1,SPACES                                                      
         MVC   M1.MMDCODE,=CL05'TYPE '                                          
         MVC   M1.MMDINAME,=CL36'TOTALS'                                        
         DROP  M1                                                               
         MVC   P+1(30),=C'RADIO     RECORDS IN/OUT     :'                       
         EDIT  RADCTR,(6,P+32)                                                  
         MVI   P+38,C'/'                                                        
         EDIT  RADOUT,(6,P+39)                                                  
         GOTO1 VPRINTER                                                         
         MVC   P+1(30),=C'RADIO2    RECORDS IN/OUT     :'                       
         EDIT  RADCTR,(6,P+32)                                                  
         MVI   P+38,C'/'                                                        
         EDIT  RADOUT2,(6,P+39)                                                 
         GOTO1 VPRINTER                                                         
         MVC   P+1(30),=C'RADIO DUPLICATES  SKIPPED    :'                       
         EDIT  RADSKPD,(6,P+32)                                                 
         GOTO1 VPRINTER                                                         
         MVC   P+1(30),=C'BAD KEY RECORDS   SKIPPED    :'                       
         EDIT  BADKEY,(6,P+32)                                                  
         GOTO1 VPRINTER                                                         
         MVC   P+1(30),=C'SERVICE RECORDS   SKIPPED    :'                       
         EDIT  SRVCSKPD,(6,P+32)                                                
         GOTO1 VPRINTER                                                         
         MVC   P+1(30),=C'BLANK     RECORDS SKIPPED    :'                       
         EDIT  BLANKCTR,(6,P+32)                                                
         GOTO1 VPRINTER                                                         
         MVC   P+1(30),=C'METRO     RECORDS IN/OUT     :'                       
         EDIT  METCTR,(6,P+32)                                                  
         MVI   P+38,C'/'                                                        
         EDIT  METOUT,(6,P+39)                                                  
         GOTO1 VPRINTER                                                         
         MVC   P+1(30),=C'OWNER     RECORDS IN/OUT     :'                       
         EDIT  OWNCTR,(6,P+32)                                                  
         MVI   P+38,C'/'                                                        
         EDIT  OWNOUT,(6,P+39)                                                  
         GOTO1 VPRINTER                                                         
         MVC   P+1(30),=C'REP       RECORDS IN/OUT     :'                       
         EDIT  REPCTR,(6,P+32)                                                  
         MVI   P+38,C'/'                                                        
         EDIT  REPOUT,(6,P+39)                                                  
         GOTO1 VPRINTER                                                         
         MVC   P+1(30),=C'FORMAT    RECORDS IN/OUT     :'                       
         EDIT  FORCTR,(6,P+32)                                                  
         MVI   P+38,C'/'                                                        
         EDIT  FOROUT,(6,P+39)                                                  
         GOTO1 VPRINTER                                                         
         MVC   P+1(30),=C'MULTIMED  RECORDS IN/OUT     :'                       
         EDIT  MULCTR,(6,P+32)                                                  
         MVI   P+38,C'/'                                                        
         EDIT  MULOUT,(6,P+39)                                                  
         GOTO1 VPRINTER                                                         
         XIT1                                                                   
***********************************************************************         
* PUT COLUMNS FOR BOX FOR RADIO RECORDS                               *         
***********************************************************************         
         SPACE 1                                                                
RADBOX   NTR1  BASE=*,LABEL=*                                                   
         ZAP   LINE,P99                                                         
         ZAP   PAGE,PONE                                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(L'RTITLE),RTITLE                                           
         MVI   BOXYORN,YES                                                      
         MVI   BOXOFF,NO                                                        
*                                                                               
B1       USING RADLINED,BOXCOLS                                                 
         MVC   BOXCOLS,SPACES                                                   
         MVI   B1.RADC1,C'L'                                                    
         MVI   B1.RADC2,C'C'                                                    
         MVI   B1.RADC3,C'C'                                                    
         MVI   B1.RADC4,C'C'                                                    
         MVI   B1.RADC5,C'C'                                                    
         MVI   B1.RADC6,C'C'                                                    
         MVI   B1.RADC7,C'C'                                                    
         MVI   B1.RADC8,C'C'                                                    
         MVI   B1.RADC9,C'C'                                                    
         MVI   B1.RADC10,C'C'                                                   
         MVI   B1.RADC11,C'C'                                                   
         MVI   B1.RADC12,C'C'                                                   
         MVI   B1.RADC13,C'C'                                                   
         MVI   B1.RADC14,C'C'                                                   
         MVI   B1.RADC15,C'R'                                                   
         DROP  B1                                                               
*                                                                               
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+2,C'T'                                                   
         MVI   BOXROWS+4,C'M'                                                   
         XR    RF,RF                                                            
         ICM   RF,3,MAXLINE                                                     
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LA    RF,BOXROWS(RF)                                                   
         MVI   0(RF),C'B'                                                       
*                                                                               
M1       USING RADLINED,MID1                                                    
         MVC   M1.RADUID,=CL06'DDS ID'                                          
         MVC   M1.RADCALL,=CL05'Call '                                          
         MVC   M1.RADBND,=CL01'B'                                               
         MVC   M1.RADFREQ,=CL05'Freq '                                          
         MVC   M1.RADLCITY,=CL24'City'                                          
         MVC   M1.RADSTATE,=CL02'St'                                            
         MVC   M1.RADFMT,=CL03'Fmt'                                             
         MVC   M1.RADOWNER,=CL05'Owner'                                         
         MVC   M1.RADREP1,=CL04'Rep1 '                                          
         MVC   M1.RADREP2,=CL04'Rep2 '                                          
         MVC   M1.RADAMC,=CL03'AMC'                                             
         MVC   M1.RADTYPE,=CL05'Type'                                           
         MVC   M1.RADTDATA,=CL07'Change '                                       
         MVC   M1.RADDATE,=CL08'Date    '                                       
         DROP  M1                                                               
M1       USING RADLINED,MID2                                                    
         MVC   M1.RADUID,=CL06'MS UID'                                          
         MVC   M1.RADCALL,=CL05'PARNT'                                          
         DROP  M1                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140CTRADIO   09/22/17'                                      
         END                                                                    
