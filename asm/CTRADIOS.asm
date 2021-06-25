*          DATA SET CTRADIOS   AT LEVEL 064 AS OF 04/12/04                      
**********************************************************************          
*  CTRADIO:  GENERATE RECORDS FROM MSTREET DATA WHICH ARE LOADED     *          
*         INTO THE CONTROL FILE.                                     *          
**********************************************************************          
*  HISTORY OF CHANGES:                                               *          
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
*                                                                    *          
**********************************************************************          
*PHASE CTRADIOB                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         PRINT NOGEN                                                            
CTRADIO  START                                                                  
         NBASE WORKL,*CTRADIO,=V(WORKAREA),RA,R5                                
         USING WORKD,RC                                                         
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         L     R8,VBOXAREA                                                      
         USING BOXD,R8                                                          
*                                                                               
         BRAS  RE,INIT             INITIALISE JOB                               
         BRAS  RE,BLDRAD           BUILD RADIO OUTPUT                           
         BRAS  RE,BLDMET                                                        
         BRAS  RE,BLDOWN                                                        
         BRAS  RE,BLDREP                                                        
         BRAS  RE,BLDFRM                                                        
         BRAS  RE,BLDMMD                                                        
         BRAS  RE,DISPTOTS                                                      
         CLOSE TAPEOUT                                                          
         B     XBASE                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY TOTAL RECORDS PROCESSED                                     *         
***********************************************************************         
         SPACE 1                                                                
DISPTOTS NTR1  ,                                                                
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
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD OUTPUT RECORDS FOR RADIO                                      *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDRAD   NTR1  ,                                                                
         OPEN  (RADIO,INPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,RADBOX                                                        
*                                                                               
         MVI   EOF,C'N'                                                         
         LA    R0,RECSVL           CLEAR SAVE RECORD AREA                       
         LHI   R1,RECSVLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         USING CRD,RECSV                                                        
BRAD0020 LA    R0,RECINL           CLEAR INPUT RECORD AREA                      
         LHI   R1,RECINLQ                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GET   RADIO,RECINL        GET INPUT AND TRANSLATE IT                   
         TR    RECIN(CRDL1Q),TRTAB                                              
*                                                                               
*&&DO                                                                           
         GOTO1 VPRINTER                                                         
         MVC   P+1(19),=C'MSTREET RECORD READ'                                  
         GOTO1 VPRINTER                                                         
         LA    R4,RECINL           A(I/P  RECORD)                               
         LA    RF,512                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
*&&                                                                             
*                                                                               
         CLC   RECIN(64),SPACES    ANY DATA IN RECORD?                          
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
         B     BRAD0020            GO BACK FOR NEXT RECORD                      
BRAD0030 EQU   *                                                                
*                                                                               
         L     RF,RADCTR           COUNT RECORD TYPE                            
         LA    RF,1(RF)                                                         
         ST    RF,RADCTR                                                        
*                                                                               
*   TEST FORCE END                                                              
****     CLC   RADCTR,=F'50'       END AFTER 50                                 
****     BH    BRAD0420            FORCE END                                    
*   TEST FORCE END                                                              
*                                                                               
         LA    R0,RECIN                                                         
         LHI   R1,RECINLQ                                                       
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         CLCL  R0,RE               SEE IF THIS RECORD IS THE SAME               
         BE    BRAD0020                                                         
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
         MVC   CT99KUID,CRUID      INSERT UID    INTO KEY                       
         MVC   SAVCRUID,CRUID      SAVE THIS RECORD'S UID                       
*                                                                               
         LA    R3,CT99DATA                                                      
         USING CRCLD,R3                                                         
         MVI   CRCLEL,CRCLELQ                                                   
         MVI   CRCLLN,CRCLLNQ                                                   
         MVC   CRCLCLL,CRCALL      CALL LETTER                                  
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
         MVI   CT9AKMED,CT9AKMRD   SET MEDIA TO RADIO                           
         CLI   CRBAND,C'A'         AM STATION?                                  
         BE    BRAD0060                                                         
         CLI   CRBAND,C'F'         FM STATION?                                  
         BE    BRAD0060                                                         
         CLI   CRBAND,C'L'         FM STATION? LOW POWER                        
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
         PUT   TAPEOUT,CARD                                                     
*                                                                               
**       LA    R4,CARD             A(O/P  RECORD)                               
***      LA    RF,80                                                            
***      GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
**       GOTO1 VPRINTER                                                         
                                                                                
*                                                                               
*&&DO                                                                           
*                                                                               
*   NO DDS ID IN USE:  THIS PASSIVE IS DISCONTINUED.                            
*                                                                               
         XC    CARD,CARD           OUTPUT LIVE 9A01 PASSIVE RECORD              
         MVC   CARD(2),=AL2(CT9XLENQ+4)                                         
         USING CT9XRECD,CARD+4                                                  
         MVC   CT9XKTYP(2),=X'9A01'                                             
         MVI   CT9XKMED,CT9XKMRD   SET MEDIA TO RADIO                           
         CLI   CRBAND,C'A'         AM STATION?                                  
         BE    BRAD0080                                                         
         CLI   CRBAND,C'F'         FM STATION?                                  
         BE    BRAD0080                                                         
         CLI   CRBAND,C'L'         FM STATION? LOW POWER                        
         BE    BRAD0080                                                         
         MVI   CT9XKMED,CT9XKMTV   SET MEDIA TO TV                              
BRAD0080 EQU   *                                                                
         MVC   CT9XKCLL+0(4),CRCALL                                             
         MVC   CT9XKCLL+4(1),CRBAND                                             
         XC    CT9XKDTE,CT9XKDTE                                                
         MVC   CT9XLEN,=AL2(CT9XLENQ)                                           
         MVI   CT9XEL,CT9XELQ                                                   
         MVI   CT9XLN,CT9XLNQ                                                   
         MVC   CT9XKUID,CRUID      USE UNIQ ID FOR PASSIVE KEY                  
         PUT   TAPEOUT,CARD                                                     
*                                                                               
***      LA    R4,CARD             A(O/P  RECORD)                               
***      LA    RF,80                                                            
***      GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
***      GOTO1 VPRINTER                                                         
*&&                                                                             
*                                                                               
*                                                                               
         AHI   R3,CRCLLNQ                                                       
         USING CRFMD,R3                                                         
         MVI   CRFMEL,CRFMELQ                                                   
         MVI   CRFMLN,CRFMLNQ                                                   
         MVC   CRFMFMT,CRFMT                                                    
         MVC   CRFMOWN,CROWNER                                                  
         MVC   CRFMREP1,CRREP1                                                  
         MVC   CRFMREP2,CRREP2                                                  
         MVC   CRFMAMC,CRAMCODE                                                 
*                                                                               
         AHI   R3,CRFMLNQ                                                       
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
         MVI   CT9AKMED,CT9AKMRD                                                
         MVC   CT9AKCLL,CRCHST1                                                 
         GOTO1 DATCON,DMCB,(3,CRCHHDT1),(2,CT9AKDTE)                            
         XC    CT9AKDTE,=XL2'FFFF'                                              
         MVC   CT9ALEN,=AL2(CT9ALENQ)                                           
         MVI   CT9AEL,CT9AELQ                                                   
         MVI   CT9ALN,CT9ALNQ                                                   
***      MVC   CT9AUID,CRDDSID     USE DDS ID FOR PASSIVE KEY                   
         MVC   CT9AUID,CRUID       USE UID    FOR PASSIVE KEY                   
         PUT   TAPEOUT,CARD                                                     
*                                                                               
*&&DO                                                                           
*  DISCONTINUED                                                                 
*                                                                               
         XC    CARD,CARD           OUTPUT -1 9A01 PASSIVE RECORD                
         MVC   CARD(2),=AL2(CT9XLENQ+4)                                         
         MVI   CT9AKTYP,CT9AKTYQ                                                
         MVI   CT9AKMED,CT9AKMRD                                                
         MVC   CT9AKCLL,CRCHST1                                                 
         GOTO1 DATCON,DMCB,(3,CRCHHDT1),(2,CT9AKDTE)                            
         XC    CT9AKDTE,=XL2'FFFF'                                              
         MVC   CT9ALEN,=AL2(CT9ALENQ)                                           
         MVI   CT9AEL,CT9AELQ                                                   
         MVI   CT9ALN,CT9ALNQ                                                   
         MVC   CT9AUID,CRDDSID     USE DDS ID FOR PASSIVE KEY                   
         PUT   TAPEOUT,CARD                                                     
*&&                                                                             
*                                                                               
BRAD0160 CLC   CRCHHST2,SPACES     -2 HISTORY                                   
         BNH   BRAD0180                                                         
         CLC   =CL7'NEW',CRCHHST2                                               
         BE    BRAD0180                                                         
*                                                                               
         XC    CARD,CARD           OUTPUT -2 9A PASSIVE RECORD                  
         MVC   CARD(2),=AL2(CT9ALENQ+4)                                         
         MVI   CT9AKTYP,CT9AKTYQ                                                
         MVI   CT9AKMED,CT9AKMRD                                                
         MVC   CT9AKCLL,CRCHST2                                                 
         GOTO1 DATCON,DMCB,(3,CRCHHDT2),(2,CT9AKDTE)                            
         XC    CT9AKDTE,=XL2'FFFF'                                              
         MVC   CT9ALEN,=AL2(CT9ALENQ)                                           
         MVI   CT9AEL,CT9AELQ                                                   
         MVI   CT9ALN,CT9ALNQ                                                   
****     MVC   CT9AUID,CRDDSID     USE DDS ID FOR PASSIVE KEY                   
         MVC   CT9AUID,CRUID       USE UID    FOR PASSIVE KEY                   
         PUT   TAPEOUT,CARD                                                     
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
         LH    RF,RECINL                                                        
         CHI   RF,CRDLQ+4                                                       
         BNH   BRAD0360            YES - IGNORE NEXT 2 ELEMENTS                 
*                                                                               
         USING CRZHD,R3                                                         
         MVI   CRZHEL,CRZHELQ                                                   
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
BRAD0360 MVI   0(R3),0             TERMINATE RECORD                             
         AHI   R3,1                                                             
*                                                                               
         SR    R3,R2                                                            
         STCM  R3,3,CT99LEN        SET LENGTH                                   
         AHI   R3,4                                                             
         STH   R3,RECOUTH          SET RECORD LENGTH FOR PUT (=LEN+4)           
*                                                                               
         GOTO1 CHECKKEY,DMCB,0                                                  
         BNZ   BRAD0380            BAD KEY:  SKIP                               
*                                                                               
         PUT   TAPEOUT,RECOUTH                                                  
*                                                                               
         L     RF,RADOUT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,RADOUT                                                        
*                                                                               
*&&DO                                                                           
         CLC   CT99KUID,=C'001599'                                              
         BE    BRAD0361                                                         
         CLC   CT99KUID,=C'000214'                                              
         BE    BRAD0361                                                         
         CLC   RADOUT,=F'50'       DISPLAY FIRST 50                             
*                                                                               
         BH    BRAD0362                                                         
*&&                                                                             
BRAD0361 EQU   *                                                                
*                                                                               
*&&DO                                                                           
         LA    R4,RECOUTH          A(O/P  RECORD)                               
         SR    RF,RF                                                            
         ICM   RF,3,RECOUTH                                                     
         MVC   P+1(05),=C'KEY1:'                                                
         EDIT  RADOUT,(6,P+20)                                                  
         GOTO1 VPRINTER                                                         
         LA    RF,64                                                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*&&                                                                             
*                                                                               
BRAD0362 EQU   *                                                                
*                                                                               
         MVC   CT99KUID,SPACES     CLEAR UID FROM KEY                           
         MVC   CT99KUID(4),SAVCALL INSERT STATION CALL LETTERS                  
         MVC   CT99KUID+4(1),SAVBAND                                            
*                                  INSERT MEDIA                                 
         LA    R3,CT99DATA                                                      
         USING CRCLD,R3                                                         
*                                                                               
         MVC   CRCLUIDX,SAVCRUID   INSERT ORIGINAL UID                          
*                                                                               
***                                                                             
*   DON'T OUTPUT KEY2 UNTIL DUPLICATE STATION CALL LETTER ISSUE                 
*        HAS BEEN RESOLVED.                                                     
*                                                                               
*                                                                               
*&&DO                                                                           
         PUT   TAPEOUT,RECOUTH                                                  
*                                                                               
         GOTO1 CHECKKEY,DMCB,1                                                  
         BNZ   BRAD0380            BAD KEY:  SKIP                               
*                                                                               
         L     RF,RADOUT2                                                       
         LA    RF,1(RF)                                                         
         ST    RF,RADOUT2                                                       
*&&                                                                             
*&&DO                                                                           
         CLC   CT99KUID,=C'KBBMF '                                              
         BE    BRAD0363                                                         
         CLC   CT99KUID,=C'KIKRA '                                              
         BE    BRAD0363                                                         
         CLC   RADOUT2,=F'50'      DISPLAY FIRST 50                             
*                                                                               
         BH    BRAD0364                                                         
*&&                                                                             
BRAD0363 EQU   *                                                                
*&&DO                                                                           
         LA    R4,RECOUTH          A(O/P  RECORD)                               
         SR    RF,RF                                                            
         ICM   RF,3,RECOUTH                                                     
         MVC   P+1(05),=C'KEY2:'                                                
         EDIT  RADOUT,(6,P+20)                                                  
         GOTO1 VPRINTER                                                         
         LA    RF,64                                                            
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
*&&                                                                             
BRAD0364 EQU   *                                                                
         CLI   EOF,C'Y'                                                         
         BE    BRAD0400                                                         
*                                                                               
BRAD0380 LA    R0,RECIN                                                         
         LHI   R1,RECINLQ                                                       
         LA    RE,RECSV                                                         
         LHI   RF,RECSVLQ                                                       
         MVCL  RE,R0               SAVE THIS RECORD                             
         B     BRAD0020                                                         
*                                                                               
BRAD0400 CLOSE RADIO                                                            
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
BRAD0420 MVI   EOF,C'Y'                                                         
         B     BRAD0040                                                         
         POP   USING                                                            
         EJECT                                                                  
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
         LA    R4,RECOUTH          A(O/P  RECORD)                               
         SR    RF,RF                                                            
         ICM   RF,3,RECOUTH                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
         MVC   P+1(13),=C'MSTREET INPUT'                                        
         GOTO1 VPRINTER                                                         
         LA    R4,RECIN            A(I/P  RECORD)                               
         SR    RF,RF                                                            
         LA    RF,512                                                           
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',(RF),=C'1D'                
         GOTO1 VPRINTER                                                         
         LTR   RB,RB                                                            
         B     CKEY0900            EXIT CC NOT ZERO:  ERROR                     
CKEY0800 EQU   *                                                                
         SR    R0,R0                                                            
CKEY0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PUT COLUMNS FOR BOX FOR RADIO RECORDS                               *         
***********************************************************************         
         SPACE 1                                                                
RADBOX   NTR1  ,                                                                
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
         B     EXITOK                                                           
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
         OPEN  (MULTIMED,INPUT)                                                 
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
         GET   MULTIMED,RECINL     GET INPUT AND TRANSLATE IT                   
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
         MVC   CT99KMMD,CMMCODE                                                 
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
         PUT   TAPEOUT,RECOUTH                                                  
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
MMD08    CLOSE MULTIMED                                                         
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
EODMMD   MVI   EOF,C'Y'                                                         
         B     MMD04                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PUT COLUMNS FOR BOX FOR MULTIMED RECORDS                            *         
***********************************************************************         
         SPACE 1                                                                
MMDBOX   NTR1  ,                                                                
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
         B     EXITOK                                                           
         DROP  M1                                                               
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
* BUILD OUTPUT RECORDS FOR METROS                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDMET   NTR1  ,                                                                
         MVI   EOF,C'N'                                                         
         OPEN  (METRO,INPUT)                                                    
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
         GET   METRO,RECINL        GET INPUT AND TRANSLATE IT                   
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
         PUT   TAPEOUT,RECOUTH                                                  
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
MET08    CLOSE METRO                                                            
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
EODMET   MVI   EOF,C'Y'                                                         
         B     MET04                                                            
         POP   USING                                                            
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
         MVI   B1.METC3,C'R'                                                    
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
         MVC   M1.METCODE,=CL04'Code'                                           
         MVC   M1.METINAME,=CL36'Metro Name'                                    
         B     EXITOK                                                           
         DROP  M1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PRINT LINE FROM METRO DATA                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
METPRT   NTR1  ,                                                                
         USING METLINED,P                                                       
         USING CMD,RECSV                                                        
         MVC   METCODE(L'CMCODE),CMCODE                                         
         MVC   METINAME,CMINAME                                                 
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD OUTPUT RECORDS FOR FORMATS                                    *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDFRM   NTR1  ,                                                                
         MVI   EOF,C'N'                                                         
         OPEN  (FORMAT,INPUT)                                                   
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
         GET   FORMAT,RECINL        GET INPUT AND TRANSLATE IT                  
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
         PUT   TAPEOUT,RECOUTH                                                  
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
FRM08    CLOSE FORMAT                                                           
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
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
         MVI   B1.FRMC3,C'R'                                                    
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
         MVC   FRMCODE(L'CFCODE),CFCODE                                         
         MVC   FRMINAME,CFINAME                                                 
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD OUTPUT RECORDS FOR OWNERS                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDOWN   NTR1  ,                                                                
         MVI   EOF,C'N'                                                         
         OPEN  (OWNER,INPUT)                                                    
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
         GET   OWNER,RECINL        GET INPUT AND TRANSLATE IT                   
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
         PUT   TAPEOUT,RECOUTH                                                  
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
OWN08    CLOSE OWNER                                                            
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
*                                                                               
EODOWN   MVI   EOF,C'Y'                                                         
         B     OWN04                                                            
         POP   USING                                                            
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
         MVI   B1.OWNC3,C'R'                                                    
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
         MVC   M1.OWNCODE,=CL05'Code'                                           
         MVC   M1.OWNINAME,=CL36'Owner Name'                                    
         B     EXITOK                                                           
         DROP  M1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT PRINT LINE FROM OWNER DATA                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
OWNPRT   NTR1  ,                                                                
         USING OWNLINED,P                                                       
         USING COD,RECSV                                                        
         MVC   OWNCODE(L'COCODE),COCODE                                         
         MVC   OWNINAME,COINAME                                                 
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
         OPEN  (REP,INPUT)                                                      
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
         GET   REP,RECINL          GET INPUT AND TRANSLATE IT                   
         TR    RECIN(CREDLQ),TRTAB                                              
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
         PUT   TAPEOUT,RECOUTH                                                  
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
REP08    CLOSE REP                                                              
         MVI   BOXREQ,C'C'                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
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
         MVI   B1.REPC3,C'R'                                                    
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
         OPEN  (TAPEOUT,OUTPUT)                                                 
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
FTITLE   DC    C'FORMAT RECORDS (9904)'                                         
MMTITLE  DC    C'MULTIMED RECORDS (9905)'                                       
*                                                                               
WORK     DS    CL64                                                             
*                                                                               
RADCTR   DS    F                                                                
BLANKCTR DS    F                                                                
METCTR   DS    F                                                                
OWNCTR   DS    F                                                                
REPCTR   DS    F                                                                
FORCTR   DS    F                                                                
MULCTR   DS    F                                                                
RADOUT   DS    F                                                                
RADOUT2  DS    F                                                                
METOUT   DS    F                                                                
OWNOUT   DS    F                                                                
REPOUT   DS    F                                                                
FOROUT   DS    F                                                                
MULOUT   DS    F                                                                
*                                                                               
SAVCRUID DS    CL6                                                              
SAVCALL  DS    CL4                                                              
SAVBAND  DS    CL1                                                              
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
         DS    0D                                                               
         DC    CL8'*RECSAVE'                                                    
RECSVL   DS    F                                                                
RECSV    DS    512C                                                             
RECSVLQ  EQU   *-RECSV                                                          
         DS    0D                                                               
         DC    CL8'*RECOUT*'                                                    
RECOUTH  DS    F                                                                
RECOUT   DS    512C                                                             
RECOUTLQ EQU   *-RECOUTH                                                        
*                                                                               
         DS    0D                                                               
         DC    CL8'**RECIN*'                                                    
RECINL   DS    F                                                                
RECIN    DS    512C                                                             
RECINLQ  EQU   *-RECIN                                                          
*                                                                               
***********************************************************************         
* DCBS AND ADCONS                                                     *         
***********************************************************************         
         SPACE 1                                                                
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
RADIO    DCB   DSORG=PS,MACRF=GM,DDNAME=RADIO,RECFM=VB,LRECL=320,      +        
               BLKSIZE=6233,EODAD=BRAD0420                                      
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
         SPACE 1                                                                
CRD      DSECT                                                                  
CRUID    DS    CL6                                                              
CRCALL   DS    CL5                                                              
CRBAND   DS    CL1                                                              
CRFREQ   DS    CL5                                                              
CRLCITY  DS    CL24                                                             
CRLSTATE DS    CL2                                                              
CRFMT    DS    CL3                                                              
CROWNER  DS    CL5                                                              
CRREP1   DS    CL4                                                              
CRREP2   DS    CL4                                                              
CRAMCODE DS    CL3                                                              
*                                                                               
CRCHST1  DS    CL7                 CALL HISTORY                                 
CRCHSTD1 DS    CL8                                                              
CRCHST2  DS    CL7                                                              
CRCHSTD2 DS    CL8                                                              
*                                                                               
CRFHST1  DS    CL5                 FREQUENCY HISTORY                            
CRFHSTD1 DS    CL8                                                              
CRFHST2  DS    CL5                                                              
CRFHSTD2 DS    CL8                                                              
*                                                                               
CRMHST1  DS    CL3                 FORMAT HISTORY                               
CRMHSTD1 DS    CL8                                                              
CRMHST2  DS    CL3                                                              
CRMHSTD2 DS    CL8                                                              
CRDLQ    EQU   *-CRD                                                            
*                                                                               
CRZHST1  DS    CL24                CITY HISTORY                                 
CRZHSTD1 DS    CL8                                                              
CRZHST2  DS    CL24                                                             
CRZHSTD2 DS    CL8                                                              
*                                                                               
CRSHST1  DS    CL2                 STATE HISTORY                                
CRSHSTD1 DS    CL8                                                              
CRSHST2  DS    CL2                                                              
CRSHSTD2 DS    CL8                                                              
*                                                                               
CRDDSID  DS    CL6                 DDS ID                                       
*                                                                               
CRPARENT DS    CL5                 MULTIMEDIA PARENT                            
*                                                                               
CRDL1Q   EQU   *-CRD                                                            
         EJECT                                                                  
***********************************************************************         
* INCOMING METRO FILE DSECT                                           *         
***********************************************************************         
         SPACE 1                                                                
CMD      DSECT                                                                  
CMCODE   DS    CL3                                                              
CMINAME  DS    CL36                                                             
CMDLQ    EQU   *-CMD                                                            
         EJECT                                                                  
***********************************************************************         
* INCOMING FORMAT FILE DSECT                                          *         
***********************************************************************         
         SPACE 1                                                                
CFD      DSECT                                                                  
CFCODE   DS    CL3                                                              
CFINAME  DS    CL36                                                             
CFDLQ    EQU   *-CFD                                                            
         EJECT                                                                  
***********************************************************************         
* INCOMING OWNER FILE DSECT                                           *         
***********************************************************************         
         SPACE 1                                                                
COD      DSECT                                                                  
COCODE   DS    CL5                                                              
COINAME  DS    CL36                                                             
CODLQ    EQU   *-COD                                                            
         EJECT                                                                  
***********************************************************************         
* INCOMING REP FILE DSECT                                             *         
***********************************************************************         
         SPACE 1                                                                
CRED     DSECT                                                                  
CRECODE  DS    CL4                                                              
CREINAME DS    CL50                                                             
CREDLQ   EQU   *-CRED                                                           
         EJECT                                                                  
***********************************************************************         
* INCOMING MULTIMEDIA FILE DSECT                                      *         
***********************************************************************         
         SPACE 1                                                                
CMMD     DSECT                                                                  
CMMCODE  DS    CL5                                                              
CMMINAME DS    CL50                                                             
CMMDLQ   EQU   *-CMMD                                                           
         EJECT                                                                  
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
METCODE  DS    CL4                                                              
         DS    C                                                                
METC2    DS    C                                                                
         DS    C                                                                
METINAME DS    CL36                                                             
         DS    C                                                                
METC3    DS    C                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT LINE FOR FORMAT INPUT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
FRMLINED DSECT                                                                  
FRMC1    DS    C                                                                
         DS    C                                                                
FRMCODE  DS    CL4                                                              
         DS    C                                                                
FRMC2    DS    C                                                                
         DS    C                                                                
FRMINAME DS    CL36                                                             
         DS    C                                                                
FRMC3    DS    C                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT LINE FOR OWNER INPUT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
OWNLINED DSECT                                                                  
OWNC1    DS    C                                                                
         DS    C                                                                
OWNCODE  DS    CL5                                                              
         DS    C                                                                
OWNC2    DS    C                                                                
         DS    C                                                                
OWNINAME DS    CL36                                                             
         DS    C                                                                
OWNC3    DS    C                                                                
         EJECT                                                                  
***********************************************************************         
* PRINT LINE FOR REP INPUT RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
REPLINED DSECT                                                                  
REPC1    DS    C                                                                
         DS    C                                                                
REPCODE  DS    CL4                                                              
         DS    C                                                                
REPC2    DS    C                                                                
         DS    C                                                                
REPINAME DS    CL36                                                             
         DS    C                                                                
REPC3    DS    C                                                                
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064CTRADIOS  04/12/04'                                      
         END                                                                    
