*          DATA SET SPLDEXTMMD AT LEVEL 002 AS OF 05/02/00                      
*PHASE SPEXTMMD,+0                                                              
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'DMLDEXT - PEEL MM DATA OFF THE SPOT FILE'                       
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALIZE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITIALIZE                     
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALIZE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         OPEN  (SDCLT,OUTPUT)                                                   
         MVC   SDREC,SPACES2                                                    
         GOTO1 =V(DATCON),DMCB,(5,0),(10,SDREC)    MM/DD/YY                     
         PUT   SDCLT,SDREC          PUT TODAY'S DATE INTO DATA SET              
         MVI   CLTTAB,X'FF'                                                     
         BAS   RE,PUTSPV                                                        
         BAS   RE,PUTBUY                                                        
         BAS   RE,PUTNBY                                                        
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   L     R3,AREC                                                          
         LA    R4,CLTTAB                                                        
         ZICM  RE,COUNTER,2                                                     
         MHI   RE,6                                                             
         AR    R4,RE                                                            
         B     GETTYPE             GET RECORD TYPE                              
*                                                                               
RECTYPA  MVC   BAMCLT,2(R3)        XX XX A/M CLT CLT                            
         B     DMXKX                                                            
*                                                                               
RECTYPB  MVC   BAGYMD,2(R3)        XX XX A/M XX CLT CLT                         
         MVC   BCLT,4(R3)                                                       
         B     DMXKX                                                            
*                                                                               
RECTYPC  MVC   BAGYMD,2(R3)        XX XX A/M 5XX'S CLT CLT                      
         MVC   BCLT,8(R3)                                                       
         B     DMXKX                                                            
*                                                                               
RECTYPD  MVC   BAMCLT,1(R3)        XX A/M CLT CLT                               
         OC    4(8,R3),4(R3)       CLIENT RECORD?                               
         BNZ   RECTYPDX                                                         
         ZICM  RE,COUNTER,2                                                     
         CHI   RE,100                                                           
         BH    RECTYPDX                                                         
         USING CLTTABD,R4                                                       
         USING CLTHDR,R3                                                        
         CLI   CCLTTYPE,C'M'       MATCHMAKER CLIENT?                           
         BNE   RECTYPDX                                                         
         GOTO1 =V(CLUNPK),DMCB,(CPROF+6,BCLT),QCLT                              
         MVC   CLTAGMD,BAGYMD      STORE CLIENT IN TABLE                        
         MVC   CLTCODE,BCLT                                                     
         MVC   CLTALPH,QCLT                                                     
         MVC   SDREC,SPACES2                                                    
         MVI   SDREC,C'C'                                                       
         MVC   SDREC+1(6),CLTAGMD                                               
         PUT   SDCLT,SDREC          PUT A/M,BCLT,QCLT TO DATA SET               
         MVI   CLTLNQ(R4),X'FF'    MARK TEMPORARY END OF TABLE                  
         ZICM  RE,COUNTER,2                                                     
         AHI   RE,1                                                             
         STCM  RE,3,COUNTER                                                     
         DROP  R3,R4                                                            
RECTYPDX B     DMXKX                                                            
*                                                                               
RECTYPF  MVC   BAMCLT,0(R3)        A/M CLT CLT                                  
         B     DMXKX                                                            
*                                                                               
RECTYPH  CLC   1(2,R3),=C'TM'      XX AGY AGY XX CLT CLT                        
         BNE   DMXPURGE            CORRECT AGENCY?                              
         MVI   BAGYMD,X'11'                                                     
         MVC   BCLT,4(R3)                                                       
         B     DMXKX                                                            
*                                                                               
RECTYPI  MVC   BAGYMD,1(R3)        XX A/M XX XX XX CLT CLT                      
         MVC   BCLT,5(R3)          CLIENT                                       
         B     DMXKX                                                            
*                                                                               
RECTYPJ  MVC   BAMCLT,8(R3)        8XX'S A/M CLT CLT                            
         B     DMXKX                                                            
*                                                                               
RECTYPK  CLC   2(2,R3),=C'TM'      XX XX AGY AGY 4XX'S CLT CLT                  
         BNE   DMXPURGE            CORRECT AGENCY?                              
         MVI   BAGYMD,X'11'                                                     
         MVC   BCLT,8(R3)                                                       
         B     DMXKX                                                            
*                                                                               
RECTYPL  CLC   2(2,R3),=C'TM'      XX XX AGY AGY 6XX'S CLT CLT                  
         BNE   DMXPURGE            CORRECT AGENCY?                              
         MVI   BAGYMD,X'11'                                                     
         MVC   BCLT,10(R3)                                                      
         B     DMXKX                                                            
*                                                                               
RECTYPM  CLI   6(R3),0             NO CLIENT IN SOME 0D31'S                     
         BE    DMXPURGE                                                         
         MVC   BAGYMD,2(R3)                                                     
         MVC   BCLT,6(R3)                                                       
         B     DMXKX                                                            
*                                                                               
RECTYPN  MVC   BAGYMD,4(R3)        XX XX XX XX A/M                              
         USING DAREORDD,R3                                                      
         LA    R5,DORFRST          FIRST ELEMENT                                
         USING DOIDELD,R5                                                       
         MVC   BCLT,DOIDCLT                                                     
         DROP  R3,R5                                                            
         B     DMXKX                                                            
*                                                                               
RECTYPO  MVC   BAGYMD,2(R3)        XX XX A/M 4XX'S CLT CLT                      
         MVC   BCLT,7(R3)          CLIENT                                       
         B     DMXKX                                                            
*                                                                               
RECTYPP  MVC   BAMCLT,4(R3)        XX XX XX XX A/M CLT CLT                      
         B     DMXKX                                                            
*                                                                               
RECTYPQ  CLC   2(2,R3),=C'TM'      XX XX AGY AGY 3XX'S CLT CLT                  
         BNE   DMXPURGE            CORRECT AGENCY?                              
         MVI   BAGYMD,X'11'                                                     
         MVC   BCLT,7(R3)                                                       
         B     DMXKX                                                            
*                                                                               
RECTYPR  MVC   BAGYMD,2(R3)        XX XX A/M XX XX CLT CLT                      
         MVC   BCLT,5(R3)                                                       
         B     DMXKX                                                            
*                                                                               
RECTYPS  MVC   BAGYMD,2(R3)        XX XX A/M CLT CLT CLT                        
         MVC   QCLT,3(R3)                                                       
         BAS   RE,GETBCLT          CONVERT EBCDIC CLIENT TO BINARY              
         BNE   DMXPURGE            PURGE IF NOT IN CLIENT LIST                  
         B     DMXKXP                                                           
*                                                                               
RECTYPTS LA    R5,SPVTAB           SUPERDESK SUPERVISOR RECORDS                 
         BAS   RE,SPVBUY                                                        
         BNE   DMXPURGE                                                         
         B     DMXKXP                                                           
*                                                                               
RECTYPTB LA    R5,BUYTAB           SUPERDESK BUYER RECORDS                      
         BAS   RE,SPVBUY                                                        
         BNE   DMXPURGE                                                         
         B     DMXKXP                                                           
*                                                                               
RECTYPU  DS    0H                  NWS BUYER RECORDS                            
         USING BYRRECD,R3                                                       
         CLI   BYREL,BYRELCDQ      BUYER DESCRIPTION ELEMENT?                   
         BNE   DMXPURGE                                                         
         MVC   BAGYMD,BYRKAGMD     AGENCY/MEDIA                                 
         MVC   EBCBYR,BYRKBYR      EBCDIC BUYER CODE                            
         MVC   BINBYR,BYRCODE      BINARY BUYER CODE                            
         DROP  R3                                                               
*                                                                               
         BAS   RE,UPDTBY           UPDATE BUYER CODE                            
         BNE   DMXPURGE            PURGE IF NOT IN TABLE                        
         B     DMXKXP                                                           
*                                                                               
RECTYPV  MVC   BAGYMD,2(R3)        NWS RECORDS                                  
         MVC   BINBYR,3(R3)                                                     
         BAS   RE,LOOKUP                                                        
         BNE   DMXPURGE                                                         
         B     DMXKXP                                                           
*                                                                               
DMXKX    BAS   RE,CHKBCLT          IS CLIENT IN CLIENT LIST?                    
         BNE   DMXPURGE                                                         
DMXKXP   DS    0H                                                               
***      MVI   P,C'K'                                                           
***      GOTO1 =V(HEXOUT),DMCB,(R3),P+2,13,=C'TOG'                              
***      GOTO1 VPRINTER            PRINT KEY                                    
         B     DMXKEEP                                                          
*                                                                               
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         CLOSE SDCLT                                                            
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        GET RECORD TYPE                                              *         
***********************************************************************         
*                                                                               
GETTYPE  LA    R2,ODTAB            SET DEFAULT TABLE ADDR                       
         CLI   0(R3),X'0D'                                                      
         BE    GETTYP05                                                         
         LA    R2,OATAB                                                         
         CLI   0(R3),X'0A'                                                      
         BE    GETTYP05                                                         
         LA    R2,OETAB                                                         
         CLI   0(R3),X'0E'                                                      
         BNE   GETTYP07                                                         
GETTYP05 MVC   BYTE,1(R3)          0D,0A,0E RECORDS                             
         B     GETTYP15                                                         
GETTYP07 LA    R2,TYPTAB           1 BYTE KEY TYPE RECORDS                      
         MVC   BYTE,0(R3)                                                       
         B     GETTYP15                                                         
*                                                                               
GETTYP10 CLC   0(1,R2),BYTE        LOOK FOR MATCH IN TAB                        
         BNE   GETTYP13                                                         
         BR    RF                  BRANCH TO ROUTINE                            
GETTYP13 LA    R2,1(R2)                                                         
         CLC   =X'FFFF',0(R2)      CK EOT                                       
         BE    GETTYP50                                                         
         CLI   0(R2),X'FF'         CK END OF TYPE                               
         BNE   GETTYP10                                                         
         LA    R2,1(R2)                                                         
GETTYP15 ICM   RF,15,0(R2)         ADDRESS OF ROUTINE                           
         LA    R2,4(R2)            ADVANCE TO RECORD TYPES                      
         B     GETTYP10                                                         
*                                                                               
GETTYP50 CLI   0(R3),X'10'         IF NO MATCH IN TABLE,                        
         BH    GETTYP60            IS IT A BUY RECORD?                          
         MVC   ERRMESS(L'UNDEFERR),UNDEFERR                                     
         LA    R5,L'UNDEFERR-1     LENGTH                                       
         BAS   RE,PRTERR           PRINT ERROR MESSAGE                          
         DC    H'00'               NO                                           
GETTYP60 LA    RF,RECTYPF                                                       
         BR    RF                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        GET BCLT FROM CLIENT LIST                                    *         
***********************************************************************         
GETBCLT  NTR1                                                                   
         LA    R5,CLTTAB           MATCH EBCDIC CLT IN TABLE TO GET             
         USING CLTTABD,R5          BINARY CLIENT CODE                           
GBCLT10  CLI   0(R5),X'FF'                                                      
         BE    NO                                                               
         CLC   CLTAGMD,BAGYMD      CORRECT AGENCY/MEDIA?                        
         BNE   GBCLT15                                                          
         CLC   CLTALPH,QCLT                                                     
         BE    GBCLT20                                                          
GBCLT15  LA    R5,CLTLNQ(R5)                                                    
         B     GBCLT10                                                          
GBCLT20  MVC   BCLT,CLTCODE                                                     
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        CHECK IF CLIENT IS IN CLIENT LIST                            *         
***********************************************************************         
CHKBCLT  NTR1                                                                   
         LA    R5,CLTTAB                                                        
         USING CLTTABD,R5                                                       
CBCLT10  CLI   0(R5),X'FF'                                                      
         BE    NO                                                               
         CLC   CLTAGMD,BAGYMD      CORRECT AGENCY/MEDIA?                        
         BNE   CBCLT20                                                          
         CLC   CLTCODE,BCLT                                                     
         BE    CBCLTX                                                           
CBCLT20  LA    R5,CLTLNQ(R5)                                                    
         B     CBCLT10                                                          
CBCLTX   B     EXIT                                                             
*                                                                               
***********************************************************************         
* UPDTBY: UPDATE THE BINARY BUYER CODES IN TABLE                      *         
***********************************************************************         
*                                                                               
UPDTBY   NTR1                                                                   
         LA    R2,BYRTABLE                                                      
         USING BYRD,R2                                                          
                                                                                
UPDTBY10 CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    NO                                                               
                                                                                
         CLC   EBCBYR,BYREB        SAME EBCDIC BUYER CODE                       
         BNE   UPDTBY20                                                         
         CLC   BAGYMD,BYRAGYMD     SAME BINARY A/M                              
         BE    UPDTBY30                                                         
                                                                                
UPDTBY20 LA    R2,BYRLNQ(R2)       NEXT ENTRY                                   
         B     UPDTBY10                                                         
                                                                                
UPDTBY30 MVC   BYRBIN,BINBYR       YES, SAVE BINARY BUYER CODE                  
                                                                                
         DROP  R2                                                               
UPDTBYX  B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* LOOKUP: LOOKUP BINARY AGENCY IN TABLE                               *         
***********************************************************************         
*                                                                               
LOOKUP   NTR1                                                                   
                                                                                
         LA    R2,BYRTABLE                                                      
         USING BYRD,R2                                                          
                                                                                
LOOKUP10 CLI   0(R2),X'FF'                                                      
         BE    NO                                                               
                                                                                
         CLC   BINBYR,BYRBIN       SAME BINARY BUYER CODE?                      
         BNE   LOOKUP20                                                         
         CLC   BAGYMD,BYRAGYMD     SAME BINARY A/M?                             
         BE    LOOKUPX                                                          
                                                                                
LOOKUP20 LA    R2,BYRLNQ(R2)                                                    
         B     LOOKUP10                                                         
                                                                                
         DROP  R2                                                               
LOOKUPX  B     EXIT                                                             
*                                                                               
***********************************************************************         
* SPVBUY  CHECK IF SUPERVISOR/BUYER IN TABLE                          *         
***********************************************************************         
* ON ENTRY R5 POINTS TO EITHER SUPERVISOR OR BUYER TABLE                        
*                                                                               
SPVBUY   NTR1                                                                   
         USING SVBYTABD,R5                                                      
SPVBUY10 CLI   0(R5),X'FF'                                                      
         BE    NO                                                               
         CLC   SVBYMED,2(R3)       A/M                                          
         BNE   SPVBUY20                                                         
         CLC   SVBYOFF,3(R3)       OFFICE CODE                                  
         BNE   SPVBUY20                                                         
         CLC   SVBYCODE,5(R3)      SUPERVISOR/BUYER CODE                        
         BE    SPVBUYX                                                          
SPVBUY20 LA    R5,SVBYLNQ(R5)                                                   
         B     SPVBUY10                                                         
*                                                                               
         DROP  R5                                                               
SPVBUYX  B     EXIT                                                             
*                                                                               
***********************************************************************         
* PUTSPV  STORE TABLES INTO DATASET                                   *         
***********************************************************************         
*                                                                               
PUTSPV   NTR1                                                                   
         USING SVBYTABD,R5                                                      
         LA    R5,SPVTAB                                                        
PUTSPV10 CLI   0(R5),X'FF'                                                      
         BE    PUTSPVX                                                          
         MVC   SDREC,SPACES2                                                    
         MVI   SDREC,C'S'                                                       
         MVC   SDREC+1(7),SVBYMED                                               
         PUT   SDCLT,SDREC                                                      
         LA    R5,SVBYLNQ(R5)                                                   
         B     PUTSPV10                                                         
*                                                                               
         DROP  R5                                                               
PUTSPVX  B     EXIT                                                             
*                                                                               
***********************************************************************         
* PUTBUY  STORE TABLES INTO DATASET                                   *         
***********************************************************************         
*                                                                               
PUTBUY   NTR1                                                                   
         USING SVBYTABD,R5                                                      
         LA    R5,BUYTAB                                                        
PUTBUY10 CLI   0(R5),X'FF'                                                      
         BE    PUTBUYX                                                          
         MVC   SDREC,SPACES2                                                    
         MVI   SDREC,C'B'                                                       
         MVC   SDREC+1(7),SVBYMED                                               
         PUT   SDCLT,SDREC                                                      
         LA    R5,SVBYLNQ(R5)                                                   
         B     PUTBUY10                                                         
*                                                                               
         DROP  R5                                                               
PUTBUYX  B     EXIT                                                             
*                                                                               
***********************************************************************         
* PUTNBY  STORE NWS TABLES INTO DATASET                               *         
***********************************************************************         
*                                                                               
PUTNBY   NTR1                                                                   
         USING BYRD,R5                                                          
         LA    R5,BYRTABLE                                                      
PUTNBY10 CLI   0(R5),X'FF'                                                      
         BE    PUTNBYX                                                          
         MVC   SDREC,SPACES2                                                    
         MVI   SDREC,C'N'                                                       
         MVC   SDREC+1(5),BYRAGYMD                                              
         PUT   SDCLT,SDREC                                                      
         LA    R5,BYRLNQ(R5)                                                    
         B     PUTNBY10                                                         
*                                                                               
         DROP  R5                                                               
PUTNBYX  B     EXIT                                                             
*                                                                               
***********************************************************************         
* PRTERR   PRINT ERRORS BEFORE DYING                                  *         
***********************************************************************         
*                                                                               
PRTERR   NTR1                                                                   
         MVC   P(80),STARS                                                      
         GOTO1 VPRINTER                                                         
         MVC   P(27),=C'THE PROGRAM DUMPED BECAUSE:'                            
         GOTO1 VPRINTER                                                         
         EX    R5,*+4                                                           
         MVC   P(0),ERRMESS                                                     
         GOTO1 VPRINTER                                                         
         MVC   P(80),STARS                                                      
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
***********************************************************************         
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
SPACES2  DC    9C' '                                                            
DATADISP DC    H'0024'                                                          
STARS    DC    80C'*'                                                           
UNDEFERR DC    C'RECORD TYPE NOT DEFINED'                                       
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
SDCLT    DCB   DDNAME=SDCLT,DSORG=PS,RECFM=FB,MACRF=PM                          
         SPACE                                                                  
*                                                                               
***********************************************************************         
*        TAB OF REC TYPES ACCORDING TO AGY AND CLT POSITION IN RECORD *         
***********************************************************************         
*                                                                               
ODTAB    DC    AL4(RECTYPA)                0DXX A/M CLT CLT                     
         DC    X'01020D1527283840'                                              
         DC    X'444548494A4E5357585C'                                          
         DC    X'696F7071727678797A7C4C'                                        
         DC    X'9091FF'                                                        
*                                                                               
         DC    AL4(DMXPURGE)               PURGE                                
         DC    X'040506141620263233'                                            
         DC    X'363B3C464750525A5B5E'                                          
         DC    X'60E2E56B'                                                      
         DC    X'6C737B7D7EFEFD4D4F'                                            
         DC    X'12224B77FF'                                                    
*                                                                               
         DC    AL4(RECTYPB),X'0C17FF'      0DXX A/M XX CLT CLT                  
         DC    AL4(RECTYPC),X'3575FF'      0DXX A/M 5X CLT CLT                  
         DC    AL4(RECTYPJ),X'03FF'        0DXX 6X A/M CLT CLT                  
         DC    AL4(RECTYPK),X'11FF'        0DXX AGY AGY 4X CLT CLT              
         DC    AL4(RECTYPL),X'13FF'        0DXX AGY AGY 6X CLT CLT              
         DC    AL4(RECTYPM),X'31FF'        0DXX A/M 3X CLT CLT                  
         DC    AL4(RECTYPN),X'34FF'        0DXX 2X A/M, CLT IN RECORD           
         DC    AL4(RECTYPO),X'37FF'        0DXX A/M 4X CLT CLT                  
         DC    AL4(RECTYPP),X'3AFF'        0DXX 2X A/M CLT CLT                  
         DC    AL4(RECTYPQ),X'43FF'        0DXX AGY AGY 3X CLT CLT              
         DC    AL4(RECTYPR),X'E3FF'        0DXX A/M 2X CLT CLT                  
         DC    AL4(RECTYPTS),X'61FF'       SD SUPERVISOR RECORDS                
         DC    AL4(RECTYPTB),X'62FF'       SD BUYER RECORDS                     
         DC    AL4(RECTYPU),X'65FF'        NWS BUYER RECORDS                    
         DC    AL4(RECTYPV),X'666768FFFF'  NWS RECORDS                          
*                                                                               
OATAB    DC    AL4(RECTYPA)                0AXX A/M CLT CLT                     
         DC    X'2122232425272A2E2F34'                                          
         DC    X'353637442C2D313242FF'                                          
         DC    AL4(DMXPURGE)               PURGE                                
         DC    X'26282943FF'                                                    
         DC    AL4(RECTYPO),X'2BFF'        0AXX A/M 4X CLT CLT                  
         DC    AL4(RECTYPS),X'41FFFF'      0AXX A/M CLT CLT CLT                 
*                                                                               
OETAB    DC    AL4(RECTYPA),X'01FFFF'      0EXX A/M CLT CLT                     
*                                                                               
TYPTAB   DC    AL4(RECTYPD)                XX A/M CLT CLT                       
         DC    X'000237B7FF'                                                    
         DC    AL4(RECTYPH),X'09FF'        XX AGY AGY XX CLT CLT                
         DC    AL4(RECTYPI),X'0BFF'        XX A/M 3X CLT CLT                    
         DC    AL4(DMXPURGE)               PURGE                                
         DC    X'030506080CFFFF'                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* BINARY A/M(1), OFFICE CODE(2), SUPERVISOR CODE(4)                             
SPVTAB   DC    X'10',CL2'A1',CL4'LCG'                                           
         DC    X'10',CL2'DD',CL4'JAR'                                           
         DC    X'10',CL2'NY',CL4'JIL'                                           
         DC    X'10',CL2'NY',CL4'LCG'                                           
         DC    X'10',CL2'PI',CL4'GRN'                                           
         DC    X'FF'                                                            
*                                                                               
* BINARY A/M(1), OFFICE CODE(2), BUYER CODE(4)                                  
BUYTAB   DC    X'10',CL2'A1',CL4'CCE'                                           
         DC    X'10',CL2'BO',CL4'AND'                                           
         DC    X'10',CL2'CL',CL4'CRG'                                           
         DC    X'10',CL2'DD',CL4'MAX'                                           
         DC    X'10',CL2'NY',CL4'MOE'                                           
         DC    X'FF'                                                            
*                                                                               
* NWS BUYERS                                                                    
* BINARY A/M(1), EBCDIC BUYER CODE(3), BINARY BUYER CODE(1)                     
BYRTABLE DC    X'11',C'AAA',X'00'                                               
         DC    X'11',C'CAB',X'00'                                               
         DC    X'11',C'CLA',X'00'                                               
         DC    X'11',C'DAA',X'00'                                               
         DC    X'11',C'DOT',X'00'                                               
         DC    X'11',C'MET',X'00'                                               
         DC    X'FF'                                                            
*                                                                               
*                                                                               
CLTTAB   DS    CL601              100 ENTRIES 6 BYTES EACH                      
COUNTER  DS    XL2                                                              
BYTE     DS    X                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
*                                                                               
BAMCLT   DS    0XL3                                                             
BAGYMD   DS    XL1                                                              
BCLT     DS    XL2                                                              
QCLT     DS    CL3                                                              
*                                                                               
FLAG     DS    X                                                                
EBCBYR   DS    CL3                EBCDIC BUYER CODE                             
BINBYR   DS    X                  BINARY BUYER CODE                             
ERRMESS  DS    CL30               ERROR MESSAGE                                 
SDREC    DS    CL9                                                              
*                                                                               
CLTTABD  DSECT                                                                  
CLTAGMD  DS    XL1                                                              
CLTCODE  DS    XL2                                                              
CLTALPH  DS    CL3                                                              
CLTLNQ   EQU   *-CLTTABD                                                        
*                                                                               
BYRD     DSECT                                                                  
BYRAGYMD DS    X                   BINARY AGENCY MEDIA                          
BYREB    DS    CL3                 EBCDIC BUYER CODE                            
BYRBIN   DS    X                   BINARY BUYER CODE                            
BYRLNQ   EQU   *-BYRD                                                           
*                                                                               
SVBYTABD DSECT                     SD SUPERVISOR/BUYER                          
SVBYMED  DS    X                   A/M                                          
SVBYOFF  DS    CL2                 OFFICE CODE                                  
SVBYCODE DS    CL4                 SUPERVISOR/BUYER CODE                        
SVBYLNQ  EQU   *-SVBYTABD                                                       
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*                                                                               
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSBYR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPLDEXTMMD05/02/00'                                      
         END                                                                    
