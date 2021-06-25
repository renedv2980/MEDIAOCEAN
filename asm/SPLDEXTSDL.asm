*          DATA SET SPLDEXTSDL AT LEVEL 031 AS OF 01/20/05                      
*PHASE SP@XTSDM SP@XTSDL                                                        
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'DMLDEXT - MERGE NEW SDESK DATA WITH BASE FILE'                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* NOTE: THESE FIELDS MAY HAVE TO BE CHANGED!!!                                  
* USE AGY TO DETERMINE WHICH AGENCY THIS IS FOR                                 
* USE BAM TO NOTE THE AGENCY HEX CODE                                           
* USE KEEPAGY TO NOTE WHETHER TO SAVE CHANGES TO THE AGENCY RECORD              
* USE PRGRUNG TO SAY WHICH PROGRAM THIS OVERNIGHT WILL RUN FOR                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
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
* P3=A(PARAM CARD)  PASS 2          PASS 2 BYTE AGENCY CODE                     
*                   PASS 2          PASS AGENCY/MEDIA (CHAR)                    
*                   PASS 1          PASS PRGRUNG TYPE                           
*                   PASS 1          PASS Y/N KEEP AGENCY RECORDS                
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,R7                                           
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
DMXKEEP  MVC   LASTKEY,0(R3)       SAVE KEY                                     
         L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
*                                                                               
DMXPURGE MVC   LASTKEY,0(R3)       SAVE KEY                                     
         L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   P,C'P'                                                           
         GOTO1 =V(HEXOUT),DMCB,(R3),P+2,13,=C'TOG'                              
         GOTO1 VPRINTER            PRINT KEY                                    
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
         MVI   CLTTAB,X'FF'                                                     
         MVI   SPVTAB,X'FF'                                                     
         MVI   BUYTAB,X'FF'                                                     
         MVI   BYRTABLE,X'FF'                                                   
         MVI   FLAG2,0                                                          
         MVI   FLAG,0                                                           
*                                                                               
         L     R2,APARAMC                                                       
         CLC   0(2,R2),=C'  '      IS AGENCY VALID?                             
         BH    *+6                                                              
         DCHO                                                                   
         MVC   AGY,0(R2)           SAVE AGENCY                                  
*                                                                               
         LA    R3,2(R2)                                                         
         GOTO1 =V(HEXIN),DMCB,(R3),BAM,2  SAVE BAM                              
         ICM   RE,15,12(R1)        IS SOURCE BAM VALID?                         
         BNZ   *+6                                                              
         DCHO                                                                   
*                                                                               
         CLI   4(R2),X'40'         IS PRGRUNG VALID?                            
         BH    *+6                                                              
         DCHO                                                                   
         MVC   PRGRUNG,4(R2)                                                    
*                                                                               
         CLI   5(R2),C'Y'          IS KEEPAGY VALID?                            
         BE    DMXINI10                                                         
         CLI   5(R2),C'N'                                                       
         BE    DMXINI10                                                         
         BH    *+6                                                              
         DCHO                                                                   
DMXINI10 MVC   KEEPAGY,5(R2)                                                    
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC - RETURN ALLOWED                        
*                                                                               
DMXREC   L     R3,AREC                                                          
         CLI   FLAG2,1             ARE CLIENT, BUYER, SUPERVISOR ,AND           
         BE    *+8                 NWS BUYER TABLES BUILT?                      
         BAS   RE,BLDCTAB          NO, BUILD TABLES                             
*                                                                               
         CLI   FLAG,1              RECORDS ARE FROM BASEFILE UNTIL              
         BE    DMXREC10            KEYS GET LOWER, THEN FROM TAPEOUT            
         CLC   LASTKEY,0(R3)                                                    
         BH    *+8                 IF READING FROM BASEFILE, GET RECORD         
         B     GETTYPE             TYPE                                         
         MVI   FLAG,1              SET FLAG FOR READING FROM TAPEOUT            
DMXREC10 B     DMXKX30             KEEP ALL RECORDS FROM TAPEOUT FILE           
*                                                                               
RECTYPA  MVC   BAMCLT,2(R3)        XX XX A/M CLT CLT                            
         B     DMXKX20                                                          
*                                                                               
RECTYPB  MVC   BAGYMD,2(R3)        XX XX A/M XX CLT CLT                         
         MVC   BCLT,4(R3)                                                       
         B     DMXKX20                                                          
*                                                                               
RECTYPC  MVC   BAGYMD,2(R3)        XX XX A/M 5XX'S CLT CLT                      
         MVC   BCLT,8(R3)                                                       
         B     DMXKX20                                                          
*                                                                               
RECTYPD  MVC   BAMCLT,1(R3)        XX A/M CLT CLT                               
         B     DMXKX20                                                          
*                                                                               
RECTYPE  MVC   BAGYMD,4(R3)        4X A/M 6X CLT CLT                            
         MVC   BAMCLT,11(R3)                                                    
         B     DMXKX20                                                          
*                                                                               
RECTYPF  MVC   BAMCLT,0(R3)        A/M CLT CLT                                  
         B     DMXKX20                                                          
*                                                                               
RECTYPH  CLC   1(2,R3),AGY         XX AGY AGY MED CLT CLT                       
         BNE   DMXKX30             KEEP IF NOT CORRECT AGENCY                   
         MVC   BAGYMD,BAM                                                       
         MVC   QMED,3(R3)                                                       
         MVC   BCLT,4(R3)                                                       
         B     DMXKX                                                            
*                                                                               
RECTYPI  MVC   BAGYMD,1(R3)        XX A/M XX XX XX CLT CLT                      
         MVC   BCLT,5(R3)          CLIENT                                       
         B     DMXKX20                                                          
*                                                                               
RECTYPJ  MVC   BAMCLT,8(R3)        8XX'S A/M CLT CLT                            
         B     DMXKX20                                                          
*                                                                               
RECTYPK  CLC   2(2,R3),AGY         XX XX AGY AGY 4XX'S CLT CLT                  
         BNE   DMXKX30             KEEP IF NOT CORRECT AGENCY                   
         MVC   BCLT,8(R3)                                                       
         MVC   BAGYMD,BAM                                                       
         NI    BAGYMD,X'F0'        CLEAR MEDIA                                  
         B     DMXKX10                                                          
*                                                                               
RECTYPL  CLC   2(2,R3),AGY         XX XX AGY AGY 6XX'S CLT CLT                  
         BNE   DMXKX30             KEEP IF NOT CORRECT AGENCY                   
         MVC   BCLT,10(R3)                                                      
         MVC   BAGYMD,BAM                                                       
         NI    BAGYMD,X'F0'        CLEAR MEDIA                                  
         B     DMXKX10                                                          
*                                                                               
RECTYPM  DS    0H                                                               
         CLI   PRGRUNG,C'O'        IF RUNNING THIS FOR OM                       
         BNE   RECM10                                                           
         CLI   1(R3),X'3D'         PURGE ALL 0D3D'S                             
         BE    DMXPURGE            XX XX A/M 3X CLT CLT                         
RECM10   MVC   BAGYMD,2(R3)                                                     
         MVC   BCLT,6(R3)                                                       
         CLI   6(R3),0             XX XX A/M 3X CLT CLT                         
         BE    DMXKX30             KEEP IF THERE ISN'T A CLIENT                 
         B     DMXKX20                                                          
*                                                                               
RECTYPN  MVC   BAGYMD,4(R3)        XX XX XX XX A/M                              
         USING DAREORDD,R3                                                      
         LA    R5,DORFRST          FIRST ELEMENT                                
         USING DOIDELD,R5                                                       
         MVC   BCLT,DOIDCLT                                                     
         DROP  R3,R5                                                            
         B     DMXKX20                                                          
*                                                                               
RECTYPO  MVC   BAGYMD,2(R3)        XX XX A/M 4XX'S CLT CLT                      
         MVC   BCLT,7(R3)          CLIENT                                       
         B     DMXKX20                                                          
*                                                                               
RECTYPP  MVC   BAMCLT,4(R3)        XX XX XX XX A/M CLT CLT                      
         B     DMXKX20                                                          
*                                                                               
RECTYPQ  CLC   2(2,R3),AGY         XX XX AGY AGY 2XX'S MED CLT CLT              
         BNE   DMXKX30             KEEP IF NOT CORRECT AGENCY                   
         MVC   BAGYMD,BAM                                                       
         MVC   QMED,6(R3)                                                       
         MVC   BCLT,7(R3)                                                       
         B     DMXKX                                                            
*                                                                               
RECTYPR  MVC   BAGYMD,2(R3)        XX XX A/M XX XX CLT CLT                      
         MVC   BCLT,5(R3)          CLT                                          
         B     DMXKX20                                                          
*                                                                               
RECTYPS  MVC   BAGYMD,2(R3)        XX XX A/M CLT CLT CLT                        
         MVC   QCLT,3(R3)                                                       
         BAS   RE,GETBCLT          PURGE IF CLIENT IN LIST                      
         BE    DMXPURGE                                                         
         B     DMXKX30                                                          
*                                                                               
RECTYPTS LA    R5,SPVTAB           SUPERDESK SUPERVISOR RECORDS                 
         BAS   RE,SPVBUY           PURGE IF SUPERVISOR IN TABLE                 
         BE    DMXPURGE                                                         
         B     DMXKX30                                                          
*                                                                               
RECTYPTB LA    R5,BUYTAB           SUPERDESK BUYER RECORDS                      
         BAS   RE,SPVBUY           PURGE IF BUYER IN TABLE                      
         BE    DMXPURGE                                                         
         B     DMXKX30                                                          
*                                                                               
RECTYPTU MVC   BAGYMD,2(R3)        SUPERDESK USER DEFINITION RECORDS            
         MVC   BCLT,3(R3)                                                       
         B     DMXKX20                                                          
*                                                                               
RECTYPU  DS    0H                  NWS BUYER RECORDS                            
         USING BYRRECD,R3                                                       
         CLI   BYREL,BYRELCDQ      KEEP IF NO BUYER DESC ELEMENT                
         BNE   DMXKX30                                                          
         MVC   BAGYMD,BYRKAGMD     AGENCY/MEDIA                                 
         MVC   EBCBYR,BYRKBYR      EBCDIC BUYER CODE                            
         MVC   BINBYR,BYRCODE      BINARY BUYER CODE                            
         BAS   RE,UPDTBY           UPDATE BUYER CODE                            
         BE    DMXPURGE            PURGE IF IN TABLE                            
         B     DMXKX30                                                          
*                                                                               
RECTYPV  MVC   BAGYMD,2(R3)        NWS RECORDS                                  
         MVC   BINBYR,3(R3)                                                     
         BAS   RE,LOOKUP           PURGE IF IN TABLE                            
         BE    DMXPURGE                                                         
         B     DMXKX30                                                          
*                                                                               
RECTYPW  DS    0H                  AGENCY RECORD                                
         CLI   KEEPAGY,C'Y'        KEEP AGENCY RECORD?                          
         BNE   DMXKX30             NO, SO KEEP THIS ONE                         
         CLC   AGY,1(R3)           COMPARE AGENCY                               
         BNE   DMXKX30                                                          
         B     DMXPURGE                                                         
*                                                                               
RECTYPX  DS    0H                  0D XX A/M                                    
         CLI   KEEPAGY,C'Y'        KEEP AGENCY RECORD?                          
         BNE   DMXKX30             THEN KEEP THESE RECORDS ALSO IF              
         MVC   BYTE,2(R3)          AGENCY DONESN'T MATCH BAM                    
         NI    BYTE,X'F0'                                                       
         MVC   BAGYMD,BAM                                                       
         NI    BAGYMD,X'F0'                                                     
         CLC   BYTE,BAGYMD                                                      
         BE    DMXPURGE                                                         
         B     DMXKX30                                                          
*                                                                               
DMXKX    DS    0H                                                               
         NI    BAGYMD,X'F0'        CLEAR MEDIA                                  
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         OI    BAGYMD,X'08'                                                     
         CLI   QMED,C'X'                                                        
         BNE   *+8                                                              
         OI    BAGYMD,X'04'                                                     
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         OI    BAGYMD,X'03'                                                     
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         OI    BAGYMD,X'02'                                                     
         CLI   QMED,C'T'                                                        
         BNE   *+8                                                              
DMXKX10  OI    BAGYMD,X'01'        IF WE KNOW MEDIA CODE IS "01"                
         MVI   QMED,0                                                           
*                                                                               
DMXKX20  BAS   RE,CHKBCLT          PURGE IF CLIENT IS IN LIST                   
         BE    DMXPURGE                                                         
*                                                                               
DMXKX30  DS    0H                                                               
         MVI   P,C'K'                                                           
         GOTO1 =V(HEXOUT),DMCB,(R3),P+2,13,=C'TOG'                              
         GOTO1 VPRINTER            PRINT KEY                                    
         B     DMXKEEP                                                          
*                                                                               
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
***********************************************************************         
*        GET CLIENTS, BUYERS, SUPERVISORS, NWS BUYERS FROM DATASET    *         
*        AND BUILD TABLES FOR EACH                                    *         
***********************************************************************         
*                                                                               
BLDCTAB  NTR1                                                                   
         GOTO1 =V(DATCON),DMCB,(5,0),(10,TODAY)                                 
         MVI   FLAG2,1                                                          
         OPEN  SDCLT                                                            
         GET   SDCLT,SDDAY                                                      
         CLC   TODAY,SDDAY                                                      
         BE    BCTAB05                                                          
         MVC   ERRMESS(L'DAYERR),DAYERR                                         
         LA    R5,L'DAYERR-1       LENGTH OF ERROR MESSAGE                      
         BAS   RE,PRTERR           PRINT ERROR MESSAGE                          
         DC    H'00'               MUST BE TODAY'S DATE                         
*                                                                               
BCTAB05  LA    R4,CLTTAB                                                        
         LA    R2,SPVTAB                                                        
         LA    R3,BUYTAB                                                        
         LA    R5,BYRTABLE                                                      
*                                                                               
BCTAB10  GET   SDCLT,SDREC                                                      
         CLI   SDREC,C'C'          CLIENT                                       
         BE    BCTAB20                                                          
         CLI   SDREC,C'S'          SUPERVISOR                                   
         BE    BCTAB30                                                          
         CLI   SDREC,C'B'          BUYER                                        
         BE    BCTAB40                                                          
         CLI   SDREC,C'N'          NWS BUYER                                    
         BE    BCTAB50                                                          
         MVC   ERRMESS(L'BADDATA),BADDATA                                       
         LA    R5,L'BADDATA-1      LENGTH                                       
         BAS   RE,PRTERR           PRINT ERROR MESSAGE                          
         DC    H'00'               BAD DATASET                                  
*                                                                               
         USING CLTTABD,R4                                                       
BCTAB20  MVC   CLTAGMD(6),SDREC+1  CLIENT TABLE                                 
         MVI   CLTLNQ(R4),X'FF'    MARK TEMPORARY END OF TABLE                  
         LA    R4,CLTLNQ(R4)                                                    
         B     BCTAB10                                                          
         DROP  R4                                                               
*                                                                               
         USING SVBYTABD,R2                                                      
BCTAB30  MVC   SVBYMED(7),SDREC+1  SUPERVISOR TABLE                             
         MVI   SVBYLNQ(R2),X'FF'   MARK TEMPORARY END OF TABLE                  
         LA    R2,SVBYLNQ(R2)                                                   
         B     BCTAB10                                                          
         DROP  R2                                                               
*                                                                               
         USING SVBYTABD,R3                                                      
BCTAB40  MVC   SVBYMED(7),SDREC+1  BUYER TABLE                                  
         MVI   SVBYLNQ(R3),X'FF'   MARK TEMPORARY END OF TABLE                  
         LA    R3,SVBYLNQ(R3)                                                   
         B     BCTAB10                                                          
         DROP  R3                                                               
*                                                                               
         USING BYRD,R5                                                          
BCTAB50  MVC   BYRAGYMD(5),SDREC+1 NWS BUYER TABLE                              
         MVI   BYRLNQ(R5),X'FF'    MARK TEMPORARY END OF TABLE                  
         LA    R5,BYRLNQ(R5)                                                    
         B     BCTAB10                                                          
         DROP  R5                                                               
*                                                                               
EOFCLT   CLOSE SDCLT                                                            
         B     EXIT                                                             
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
         OC    BCLT,BCLT           HAVE BINARY CLIENT?                          
         BZ    CBCLTX                                                           
         CLC   CLTCODE,BCLT        CORRECT CLIENT?                              
         BE    CBCLTX                                                           
CBCLT20  LA    R5,CLTLNQ(R5)                                                    
         B     CBCLT10                                                          
CBCLTX   B     YES                                                              
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
DATADISP DC    H'0024'                                                          
STARS    DC    80C'*'                                                           
UNDEFERR DC    C'RECORD TYPE NOT DEFINED'                                       
DAYERR   DC    C'CLIENT DATASET MUST BE GENERATED TODAY'                        
BADDATA  DC    C'BAD DATA SET'                                                  
*                                                                               
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
SDCLT    DCB   DDNAME=SDCLT,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=EOFCLT             
         SPACE                                                                  
*                                                                               
***********************************************************************         
*        TAB OF REC TYPES ACCORDING TO AGY AND CLT POSITION IN RECORD *         
***********************************************************************         
*                                                                               
ODTAB    DC    AL4(RECTYPA)                0DXX A/M CLT CLT                     
         DC    X'01020D1527283840'                                              
         DC    X'444548494A4E5357585C'                                          
         DC    X'696D6F7071727678797A7C4C'                                      
         DC    X'9091FF'                                                        
*                                                                               
         DC    AL4(DMXKX30)                KEEP                                 
         DC    X'060E14161819203233'                                            
         DC    X'363B3C464750525A5B5E'                                          
         DC    X'60E2E5'                                                        
         DC    X'6C737B7D7EFEFD4D4F'                                            
         DC    X'12224B77FF'                                                    
*                                                                               
         DC    AL4(DMXPURGE),X'5DFF'       0D5D                                 
*                                                                               
         DC    AL4(RECTYPB),X'0C17FF'      0DXX A/M XX CLT CLT                  
         DC    AL4(RECTYPC),X'3575FF'      0DXX A/M 5X CLT CLT                  
         DC    AL4(RECTYPE),X'3EFF'        0DXX 2X A/M 6X CLT CLT               
         DC    AL4(RECTYPJ),X'03FF'        0DXX 6X A/M CLT CLT                  
         DC    AL4(RECTYPK),X'11FF'        0DXX AGY AGY 4X CLT CLT              
         DC    AL4(RECTYPL),X'13FF'        0DXX AGY AGY 6X CLT CLT              
         DC    AL4(RECTYPM),X'313DFF'      0DXX A/M 3X CLT CLT                  
         DC    AL4(RECTYPN),X'34FF'        0DXX 2X A/M, CLT IN RECORD           
         DC    AL4(RECTYPO),X'37FF'        0DXX A/M 4X CLT CLT                  
         DC    AL4(RECTYPP),X'3AFF'        0DXX 2X A/M CLT CLT                  
         DC    AL4(RECTYPQ),X'43FF'        0DXX AGY AGY 2X MED CLT CLT          
         DC    AL4(RECTYPR),X'E3FF'        0DXX A/M 2X CLT CLT                  
         DC    AL4(RECTYPTS),X'61FF'       SD SUPERVISOR RECORDS                
         DC    AL4(RECTYPTB),X'62FF'       SD BUYER RECORDS                     
         DC    AL4(RECTYPTU),X'08FF'       0D A/M CLT CLT (SD UDEF REC)         
         DC    AL4(RECTYPU),X'65FF'        NWS BUYER RECORDS                    
         DC    AL4(RECTYPV),X'6667686BFF'  NWS RECORDS                          
         DC    AL4(RECTYPX),X'040526FFFF'  0DXX A/M                             
*                                                                               
OATAB    DC    AL4(RECTYPA)                0AXX A/M CLT CLT                     
         DC    X'2122232425272A2E2F34'                                          
         DC    X'353637442C2D313242FF'                                          
         DC    AL4(DMXKX30),X'26282943FF'  KEEP                                 
         DC    AL4(RECTYPO),X'2BFF'        0AXX A/M 4X CLT CLT                  
         DC    AL4(RECTYPS),X'41FFFF'      0AXX A/M CLT CLT CLT                 
*                                                                               
OETAB    DC    AL4(RECTYPA),X'01FFFF'      0EXX A/M CLT CLT                     
*                                                                               
TYPTAB   DC    AL4(RECTYPD),X'000237B7FF'  XX A/M CLT CLT                       
         DC    AL4(RECTYPH),X'09FF'        XX AGY AGY XX CLT CLT                
         DC    AL4(RECTYPI),X'0BFF'        XX A/M 3X CLT CLT                    
         DC    AL4(RECTYPW),X'06FF'        06 AGY AGY                           
         DC    AL4(DMXKX30)                KEEP                                 
         DC    X'0305080CFFFF'                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* THESE FIELDS MAY HAVE TO BE CHANGED                                           
***KEEPAGY  DC    C'N'               KEEP CHANGES TO AGENCY RECORD              
*                                                                               
* VALUES FOR NET STEWARD ON CANTEST                                             
***AGY      DC    C'HA'              2 BYTE AGENCY CODE                         
***BAM      DC    X'21'              A/M CODE                                   
***PRGRUNG  DC    C'P'               PROGRAM RUNNING IS OM                      
*                                                                               
* VALUES FOR TMNY PROGRAMS                                                      
***AGY      DC    C'TM'              2 BYTE AGENCY CODE                         
***BAM      DC    X'11'              A/M CODE                                   
***PRGRUNG  DC    C'D'               POGRAM RUNNING SPOT DESKTOP                
***PRGRUNG  DC    C'O'               PROGRAM RUNNING IS OM                      
***PRGRUNG  DC    C'S'               PROGRAM RUNNING (SUPERDESK)                
***PRGRUNG  DC    C'M'               PROGRAM RUNNING (MATCHMAKER)               
***PRGRUNG  DC    C'T'               PROGRAM RUNNING (TAM)                      
*                                                                               
AGY      DS    CL2                2 BYTE AGENCY CODE                            
BAM      DS    X                  A/M CODE                                      
PRGRUNG  DS    C                  POGRAM RUNNING                                
KEEPAGY  DS    C                  Y/N KEEP CHANGES TO AGENCY RECORD             
*                                                                               
CLTTAB   DS    CL601              100 ENTRIES 6 BYTES EACH                      
SPVTAB   DS    CL350              50 ENTRIES 7  BYTES EACH                      
BUYTAB   DS    CL350              50 ENTRIES 7 BYTES EACH                       
BYRTABLE DS    CL250              50 ENTRIES 5 BYTES EACH                       
*                                                                               
QMED     DS    C                                                                
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
FLAG2    DS    X                                                                
LASTKEY  DS    XL13                                                             
EBCBYR   DS    CL3                 EBCDIC BUYER CODE                            
BINBYR   DS    X                   BINARY BUYER CODE                            
SDDAY    DS    CL9                                                              
TODAY    DS    CL8                                                              
ERRMESS  DS    CL40                                                             
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
SDREC    DS    CL9                                                              
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
**PAN#1  DC    CL21'031SPLDEXTSDL01/20/05'                                      
         END                                                                    
