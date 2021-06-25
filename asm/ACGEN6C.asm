*          DATA SET ACGEN6C    AT LEVEL 026 AS OF 01/16/13                      
*&&      SET   NOP=N                                                            
*PHASE T00A6CA                                                                  
GEN6C    TITLE 'FILE ROUTINE FACILITIES'                                        
GEN6C    CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
***********************************************************************         
* BRANCH INDEX HELD IN HIGH ORDER BYTE OF RF                          *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
ROUT     NMOD1 RTWORKL,ACGEN6C*,R6,CLEAR=YES                                    
         USING RTWORKD,RC                                                       
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
*                                                                               
         B     LEFTFLD             LEFT-JUSTIFY FIELD                           
         DS    4X'00'              UNUSED                                       
         B     VUIDD               VALIDATE USER-ID                             
         B     NAMCHA              WRITE NAME CHANGE POINTER                    
*                                                                               
         B     GETEL               GET ELEMENT                                  
         B     DELEL               DELETE ELEMENT                               
         B     ADDEL               ADD ELEMENT                                  
         B     REPEL               REPLACE ELEMENT                              
*                                                                               
         B     ADDBAL              ADD BALANCE/PEEL-OFF ELEMENTS                
         B     ADDRST              ADD STATUS ELEMENT                           
         B     ADDAST              ADD ADDITIONAL STATUS ELEMENT                
         B     GETNAM              GET NAME                                     
         B     GETADR              GET ADDRESS LINE                             
         B     GETFFT              GET FREE FORM TEXT FROM ELEMENT              
         B     BLDFFT              BUILD FREE FORM TEXT ELEMENT                 
         B     GETSPA              GET SPAEL                                    
         B     BLDSPA              BUILD SPAEL                                  
*                                                                               
         B     VALDD               VALIDATE DICTIONARY REF                      
         B     DISDD               DISPLAY DICTIONARY REF                       
         B     PIDPAS              BUILD PID PASSIVE                            
         B     BLDPID              BUILD PIDEL                                  
         DC    8XL4'00'                                                         
*                                                                               
ROUTL    MVI   BCDUB,0             SET CC LOW                                   
         B     ROUTCC                                                           
ROUTH    MVI   BCDUB,2             SET CC HIGH                                  
         B     ROUTCC                                                           
ROUTE    MVI   BCDUB,1             SET CC EQUAL                                 
ROUTCC   CLI   BCDUB,1                                                          
*                                                                               
ROUTX    XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
RTPARM   DS    0XL4                * PARAMETERS 1-6 *                           
RTPARM1  DS    XL4                                                              
RTPARM2  DS    XL4                                                              
RTPARM3  DS    XL4                                                              
RTPARM4  DS    XL4                                                              
RTPARM5  DS    XL4                                                              
RTPARM6  DS    XL4                                                              
RTDATA   DS    XL300               FOR EACH ROUTINE                             
RTWORKL  EQU   *-RTWORKD                                                        
GEN6C    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LEFT JUSTIFY FIELD                                       *         
*                                                                     *         
* NTRY: R1=A(FIELD)                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING FHD,R1                                                           
LEFTFLD  XR    R0,R0                                                            
         IC    R0,FHLN                                                          
         LA    RE,FHDAD                                                         
         SR    R0,RE                                                            
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         SR    R0,RE                                                            
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         LA    RF,FHDA(RE)                                                      
*                                                                               
LFLD02   CLI   FHDA,C' '                                                        
         BH    LEFTFLDX                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FHDA(0),FHDA+1                                                   
         MVI   0(RF),0                                                          
         BCT   R0,LFLD02                                                        
*                                                                               
LEFTFLDX B     ROUTE                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE USER-ID NAME                                    *         
*                                                                     *         
* NTRY: R1=A(NAME)                                                    *         
* EXIT: BCWORK=NUMBER                                                 *         
***********************************************************************         
         SPACE 1                                                                
VUIDD    LA    R2,IOKEY                                                         
         USING CTIREC,R2           R2=A(ID RECORD)                              
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,0(R1)                                                     
         L     R1,=AL4(XOCONFIL+XORD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   ROUTX                                                            
*                                                                               
         L     R2,AIO2                                                          
         LA    RF,CTIDATA                                                       
         USING CTDSCD,RF           RF=A(DESCRIPTION ELEMENT)                    
         XR    RE,RE                                                            
VUID02   IC    RE,CTDSCLEN                                                      
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    *+8                                                              
         BXH   RF,RE,VUID02                                                     
         SH    RE,=Y(CTDSC+1-CTDSCD)                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BCWORK(0),CTDSC                                                  
         B     ROUTE                                                            
         DROP  RF,R2                                                            
         EJECT                                                                  
***********************************************************************         
* WRITE NAME CHANGE POINTER FOR ACCOUNT RECORD                        *         
*                                                                     *         
* NTRY: R1=A(RECORD KEY)                                              *         
***********************************************************************         
         SPACE 1                                                                
NAMCHA   MVC   RTIOWS,IOWS         SAVE I/O WORKING STORAGE                     
         LA    R2,IOKEY                                                         
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,0(R1)                                                     
*                                                                               
         MVC   IOKEY(L'ACTKEY),ACTKEY                                           
         L     R1,=AL4(XORD+XOACCDIR) READ ACCOUNT DIRECTORY RECORD             
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RTACCDIR,ACTRECD                                                 
         LA    R2,RTACCDIR                                                      
         LA    R3,IOKEY                                                         
         USING ANCRECD,R3                                                       
         XC    ANCKEY(ACCKLEN),ANCKEY                                           
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         L     R1,=AL4(XORDD+XOACCDIR)                                          
         GOTOX ('XIO',AGROUTS)                                                  
         TM    IOERR,IOEDEL                                                     
         BO    NAMCHAX             NAME CHANGE POINTER ALREADY ON FILE          
*                                                                               
         XC    ANCKEY(ACCKLEN),ANCKEY  ADD IT TO FILE                           
         MVI   ANCKTYP,ANCKTYPQ                                                 
         MVC   ANCKCULA,ACTKCULA                                                
         OI    ANCKSTAT,ANCSDELT                                                
         MVC   ANCKDA,ACTKDA                                                    
         L     R1,=AL4(XOADD+XOACCDIR)                                          
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NAMCHAX  MVC   IOWS(L'RTIOWS),RTIOWS                                            
         B     ROUTE                                                            
         SPACE 1                                                                
         DROP  R2,R3                                                            
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
         ORG   RTDATA                                                           
RTIOWS   DS    XL(IOWSX-IOWS)                                                   
RTACCDIR DS    XL(ACCKLEN)                                                      
GEN6C    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET ELEMENT ROUTINE                                                 *         
*                                                                     *         
* NTRY: P1=(ELEMENT CODE, A(RECORD))                                  *         
*       P2=(L'SEARCH ARGUMENT,A(SEARCH ARGUMENT))                     *         
* EXIT: CC=EQUAL IF ELEMENT FOUND, BOELEM=ELEMENT                     *         
***********************************************************************         
         SPACE 1                                                                
GETEL    XC    BOELEM,BOELEM                                                    
         LM    RF,R0,0(R1)                                                      
         GOTO1 VHELLO,BCPARM,(C'G',GCFILNAM),(RF),(R0)                          
         CLI   12(R1),0                                                         
         BNE   ROUTL                                                            
         L     RF,12(R1)                                                        
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   BOELEM(0),0(RF)                                                  
         B     ROUTE                                                            
         SPACE 1                                                                
***********************************************************************         
* DELETE ELEMENT ROUTINE                                              *         
*                                                                     *         
* NTRY: P1=(ELEMENT CODE, A(RECORD))                                  *         
*       P2=(L'SEARCH ARGUMENT,A(SEARCH ARGUMENT))                     *         
***********************************************************************         
         SPACE 1                                                                
DELEL    LM    RF,R0,0(R1)                                                      
         GOTO1 VHELLO,BCPARM,(C'D',GCFILNAM),(RF),(R0)                          
         CLI   12(R1),0                                                         
         BNE   ROUTL                                                            
         B     ROUTE                                                            
         SPACE 1                                                                
***********************************************************************         
* ADD ELEMENT ROUTINE                                                 *         
*                                                                     *         
* NTRY: P1=A(RECORD), BOELEM=ELEMENT                                  *         
***********************************************************************         
         SPACE 1                                                                
ADDEL    L     RF,0(R1)                                                         
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),(RF),BOELEM                        
         CLI   12(R1),0                                                         
         BNE   ROUTL                                                            
         B     ROUTE                                                            
         EJECT                                                                  
***********************************************************************         
* REPLACE ELEMENT ROUTINE                                             *         
*                                                                     *         
* NTRY: P1=(ELEMENT CODE, A(RECORD))                                  *         
*       P2=(L'SEARCH ARGUMENT,A(SEARCH ARGUMENT)) OR 0                *         
*       P3=A(REPLACEMENT ELEMENT) OR 0                                *         
* EXIT: CC=EQUAL IF ELEMENT HAS NOT CHANGED                           *         
*       CC=LOW IF ELEMENT HAS CHANGED                                 *         
***********************************************************************         
         SPACE 1                                                                
REPEL    LM    R2,R4,0(R1)                                                      
*                                                                               
REPEL02  GOTO1 VHELLO,BCPARM,(C'G',GCFILNAM),(R2),(R3)                          
         CLI   12(R1),0                                                         
         BE    REPEL04                                                          
*                                                                               
         LTR   R4,R4               ELEMENT NOT IN RECORD                        
         BZ    REPELUNC                                                         
         CLI   0(R4),0                                                          
         BE    REPELUNC                                                         
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),(R2),(R4)                          
         CLI   12(R1),0                                                         
         BE    REPELCHA                                                         
         DC    H'0'                                                             
*                                                                               
REPEL04  LTR   R4,R4               TEST FOR REPLACE ELEMENT                     
         BZ    *+12                                                             
         CLI   0(R4),0                                                          
         BNE   REPEL06                                                          
         GOTO1 VHELLO,BCPARM,(C'D',GCFILNAM),(R2),(R3)                          
         B     REPELCHA                                                         
*                                                                               
REPEL06  L     RF,12(R1)           TEST ELEMENT IS DIFFERENT                    
         IC    RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(RF)                                                    
         BE    REPELUNC                                                         
*                                                                               
         GOTO1 VHELLO,BCPARM,(C'D',GCFILNAM),(R2),(R3)                          
         GOTO1 (RF),(R1),(C'P',GCFILNAM),(R2),(R4)                              
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REPELCHA B     ROUTL               ELEMENT WAS CHANGED                          
*                                                                               
REPELUNC B     ROUTE               ELEMENT WAS NOT CHANGED                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD BALANCE/PEEL-OFF ELEMENTS                            *         
*                                                                     *         
* NTRY: R1=A(RECORD)                                                  *         
***********************************************************************         
         SPACE 1                                                                
ADDBAL   LR    R2,R1                                                            
         LA    R3,RTDATA                                                        
         USING ABLELD,R3           ADD BALANCE ELEMENT                          
         MVI   ABLEL,ABLELQ                                                     
         MVI   ABLLN,ABLLN3Q                                                    
         ZAP   ABLFRWD,BCPZERO                                                  
         ZAP   ABLDR,BCPZERO                                                    
         ZAP   ABLCR,BCPZERO                                                    
         ZAP   ABLURG,BCPZERO                                                   
         XC    ABLTXS,ABLTXS                                                    
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),(R2),ABLELD                        
         CLI   12(R1),0                                                         
         BNE   ROUTX                                                            
*                                                                               
         USING APOELD,R3           ADD PEEL OFF ELEMENT                         
         XC    APOELD(APOLN1Q),APOELD                                           
         MVI   APOEL,APOELQ                                                     
         MVI   APOLN,APOLN1Q                                                    
         ZAP   APODR,BCPZERO                                                    
         ZAP   APOCR,BCPZERO                                                    
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),(R2),APOELD                        
         CLI   12(R1),0                                                         
         B     ROUTX                                                            
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO ADD RECORD STATUS ELEMENT                                *         
*                                                                     *         
* NTRY: R1=A(RECORD)                                                  *         
***********************************************************************         
         SPACE 1                                                                
ADDRST   LR    R2,R1                                                            
         LA    R3,RTDATA                                                        
         USING RSTELD,R3                                                        
         XC    RSTELD(RSTLN3Q),RSTELD                                           
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         MVC   RSTBDATE,BCTODAYP                                                
         MVC   RSTTDATE,BCTODAYP                                                
         MVI   RSTCOSTG,C' '                                                    
         MVI   RSTFILT1,C' '                                                    
         MVI   RSTFILT2,C' '                                                    
         MVI   RSTFILT3,C' '                                                    
         MVI   RSTFILT4,C' '                                                    
         MVI   RSTFILT5,C' '                                                    
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),(R2),RSTELD                        
         CLI   12(R1),0                                                         
         B     ROUTX                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD RECORD ADDITIONAL STATUS ELEMENT                     *         
*                                                                     *         
* NTRY: R1=A(RECORD)                                                  *         
***********************************************************************         
         SPACE 1                                                                
ADDAST   LR    R2,R1                                                            
         LA    R3,RTDATA                                                        
         USING ASTELD,R3                                                        
         XC    ASTELD(ASTLN1Q),ASTELD                                           
         MVI   ASTEL,ASTELQ                                                     
         MVI   ASTLN,ASTLN1Q                                                    
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),(R2),ASTELD                        
         CLI   12(R1),0                                                         
         B     ROUTX                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET NAME                                                            *         
*                                                                     *         
* NTRY: R1=A(RECORD)                                                  *         
* EXIT: FVIFLD=NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETNAM   LR    R2,R1                                                            
         GOTO1 VHELLO,BCPARM,(C'G',GCFILNAM),('NAMELQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BNE   ROUTL                                                            
         L     R2,12(R1)                                                        
         USING NAMELD,R2                                                        
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),NAMEREC                                                
         B     ROUTE                                                            
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* GET ADDRESS LINE                                                    *         
*                                                                     *         
* NTRY: P1=(LINE NUMBER (1-4), A(RECORD))                             *         
*       P1=X'80' BIT ON - GET THE OADEL NOT THE ADREL                 *         
***********************************************************************         
         SPACE 1                                                                
GETADR   MVC   RTPARM1,0(R1)                                                    
         XR    R2,R2                                                            
         ICM   R2,7,RTPARM1+1                                                   
         LA    R0,ADRELQ                                                        
         TM    RTPARM1,X'80'                                                    
         BZ    *+8                                                              
         LA    R0,OADELQ                                                        
*                                                                               
         NI    RTPARM1,FF-(X'80')                                               
         CLI   RTPARM1,1                                                        
         BL    *+12                                                             
         CLI   RTPARM1,4                                                        
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VHELLO,BCPARM,(C'G',GCFILNAM),((R0),(R2)),0                      
         CLI   12(R1),0                                                         
         BNE   ROUTL                                                            
         L     R2,12(R1)                                                        
         USING ADRELD,R2                                                        
         CLC   ADRNUM,RTPARM1                                                   
         BL    ROUTL                                                            
         XR    R4,R4                                                            
         IC    R4,RTPARM1                                                       
         BCTR  R4,0                                                             
         MH    R4,=Y(L'ADRADD1)                                                 
         LA    R4,ADRADD1(R4)                                                   
         MVC   FVIFLD(L'ADRADD1),0(R4)                                          
*                                                                               
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET FREE FORM TEXT ELEMENT                                          *         
*                                                                     *         
* NTRY: P1=A(RECORD)                                                  *         
*       P2=(SEQUENCE NO,TEXT TYPE)                                    *         
* EXIT: FVIFLD=TEXT                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETFFT   L     R2,0(R1)                                                         
         MVC   0(1,RC),7(R1)                                                    
         MVC   1(1,RC),4(R1)                                                    
*                                                                               
         LA    RF,1                                                             
         CLI   1(RC),0                                                          
         BE    *+8                                                              
         LA    RF,2                                                             
         GOTO1 VHELLO,BCPARM,(C'G',GCFILNAM),('FFTELQ',(R2)),          *        
               ((RF),(RC))                                                      
         CLI   12(R1),0                                                         
         BNE   ROUTL                                                            
*                                                                               
         L     R2,12(R1)           OUTPUT TEXT                                  
         USING FFTELD,R2                                                        
         XR    RF,RF                                                            
         IC    RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),FFTDATA                                                
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET SPAEL                                                           *         
*                                                                     *         
* NTRY: P1=A(RECORD)                                                  *         
*       P2=(SPA TYPE)                                                 *         
* EXIT: FVIFLD=TEXT                                                   *         
***********************************************************************         
         SPACE 1                                                                
GETSPA   L     R2,0(R1)                                                         
         MVC   0(1,RC),7(R1)                                                    
*                                                                               
         GOTO1 VHELLO,BCPARM,(C'G',GCFILNAM),('FFTELQ',(R2)),(1,(RC))           
         CLI   12(R1),0                                                         
         BNE   ROUTL                                                            
*                                                                               
         L     R2,12(R1)           OUTPUT TEXT                                  
         USING SPAELD,R2                                                        
         XR    RF,RF                                                            
         IC    RF,SPALN                                                         
         SH    RF,=Y(SPAAULA-SPAELD+1)                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),SPAAULA                                                
         B     ROUTE                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD FREE FORM TEXT ELEMENT                             *         
*                                                                     *         
* NTRY: P1=A(RECORD)                                                  *         
*       P2=(SEQUENCE NO,TEXT TYPE)                                    *         
*       FVIFLD=TEXT                                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDFFT   L     R2,0(R1)                                                         
*                                                                               
         LA    R3,RTDATA                                                        
         USING FFTELD,R3                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVC   FFTTYPE,7(R1)                                                    
         MVC   FFTSEQ,4(R1)                                                     
*                                                                               
         LA    RF,1                DELETE CURRENT ELEMENT                       
         CLI   FFTSEQ,0                                                         
         BE    *+8                                                              
         LA    RF,2                                                             
         GOTO1 VHELLO,BCPARM,(C'D',GCFILNAM),('FFTELQ',(R2)),          *        
               ((RF),FFTTYPE)                                                   
*                                                                               
         CLI   FVILEN,0                                                         
         BE    ROUTE                                                            
         MVC   FFTDLEN,FVILEN                                                   
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FFTDATA(0),FVIFLD                                                
         LA    RE,FFTLN1Q+2(RE)                                                 
         STC   RE,FFTLN                                                         
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),(R2),FFTELD                        
*                                                                               
BLDFFTX  B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD SPAEL - DOES NOT VALIDATE THE ACCOUNT              *         
*                                                                     *         
* NTRY: P1=A(RECORD)                                                  *         
*       P2=(SPA TYPE)                                                 *         
*       P3=A(DATA)                                                    *         
***********************************************************************         
         SPACE 1                                                                
BLDSPA   L     R2,0(R1)                                                         
         L     R4,8(R1)            DATA                                         
*                                                                               
         LA    R3,RTDATA                                                        
         USING SPAELD,R3                                                        
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVC   SPATYPE,7(R1)                                                    
*                                                                               
         GOTO1 VHELLO,BCPARM,(C'D',GCFILNAM),('SPAELQ',(R2)),          *        
               (1,SPATYPE)                                                      
         CLI   FVILEN,0                                                         
         BE    BLDSPAX                                                          
*                                                                               
         MVC   SPAAULA,0(R4)                                                    
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),(R2),SPAELD                        
*                                                                               
BLDSPAX  B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A DICTIONARY EQUATE                             *         
*                                                                     *         
* P1 = SYSTEM                                                         *         
* RETURNS DICTIONARY NUMBER IN BCHALF                                 *         
* RETURNS DICTIONARY REFERENCE IN BCFULL                              *         
***********************************************************************         
         SPACE 1                                                                
VALDD    L     R2,0(R1)                                                         
         GOTOX ('SWCHFC',AGROUTS),=AL1(GCSYSGEN)                                
         XC    BCFULL,BCFULL                                                    
         GOTO1 VSCANNER,BOPARM,FVADDR,(3,MYSCAN)                                
         MVC   NOFLD,4(R1)                                                      
*                                                                               
         CLI   NOFLD,2                                                          
         BNL   *+14                NOT ENOUGH INPUT FIELDS                      
         MVC   FVMSGNO,=AL2(AE$TFPRM)                                           
         B     VDICL                                                            
         CLI   NOFLD,3                                                          
         BNH   *+14                TOO MANY INPUT FIELDS                        
         MVC   FVMSGNO,=AL2(AE$TMPRM)                                           
         B     VDICL                                                            
*                                                                               
         LA    R3,MYSCAN                                                        
         USING SCANBLKD,R3                                                      
         CLC   =C'GE#',FVIFLD      OVERRIDING WITH GEN SYSTEM EQUATE?           
         BE    VDIC04              (MUST BE SET EXPLICITLY)                     
*                                                                               
         LR    RF,R2               R2 = SYSTEM                                  
         MH    RF,=Y(3)                                                         
         LA    RF,PFXTABLE(RF)     RF=PFXTABLE+(SYSTEM * 3)                     
         MVC   BCFULL(2),0(RF)                                                  
         MVI   BCFULL+2,C'#'       BCFULL(3)=DICTIONARY EQUATE PREFIX           
*                                                                               
         CLC   FVIFLD(3),BCFULL    TEST USER ENTERED PREFIX                     
         BE    VDIC04                                                           
*                                                                               
         CLI   SC1STLEN,5          NO - SEE IF CAN INSERT IT                    
         BNH   VDIC02                                                           
         MVC   FVMSGNO,=AL2(CE#EQPRE)                                           
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVXTRA(3),BCFULL                                                 
         B     VDICL                                                            
*                                                                               
VDIC02   MVC   SC2NDFLD,SC1STFLD   SAVE THE INPUT                               
         MVC   SC1STFLD(3),BCFULL  PREFIX INPUT WITH THE PREFIX                 
         MVC   SC1STFLD+3(5),SC2NDFLD                                           
         XR    RE,RE                                                            
         IC    RE,SC1STLEN         BUMP LENGTH                                  
         LA    RE,3(RE)                                                         
         STC   RE,SC1STLEN                                                      
*                                                                               
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
VDIC04   XC    GQKEY,GQKEY         READ EQUATE NAME PASSIVE                     
         MVI   GQKREC,GQKRECQ                                                   
         MVC   GQKQNAME,SC1STFLD                                                
         L     R1,=AL4(XOHI+XOGENDIR+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   *+14                                                             
         CLC   GQKEY(GQKMNUM-GQKEY),IOKEYSAV                                    
         BE    VDIC06                                                           
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVMSGNO,=AL2(CE#EQNOF)                                           
         B     VDICL                                                            
         SPACE 1                                                                
VDIC06   MVC   BCHALF,GQKMNUM      RETURN REFERENCE # IN BCHALF                 
         MVC   BCFULL+1(L'GQKMNUM),GQKMNUM AND SAVE IT FOR DD REFERENCE         
         POP   USING                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SC1STLEN                                                      
         LA    RF,1(RF)            FOR THE COMMA                                
         STC   RF,CURLEN           SET CURRENT DISPLACEMENT INTO FIELD          
         LA    R3,SCBLKLQ(R3)                                                   
*                                                                               
         TM    SC1STVAL,SCNUMQ     IS THIS FIELD NUMERIC?                       
         BO    *+20                                                             
         MVC   FVERRNDX,CURLEN                                                  
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VDICL                                                            
*                                                                               
         L     RF,SC1STNUM         LENGTH CONSTRAINED 2<LEN<80                  
         CH    RF,=H'80'                                                        
         BNH   *+20                                                             
         MVC   FVERRNDX,CURLEN     TOO LARGE                                    
         MVC   FVMSGNO,=AL2(AE$FLVTL)                                           
         B     VDICL                                                            
*                                                                               
         CH    RF,=H'2'                                                         
         BNL   *+20                                                             
         MVC   FVERRNDX,CURLEN     TOO SMALL                                    
         MVC   FVMSGNO,=AL2(AE$FLVTS)                                           
         B     VDICL                                                            
*                                                                               
         STC   RF,ESCLEN           SAVE THE LENGTH                              
         STC   RF,BCFULL+3         PUT IN THE DD REFERENCE LENGTH               
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SC1STLEN         LENGTH OF THIS INPUT                         
         XR    RE,RE                                                            
         IC    RE,CURLEN           LENGTH OF VALIDATED INPUT                    
         LA    RF,1(RE,RF)         +1 FOR THE COMMA                             
         STC   RF,CURLEN           SET CURRENT DISPLACEMENT INTO FIELD          
*                                                                               
         CLI   NOFLD,3             SETTING ALIGNMENT?                           
         BE    VDIC07              YES                                          
         CLI   ESCLEN,2            DEFAULT ALIGNMENT IS LEFT                    
         BNE   *+8                                                              
         MVI   BCFULL,DD#ESCL2     DD LENGTH=2                                  
         CLI   ESCLEN,3                                                         
         BNE   *+8                                                              
         MVI   BCFULL,DD#ESCL3     DD LENGTH=3                                  
         BNH   *+8                                                              
         MVI   BCFULL,DD#ESCL      DD LENGTH>3                                  
         B     VDICE                                                            
*                                                                               
VDIC07   LA    R3,SCBLKLQ(R3)                                                   
         CLI   SC1STLEN,2          ONLY 2 CHARACTERS MAX ALLOWED                
         BNH   *+20                                                             
         MVC   FVERRNDX,CURLEN     NOT VALID IF >2 CHARS                        
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VDICL                                                            
*                                                                               
         CLI   ESCLEN,3            ORDINARY DD EQUATE                           
         BH    VDIC14              YES                                          
         BL    VDIC10              LENGTH 2?                                    
*                                                                               
         CLC   =C'LU',SC1STFLD     IS IT UNDERLINED?                            
         BNE   *+12                NO                                           
         MVI   BCFULL,DD#ESUL3                                                  
         B     VDICE                                                            
*                                                                               
         CLC   =C'L ',SC1STFLD     IS IT LEFT ALIGNED?                          
         BE    VDIC08                                                           
         CLC   =C'  ',SC1STFLD     DEFAULT IS LEFT ALIGNED                      
         BE    VDIC08                                                           
         MVC   FVERRNDX,CURLEN     NOT VALID                                    
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VDICL                                                            
*                                                                               
VDIC08   MVI   BCFULL,DD#ESCL3                                                  
         B     VDICE                                                            
*                                                                               
VDIC10   CLI   ESCLEN,2            LENGTH 2?                                    
         BE    *+6                                                              
         DC    H'0'                SHOULD HAVE TRAPPED THIS ABOVE               
*                                                                               
         CLC   =C'LU',SC1STFLD     IS IT UNDERLINED?                            
         BNE   *+12                NO                                           
         MVI   BCFULL,DD#ESUL2                                                  
         B     VDICE                                                            
*                                                                               
         CLC   =C'L ',SC1STFLD     IS IT LEFT ALIGNED?                          
         BE    VDIC12                                                           
         CLC   =C'  ',SC1STFLD     DEFAULT IS LEFT ALIGNED                      
         BE    VDIC12                                                           
         MVC   FVERRNDX,CURLEN     NOT VALID                                    
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VDICL                                                            
*                                                                               
VDIC12   MVI   BCFULL,DD#ESCL2                                                  
         B     VDICE                                                            
*                                                                               
VDIC14   LA    RF,DDTAB                                                         
         USING DDTABD,RF                                                        
         XR    RE,RE                                                            
         IC    RE,SC1STLEN                                                      
         BCTR  RE,0                                                             
*                                                                               
VDIC16   CLI   DEQU,EOT            NO MATCH ON CHARACTERS                       
         BNE   *+20                                                             
         MVC   FVERRNDX,CURLEN     NOT VALID                                    
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VDICL                                                            
*                                                                               
         EX    RE,*+8              TRY TO MATCH ON INPUT                        
         BE    VDIC18                                                           
         CLC   DCHAR(0),SC1STFLD                                                
         LA    RF,DDTABL(RF)                                                    
         B     VDIC16                                                           
*                                                                               
VDIC18   MVC   BCFULL(L'DEQU),DEQU SET EQUATE                                   
                                                                                
         B     VDICE                                                            
*                                                                               
VDICE    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     ROUTE                                                            
*                                                                               
VDICL    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     ROUTL                                                            
*                                                                               
DDTABD   DSECT                                                                  
DEQU     DS    XL1                                                              
DCHAR    DS    XL2                                                              
DDTABL   EQU   *-DDTABD                                                         
*                                                                               
RTWORKD  DSECT                                                                  
         ORG   RTDATA                                                           
NOFLD    DS    XL1                 NUMBER OF SCANNED FIELDS                     
ESCLEN   DS    XL1                                                              
CURLEN   DS    XL1                                                              
MYSCAN   DS    3CL(SCBLKLQ)                                                     
*                                                                               
GEN6C    CSECT                                                                  
DDTAB    DC    AL1(DD#ESCL),CL2'L '                                             
         DC    AL1(DD#ESCL),CL2'  '      DEFAULT IS LEFT ALIGNED                
         DC    AL1(DD#ESUL),CL2'LU'                                             
         DC    AL1(DD#ESCR),CL2'R '                                             
         DC    AL1(DD#ESUR),CL2'RU'                                             
         DC    AL1(DD#ESCC),CL2'C '                                             
         DC    AL1(DD#ESUC),CL2'CU'                                             
         DC    AL1(DD#ESCF),CL2'F '                                             
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO DISPLAY A DICTIONARY REFERENCE                           *         
*                                                                     *         
* P1 = SYSTEM                                                         *         
* BCFULL HOLDS DICTIONARY REFERENCE ON ENTRY                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
DISDD    L     R2,0(R1)            SYSTEM                                       
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVI   GMKTYP,GMKTGDIC                                                  
         STC   R2,GMKSYS           SET UP THE SYSTEM                            
         XR    RF,RF                                                            
         ICM   RF,3,BCFULL+1       GET DICTIONARY NUMBER                        
         TM    BCFULL+1,X'40'      IS IT GENERAL?                               
         BZ    *+12                                                             
         SH    RF,=Y(GE#GEN)                                                    
         MVI   GMKSYS,GENSYS       MOVE IN GENERAL SYSTEM NUMBER                
*                                                                               
         STCM  RF,3,GMKMSG         SAVE DICTIONARY NUMBER                       
         L     R1,=AL4(XOHI+XOGENDIR+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GMKEY(GMKLANG-GMKEY),IOKEYSAV                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         LA    R3,GMFIRST(R1)                                                   
         USING GMQSYD,R3                                                        
         XR    RF,RF                                                            
         CLI   GMQSYEL,GMQSYELC                                                 
         BE    *+14                                                             
         IC    RF,GMQSYELL                                                      
         BXH   R3,RF,*-12                                                       
         DC    H'0'                                                             
*                                                                               
         MVC   FVIFLD(L'GMQSYSYM),GMQSYSYM                                      
         LA    R3,FVIFLD+L'GMQSYSYM                                             
         LA    R0,L'GMQSYSYM                                                    
         CLI   0(R3),C' '          FIND THE FIRST NON-SPACE                     
         BH    *+10                                                             
         BCTR  R3,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         LA    R3,1(R3)                                                         
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BCFULL+3         GET THE LENGTH OF THE DDICT REF.             
         CURED (RF),(3,(R3)),0,DMCB=BOPARM,ALIGN=LEFT                           
         AR    R3,R0               LENGTH RETURNED IN R0                        
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         XR    RF,RF                                                            
         IC    RF,BCFULL+3         GET THE LENGTH OF THE DDICT REF.             
         CLM   RF,1,=AL1(3)        ORDINARY DD EQUATE                           
         BH    DDIC04              YES                                          
         BL    DDIC02              LENGTH 2?                                    
*                                                                               
         CLI   BCFULL,DD#ESUL3     LENGTH 3 & UNDERLINED?                       
         BNE   *+14                NO                                           
         MVC   0(2,R3),=C'LU'                                                   
         B     DDICE                                                            
         CLI   BCFULL,DD#ESCL3     LENGTH 3 & LEFT ALIGNED                      
         BNE   *+14                                                             
         MVC   0(2,R3),=C'L '                                                   
         B     DDICE                                                            
         DC    H'0'                                                             
*                                                                               
DDIC02   CLI   BCFULL,DD#ESUL2     LENGTH 2 & UNDERLINED?                       
         BNE   *+14                NO                                           
         MVC   0(2,R3),=C'LU'                                                   
         B     DDICE                                                            
         CLI   BCFULL,DD#ESCL2     LENGTH 2 & LEFT ALIGNED                      
         BNE   *+14                                                             
         MVC   0(2,R3),=C'L '                                                   
         B     DDICE                                                            
         DC    H'0'                                                             
*                                                                               
DDIC04   LA    RF,DDTAB                                                         
         USING DDTABD,RF                                                        
DDIC06   CLI   DEQU,EOT            NO MATCH ON CHARACTERS                       
         BE    DDICL                                                            
         CLC   DEQU,BCFULL                                                      
         BE    *+12                                                             
         LA    RF,DDTABL(RF)                                                    
         B     DDIC06                                                           
*                                                                               
         MVC   0(L'DCHAR,R3),DCHAR                                              
         B     DDICE                                                            
         DROP  RF                                                               
         POP   USING                                                            
*                                                                               
DDICE    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     ROUTE                                                            
*                                                                               
DDICL    GOTOX ('SWCHFC',AGROUTS),=AL1(0)                                       
         B     ROUTL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD OR AMEND PID PASSIVE FROM RECORD PASSED            *         
*         FROM THE BINARY PID NUMBER                                  *         
*                                                                     *         
* NTRY: P1=(0,A(ACCMST RECORD))                                       *         
*          (X'80',A(ACCMST RECORD))  - DELETE PID PASSIVE             *         
*       P2=(PASSIVE SUB TYPE,A(BINARY PID CODE))                      *         
* EXIT:                                                               *         
***********************************************************************         
         SPACE 1                                                                
PIDPAS   MVC   RTIOWS1,IOWS        SAVE I/O WORKING STORAGE                     
         XC    RTVALS(RTVALLQ),RTVALS                                           
*                                                                               
         ICM   R3,B'0111',1(R1)    R3=A(ACCMST RECORD)                          
         USING APPRECD,R3                                                       
         CLI   APPKTYP,APPKTYPQ    IS IT AN APPROVER OR LIMLIST RECORD          
         BNE   PIDP04              NO - MUST BE ACCOUNT RECORD                  
         B     ROUTH               EXIT HIGH IF NOT APPROVER OR LIMLIST         
         DROP  R3                                                               
*                                                                               
         USING ACTRECD,R3                                                       
PIDP04   CLI   ACTKCPY,X'40'       IS IT AN ACCOUNT RECORD                      
         BH    *+6                                                              
         DC    H'0'                ERROR - NOT ACCOUNT RECORD                   
         MVC   RTTYP,4(R1)         PASSIVE SUB TYPE                             
         CLI   RTTYP,0             ANY PASSIVE SUB TYPE PASSED?                 
         BNE   *+6                                                              
         DC    H'0'                ERROR - NOT PASSIVE SUB TYPE                 
         ICM   RF,B'0111',5(R1)    TEST/SAVE A(BINARY PID CODE PASSED)          
         BNZ   *+6                                                              
         DC    H'0'                ERROR - NO PID CODE                          
         MVC   RTPID,0(RF)         SAVE BINARY PID CODE                         
         MVC   RTCPY,ACTKCPY       SAVE COMPANY CODE                            
         MVC   RTULA,ACTKULA       SAVE U/L ACCOUNT                             
         MVC   RTSTA,0(R1)         SAVE STATUS                                  
         DROP  R3                                                               
                                                                                
PIDP08   LA    R2,IOKEY                                                         
         MVC   IOKEY,0(R3)                                                      
         L     R1,=AL4(XORDD+XOACCDIR) READ DIRECTORY RECORD FOR DA             
         GOTOX ('XIO',AGROUTS)                                                  
         BE    PIDP10                                                           
         TM    IOERR,IOEDEL        IS IT MAKRED AS DELETED                      
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING APPRECD,R2                                                       
PIDP10   MVC   RTDA,APPKDA         SAVE DISK ADDRESS                            
*                                                                               
         CLI   APPKTYP,APPKTYPQ    IS IT AN APPROVER OR LIMLIST REC?            
         BNE   PIDP20                                                           
         MVC   RTCPY,APPKCPY       SAVE COMPANY CODE                            
         MVC   RTSTA,APPKSTA       SAVE STATUS                                  
*                                                                               
         USING PIDRECD,R2                                                       
PIDP20   LA    R2,IOKEY            READ FOR PASSIVE                             
         XC    PIDKEY,PIDKEY       CLEAR KEY                                    
         MVC   PIDKCPY,RTCPY       SET UP PASSIVE KEY                           
         MVI   PIDKTYP,PIDKTYPQ                                                 
         MVI   PIDKSUB,PIDKSUBQ                                                 
         MVC   PIDKPID,RTPID       SET PASSIVE PID                              
         MVC   PIDKSTYP,RTTYP      SET PASSIVE SUB TYPE                         
         MVC   PIDKULA,RTULA       U/L ACCOUNT OR BINARY ZERO                   
*                                                                               
         L     R1,=AL4(XOHIUPD+XOACCDIR) READ HIGH FOR PID PASSIVE              
         GOTOX ('XIO',AGROUTS)                                                  
*                                                                               
         LHI   RF,PIDKPER-PIDKEY-1                                              
         OC    RTULA,RTULA         IS IT AN ACCOUNT RECORD?                     
         BZ    *+8                 NO - OK                                      
         LHI   RF,PIDKSEQ-PIDKEY-1 CHECK U/L ACCOUNT?                           
         EXCLC RF,PIDKEY,IOKEYSAV  FOUND A MATCHING PID                         
         BNE   PIDP50              NO - ADD NEW RECORD                          
*                                                                               
PIDP40   MVC   PIDKSTA,RTSTA       REFRESH STATUS AND ADDRESS                   
         MVC   PIDKDA,RTDA                                                      
         L     R1,=AL4(XOWRITE+XOACCDIR)                                        
         GOTOX ('XIO',AGROUTS)                                                  
         BE    PIDPASX                                                          
         DC    H'0'                                                             
*                                                                               
PIDP50   MVC   IOKEY,IOKEYSAV      RESTORE KEY                                  
         MVC   PIDKSTA,RTSTA                                                    
         MVC   PIDKDA,RTDA                                                      
         L     R1,=AL4(XOADD+XOACCDIR)                                          
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PIDPASX  MVC   IOWS(L'RTIOWS1),RTIOWS1                                          
         B     ROUTE                                                            
         SPACE 1                                                                
         DROP  R2                                                               
         SPACE 1                                                                
RTWORKD  DSECT                                                                  
         ORG   RTDATA                                                           
RTIOWS1  DS    XL(IOWSX-IOWS)                                                   
RTVALS   DS    0H                                                               
RTCPY    DS    XL(L'PIDKCPY)                                                    
RTTYP    DS    XL(L'PIDKSTYP)                                                   
RTPID    DS    XL(L'PIDKPID)                                                    
RTULA    DS    XL(L'PIDKULA)                                                    
RTSTA    DS    XL(L'PIDKSTA)                                                    
RTDA     DS    XL(L'PIDKDA)                                                     
RTVALLQ  EQU   *-RTVALS                                                         
GEN6C    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PIDEL - DOES NOT VALIDATE THE PID                  *         
*                                                                     *         
* NTRY: P1=A(ACCMST RECORD))                                          *         
*       P2=(PID NUMBER)  - REMOVE PIDEL IF BINARY ZERO PID PASSED     *         
***********************************************************************         
         SPACE 1                                                                
BLDPID   L     R2,0(R1)                                                         
         L     R4,4(R1)            DATA                                         
*                                                                               
         GOTO1 VHELLO,BCPARM,(C'D',GCFILNAM),('PIDELQ',(R2)),0                  
*                                                                               
         OC    0(L'PIDNO,R4),0(R4)                                              
         BZ    BLDPIDX             EXIT IF NO PID PASSED                        
*                                                                               
         LA    R3,RTDATA                                                        
         USING PIDELD,R3                                                        
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,0(R4)                                                      
*                                                                               
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),(R2),PIDELD                        
*                                                                               
BLDPIDX  B     ROUTE                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         LTORG                                                                  
FF       EQU   X'FF'                                                            
GENSYS   EQU   X'0F'                                                            
*                                                                               
       ++INCLUDE DDPFXTBLE                                                      
         EJECT                                                                  
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* CTMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DDDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACGEN6C   01/16/13'                                      
         END                                                                    
