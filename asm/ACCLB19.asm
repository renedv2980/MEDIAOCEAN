*          DATA SET ACCLB19    AT LEVEL 115 AS OF 08/16/00                      
*PHASE T62119A                                                                  
CLB19    TITLE '- AUTO REVERSE/UNREVERSE'                                       
CLB19    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB19**,R8,RR=RE                                              
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK         RC=A(LOCAL WORKING STORAGE)                  
         USING OVERWRK,RC                                                       
         STCM  RF,8,OVACT                                                       
         XC    OVRCNT,OVRCNT       CLEAR REVERSED/CLEARED ITEM COUNT            
         ST    RE,BORELO                                                        
         L     R5,ALSVALS                                                       
         USING LSVALSD,R5                                                       
         USING TLSTD,LSTLST                                                     
*                                                                               
D        USING TRNRECD,IOKEY       DIRECTORY RECORD                             
S        USING TRNRECD,IOKEYSAV    SAVED DIRECTORY RECORD                       
         MVC   D.TRNKEY,BCSPACES   BUILD KEY TO READ THROUGH JOB                
         MVC   D.TRNKCPY,CUABIN                                                 
         MVC   D.TRNKUNT(L'BCCPYPRD),BCCPYPRD                                   
         MVC   D.TRNKACT,BCJOBCOD                                               
         LA    R1,IOHI+IOACCDIR+IO1                                             
READ10   GOTO1 AIO,(R1)                                                         
         BNE   EXITN                                                            
         CLC   D.TRNKCULA,S.TRNKCULA                                            
         BNE   READ60              STOP AT CHANGE OF JOB                        
         CLC   D.TRNKWORK,BILLWC                                                
         BE    READ60              STOP AT BILLS                                
         CLC   D.TRNKDATE,BCSPACES                                              
         BE    READ50              NOT A TRANSACTION - SKIP                     
*                                                                               
         USING TRNRECD,R2          R2=ACCMST RECORD)                            
O        USING PTAELD,R3           R3=A(ORIGINAL BILL PTAEL)                    
N        USING PTAELD,OVELEM       BUILD NEW PTAEL IN OVELEM                    
R        USING PTAELD,R1           FIND REVERSAL PTAEL                          
READ20   GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R3,TRNRFST                                                       
         SR    R1,R1                                                            
READ30   IC    R1,O.PTALN                                                       
         AR    R3,R1                                                            
         CLI   O.PTAEL,0                                                        
         BE    READ50                                                           
         CLI   O.PTAEL,PTAELQ                                                   
         BNE   READ30                                                           
         CLI   O.PTATYPE,PTATRAL                                                
         BNE   READ30                                                           
         TM    O.PTASTAT1,PTASPEND                                              
         BO    READ30                                                           
         CLC   O.PTARBLNO,CSBILNUM                                              
         BNE   READ30                                                           
         TM    O.PTASTAT1,PTASREVU TEST REVERSAL OF BILL IS UPDATED             
         BO    READ50                                                           
*                                                                               
         GOTO1 AIO,IOREAD+IOLOCK+IOACCDIR+IO1                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOGET+IOLOCK+IOACCMST+IO1                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   OVACT,OVACLR        CLEAR ACTIVITY PENDING ETC.                  
         BNE   READ34                                                           
         NI    TRNRSTA2,FF-TRNSBILP                                             
         NI    O.PTASTAT1,FF-PTASREVD                                           
         LA    R1,O.PTAEL                                                       
         SR    R0,R0                                                            
READ32   IC    R0,R.PTALN                                                       
         AR    R1,R0                                                            
         CLI   R.PTAEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   R.PTAEL,PTAELQ                                                   
         BNE   READ32                                                           
         TM    R.PTASTAT1,PTASPEND+PTASREVS                                     
         BNO   READ32                                                           
         TM    R.PTASTAT2,PTASAUTR                                              
         BNO   READ32                                                           
         MVI   R.PTAEL,FF                                                       
         GOTO1 VHELLO,BCPARM,(C'D',ACCMST),('FF',TRNRECD),0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     READ40                                                           
*                                                                               
READ34   OI    TRNRSTA2,TRNSBILP   SET ACTIVITY PENDING                         
         IC    R1,O.PTALN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   N.PTAEL(0),O.PTAEL  COPY ORIGINAL ELEMENT                        
         TM    O.PTASTAT1,PTASREVD TEST ALREADY REVERSED                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    O.PTASTAT1,PTASREVD SET ORIGINAL BILL REVERSED                   
         MVC   N.PTADATE,BCTODAYC                                               
         MVC   N.PTAPERS,CUPASS                                                 
         NI    N.PTASTAT1,PTASHOUR+PTASCASH                                     
         OI    N.PTASTAT1,PTASPEND+PTASREVS                                     
         OI    N.PTASTAT2,PTASAUTR                                              
         ZAP   BODUB1,N.PTANET                                                  
         MP    BODUB1,=P'-1'                                                    
         ZAP   N.PTANET,BODUB1                                                  
         XC    N.PTASEQN,N.PTASEQN                                              
         XC    N.PTAMOA,N.PTAMOA                                                
         ZAP   BODUB1,N.PTACDSC                                                 
         MP    BODUB1,=P'-1'                                                    
         ZAP   N.PTACDSC,BODUB1                                                 
         SR    R1,R1                                                            
         ICM   R1,3,N.PTAHOURS                                                  
         LCR   R1,R1                                                            
         STCM  R1,3,N.PTAHOURS                                                  
         ZAP   BODUB1,N.PTARCOM                                                 
         MP    BODUB1,=P'-1'                                                    
         ZAP   N.PTARCOM,BODUB1                                                 
         XC    N.PTARDATE(N.PTARCVAL-N.PTARDATE),N.PTARDATE                     
         CLC   CSCPYCUR,CSBILCUR   TEST FOREIGN CURRENCY BILL                   
         BE    READ38                                                           
         ZAP   BODUB1,N.PTANETF    SET FOREIGN NET AND COMMISSION               
         MP    BODUB1,=P'-1'                                                    
         ZAP   N.PTANETF,BODUB1                                                 
         CLI   N.PTALN,PTARLN2Q    TEST FOREIGN COMMISSION CARRIED              
         BL    READ38                                                           
         ZAP   BODUB1,N.PTARFCOM                                                
         MP    BODUB1,=P'-1'                                                    
         ZAP   N.PTARFCOM,BODUB1                                                
*                                                                               
READ38   GOTO1 VHELLO,BCPARM,(C'P',ACCMST),TRNRECD,N.PTAELD                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
READ40   GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   D.TRNKSTA,TRNRSTA   REFRESH DIRECTORY STATUS AREA                
         GOTO1 AIO,IOWRITE+IOACCDIR+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    R1,OVRCNT           INCREMENT COUNT OF REVERSALS                 
         LA    R1,1(R1)                                                         
         STH   R1,OVRCNT                                                        
*                                                                               
READ50   LA    R1,IOSQ+IOACCDIR+IO1                                             
         B     READ10                                                           
*                                                                               
READ60   OC    OVRCNT,OVRCNT       ANY CHANGE TO COUNT                          
         BZ    READ80                                                           
         MVC   IODAOVER,TLDA       GET BILL POSTING                             
         GOTO1 AIO,IOGET+IOLOCK+IOACCMST+IO3                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         MVC   D.TRNKEY,TRNKEY     READ DIRECTORY RECORD                        
         GOTO1 AIO,IOREAD+IOLOCK+IOACCDIR+IO3                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,TRNRFST                                                       
         SR    R0,R0                                                            
         USING RATELD,RF                                                        
READ62   IC    R0,RATLN                                                         
         AR    RF,R0                                                            
         CLI   RATEL,0                                                          
         BE    READ68                                                           
         CLI   RATEL,RATETAXQ                                                   
         BNE   READ62                                                           
         LH    RE,RATRATE                                                       
         CLI   OVACT,OVACLR        TEST REVERSE/CLEAR                           
         BE    READ64                                                           
         OI    TRNRSTA2,TRNSBILP                                                
         AH    RE,OVRCNT           ADD REVERSALS                                
         STH   RE,RATRATE                                                       
         B     READ70                                                           
*                                                                               
READ64   NI    TRNRSTA2,FF-TRNSBILP                                             
         SH    RE,OVRCNT           SUBTRACT REVERSALS                           
         BZ    *+12                                                             
         STH   RE,RATRATE                                                       
         B     READ70                                                           
         MVI   RATEL,FF            NO REVERSALS LEFT - DELETE ELEMENT           
         GOTO1 VHELLO,BCPARM,(C'D',ACCMST),('FF',AIO3),0                        
         CLI   12(R1),0                                                         
         BE    READ70                                                           
         DC    H'0'                                                             
*                                                                               
READ68   CLI   OVACT,OVACLR        TEST CLEAR                                   
         BNE   *+6                                                              
         DC    H'0'                CLEAR MUST FIND ELEMENT                      
         OI    TRNRSTA2,TRNSBILP                                                
         LA    RF,BOELEM                                                        
         XC    RATELD(RATLNQ),RATELD                                            
         MVI   RATEL,TRNELQ+1      ENSURE NEW ELEMENT GOES AFTER TRNEL          
         MVI   RATLN,RATLNQ                                                     
         MVC   RATRATE,OVRCNT      COUNT OF REVERSALS                           
         GOTO1 VHELLO,BODMCB,(C'P',ACCMST),AIO3,BOELEM,0,0                      
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,16(R1)           PICK-UP A(ELEMENT) IN RECORD                 
         MVI   RATEL,RATETAXQ      AND SET CORRECT ELEMENT CODE                 
*                                                                               
READ70   MVC   IODAOVER,TLDA       WRITE BACK UPDATED BILL POSTING              
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   D.TRNKSTA,TRNRSTA                                                
         GOTO1 AIO,IOWRITE+IOACCDIR+IO3                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,BCJOBDA    GET JOB RECORD                               
         GOTO1 AIO,IOGET+IOLOCK+IOACCMST+IO3                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         USING FFTELD,RF                                                        
         SR    R0,R0                                                            
         B     *+10                                                             
READ72   IC    R0,FFTLN                                                         
         AR    RF,R0                                                            
         CLI   FFTEL,0             TEST EOR                                     
         BE    READ74                                                           
         CLI   FFTTYPE,FFTTAUTR    TEST AUTOREV BILL NUMBER                     
         BNE   READ72                                                           
         CLI   OVACT,OVACLR        TEST REVERSE/CLEAR                           
         BE    *+6                                                              
         DC    H'0'                MUSTN'T FIND ELEMENT IF REVERSE              
         MVI   FFTEL,FF            CLEARING AUTOREV - DELETE ELEMENT            
         GOTO1 VHELLO,BCPARM,(C'D',ACCMST),('FF',AIO3),0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,BCJOBDA    PUT BACK UPDATED JOB RECORD                  
         GOTO1 AIO,IOPUT+IOACCMST+IO3                                           
         BE    READ80                                                           
         DC    H'0'                                                             
*                                                                               
READ74   CLI   OVACT,OVAARV        NO ELEMENT - MUST BE AUTOREV                 
         BE    READ80              ELEMENT TO BE ADDED BY FORMAT O/LAY          
         DC    H'0'                                                             
*                                                                               
READ80   B     EXITY                                                            
         DROP  R2,RF,D,N,O,R,S                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT POINTS                                                 *         
***********************************************************************         
         SPACE 1                                                                
EXITY    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,FF                                                             
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                                                              
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
ACCMST   DC    CL8'ACCMST'                                                      
BILLWC   DC    CL2'99'                                                          
FF       EQU   X'FF'                                                            
         EJECT                                                                  
* ACCLBCOLSC                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCLBCOLSC                                                     
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORKC                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKC                                                     
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   OSVALS                                                           
         ORG   OSVALS+OSVALSL                                                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
         ORG   OVERWRK                                                          
OVRCNT   DS    H                   COUNT OF REVERSED ITEMS                      
OVACT    DS    XL1                                                              
OVAARV   EQU   2                   SEE SELPRB ENTRY IN SELTAB                   
OVACLR   EQU   3                   SEE SELPRB ENTRY IN SELTAB                   
OVELEM   DS    XL255                                                            
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'115ACCLB19   08/16/00'                                      
         END                                                                    
