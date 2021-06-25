*          DATA SET ACCLB62    AT LEVEL 042 AS OF 08/16/00                      
*PHASE T62162A                                                                  
*&&      SET   NOP=N                                                            
***********************************************************************         
* BUILD BILL PAGE PANELS                                              *         
*                                                                     *         
* NTRY: P1 = C'ADD' TO ADD PANELS FOR NEW BILL                        *         
*          = C'UPDATE' TO UPDATE PANELS FOR EXISTING BILL             *         
***********************************************************************         
CLB62    TITLE '- PC COMMS - BUILD BILL PAGE PANELS'                            
CLB62    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,**CB62**,R8,CLEAR=YES,RR=RE                                    
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
         L     RC,AOVERWRK                                                      
         USING BPWORKD,RC                                                       
         L     R7,ALINK                                                         
         USING LINKD,R7                                                         
         USING BLHELD,BEWBLH                                                    
         L     R6,AGOPBLK                                                       
         USING GOBLOCKD,R6                                                      
         L     RF,0(R1)                                                         
         MVC   BPACT,0(RF)                                                      
*                                                                               
         MVC   IODAOVER,BEWHDRDA   READ BILL HEADER RECORD INTO IO4             
         LHI   R1,IOGET+IOACCMST+IO4                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RF,ADDPAN                                                        
         CLI   BPACT,BPACTADD                                                   
         BE    BPAN02                                                           
         LA    RF,UPDPAN                                                        
         CLI   BPACT,BPACTUPD                                                   
         BE    BPAN02                                                           
         DC    H'0'                                                             
BPAN02   BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITN    LTR   RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITY    CR    RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ADD NEW BILL PANELS                                                 *         
***********************************************************************         
         SPACE 1                                                                
ADDPAN   NTR1  ,                                                                
         PUSH  USING                                                            
         USING BFMRECD,IOKEY       READ THROUGH RECORDS FOR FORMAT              
         MVC   BFMKEY,BEWFMTKY                                                  
         LA    R1,IOHIGH+IOACCDIR+IO2                                           
         B     *+8                                                              
APAN02   LA    R1,IOSEQ+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BNE   ADDPANX                                                          
         CLC   BFMKEY(BFMKLVL-BFMKEY),IOKEYSAV                                  
         BNE   ADDPANX                                                          
         CLI   BFMKSEQ,0           TEST RECORD IS CONTINUATION                  
         BE    *+6                                                              
         DC    H'0'                TOO COMPLICATED TO IMPLEMENT                 
         CLC   BFMKWHER,=AL2(BFMKWPHQ)  PAGE HEADER                             
         BE    APAN04                                                           
         CLC   BFMKWHER,=AL2(BFMKWPFQ)  PAGE FOOTER                             
         BNE   APAN02                                                           
*                                                                               
APAN04   DS    0H                                                               
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BPSAVKEY,IOKEY                                                   
*                                                                               
         L     R2,AIO1                                                          
         USING BEDRECD,R2                                                       
         GOTO1 AUPDPAN,BOPARM,=C'ADD',AIO2,BEDRECD,AIO4                         
         MVC   IOKEY,BPSAVKEY                                                   
         MVC   BEDKEY,BEWHDRKY                                                  
         MVC   BEDKLVL,BFMKLVL                                                  
         MVC   BEDKWHER,BFMKWHER                                                
         XC    BEDRSTA,BEDRSTA                                                  
         TM    0(R1),X'80'         TEST RECORD DID CONTAIN PANELS               
         BZ    *+8                                                              
         OI    BEDRSTAT,BEDSPAN                                                 
*                                                                               
         GOTO1 AFMTTXT,BOPARM,(C'S',BEDRECD)                                    
*                                                                               
         GOTO1 AIO,IOADD+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   IOKEY,BPSAVKEY      RE-ESTABLISH READ SEQUENCE                   
         GOTO1 AIO,IOREAD+IOACCDIR+IO2                                          
         B     APAN02                                                           
*                                                                               
ADDPANX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* UPDATE EXISTING BILL PANELS                                         *         
***********************************************************************         
         SPACE 1                                                                
UPDPAN   NTR1  ,                                                                
*                                                                               
         PUSH  USING                                                            
         USING BEDRECD,IOKEY       READ THROUGH RECORDS FOR BILL                
         MVC   BEDKEY,BEWHDRKY                                                  
         LA    R1,IOHIGH+IOACCDIR+IO2                                           
         B     *+8                                                              
UPAN02   LA    R1,IOSEQ+IOACCDIR+IO2                                            
         GOTO1 AIO                                                              
         BNE   UPDPANX                                                          
         CLC   BEDKEY(BEDKLVL-BEDKEY),IOKEYSAV                                  
         BNE   UPDPANX                                                          
         CLC   BEDKWHER,=AL2(BEDKWPHQ)  PAGE HEADER                             
         BE    UPAN04                                                           
         CLC   BEDKWHER,=AL2(BEDKWPFQ)  PAGE FOOTER                             
         BE    UPAN04                                                           
         TM    BEDKSTAT,BEDSPAN    OTHER RECORDS MAY CONTAIN PANELS             
         BZ    UPAN02                                                           
*                                                                               
UPAN04   GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BPSAVKEY,IOKEY                                                   
         GOTO1 AUPDPAN,BOPARM,=C'UPDATE',AIO2,AIO2,AIO4                         
         TM    0(R1),X'40'         TEST RECORD HAS CHANGED                      
         BZ    UPAN08                                                           
         GOTO1 AIO,IOPUT+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPAN08   MVC   IOKEY,BPSAVKEY      RE-ESTABLISH READ SEQUENCE                   
         LA    R1,IOREAD+IOACCDIR+IO2                                           
         GOTO1 AIO                                                              
         B     UPAN02                                                           
*                                                                               
UPDPANX  DS    0H                                                               
         B     EXITY                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
ACCOUNT  DC    C'ACCOUNT'                                                       
ADDCODE  DC    C'ADD=CODE'                                                      
ADDEND   DC    C'ADD=END'                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         EJECT                                                                  
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
* ACCLBLINK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBLINK                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
BPWORKD  DSECT                                                                  
*                                                                               
BPACT    DS    CL1                                                              
BPACTADD EQU   C'A'                ADD NEW BILL PANELS                          
BPACTUPD EQU   C'U'                UPDATE EXISTING BILL PANELS                  
*                                                                               
BPSAVKEY DS    XL42                SAVE KEY FOR READ SEQUENCE                   
*                                                                               
         DS    (OVERWRKL-(*-BPWORKD))X                                          
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042ACCLB62   08/16/00'                                      
         END                                                                    
