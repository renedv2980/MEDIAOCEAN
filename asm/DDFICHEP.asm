*          DATA SET DDFICHEP   AT LEVEL 001 AS OF 10/08/81                      
*PHASE FICHEP,*                                                                 
         TITLE 'FICHEP - PRINT MICRO FICHE TAPE'                                
* UPSI 1       PRINT DATA RECORDS                                               
* UPSI 01      PRINT CTRL RECORDS                                               
* UPSI 001     FIRST PAGE ONLY OF EACH REPORT                                   
*                                                                               
         PRINT NOGEN                                                            
FICHEP   CSECT                                                                  
         NBASE 0,*FICHEP*,=A(FCHWORK)                                           
         COMRG                                                                  
         MVC   UPSI,23(R1)         SAVE UPSI BYTE                               
         CLI   UPSI,0                                                           
         BNE   *+8                                                              
         MVI   UPSI,B'11100000'    DEFAULT=DATA/CTRL/FIRSTPAGE                  
*                                                                               
FCHOPN   LA    R1,MFTAP            OPEN FICHE TAPE                              
         OPENR (1)                                                              
         LA    R2,CCWPRNT          BUILD PRINT CCB                              
         ST    R2,CCBPRNT+8                                                     
         LA    R2,MFDATA           BUILD PRINT CCW                              
         ST    R2,DUB                                                           
         MVC   CCWPRNT+1(3),DUB+1                                               
         B     FCH1                                                             
*                                                                               
FCHCLS   LA    R1,MFTAP            CLOSE FICHE TAPE                             
         CLOSER (1)                                                             
*                                                                               
FCHXIT   EOJ                                                                    
         EJECT                                                                  
FCH1     LA    R1,MFTAP            READ NEXT FICHE TAPE RECORD                  
         LA    R0,MFREC                                                         
         GET   (1),(0)                                                          
         CLC   MFREC(3),PANAM      TEST RECORD TYPE                             
         BNE   FCH5                                                             
*                                                                               
FCH2     MVI   MFCTL,X'09'         CTRL RECORD - SET TO PRINT/SPACE1            
         CLI   MFREC+3,C'F'                                                     
         BNE   FCH3                                                             
         MVI   CCWPRNT,X'8B'       SKIP TO CHNL1 IF FICHE BREAK REC             
         LA    R1,CCBPRNT                                                       
         EXCP  (1)                                                              
         WAIT  (1)                                                              
         MVI   FLGPRNT,X'80'       SET PRINT ON FLAG                            
         B     FCH4                                                             
*                                                                               
FCH3     TM    UPSI,X'20'          TEST FOR FIRST PAGE ONLY                     
         BZ    FCH4                                                             
         MVI   FLGPRNT,X'00'       SET PRINT OFF FLAG                           
         B     FCH1                                                             
*                                                                               
FCH4     TM    UPSI,X'40'          TEST FOR PRINT CTRL RECS                     
         BO    FCH6                                                             
         B     FCH1                                                             
*                                                                               
FCH5     TM    FLGPRNT,X'80'       DATA RECORD                                  
         BZ    FCH1                                                             
         TM    UPSI,X'80'          TEST FOR PRINT DATA RECS                     
         BZ    FCH1                                                             
*                                                                               
FCH6     MVC   CCWPRNT(1),MFCTL    PRINT TAPE RECORD                            
         LA    R1,CCBPRNT                                                       
         EXCP  (1)                                                              
         WAIT  (1)                                                              
         B     FCH1                                                             
*                                                                               
FCHEOF   B     FCHCLS                                                           
         EJECT                                                                  
DUB      DC    D'0'                                                             
CCBPRNT  CCB   SYSLST,0,X'0200'                                                 
CCWPRNT  CCW   X'01',0,X'20',132                                                
*                                                                               
PANAM    DC    C'&&&&&&'                                                        
UPSI     DC    X'00'                                                            
FLGPRNT  DC    X'80'                                                            
*                                                                               
MFREC    DS    0CL133                                                           
MFCTL    DS    CL1                                                              
MFDATA   DS    CL132                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
MFTAP    DTFMT DEVADDR=SYS008,TYPEFLE=INPUT,EOFADDR=FCHEOF,FILABL=NO,  X        
               REWIND=UNLOAD,IOAREA1=MFIOA,WORKA=YES,                  X        
               RECFORM=FIXBLK,BLKSIZE=3325,RECSIZE=133                          
*                                                                               
MFIOA    DS    3325C                                                            
*                                                                               
FCHWORK  DS    100D                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001DDFICHEP  10/08/81'                                      
         END                                                                    
