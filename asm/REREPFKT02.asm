*          DATA SET REREPFKT02 AT LEVEL 052 AS OF 01/30/96                      
*PHASE REFK02A,*                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'REREPFKTP  (REFK02A) --- ACTUALIZER '                           
*                                                                               
********************************************************************            
*                                                                  *            
*        REREPFKTP  -- TRANSFORM AND SEND EDI TAPES BACK TO KATZ   *            
*                                                                  *            
* ---------------------------------------------------------------- *            
* UPDATE HISTORY:                                                  *            
* JAN26/96 (ABBEY) --- ORIGINAL ENTRY                              *            
*                                                                  *            
*                    **  END TOMBSTONE  **                         *            
********************************************************************            
*  RUN-TIME SWITCHES AND INPUT VALUES:                             *            
*      QUESTOR    =   Y   PRINT CONTRACT DETAIL                    *            
*      QUESTOR+1  =   Y   PRINT ACTUAL DATA                        *            
********************************************************************            
*                                                                               
         PRINT NOGEN                                                            
REFK02   CSECT                                                                  
         NMOD1 0,**REFK**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         ST    R5,RELO                                                          
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   MAIN100                                                          
*                                                                               
MAIN     GOTO1 INITIAL,DMCB,(RC)   ESTABLISH WORK AREAS                         
         BAS   RE,CHNGREC                                                       
MAIN100  DS    0H                                                               
MAINX    XIT1                                                                   
         EJECT                                                                  
*   INITIALIZATIONS ....                                                        
         DS    0H                                                               
INITIAL  NTR1                                                                   
         MVC   P+1(23),=C'ACKNOWLEDGEMENT TO KATZ'                              
         GOTO1 REPORT                                                           
         OPEN  (FILOUTA,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (INTAPE,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,ADCONLST                                                      
         USING ADCONSD,RF                                                       
         L     RF,COVAIL                                                        
         DROP  RF                                                               
         GOTO1 (RF),DMCB,C'GET',100000,100000                                   
*                                  GET 100K STORAGE SPACE                       
         OC    P2(4),P2                                                         
         BNZ   INIT0020                                                         
         DC    H'0'                                                             
INIT0020 EQU   *                                                                
         L     RF,P2               INITIALIZE THE WORK AREA                     
         XCEFL 0(RF),P3                                                         
         MVC   LBLDAREA,P3         L(ADD'L SPACE GIVEN)                         
         MVC   ARECAREA,P2         TAPE RECORD DELIVERY AREA                    
         L     RF,ARECAREA                                                      
         A     RF,=F'40000'        TAPE BUFFER AREA:                            
*                                                                               
         B     MAINX                                                            
*----------------------------------------------------------------*              
*  CHNGREC : MAKE ANY CHANGES NECESSARY TO THE RECORD            *              
*----------------------------------------------------------------*              
         DS    0H                                                               
CHNGREC  NTR1                                                                   
CR00     DS    0H                                                               
         L     R4,ARECAREA                                                      
         USING DKHEADR1,R4                                                      
CR100    GET   INTAPE,(R4)                                                      
         LA    R2,RECTOPUT+1                                                    
         LA    R3,528                                                           
         LR    R5,R3                                                            
         MVCL  R2,R4                                                            
         L     R4,ARECAREA                                                      
         CLI   DKHDTYP,DKHDTYPQ    HEADER ONE REC?                              
         BE    CR500                                                            
         CLI   DKHDTYP,DKH2TYPQ    HEADER TWO REC?                              
         BE    CR600                                                            
         CLI   DKHDTYP,DKH3TYPQ    HEADER THREE REC?                            
         BE    CR700                                                            
         CLI   DKHDTYP,DKDETYPQ    DETAIL?                                      
         BNE   CR150                                                            
         CLC   DKHDREF(4),=C'9999' DETAIL COMMENT?                              
         BNE   CR800                                                            
         B     CR00                                                             
CR150    CLI   DKHDTYP,DKTTYPQ     TRAILER?                                     
         BE    CR900                                                            
         DC    H'0'                DIE -> RECORD ISN'T ANYTHING WE KNOW         
*                                                                               
         DROP  R4                                                               
CR500    MVI   RECTOPUT,C'1'       HEADER ONE REC                               
         LA    R5,RECTOPUT+1                                                    
         USING DKHEADR1,R5                                                      
         MVI   DKHDMOVI,C' '                                                    
         DROP  R5                                                               
         BAS   RE,PUTRECS          PUT THIS TO KATZ ACKNOWLDGMT                 
*PUT THIS RECORD TO THE TAPE BACK TO KATZ CONFIRMATION                          
         B     CR00                                                             
*                                                                               
CR600    MVI   RECTOPUT,C'1'       HEADER TWO REC                               
         LA    R5,RECTOPUT+1                                                    
         USING DKHEADR2,R5                                                      
*PUT THIS RECORD TO THE TAPE BACK TO KATZ CONFIRMATION                          
         MVI   DKH2TIND,C' '                                                    
         MVC   DKH2CTRY,SPACES                                                  
         MVC   DKH2CTRL,SPACES                                                  
         MVI   DKH2DVCD,C' '                                                    
         DROP  R5                                                               
         BAS   RE,PUTRECS          PUT TO KATZ ACKNOWLEDGEMENT                  
         B     CR00                                                             
*                                                                               
CR700    MVI   RECTOPUT,C'1'       HEADER THREE REC                             
         LA    R5,RECTOPUT                                                      
         USING DKHEADR3,R5                                                      
         MVI   DKH3FILL,C' '                                                    
         DROP  R5                                                               
         BAS   RE,PUTRECS          PUT TO KATZ ACKNOWLEDGEMENT                  
*PUT THIS RECORD TO THE TAPE BACK TO KATZ CONFIRMATION                          
         B     CR00                                                             
*                                                                               
CR800    MVI   RECTOPUT,C'1'       DETAIL RECORD/ACKNOWLEDGEMENT                
         LA    R5,RECTOPUT+1                                                    
         USING DKDETAIL,R5                                                      
         MVC   TEMPREC(90),DKDSPTS                                              
         MVC   DKDSPTS(78),SPACES                                               
         MVC   DKDSPTS+78(90),TEMPREC                                           
         BAS   RE,PUTRECS          PUT TO KATZ ACKNOWLEDGEMENT                  
*        MVI   DKDFILL,COST EXCEPTION                                           
*PUT THIS RECORD TO THE TAPE BACK TO KATZ CONFIRMATION                          
         B     CR00                                                             
*                                                                               
CR900    MVC   RECTOPUT(2),=C'99'  TRAILER RECORD/ACKNOWLEDGEMENT               
         BAS   RE,PUTRECS          PUT TO KATZ ACKNOWLEDGEMENT                  
*PUT THIS RECORD TO THE TAPE BACK TO KATZ CONFIRMATION                          
         B     CR00                                                             
*                                                                               
CRX      CLOSE FILOUTA             CLOSE OUTPUT FILES                           
         CLOSE (INTAPE,REWIND)     CLOSE INPUT FILE                             
*                                                                               
         B     MAINX                                                            
         EJECT                                                                  
*                                                                               
*----------------------------------------------------------------*              
*  PUTRECS:  GENERATE OUTFILE ENTRIES                            *              
*----------------------------------------------------------------*              
*                                                                               
         DS    0H                                                               
PUTRECS  NTR1                                                                   
*                                                                               
         PUT   FILOUTA,RECTOPUT    PUT RECORD TO OUTPUT                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R4,RECTOPUT         A(RECORD LENGTH FIELD)                       
         GOTO1 =V(PRNTBL),DMCB,(0,(R4)),(R4),C'DUMP',506,=C'1D'                 
         GOTO1 REPORT                                                           
*                                  DISPLAY 80 CHARACTER RECORD                  
PRX      B     MAINX                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
RELO     DS    F                   RELOCATION FACTOR                            
ARECAREA DS    A                   TAPE RECORD DELIVERY AREA                    
LBLDAREA DS    F                                                                
RECTOPUT DS    CL529               AREA FOR RECORD                              
TEMPREC  DS    CL90                                                             
*                                                                               
INTAPE   DCB   DDNAME=INTAPE,DSORG=PS,RECFM=FB,LRECL=529,              X        
               BLKSIZE=21160,MACRF=GM,EODAD=CRX                                 
FILOUTA  DCB   DDNAME=FILOUTA,DSORG=PS,RECFM=FB,MACRF=PM,              X        
               LRECL=529,BLKSIZE=21160,BUFNO=2                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*  INCLUDE REGENCON                CONTRACT RECORD                              
*  INCLUDE REREPWORKD                                                           
*  INCLUDE REREPMODES                                                           
*                                                                               
FILED    DSECT                                                                  
RECORD   DS    CL2048                                                           
         ORG   RECORD                                                           
       ++INCLUDE REREPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE DDCOMFACSD                                                     
         EJECT                                                                  
       ++INCLUDE DRKZEDIIND        RECORDS IN FROM TAPE                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'052REREPFKT0201/30/96'                                      
         END                                                                    
