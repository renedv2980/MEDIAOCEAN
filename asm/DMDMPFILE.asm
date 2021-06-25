*          DATA SET DMDMPFILE  AT LEVEL 003 AS OF 05/06/14                      
         TITLE 'INITIALISE DUMP FILE WHENEVER IT IS CHANGED/MOVED'              
**********************************************************************          
* THE FOLLOWING NUMBER COMES FROM FASTART - WHERE IT SETS SSBDMPCY              
* BEFORE YOU RUN THIS PROGRAM YOU MUST MAKE SURE IT IS CURRENT                  
**********************************************************************          
*                                                                               
* DMPCYLQ AND DMPBLKQ COME FROM FADMPHDR AND ARE USED IN                        
* FAABEND, FADUMP, FASTART                                                      
* THEY NEED TO BE IN SYNC                                                       
*                                                                               
*PHASE DMPFILEA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATTIM                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*                                                                               
         PRINT NOGEN                                                            
DMPFILE  CSECT                                                                  
         NBASE 0,DMPFILE,=V(REGSAVE)                                            
         ENTRY SSB                                                              
         ENTRY UTL                                                              
*                                                                               
         USING DMPHDRD,DMPH                                                     
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         BRAS  RE,INPUT            READ CARDS                                   
         BNE   EXIT                BAD CARD                                     
*                                                                               
         LA    R9,DMPFIL                                                        
         USING DTFPHD,R9                                                        
         GOTO1 VDADDS,P1,VDAOPEN,,,DMPFIL                                       
         MVC   P(30),=CL30'DUMP FILE OPENED'                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         BRAS  RE,DMPRPT           GET NUMBER OF DUMPS IN THIS FILE             
         MVC   P(30),=CL30'DUMP FILE REPORTED'                                  
         GOTO1 VPRINTER                                                         
         LHI   R3,1                R3 = CURRENT DUMP NUMBER                     
*                                                                               
MAIN02   C     R3,DNUMDMPS         CURRENT DUMP NUMBER                          
         BH    CLOSE                                                            
*                                                                               
         BRAS  RE,SETHDR                                                        
         XR    R0,R0                                                            
         LR    R1,R3                                                            
         BCTR  R1,0                                                             
         LHI   RF,DMPCYLQ                                                       
         MR    R0,RF                                                            
         L     RF,DTRKCYL                                                       
         MR    R0,RF                                                            
*                                                                               
         AHI   R1,1                                                             
         SLL   R1,16                                                            
         ST    R1,DNEXT            SET DISK ADDRESS OF START OF DUMP            
         ST    R1,DISKADDR                                                      
         MVI   DISKADDR+2,1                                                     
*                                                                               
         MVC   P1,=AL4(VWTCKD)     WRITE HEADER RECORD TO FIRST TRACK           
         LA    RE,DMPHDR                                                        
         ST    RE,P2                                                            
         LA    RE,DMPHDRL          SIZE OF HEADER REC                           
         ST    RE,P3                                                            
         LA    RE,DISKADDR                                                      
         ST    RE,P5                                                            
         GOTO1 VDADDS,P1                                                        
         OC    P3(2),P3            TEST FOR ERRORS                              
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P(30),=CL30'DUMP HEADER WRITTEN'                                 
         GOTO1 VPRINTER                                                         
*                                                                               
         AHI   R3,1                                                             
         B     MAIN02                                                           
*                                                                               
CLOSE    GOTO1 VDADDS,P1,15,,,DMPFIL     CLOSE THE FILE                         
         MVC   P(30),=CL30'DUMP FILE CLOSED'                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         XBASE ,                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COUNT THE TOTAL NUMBER OF TRACKS IN THE FILE AND SET     *         
* THE NUMBER OF DUMPS THAT CAN BE CONTAINED IN IT                     *         
***********************************************************************         
DMPRPT   NTR1  ,                                                                
         LHI   R0,DMPBLKQ                                                       
         GOTO1 VDADDS,P1,VDARPRT,0,(R0),(R9)                                    
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,P3+2                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    RF,DRECTRK          SAVE RECORDS PER TRACK                       
         L     RF,P2                                                            
         LH    RF,2(RF)                                                         
         ST    RF,DTRKCYL          SAVE TRKS PER CYL                            
*                                                                               
         LA    RF,DMTX                                                          
         TM    DIND,DINDXAM        HIGH CORE EXTENT MATRIX                      
         BZ    *+8                                                              
         ICM   RF,15,DMTX                                                       
                                                                                
         USING EXTENTD,RF                                                       
         SAM31                                                                  
         XR    RE,RE                                                            
         XR    R0,R0                                                            
DIN02    CLI   0(RF),X'FF'                                                      
         BE    DIN04                                                            
         ICM   R0,3,EXT#TRKS                                                    
         AR    RE,R0                                                            
         AHI   RF,EXTLNQ                                                        
         B     DIN02                                                            
         DROP  RF                                                               
*                                                                               
DIN04    SAM24                                                                  
         ST    RE,DTOTTRKS                                                      
         SRDL  RE,32                                                            
         D     RE,DTRKCYL          RF=NUM OF CYLS IN DMPFILE                    
         ST    RF,DTOTCYLS                                                      
*                                                                               
         XR    RE,RE                                                            
         D     RE,=A(DMPCYLQ)                                                   
         ST    RF,DNUMDMPS         SET NUMBER OF DUMPS FILE WILL HOLD           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET INFORMATION IN DUMP HEADER                           *         
* NTRY:  R3    = DUMP NUMBER TO SET                                   *         
***********************************************************************         
SETHDR   NTR1  ,                                                                
         LA    R0,DMPHDR           CLEAR HEADER                                 
         LHI   R1,DMPHDRL                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         STC   R3,DMPNUM           SET DUMP NUMBER                              
         MVI   DMPCYL,DMPCYLQ                                                   
         MVC   DMPDESC,=CL8'NEW INIT'                                           
         MVI   DMPSTAT,C'N'                                                     
         MVC   DMPWHO,SPACES                                                    
         GOTO1 VDATIM,DMCB,(X'01',DMPTIME)                                      
         GOTO1 VDATCON,DMCB,(X'05',0),(X'03',DMPHDTE)                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* READ INPUT CARDS                                                    *         
***********************************************************************         
INPUT    NTR1  ,                                                                
*                                                                               
INPUTNXT GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   =C'*/',CARD                                                      
         BE    INPUTOK                                                          
         CLC   =C'XX',CARD                                                      
         BE    INPUTOK                                                          
         CLI   CARD,C'*'                                                        
         BE    INPUTNXT            COMMENT LINE                                 
         CLC   =C'DDSIO=',CARD                                                  
         BNE   INPUT02                                                          
         L     RE,=V(DDSIO)                                                     
         MVC   0(8,RE),CARD+6      DDSIO OVER-RIDE                              
         B     INPUTNXT                                                         
*                                                                               
         USING SSBD,RE                                                          
INPUT02  CLC   =C'DSPACE=',CARD                                                 
         JNE   INPUTERR                                                         
         L     RE,=A(SSB)                                                       
         MVC   SSODSPAC,CARD+7 DSPACE VALUE                                     
         B     INPUTNXT                                                         
*                                                                               
INPUTERR WTO   'INVAILD INPUT CARD'                                             
         LTR   RE,RE               SET CC NOT EQUAL                             
         B     EXIT                                                             
*                                                                               
INPUTOK  SR    RE,RE                                                            
         B     EXIT                                                             
         DROP  RE                  SSB                                          
         EJECT ,                                                                
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
EXIT     XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
VDAOPEN  EQU   X'0E'                                                            
VDACLOSE EQU   X'0F'                                                            
VDARPRT  EQU   X'10'                                                            
VWTCKD   EQU   X'05'                                                            
VDATIM   DC    V(DATTIM)                                                        
VDATCON  DC    V(DATCON)                                                        
VCPRINT  DC    V(CPRINT)                                                        
VDADDS   DC    V(DADDS)                                                         
VPRINTER DC    V(PRINTER)                                                       
VPRNTBL  DC    V(PRNTBL)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VCARDS   DC    V(CARDS)                                                         
*                                                                               
DRECTRK  DC    A(0)                                                             
DTRKCYL  DC    A(0)                                                             
DTOTTRKS DC    A(0)                                                             
DTOTCYLS DC    A(0)                                                             
DNUMDMPS DC    A(0)                                                             
*                                                                               
DISKADDR DC    F'0'                                                             
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
P1       DC    F'0'                                                             
P2       DC    F'0'                                                             
P3       DC    F'0'                                                             
P4       DC    F'0'                                                             
P5       DC    F'0'                                                             
P6       DC    F'0'                                                             
P7       DC    F'0'                                                             
P8       DC    F'0'                                                             
*                                                                               
WORK     DC    20F'0'                                                           
*                                                                               
CARD     DC    CL80' '                                                          
         DS    0D                                                               
         DC    CL8'DMPHDR*'                                                     
DMPH     DC    (DMPHDRL)X'00'                                                   
*                                                                               
         DS    0D                                                               
         DC    CL8'DMPFIL*'                                                     
DMPFIL   DMDA                                                                   
*                                                                               
         DS    0L                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DC    X'0000',X'FF',1052X'00'                                          
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    256X'00'                                                         
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED BOOKS                                                *         
***********************************************************************         
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DMXTNTD                                                        
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE FADMPHDR                                                       
       ++INCLUDE FASSB                                                          
         ORG   SSBD                                                             
       ++INCLUDE FASSBOFF                                                       
         ORG                                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DMDMPFILE 05/06/14'                                      
         END                                                                    
