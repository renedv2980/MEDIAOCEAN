*          DATA SET CTFAX      AT LEVEL 031 AS OF 05/30/96                      
*PHASE CTFAX                                                                    
*INCLUDE STXITER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE REGSAVE                                                                
CTFAX    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*CTFAX**,=V(REGSAVE)                                           
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         B     MAIN                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(CTFAX,65000)                                                   
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG   *                                                                
         EJECT                                                                  
*                                                                               
MAIN     DS    0H                                                               
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',=C'NCTFILE X',  +        
               IO,0                                                             
         MVI   DATADISP+1,28                                                    
*                                                                               
         GOTO1 COVRPG,DMCB,=X'0011',=C'DEIS   ',=C'SDE',FAX,TABLE               
         MVC   P(16),FAX                                                        
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PRTHDR                                                        
*                                                                               
         GOTO1 COVRPG,DMCB,=X'0011',=C'**SDE  ',0,FAX,TABLE                     
         MVC   P(16),FAX                                                        
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PRTHDR                                                        
*                                                                               
         GOTO1 COVRPG,DMCB,=X'0011',=C'HILL   ',=C'SP ',FAX,TABLE               
         MVC   P(16),FAX                                                        
         GOTO1 =V(PRINTER)                                                      
         BAS   RE,PRTHDR                                                        
         XBASE                                                                  
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                       COVER PAGE                                    *         
*                                                                     *         
*        PARAM 1:  AGENCY CODE                                        *         
*        PARAM 2:  FAX CODE                                           *         
*        PARAM 3:  REQUESTOR                                          *         
*        PARAM 4:  ADDRESS TO PUT FAX NUMBER                          *         
*        PARAM 4:  ADDRESS OF TABLE TO PUT COVER PAGE                 *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
COVRPG   NTR1                                                                   
         L     R2,16(R1)           ADDRESS OF TABLE                             
         LA    R4,MAXLINES         MAXIMUM NUMBER OF LINES                      
COVRPG10 MVC   0(132,R2),SPACES    CLEAR TABLE                                  
         LA    R2,132(R2)                                                       
         BCT   R4,COVRPG10                                                      
*                                                                               
         L     R1,DMCB+4                                                        
         MVC   FAXCODE,0(R1)                                                    
         XC    REQUESTR,REQUESTR                                                
         ZICM  R1,DMCB+8,4                                                      
         BZ    *+10                                                             
         MVC   REQUESTR,0(R1)                                                   
         L     R5,DMCB+12          ADDRESS TO STORE FAX NUMBER                  
         L     R2,DMCB+16          ADDRESS OF TABLE                             
*                                                                               
         XC    KEY,KEY             READ ID RECORD                               
         LA    R6,KEY                                                           
         USING CTIREC,R6                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         L     R1,DMCB                                                          
         MVC   CTIKNUM,0(R1)                                                    
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO                                                            
         CLC   CTIKEY,KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   ELCODE,CTDSCELQ     AGENCY NAME                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTDSCD,R6                                                        
         MVC   AGENCY,CTDSC                                                     
         DROP  R6                                                               
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,CTAGYELQ     AGENCY POWERCODE                             
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTAGYD,R6                                                        
         MVC   AGY,CTAGYID                                                      
         DROP  R6                                                               
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,CTDSTELQ     DESTINATION DETAIL                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTDSTD,R6                                                        
         MVC   NAME,CTDSTNAM       AGENCY NAME                                  
         MVC   ADDR,CTDSTADD       AGENCY ADDRESS                               
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY             READ FAX RECORD                              
         LA    R6,KEY                                                           
         USING CTFXREC,R6                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,AGY                                                      
         MVC   CTFXCODE,FAXCODE                                                 
*                                                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO                                                            
         CLC   CTFXKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                SHOULD BE THERE ( CHANGE THIS?)              
*                                                                               
         MVC   34(10,R2),=C'FAX HEADER'                                         
         LA    R2,132(R2)                                                       
         MVC   34(10,R2),=C'----------'                                         
         LA    R2,132(R2)                                                       
         LA    R2,132(R2)                                                       
         MVC   21(8,R2),=C'SENT TO:'                                            
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'02'        ATTENTION ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   COVRPG20                                                         
         USING CTFXATT,R6                                                       
*                                                                               
         ZIC   R1,CTFX2LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   34(0,R2),CTFX2ATT                                                
         LA    R2,132(R2)                                                       
         DROP  R6                                                               
*                                                                               
COVRPG20 LA    R6,IO                                                            
         USING CTFXREC,R6                                                       
         MVC   34(4,R2),=C'FAX='                                                
*                                                                               
         XC    0(16,R5),0(R5)                                                   
         ZIC   R1,CTFX1LEN         FAX NUMBER                                   
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   0(0,R5),CTFX1NUM    SAVE FAX NUMBER                              
*                                                                               
         EX    R1,*+4                                                           
         MVC   38(0,R2),CTFX1NUM                                                
         LA    R1,38(R2)                                                        
         BAS   RE,EDITTEL                                                       
*                                                                               
         LA    R2,132(R2)                                                       
         LA    R2,132(R2)                                                       
         LR    R4,R2                                                            
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'03'        MESSAGE ELEMENTS                             
         BAS   RE,GETEL                                                         
         BNE   COVRPG40                                                         
         USING CTFXMSG,R6                                                       
*                                                                               
COVRPG30 ZIC   R3,CTFX3LIN                                                      
         BCTR  R3,0                                                             
         MH    R3,=H'132'                                                       
         LR    R2,R4                                                            
         AR    R2,R3               POINT TO CORRECT LINE FOR MESSAGE            
*                                                                               
         ZIC   R1,CTFX3LEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+4                                                           
         MVC   34(0,R2),CTFX3MSG                                                
         LA    R2,132(R2)                                                       
         BAS   RE,NEXTEL                                                        
         BE    COVRPG30                                                         
         DROP  R6                                                               
*                                                                               
COVRPG40 DS    0H                                                               
         LA    R2,132(R2)                                                       
         MVC   21(10,R2),=C'SENT FROM:'                                         
*                                                                               
         MVI   FOUNDREC,C'N'                                                    
         OC    REQUESTR,REQUESTR                                                
         BZ    COVRPG50                                                         
         XC    KEY,KEY             READ FAX RECORD FOR REQUESTOR                
         LA    R6,KEY                                                           
         USING CTFXREC,R6                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,AGY                                                      
         MVC   CTFXCODE(2),=C'**'  LOOK FOR **RRR (REQUESTOR)                   
         MVC   CTFXCODE+2(3),REQUESTR                                           
         MVC   CTFXCODE+5(2),SPACES                                             
*                                                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,IO                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IO                                                            
         CLC   CTFXKEY,KEYSAVE                                                  
         BNE   COVRPG50            REQUESTOR DOESN'T HAVE A FAX ELEMENT         
*                                                                               
         MVI   FOUNDREC,C'Y'                                                    
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   COVRPG20                                                         
         USING CTFXATT,R6                                                       
*                                                                               
         ZIC   R1,CTFX2LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   34(0,R2),CTFX2ATT                                                
         LA    R2,132(R2)                                                       
         DROP  R6                                                               
*                                                                               
COVRPG50 DS    0H                                                               
         MVC   34(33,R2),NAME                                                   
         LA    R2,132(R2)                                                       
         MVC   34(33,R2),ADDR                                                   
         LA    R2,132(R2)                                                       
         MVC   34(3,R2),=C'ID='                                                 
         MVC   37(10,R2),AGENCY                                                 
         LA    R2,132(R2)                                                       
*                                                                               
         OC    REQUESTR,REQUESTR                                                
         BZ    COVRPG80                                                         
         MVC   34(10,R2),=C'REQUESTOR='                                         
         MVC   44(3,R2),REQUESTR                                                
         LA    R2,132(R2)                                                       
*                                                                               
         CLI   FOUNDREC,C'Y'       WAS THERE A FAX RECORD?                      
         BNE   COVRPG80                                                         
         LA    R6,IO                                                            
         MVI   ELCODE,X'04'        RETURN NUMBER ELELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   COVRPG60                                                         
         USING CTFXTEL,R6                                                       
*                                                                               
         MVC   34(10,R2),=C'TELEPHONE='                                         
         ZIC   R1,CTFX4LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   44(0,R2),CTFX4TEL                                                
         LA    R1,44(R2)                                                        
         BAS   RE,EDITTEL                                                       
         LA    R2,132(R2)                                                       
         DROP  R6                                                               
*                                                                               
COVRPG60 DS    0H                                                               
         LA    R6,IO                                                            
         USING CTFXREC,R6                                                       
*                                                                               
         MVC   34(4,R2),=C'FAX='   FAX NUMBER                                   
         ZIC   R1,CTFX1LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   38(0,R2),CTFX1NUM                                                
         LA    R1,38(R2)                                                        
         BAS   RE,EDITTEL                                                       
*                                                                               
         LA    R2,132(R2)                                                       
         LA    R2,132(R2)                                                       
         LR    R4,R2                                                            
*                                                                               
         MVI   ELCODE,X'03'        MESSAGE ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   COVRPG80                                                         
         USING CTFXMSG,R6                                                       
*                                                                               
COVRPG70 ZIC   R3,CTFX3LIN                                                      
         BCTR  R3,0                                                             
         MH    R3,=H'132'                                                       
         LR    R2,R4                                                            
         AR    R2,R3               POINT TO CORRECT LINE FOR MESSAGE            
*                                                                               
         ZIC   R1,CTFX3LEN                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+4                                                           
         MVC   34(0,R2),CTFX3MSG                                                
         LA    R2,132(R2)                                                       
         BAS   RE,NEXTEL                                                        
         BE    COVRPG70                                                         
         DROP  R6                                                               
*                                                                               
COVRPG80 DS    0H                                                               
         LA    R2,132(R2)                                                       
         MVC   21(12,R2),=C'FAX SENT ON:'             DATE                      
         GOTO1 =V(DATCON),DMCB,(5,0),(8,34(R2))                                 
         LA    R2,132(R2)                                                       
         MVC   21(10,R2),=C'TIME SENT:'                                         
         THMS  DDSTIME=YES                                                      
         ST    R1,FULL             0HHMMSS+ (DDS TIME)                          
         ST    R0,PACKOF4B         0060000+ (DDS CLOCK TIME DIFFERENCE)         
         AP    FULL,PACKOF4B       FULL = ACTUAL TIME                           
         XC    DUB,DUB                                                          
         MVC   DUB+5(3),FULL                                                    
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         C     R1,=F'2400'                                                      
         BL    *+8                                                              
         S     R1,=F'2400'                                                      
         EDIT  (R1),(5,34(R2)),2                                                
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                       EDIT TELEPHONE NUMBER                         *         
*        PUT IN () & -                                                *         
*        R1=A(CL25 TELEPHONE)                                         *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
EDITTEL  NTR1                                                                   
         MVC   WORK(25),0(R1)                                                   
         MVC   0(25,R1),SPACES                                                  
         LA    R2,WORK                                                          
         CLC   WORK+7(4),SPACES    IF > 7 CHARACTERS                            
         BE    EDITEL20                                                         
         CLI   WORK,C'1'                                                        
         BH    EDITEL10                                                         
         MVC   0(2,R1),=C'1-'                                                   
         LA    R1,2(R1)                                                         
         LA    R2,1(R2)                                                         
*                                                                               
EDITEL10 MVI   0(R1),C'('          ASSUME AREA CODE TO START                    
         MVC   1(3,R1),0(R2)                                                    
         MVI   4(R1),C')'                                                       
         LA    R2,3(R2)                                                         
         LA    R1,6(R1)                                                         
*                                                                               
EDITEL20 MVC   0(3,R1),0(R2)                                                    
         MVI   3(R1),C'-'                                                       
         MVC   4(10,R1),3(R2)                                                   
         B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                        PRINT COVER PAGE                             *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRTHDR   NTR1                                                                   
         LA    R2,TABLE                                                         
         LA    R4,MAXLINES                                                      
PRTHDR10 MVC   P(132),0(R2)                                                     
         GOTO1 =V(PRINTER)                                                      
         LA    R2,132(R2)                                                       
         BCT   R4,PRTHDR10                                                      
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
TOUT     DCB   DDNAME=TOUT,DSORG=PS,MACRF=(PM),                        X        
               RECFM=VB,LRECL=2048                                              
*                                                                               
SSB      DC    F'0'                                                             
UTL      DC    F'0',X'0A'                                                       
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
PACKOF4B DS    PL4                                                              
DMCB     DS    6F                                                               
DATADISP DS    H                                                                
AGY      DS    CL2                                                              
AGENCY   DS    CL10                                                             
REQUESTR DS    CL3                                                              
FOUNDREC DS    C                                                                
FAXCODE  DS    CL7                                                              
FAX      DS    CL16                                                             
NAME     DS    CL33                                                             
ADDR     DS    CL33                                                             
ELCODE   DS    CL1                                                              
KEY      DS    CL25                                                             
KEYSAVE  DS    CL25                                                             
WORK     DS    CL64                                                             
IO       DS    XL1500              IO AREA                                      
TABLE    DS    CL(MAXLINES*132)                                                 
MAXLINES EQU   33                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031CTFAX     05/30/96'                                      
         END                                                                    
